# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Check modeled values"
#' author:
#'   - Natsis Athanasios^[Laboratory of Atmospheric Physics, AUTH, natsisphysicist@gmail.com]
#'
#' documentclass:  article
#' classoption:    a4paper,oneside
#' fontsize:       10pt
#' geometry:       "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#' link-citations: yes
#' colorlinks:     yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections: no
#'     fig_caption:     no
#'     keep_tex:        yes
#'     keep_md:         yes
#'     latex_engine:    xelatex
#'     toc:             yes
#'     toc_depth:       4
#'     fig_width:       7
#'     fig_height:      4.5
#'   html_document:
#'     toc:             true
#'     keep_md:         yes
#'     fig_width:       7
#'     fig_height:      4.5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#+ echo=F, include=T


## __ Document options ---------------------------------------------------------

#+ echo=FALSE, include=TRUE
knitr::opts_chunk$set(comment    = ""       )
# knitr::opts_chunk$set(dev        = c("pdf", "png")) ## expected option
# knitr::opts_chunk$set(dev        = "pdf"    )       ## for too much data
knitr::opts_chunk$set(dev        = "cairo_pdf"    )       ## for too much data
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
knitr::opts_chunk$set(fig.pos    = '!h'     )

#+ echo=FALSE, include=TRUE
## __ Set environment ----------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./Check_model.R"
tic <- Sys.time()

if (!interactive()) {
    dir.create("./runtime/", showWarnings = FALSE)
    pdf( file = paste0("./runtime/", basename(sub("\\.R$",".pdf", Script.Name))))
}

REMOTE <- FALSE
# REMOTE <- TRUE
if (REMOTE & !isTRUE(getOption('knitr.in.progress'))) {
    devoutput <- sub("\\.R", "_files", Script.Name)
    warning("Working R in remote mode")
    png(
        filename = paste0(devoutput, "/Rplot%04d.png"),
        width    = 960,
        height   = 540
        )
}

#+ echo=FALSE, include=TRUE
library(data.table  , quietly = TRUE, warn.conflicts = FALSE)
library(pander      , quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2     , quietly = TRUE, warn.conflicts = FALSE)
library(RColorBrewer, quietly = TRUE, warn.conflicts = FALSE)
library(pracma      , quietly = TRUE, warn.conflicts = FALSE)
library(janitor     , quietly = TRUE, warn.conflicts = FALSE)
library(forcats     , quietly = TRUE, warn.conflicts = FALSE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

source("~/MANUSCRIPTS/02_2024_enhancement/GHI_enh_00_dictionary.R")

## Override notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'An error occurred!'"))
    }
})



DT <- readRDS("./Model_CS.Rds")
DT <- readRDS("./Model_CS_2.Rds")
DT <- data.table(remove_constant(DT))

## suspicious data
if (!is.null(DT$lambda)) {
    table(DT$lambda)
    table(DT[lambda != 280, sza])
}


## remove extreme szas
DT <- DT[ sza <= 78 & sza >= 17, ]

DT <- data.table(remove_constant(DT))
DT[, hostname := NULL  ]
DT[, ticTime  := NULL  ]
DT[, tacTime  := NULL  ]


## ignore atmosphere file variations
DT <- DT[ atmosphere_file == "afglms"]


##  Compare GHI  ---------------------------------------------------------------

#'
#' ## Plot global for all months
#'
#' Does not include sun-earth distance and spectrum integral adjustment.
#'
#+ glo-month, include=T, echo=F

DT[, glo := (edir + edn) / 1000 ]

for (am in 1:12) {
    pp <- DT[month == am]

    pp$type <- gsub("Exact W", "WC",      pp$type)
    pp$type <- gsub("Low W", "WC-1σ",     pp$type)
    pp$type <- gsub("Exact B.", "AOD, ",  pp$type)
    pp$type <- gsub("Low B.", "AOD-1σ, ", pp$type)

    pp$atmosphere_file <- gsub("afglms", ", 'summer'", pp$atmosphere_file)
    pp$atmosphere_file <- gsub("afglmw", ", 'winter'", pp$atmosphere_file)

    p <- ggplot(data = pp,
                aes(sza, glo,
                    col = fct_reorder(
                        interaction(type, atmosphere_file, sep = " "),
                        glo, mean, .desc = T)
                )
    ) +
        geom_line(linewidth = 1.1) +
        ylab(expression(paste("Clear sky GHI [", W/m^{2},"]"))) +
        xlab("SZA") +
        scale_color_discrete(name = "") +
        labs(title = month.name[am] ) +
        theme_bw()

    p + theme(aspect.ratio = 1)
    print(p)

}




#'
#' ## Plot global relative to a month/atmosphere
#'
#' Does not include sun-earth distance and spectrum integral adjustment.
#'
#+ glo-relative, include=T, echo=F


# ## auto select
# base <- DT[which.max(sum(glo)), sum(glo), by = .(month ,type, atmosphere_file) ]
#
# refdt <- DT[ type == base$type & atmosphere_file == base$atmosphere_file & month == base$month,
#              .(sza, glo, atmosphere_file, month, type)]
# names(refdt)[names(refdt) == "glo"] <- "refglo"
#
# refdt <- unique(refdt)
#
# refdt <- remove_constant(refdt)
#
# DT2 <- data.table(dplyr::left_join(DT, refdt, by = "sza"))
#
# DT2[, glorel := glo / refglo]


## select type
bb <- DT[type == "Exact B.Exact W", sum(glo), by = .(month, type, atmosphere_file) ]
## select brighter month and atmosphere
base <- bb[ which.max(V1)]
## get values
refdt <- DT[type == base$type & atmosphere_file == base$atmosphere_file & month == base$month,
             .(sza, glo, atmosphere_file, month, type)]
names(refdt)[names(refdt) == "glo"] <- "refglo"

## apply values
refdt$atmosphere_file <- NULL
refdt$type <- NULL
refdt$month <- NULL

DT2 <- merge(DT, refdt)
DT2[, glorel := glo / refglo]

pander(base, caption = "Use as base")


for (am in 1:12) {
    pp <- DT2[month == am]

    p <- ggplot(data = pp,
                aes(sza, glorel,
                    col = fct_reorder(
                        interaction(type, atmosphere_file, sep = " "),
                        glorel, mean, .desc = T)
                )) +
        geom_hline(yintercept = 1, colour = "black", show.legend = F, linetype = 3) +
        geom_line() +
        scale_color_discrete(name = "") +
        coord_cartesian(ylim = range(DT2$glorel)) +
        labs(title = paste(month.name[am], "(ref:", base$type, month.abb[base$month], base$atmosphere_file, ")") ) +
        theme_bw()
    print(p)

}





#'
#' ## Plot global relative to exact tau
#'
#' Does not include sun-earth distance and spectrum integral adjustment.
#'
#+ include=T, echo=F


for (am in 1:12) {
    pp <- DT[month == am]

    # pp <- remove_constant(pp)

    ref <- pp[type == "Exact B.Exact W", .(atmosphere_file, sza, glo) ]

    suppressWarnings(
        pp <- data.table(dplyr::left_join(pp, ref, by = c("atmosphere_file", "sza")))
    )

    pp[, glorel := glo.x / glo.y]

    p <- ggplot(data = pp,
                aes(sza, glorel,
                    col = fct_reorder(
                        interaction(type, atmosphere_file, sep = " "),
                        glorel, mean, .desc = T)
                )) +
        geom_line() +
        scale_color_discrete(name = "") +
        labs(title = month.name[am] ) +
        theme_bw()
    print(p)

}



library(directlabels)


#'
#' ## Plot global differences
#'
#+ include=T, echo=F

for (am in 1:12) {
    pp <- DT[month == am]

    # pp <- remove_constant(pp)

    ref <- pp[type == "Exact B.Exact W", .(atmosphere_file, sza, glo) ]

    suppressWarnings(
        pp <- data.table(dplyr::left_join(pp, ref, by = c("atmosphere_file", "sza")))
    )

    pp[, glodiff := glo.x - glo.y]

    p <- ggplot(data = pp,
                aes(sza, glodiff,
                    col = fct_reorder(
                        interaction(type, atmosphere_file, sep = " "),
                        glodiff, mean, .desc = T)
                )) +
        geom_line() +
        scale_color_discrete(name = "") +
        ylab(expression(paste("Difference ", W/m^{2}))) +
        labs(title = month.name[am] ) +
        scale_x_continuous(expand=c(0, 20)) +
        theme_bw() +
        geom_dl(aes(label = interaction(type, atmosphere_file, sep = " ")),
                method = list(dl.combine("first.points", "last.points"), cex = 0.6)) +
        theme(legend.position = "none")
    print(p)

}







#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
if (interactive() & difftime(tac,tic,units = "sec") > 30) {
    system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
    system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'R script ended'"))
}
