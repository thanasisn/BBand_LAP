#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Daily raw CM-21 data **SIG** "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#'
#' link-citations:  yes
#' colorlinks:      yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     fig_width:        8
#'     fig_height:       5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#'
#'  **SIG**
#'
#' **Source code: [`github.com/thanasisn/BBand_LAP`](`https://github.com/thanasisn/BBand_LAP`)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](`https://thanasisn.netlify.app/3-data_display`)**
#'
#'
#+ echo=F, include=T


#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(fig.pos    = '!h'     )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_db/Plot_daily_CM21_sig.R"

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)


## __  Variables  --------------------------------------------------------------
OUT_FOLDER <- "~/BBand_LAP/REPORTS/DAILY/CM21_signal/"
OUT_PREFIX <- "CM21_signal_"
dir.create(OUT_FOLDER, showWarnings = FALSE, recursive = TRUE)
tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))

## __ Execution control  -------------------------------------------------------
BB_meta  <- read_parquet(DB_META_fl)

metalist <- BB_meta               |>
    select(matches("Day|cm21"))   |>
    filter(!is.na(cm21_basename)) |>
    select("day", cm21_dark_computed)
metalist$year <- year(metalist$day)
metalist <- metalist[, .(updated = max(cm21_dark_computed, na.rm = TRUE)), by = year ]


plotfiles <- data.table(path = list.files(path    = OUT_FOLDER,
                                          pattern = OUT_PREFIX,
                                          full.names  = TRUE,
                                          ignore.case = TRUE))
plotfiles$mtime <- file.mtime(plotfiles$path)
plotfiles$year  <- as.numeric(
    sub("CM21_signal_", "", sub("\\.pdf", "", basename(plotfiles$path))))

selected    <- merge(metalist, plotfiles, all = TRUE)
years_to_do <- selected[is.na(path) | updated > mtime, year ]

# TEST
# years_to_do <- 2022

for (YYYY in sort(years_to_do)) {
    ## load data for year
    year_data <- data.table(opendata() |> filter(year == YYYY) |> collect())
    cat(YYYY, "rows:", nrow(year_data), "\n")
    ## days with data
    daystodo <- year_data[!is.na(CM21_sig), unique(as.Date(Date))]
    daystodo <- sort(daystodo)
    ## signal limit for year
    # ylim <- range(year_data[, .(CM21_sig, CM21_sig_wo_dark)], na.rm = TRUE)

    if (!interactive()) {
        pdffile <- paste0(OUT_FOLDER, "/", OUT_PREFIX, YYYY, ".pdf")
        cat("Ploting:", pdffile, "\n")
        pdf(file = pdffile)
    }

    for (aday in sort(daystodo)) {
        dd   <- year_data[as.Date(Date) == aday]
        aday <- as.Date(aday, origin = "1970-01-01")

        layout(matrix(c(1,2,3,3,3,3), 6, 1, byrow = TRUE))

        ## Night signal
        par("mar" = c(0,4,2,1))

        if  (all(is.na(dd[Elevat < 0, CM21_sig]))) {
            plot.new()
        } else {
            plot(dd[Elevat < 0, Date], dd[Elevat < 0, CM21_sig],
                 ylim = range(dd[Elevat < 0, .(CM21_sig, CM21_sig_wo_dark)], na.rm = TRUE),
                 pch = 19,  cex = 0.5, col = "darkolivegreen",
                 xaxt = "n",
                 xlab = "", ylab = "Night [V]")
            points(dd[Elevat < 0, Date], dd[Elevat < 0, CM21_sig_wo_dark],
                   pch = 19,  cex = 0.5, col = "green")
            abline(h = 0, col = "grey")
        }

        title(paste0("CM-21  doy: ", yday(aday), "  ",
                     aday, "  dark ",
                     tolower(BB_meta[day == aday, chp1_dark_flag])))

        ## Signal SD
        par("mar" = c(0,4,0,1))
        plot(dd$Date, dd$CM21_sig_sd,
             ylim = range(c(0, dd$CM21_sig_sd), na.rm = T),
             pch  = 19,  cex = 0.5, col = "red",
             xaxt = "n", xlab = "", ylab = "Signal SD [V]")
        abline(h = 0, col = "grey", lty = 2)

        ## Signal
        par("mar" = c(3,4,0,1))
        plot(dd$Date, dd$CM21_sig, type = "l",
             ylim = range(dd[, .(CM21_sig, CM21_sig_wo_dark)], na.rm = TRUE),
             lwd  = 1.5,
             pch  = 19,  cex = 0.5, col = "darkolivegreen",
             xlab = "", ylab = "Signal [V]")
        lines(dd$Date, dd$CM21_sig_wo_dark,
              col = "green",)
        abline(h = 0, col = "grey", lty = 2)
        # points(dd$Date, dd$CM21_sig_wo_dark,
        #        pch = 19,  cex = 0.5, col = "blue",)

        text(dd$Date[1], max(dd$CM21_sig, dd$CM21_sig_wo_dark, na.rm = TRUE),
             labels = tag, pos = 4, cex =.9)

        legend("topright", pch = 19, bty = "n",
               legend = c(
                   "Signal",
                   "Signal dark corrected",
                   "Signal SD"),
               col = c("darkolivegreen",
                       "green",
                       "red")
               )
    }
    dev.off()
}




#' **END**
#+ include=T, echo=F
# myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
