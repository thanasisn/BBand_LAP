#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Daily CM-21 radiation data **L1** "
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
#'     toc_depth:        4
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
#'  **L1**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
#'
#+ echo=F, include=T


#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_db/31_Plot_daily_CM21_L1.R"
renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)


## __  Variables  --------------------------------------------------------------
OUT_FOLDER <- "~/BBand_LAP/REPORTS/DAILY/CM21_GLB_L1/"
OUT_PREFIX <- "CM21_global_L1_"
dir.create(OUT_FOLDER, showWarnings = FALSE, recursive = TRUE)
tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))

## __ Execution control  -------------------------------------------------------
BB_meta  <- read_parquet(DB_META_fl)

metalist <- BB_meta               |>
    select(matches("Day|cm21"))   |>
    filter(!is.na(cm21_basename)) |>
    select("day", cm21_dark_computed)
metalist$year <- year(metalist$day)
metalist <- metalist[, .(updated = max(cm21_dark_computed, na.rm = TRUE)), by = year]


plotfiles <- data.table(path = list.files(path    = OUT_FOLDER,
                                          pattern = OUT_PREFIX,
                                          full.names  = TRUE,
                                          ignore.case = TRUE))
plotfiles$mtime <- file.mtime(plotfiles$path)
plotfiles$year  <- as.numeric(
    sub(OUT_PREFIX, "", sub("\\.pdf", "", basename(plotfiles$path))))

selected    <- merge(metalist, plotfiles, all = TRUE)
years_to_do <- selected[is.na(path) | updated > mtime, year ]


# TEST
# years_to_do <- 2004

for (YYYY in sort(years_to_do)) {
    ## load data for year
    year_data <- data.table(opendata() |> filter(year == YYYY) |> collect())
    cat(YYYY, "rows:", nrow(year_data), "\n")
    ## days with data
    daystodo <- year_data[!is.na(GLB_wpsm), unique(as.Date(Date))]
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

        layout(matrix(c(1,2,2,2,2), 5, 1, byrow = TRUE))

        ## Global SD
        par("mar" = c(0,4,2,1))
        plot(dd$Date, dd$GLB_SD_wpsm,
             ylim = range(c(0, dd$GLB_SD_wpsm), na.rm = T),
             pch  = 19,  cex = 0.5, col = "red",
             xaxt = "n", xlab = "", ylab = "Global SD [Watt/m^2]")
        abline(h = 0, col = "grey", lty = 2)

        title(paste0("Global Irradiance  doy: ", yday(aday), "  ", aday))


        ## Radiation
        par("mar" = c(3,4,0,1))
        plot(dd$Date, dd$GLB_wpsm, type = "l",
             lwd  = 1.5,
             pch  = 19,  cex = 0.5, col = "green",
             xlab = "", ylab = "Radiation Flux [Watt/m^2]")
        abline(h = 0, col = "grey", lty = 2)


        ## Decorations
        text(dd$Date[1], max(dd[, .(GLB_wpsm)], na.rm = TRUE),
             labels = tag, pos = 4, cex = .9)

        legend("topright", bty = "n",
               lty = c( 1, NA),
               pch = c(NA, 19),
               legend = c(
                   "Global",
                   "Signal SD"),
               col = c("green",
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
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

