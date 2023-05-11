# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Inspect CM-21 radiation data **L1** "
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
#' params:
#'   CLEAN: TRUE
#'
#' ---

#'
#'  **SIG**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
#'
#+ echo=F, include=T

## TODO
## - apply changes
## - make similar to CM_21_GLB

#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"  )
knitr::opts_chunk$set(fig.align  = "center")
knitr::opts_chunk$set(fig.pos    = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_db/Inspect_CM21_rad.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)
library(pander,     warn.conflicts = TRUE, quietly = TRUE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )


## __  Variables  --------------------------------------------------------------
CLEAN        <- TRUE
# CLEAN        <- FALSE


## __ Execution control  -------------------------------------------------------
## When knitting
if (exists("params")) {
    # params <- list(CLEAN = CLEAN)
    CLEAN <- params$CLEAN
}
## When running
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    if (any(args == "CLEAN")) { CLEAN <- TRUE  }
    if (any(args == "DIRTY")) { CLEAN <- FALSE }
    cat("Arguments", paste(args),"\n")
}

# cat(paste("\n**CLEAN:", CLEAN, "**\n"))



## years in the data base
datayears <- opendata() |>
    filter(!is.na(GLB_wpsm)) |>
    select(year) |>
    unique()     |>
    collect()    |>
    pull()


## TODO compare output files with parsed dates from meta
years_to_do <- datayears

# TEST
# years_to_do <- 1995

#'
#' ## Intro
#'
#' Produce yearly plots for **CM-21**.
#'
#'

#+ include=TRUE, echo=FALSE, results="asis"
for (YYYY in sort(years_to_do)) {
    days_of_year <- seq.Date(as.Date(paste0(YYYY, "-01-01")),
                             as.Date(paste0(YYYY, "-12-31")), by = "day")
    ## don't go to the future
    days_of_year <- days_of_year[days_of_year <= Sys.Date()]

    cat("\n\n\\FloatBarrier\n\n")
    cat("\\newpage\n\n")
    cat("\n## Year:", YYYY, "\n\n")

    ## load data for year
    year_data <- data.table(
        opendata()           |>
        filter(year == YYYY) |>
        # filter(Elevat > -5)  |>
        collect()
    )


    ## Check for night time extreme values -------------------------------------
    dark_test <- year_data[Elevat < DARK_ELEV &
                               (GLB_wpsm < CM21_MINnightLIM | GLB_wpsm > CM21_MAXnightLIM)]

    if (nrow(dark_test) > 0) {
        cat("\n### Night radiation outlier days\n\n")
        cat(
            pander(
                dark_test[, .(Min_GLB = min(GLB_wpsm, na.rm = TRUE),
                              Max_GLB = max(GLB_wpsm, na.rm = TRUE)),
                          by = as.Date(Date)]
            )
        )
        cat('\n\n')
    }


    hist(year_data[Elevat < DARK_ELEV, GLB_wpsm],
         main = paste(YYYY, "Elevat  >", DARK_ELEV, "[Watt/m^2]"),
         breaks = 100 , las = 1, probability = T, xlab = "watt/m^2")
    abline(v = CHP1_MAXnightLIM, col = "red", lty = 3)
    abline(v = CHP1_MINnightLIM, col = "red", lty = 3)
    cat('\n\n')


    plot(year_data[Elevat < DARK_ELEV, GLB_wpsm, Date],
         pch  = 19,
         cex  = .1,
         main = paste("GHI Night time measurements ", YYYY),
         xlab = "",
         ylab = "[Watt/m^2]" )
    abline(h = CHP1_MAXnightLIM, col = "red", lty = 3)
    abline(h = CHP1_MINnightLIM, col = "red", lty = 3)
    cat('\n\n')


    ## Distribution of Global and SD -------------------------------------------
    wattlimit <- 50
    hist(year_data[GLB_wpsm > wattlimit, GLB_wpsm],
         main = paste(YYYY, "Global  >", wattlimit, "[Watt/m^2]"),
         breaks = 100 , las = 1, probability = T, xlab = "watt/m^2")
    lines(density(year_data$GLB_wpsm, na.rm = T), col = "orange", lwd = 3)
    cat('\n\n')

    hist(year_data$GLB_SD_wpsm,
         main = paste(YYYY, "Global SD"),
         breaks = 100 , las = 1, probability = T, xlab = "[Watt/m^2]")
    lines(density(year_data$GLB_SD_wpsm, na.rm = T), col = "orange", lwd = 3)
    cat('\n\n')


    ## Scatter points by sun position ------------------------------------------
    plot(year_data$Elevat, year_data$GLB_wpsm,
         pch  = 19,
         cex  = .1,
         main = paste("Global radiation ", YYYY),
         xlab = "Elevation [°]",
         ylab = "[Watt/m^2]" )
    cat('\n\n')

    plot(year_data$Azimuth, year_data$GLB_wpsm,
         pch  = 19,
         cex  = .1,
         main = paste("Global radiation ", YYYY),
         xlab = "Azimuth [°]",
         ylab = "[Watt/m^2]" )
    cat('\n\n')

    ## Scatter points by date --------------------------------------------------
    plot(year_data$Date, year_data$GLB_wpsm,
         pch  = 19,
         cex  = .1,
         main = paste("Global radiation ", YYYY),
         xlab = "",
         ylab = "[Watt/m^2]" )
    cat('\n\n')


    ## Scatter points by time of day -------------------------------------------
    plot(year_data[preNoon == TRUE, Elevat],
         year_data[preNoon == TRUE, GLB_wpsm],
         pch  = 19,
         cex  = .1,
         col  = "blue",
         main = paste("Direct Horizontal ", YYYY),
         xlab = "Elevation [°]",
         ylab = "[Watt/m^2]" )
    points(year_data[preNoon == FALSE, Elevat],
           year_data[preNoon == FALSE, GLB_wpsm],
           pch = 19,
           cex = 0.1,
           col = "green")
    legend("topleft",
           legend = c("Before noon", "After noon"),
           col    = c("blue",        "green"),
           pch    = 19, bty = "n")
    cat('\n\n')


    minelevet <- -1
    xlim <- range(year_data$Elevat)
    gap  <- 1
    xlim[2] <- xlim[2] + abs(diff(range(year_data[Elevat > minelevet ,Elevat]))) + gap
    xlim[1] <- minelevet

    plot(year_data[preNoon == TRUE & Elevat > minelevet, Elevat],
         year_data[preNoon == TRUE & Elevat > minelevet, GLB_wpsm],
         xlim = xlim,
         pch  = 19,
         cex  = .05,
         col  = "blue",
         main = paste("GHI morning evening balance", YYYY),
         xaxt = "n",
         xlab = "Sun Elevation",
         ylab = "[Watt/m^2]" )

    points(-year_data[preNoon == FALSE & Elevat > minelevet, Elevat] + gap + abs(diff(range(year_data$Elevat))),
           year_data[preNoon == FALSE & Elevat > minelevet, GLB_wpsm],
           pch = 19,
           cex = 0.05,
           col = "green")
    cat('\n\n')





    ## Box plots by week -------------------------------------------------------
    year_data[ , weekn := week(Date) ]

    boxplot(year_data[Elevat > 0, GLB_wpsm] ~ year_data[Elevat > 0, weekn],
            xlab = "Week", ylab = "[Watt/m^2]")
    title(main = paste(YYYY, "GHI (Elevation > 0)"))
    cat('\n\n')

    boxplot(year_data[Elevat > 0, GLB_SD_wpsm] ~ year_data[Elevat > 0, weekn],
            xlab = "Week", ylab = "[Watt/m^2]")
    title(main = paste(YYYY, "GHI SD (Elevation > 0)"))
    cat('\n\n')

    boxplot(year_data[, CM21_sig - CM21_sig_wo_dark] ~ year_data[, weekn],
            xlab = "Week", ylab = "[V]")
    title(main = paste(YYYY, "Dark correction"))
    cat('\n\n')


    cat('\n\n\\footnotesize\n\n')
    cat(pander(summary(year_data[, .(Date, SZA, GLB_wpsm, GLB_SD_wpsm)])))
    cat('\n\n\\normalsize\n\n')

}





#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

