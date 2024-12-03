# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Inspect raw CHP-1 data **SIG** "
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
Script.Name <- "~/BBand_LAP/inspect_db/01_Inspect_CHP1_sig_snc_temp.R"
renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )


## __  Variables  --------------------------------------------------------------
OutliersPlot <- 4
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

cat(paste("\n**CLEAN:", CLEAN, "**\n"))

## years in the data base
datayears <- opendata() |>
    filter(!is.na(CHP1_sig)) |>
    select(year) |>
    unique()     |>
    collect()    |>
    pull()

BB_meta   <- read_parquet(DB_META_fl)


## TODO compare output files with parsed dates from meta
years_to_do <- datayears

# TEST
# years_to_do <- 2016

#'
#' ## Intro
#'
#' Produce yearly plots for **CHP-1**.
#'
#' Shows only **raw data** aspects.
#'
#' It can use flags to show 'CLEAN'/'DIRTY' data.
#'
#' For 'CLEAN' data, it removes from view:
#'
#' - Bad recordings ranges `chp1_bad_data_flag`
#' - Tracker async cases `Async_tracker_flag`
#' - Physical recording limits `chp1_signal_lower_limit()` and `chp1_signal_upper_limit()`
#'
#' Mark outliers for signal and SD with:
#'
#' **mean(variable) -/+ `r OutliersPlot` * sd(variable)**
#'
#' This is just a report it doesn't alter the data.
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
    year_data <- data.table(opendata() |> filter(year == YYYY) |> collect())
    setorder(year_data, Date)


    ## Recording limits
    year_data[, sig_lowlim := chp1_signal_lower_limit(Date)]
    year_data[, sig_upplim := chp1_signal_upper_limit(Date)]

    ## Choose what to plot (data.table slicing dong work)
    if (CLEAN) {
        year_data[!is.na(CHP1_sig), .N]
        cat("\nRemove bad data regions\n")
        cat(year_data[!is.na(chp1_bad_data_flag), .N], year_data[!is.na(CHP1_sig), .N], "\n\n")
        year_data$CHP1_sig   [!is.na(year_data$chp1_bad_data_flag)] <- NA
        year_data$CHP1_sig_sd[!is.na(year_data$chp1_bad_data_flag)] <- NA

        cat("\nRemove tracker async cases\n")
        cat(year_data[Async_tracker_flag == TRUE, .N], year_data[!is.na(CHP1_sig), .N], "\n\n")
        year_data$CHP1_sig   [year_data$Async_tracker_flag == TRUE] <- NA
        year_data$CHP1_sig_sd[year_data$Async_tracker_flag == TRUE] <- NA

        cat("\nRemove data above physical limits\n")
        cat(year_data[CHP1_sig > sig_upplim, .N], year_data[!is.na(CHP1_sig), .N], "\n\n")
        year_data$CHP1_sig   [year_data$CHP1_sig > year_data$sig_upplim] <- NA
        year_data$CHP1_sig_sd[year_data$CHP1_sig > year_data$sig_upplim] <- NA

        cat("\nRemove data below physical limits\n")
        cat(year_data[CHP1_sig < sig_lowlim, .N], year_data[!is.na(CHP1_sig), .N], "\n\n")
        year_data$CHP1_sig   [year_data$CHP1_sig < year_data$sig_lowlim] <- NA
        year_data$CHP1_sig_sd[year_data$CHP1_sig < year_data$sig_lowlim] <- NA

        cat("\nRemove bad temperatrue data\n")
        year_data[!is.na(chp1_bad_temp_flag), chp1_temperature    := NA]
        year_data[!is.na(chp1_bad_temp_flag), chp1_temperature_SD := NA]
    }

    ## Missing days
    cat("\n**Days without any CHP-1 data:**\n\n")
    dwd <- year_data[!is.na(CHP1_sig), unique(as.Date(Date))]
    empty_days <- days_of_year[!days_of_year %in% dwd]
    cat(format(empty_days), " ")
    cat("\n\n")


    ## Get outliers limits
    suppressWarnings({
        ## Try to find outliers
        yearlims <- data.table()
        for (an in grep("CHP1_sig", names(year_data), value = TRUE)) {
            daily <- year_data[ , .(dmin = min(get(an),na.rm = T),
                                    dmax = max(get(an),na.rm = T)), by = as.Date(Date) ]
            low <- daily[!is.infinite(dmin), mean(dmin) - OutliersPlot * sd(dmin)]
            upe <- daily[!is.infinite(dmax), mean(dmax) + OutliersPlot * sd(dmax)]
            yearlims <- rbind(yearlims, data.table(an = an,low = low, upe = upe))
        }
    })

    cat("\n\n### Proposed outliers limits \n")
    cat('\n\n\\footnotesize\n\n')
    cat(pander(yearlims))
    cat('\n\n\\normalsize\n\n')

    cat("\n**Days with outliers:**\n\n")
    cat(format(
        year_data[CHP1_sig > yearlims[ an == "CHP1_sig", upe], unique(as.Date(Date))]
        ))
    cat("\n\n")
    cat(format(
        year_data[CHP1_sig < yearlims[ an == "CHP1_sig", low], unique(as.Date(Date))]
    ))
    cat("\n\n")
    cat(format(
        year_data[CHP1_sig_sd > yearlims[ an == "CHP1_sig_sd", upe], unique(as.Date(Date))]
    ))
    cat("\n\n")
    cat(format(
        year_data[CHP1_sig_sd < yearlims[ an == "CHP1_sig_sd", low], unique(as.Date(Date))]
    ))
    cat("\n\n")

    cat("\n**Days hitting physical limit:**\n\n")
    cat(format(
        year_data[CHP1_sig > sig_upplim, unique(as.Date(Date))]
    ))
    cat("\n\n")
    cat(format(
        year_data[CHP1_sig < sig_lowlim, unique(as.Date(Date))]
    ))

    cat('\n\n\\footnotesize\n\n')
    cat(pander(summary(year_data[, .(Date, SZA, CHP1_sig, CHP1_sig_sd, Async_tracker_flag)])))
    cat('\n\n\\normalsize\n\n')


    hist(year_data$CHP1_sig,
         breaks = 50,
         main   = paste("CHP1 signal ",  YYYY))
    abline(v = yearlims[ an == "CHP1_sig", low], lty = 3, col = "red")
    abline(v = yearlims[ an == "CHP1_sig", upe], lty = 3, col = "red")
    cat('\n\n')


    hist(year_data$CHP1_sig_sd,
         breaks = 50,
         main   = paste("CHP1 signal SD", YYYY))
    abline(v = yearlims[ an == "CHP1_sig_sd", low], lty = 3, col = "red")
    abline(v = yearlims[ an == "CHP1_sig_sd", upe], lty = 3, col = "red")
    cat('\n\n')


    plot(year_data$Elevat, year_data$CHP1_sig,
         pch  = 19,
         cex  = .1,
         main = paste("CHP1 signal ", YYYY ),
         xlab = "Elevation",
         ylab = "CHP1 signal")
    points(year_data$Elevat, year_data$sig_lowlim, pch = ".", col = "red")
    points(year_data$Elevat, year_data$sig_upplim, pch = ".", col = "red")
    cat('\n\n')


    plot(year_data$Date, year_data$CHP1_sig,
         pch  = 19,
         cex  = .1,
         main = paste("CHP1 signal ", YYYY),
         xlab = "",
         ylab = "CHP1 signal")
    points(year_data$Date, year_data$sig_lowlim, pch = ".", col = "red")
    points(year_data$Date, year_data$sig_upplim, pch = ".", col = "red")
    abline(h = yearlims[ an == "CHP1_sig", low], lty = 3, col = "red")
    abline(h = yearlims[ an == "CHP1_sig", upe], lty = 3, col = "red")
    cat('\n\n')


    plot(year_data$Elevat, year_data$CHP1_sig_sd,
         pch  = 19,
         cex  = .1,
         main = paste("CHP1 signal SD", YYYY),
         xlab = "Elevation",
         ylab = "CHP1 signal SD")
    abline(h = yearlims[ an == "CHP1_sig_sd", low], lty = 3, col = "red")
    abline(h = yearlims[ an == "CHP1_sig_sd", upe], lty = 3, col = "red")
    cat('\n\n')


    all    <- cumsum(tidyr::replace_na(year_data$CHP1_sig, 0))
    pos    <- year_data[ CHP1_sig > 0 ]
    pos$V1 <- cumsum(tidyr::replace_na(pos$CHP1_sig, 0))
    neg    <- year_data[ CHP1_sig < 0 ]
    neg$V1 <- cumsum(tidyr::replace_na(neg$CHP1_sig, 0))
    xlim   <- range(year_data$Date)
    plot(year_data$Date, all,
         type = "l",
         xlim = xlim,
         ylab = "",
         yaxt = "n", xlab = "",
         main = paste("Cum Sum of CHP-1 signal ",  YYYY))
    par(new = TRUE)
    plot(pos$Date, pos$V1,
         xlim = xlim,
         col = "blue", type = "l",
         ylab = "", yaxt = "n", xlab = "", xaxt = "n")
    par(new = TRUE)
    plot(neg$Date, neg$V1,
         xlim = xlim,
         col = "red", type = "l",
         ylab = "", yaxt = "n", xlab = "", xaxt = "n")
    legend("left", legend = c("Positive signal",
                              "Negative signal",
                              "All signal"),
           lty = 1, bty = "n", cex = 0.8,
           col = c("blue", "red", "black"))
    cat('\n\n')


    all    <- cumsum(tidyr::replace_na(year_data$CHP1_sig_sd, 0))
    pos    <- year_data[ CHP1_sig > 0 ]
    pos$V1 <- cumsum(tidyr::replace_na(pos$CHP1_sig_sd, 0))
    neg    <- year_data[ CHP1_sig < 0 ]
    neg$V1 <- cumsum(tidyr::replace_na(neg$CHP1_sig_sd, 0))
    xlim   <- range(year_data$Date)
    plot(year_data$Date, all,
         type = "l",
         xlim = xlim,
         ylab = "",
         yaxt = "n", xlab = "",
         main = paste("Cum Sum of CHP-1 sd ",  YYYY))
    par(new = TRUE)
    plot(pos$Date, pos$V1,
         xlim = xlim,
         col = "blue", type = "l",
         ylab = "", yaxt = "n", xlab = "", xaxt = "n")
    par(new = TRUE)
    plot(neg$Date, neg$V1,
         xlim = xlim,
         col = "red", type = "l",
         ylab = "", yaxt = "n", xlab = "", xaxt = "n")
    legend("left", legend = c("Positive signal",
                              "Negative signal",
                              "All signal"),
           lty = 1, bty = "n", cex = 0.8,
           col = c("red", "blue", "black"))
    cat('\n\n')


    month_vec <- strftime(  year_data$Date, format = "%m")
    dd        <- aggregate( year_data[, .(CHP1_sig, CHP1_sig_sd, Elevat, Azimuth)],
                            list(month_vec), FUN = summary, digits = 6 )

    boxplot(year_data$CHP1_sig ~ month_vec )
    title(main = paste("CHP1value by month", YYYY))
    cat('\n\n')

    boxplot(year_data$CHP1_sig_sd ~ month_vec)
    title(main = paste("CHP1sd by month", YYYY))
    cat('\n\n')

    count <- year_data[ , .(Asyncs = sum(Async_tracker_flag)), by = .(Day = as.Date(Date))]

    plot(count$Day, count$Asyncs,
         main = paste("Daily Async cases", YYYY))
    cat("\n\n")

    hist(count$Asyncs,
         main = paste("Daily Async cases", YYYY))
    cat("\n\n")

    # boxplot(year_data$Elevat ~ month_vec )
    # title(main = paste("Elevation by month", YYYY) )
    # cat('\n\n')

    # boxplot(year_data$Azimuth ~ month_vec )
    # title(main = paste("Azimuth by month", YYYY) )
    # cat('\n\n')


    if (year_data[!is.na(chp1_temperature), .N] > 0) {
        cat("\n\n\\FloatBarrier\n\n")
        cat("\n## Temperature data:", YYYY, "\n\n")

        if (CLEAN) {
            year_data$chp1_temperature[!is.na(year_data$chp1_temp_bad_data)]    <- NA
            year_data$chp1_temperature_SD[!is.na(year_data$chp1_temp_bad_data)] <- NA
        }

        suppressWarnings({
            ## Try to find outliers
            yearlims <- data.table()
            for (an in grep("chp1_temperature", names(year_data), value = TRUE)) {
                daily <- year_data[ , .(dmin = min(get(an),na.rm = T),
                                        dmax = max(get(an),na.rm = T)), by = as.Date(Date) ]
                low <- daily[!is.infinite(dmin), mean(dmin) - OutliersPlot * sd(dmin)]
                upe <- daily[!is.infinite(dmax), mean(dmax) + OutliersPlot * sd(dmax)]
                yearlims <- rbind(yearlims, data.table(an = an,low = low, upe = upe))
            }
        })

        cat("\n**Days with outliers:**\n\n")
        cat(format(
            year_data[chp1_temperature > yearlims[ an == "chp1_temperature", upe], unique(as.Date(Date))]
        ))
        cat("\n\n")
        cat(format(
            year_data[chp1_temperature < yearlims[ an == "chp1_temperature", low], unique(as.Date(Date))]
        ))
        cat("\n\n")

        hist(year_data$chp1_temperature,
             breaks = 50,
             main   = paste("CHP1 temperature ",  YYYY))
        abline(v = yearlims[ an == "chp1_temperature", low], lty = 3, col = "red")
        abline(v = yearlims[ an == "chp1_temperature", upe], lty = 3, col = "red")
        cat('\n\n')

        hist(year_data$chp1_temperature_SD,
             breaks = 50,
             main   = paste("CHP1 temperature SD", YYYY))
        abline(v = yearlims[ an == "chp1_temperature_SD", low], lty = 3, col = "red")
        abline(v = yearlims[ an == "chp1_temperature_SD", upe], lty = 3, col = "red")
        cat('\n\n')

        plot(year_data$Date, year_data$chp1_temperature,
             pch  = 19,
             cex  = .1,
             main = paste("CHP1 temperature ", YYYY ),
             xlab = "",
             ylab = "CHP1 temperature" )
        abline(h = yearlims[ an == "chp1_temperature", low], lty = 3, col = "red")
        abline(h = yearlims[ an == "chp1_temperature", upe], lty = 3, col = "red")
        cat('\n\n')

        plot(year_data$Elevat, year_data$chp1_temperature_SD,
             pch  = 19,
             cex  = .1,
             main = paste("CHP1 temperature SD", YYYY ),
             xlab = "Elevation",
             ylab = "CHP1 temperature SD ")
        abline(h = yearlims[ an == "chp1_temperature_SD", low], lty = 3, col = "red")
        abline(h = yearlims[ an == "chp1_temperature_SD", upe], lty = 3, col = "red")
        cat('\n\n')

    }

}





#' **END**
#+ include=T, echo=F
# myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

