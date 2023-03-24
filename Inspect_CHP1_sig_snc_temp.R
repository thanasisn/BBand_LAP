# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Inspect raw CHP1 data. **SIG** "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Inspect raw data from CHP1."
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
#' params:
#'    ALL_YEARS: TRUE
#' ---

#'
#'  **LAP -> SIG**
#'
#' **Source code: [github.com/thanasisn/BBand_LAP](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [thanasisn.netlify.app/3-data_display/](https://thanasisn.netlify.app/3-data_display/)**
#'
#'
#+ echo=F, include=T


#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""      )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"    )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(fig.pos    = '!h'     )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/Inspect_CHP1_sig_snc_temp.R"

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)
library(pander,     warn.conflicts = TRUE, quietly = TRUE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )


## __  Variables  --------------------------------------------------------------
OutliersPlot <- 4


## years in the data base
datayears <- opendata() |> select(year) |> unique() |> collect() |> pull()

BB_meta   <- read_parquet(DB_META_fl)


## TODO compare output files with parsed dates from meta
years_to_do <- datayears


#'
#' Mark outlies with
#'
#' `r OutliersPlot`
#'




#+ include=TRUE, echo=FALSE, results="asis"
for (YYYY in years_to_do) {
    days_of_year <- seq.Date(as.Date(paste0(YYYY, "-01-01")),
                             as.Date(paste0(YYYY, "-12-31")), by = "day")

    days_of_year <- days_of_year[days_of_year <= Sys.Date()]

    cat("\n\n\\FloatBarrier\n\n")
    cat("\\newpage\n\n")
    cat("\n## Year:", YYYY, "\n\n")

    ## load data for year
    year_data <- opendata() |> filter(year == YYYY) |> collect()


    cat("\n**Days without any CHP-1 data:**\n\n")
    dwd <- year_data |> filter(!is.na(CHP1_sig)) |>
        select(Date) |> mutate(Date = as.Date(Date)) |> unique() |> pull()
    empty_days <- days_of_year[!days_of_year %in% dwd]
    cat(format(empty_days), " ")
    cat("\n\n")


    # #     ## add signal limits on plots
    # #     year_data[ , sig_lowlim := signal_lower_limit(Date) ]
    # #     year_data[ , sig_upplim := signal_upper_limit(Date) ]
    #
    #
    #     ####    Yearly Plots    ####################################################
    #
    #
    #     ####  Do some plots for this year before filtering  ####
    #     suppressWarnings({
    #         ## Try to find outliers
    #         yearlims <- data.table()
    #         for (an in grep("CHP1",names(year_data),value = T)){
    #             daily <- year_data[ , .(dmin = min(get(an),na.rm = T),
    #                                     dmax = max(get(an),na.rm = T)), by = as.Date(Date) ]
    #             low <- daily[ !is.infinite(dmin), mean(dmin) - OutliersPlot * sd(dmin)]
    #             upe <- daily[ !is.infinite(dmax), mean(dmax) + OutliersPlot * sd(dmax)]
    #             yearlims <- rbind(yearlims, data.table(an = an,low = low, upe = upe))
    #         }
    #     })
    #
    #     cat("\n\n### Proposed outliers limits \n")
    #     cat("\n\n")
    #     cat(pander(yearlims))
    #     cat("\n\n")
    #
    #
    #     cat('\n\n\\scriptsize\n\n')
    #     cat(pander( summary(year_data[,-c('Date','Azimuth')]) ))
    #     cat('\n\n\\normalsize\n\n')
    #
    #
    #     hist(year_data$CHP1value, breaks = 50, main = paste("CHP1 signal ",  YYYY ) )
    #     cat('\n\n')
    #
    #     hist(year_data$CHP1sd,    breaks = 50, main = paste("CHP1 signal SD",YYYY ) )
    #     cat('\n\n')
    #
    #     plot(year_data$Elevat, year_data$CHP1value, pch = 19, cex = .5,
    #          main = paste("CHP1 signal ", YYYY ),
    #          xlab = "Elevation",
    #          ylab = "CHP1 signal" )
    #     points(year_data$Elevat, year_data$sig_lowlim, pch = ".", col = "red")
    #     points(year_data$Elevat, year_data$sig_upplim, pch = ".", col = "red")
    #     cat('\n\n')
    #
    #
    #     plot(year_data$Date, year_data$CHP1value, pch = 19, cex = .5,
    #          main = paste("CHP1 signal ", YYYY ),
    #          xlab = "Elevation",
    #          ylab = "CHP1 signal" )
    #     points(year_data$Date, year_data$sig_lowlim, pch = ".", col = "red")
    #     points(year_data$Date, year_data$sig_upplim, pch = ".", col = "red")
    #     # abline(v=signal_physical_limits$Date)
    #     cat('\n\n')
    #
    #
    #
    #     plot(year_data$Elevat, year_data$CHP1sd,    pch = 19, cex = .5,
    #          main = paste("CHP1 signal SD", YYYY ),
    #          xlab = "Elevation",
    #          ylab = "CHP1 signal Standard Deviations")
    #     abline( h = yearlims[ an == "CHP1sd", low], col = "red")
    #     abline( h = yearlims[ an == "CHP1sd", upe], col = "red")
    #     cat('\n\n')
    #
    #
    #     par(mar = c(2,4,2,1))
    #     month_vec <- strftime(  year_data$Date, format = "%m")
    #     dd        <- aggregate( year_data[,c("CHP1value", "CHP1sd", "Elevat", "Azimuth")],
    #                             list(month_vec), FUN = summary, digits = 6 )
    #
    #
    #     # cat("\n\n### CHP 1 measurements, monthly aggregation\n")
    #     # cat("\n\n")
    #     # cat(pander(dd$CHP1value))
    #     # cat("\n\n")
    #     #
    #     # cat("\n\n### CHP 1 standard deviation, monthly aggregation\n")
    #     # cat(pander(dd$CHP1sd))
    #     #
    #     # cat("\n\n### Sun Elevation\n")
    #     # cat(pander(dd$Elevat))
    #     #
    #     # cat("\n\n### Sun Azimuth\n")
    #     # cat(pander(dd$Azimuth))
    #
    #     boxplot(year_data$CHP1value ~ month_vec )
    #     title(main = paste("CHP1value by month", YYYY) )
    #     cat('\n\n')
    #
    #     boxplot(year_data$CHP1sd ~ month_vec )
    #     title(main = paste("CHP1sd by month", YYYY) )
    #     cat('\n\n')
    #
    #     boxplot(year_data$Elevat ~ month_vec )
    #     title(main = paste("Elevation by month", YYYY) )
    #     cat('\n\n')
    #
    #     boxplot(year_data$Azimuth ~ month_vec )
    #     title(main = paste("Azimuth by month", YYYY) )
    #     cat('\n\n')
    #



}




# #     ## add signal limits on plots
# #     year_data[ , sig_lowlim := signal_lower_limit(Date) ]
# #     year_data[ , sig_upplim := signal_upper_limit(Date) ]
#
#
#     ####    Yearly Plots    ####################################################
#
#
#     ####  Do some plots for this year before filtering  ####
#     suppressWarnings({
#         ## Try to find outliers
#         yearlims <- data.table()
#         for (an in grep("CHP1",names(year_data),value = T)){
#             daily <- year_data[ , .(dmin = min(get(an),na.rm = T),
#                                     dmax = max(get(an),na.rm = T)), by = as.Date(Date) ]
#             low <- daily[ !is.infinite(dmin), mean(dmin) - OutliersPlot * sd(dmin)]
#             upe <- daily[ !is.infinite(dmax), mean(dmax) + OutliersPlot * sd(dmax)]
#             yearlims <- rbind(yearlims, data.table(an = an,low = low, upe = upe))
#         }
#     })
#
#     cat("\n\n### Proposed outliers limits \n")
#     cat("\n\n")
#     cat(pander(yearlims))
#     cat("\n\n")
#
#
#     cat('\n\n\\scriptsize\n\n')
#     cat(pander( summary(year_data[,-c('Date','Azimuth')]) ))
#     cat('\n\n\\normalsize\n\n')
#
#
#     hist(year_data$CHP1value, breaks = 50, main = paste("CHP1 signal ",  YYYY ) )
#     cat('\n\n')
#
#     hist(year_data$CHP1sd,    breaks = 50, main = paste("CHP1 signal SD",YYYY ) )
#     cat('\n\n')
#
#     plot(year_data$Elevat, year_data$CHP1value, pch = 19, cex = .5,
#          main = paste("CHP1 signal ", YYYY ),
#          xlab = "Elevation",
#          ylab = "CHP1 signal" )
#     points(year_data$Elevat, year_data$sig_lowlim, pch = ".", col = "red")
#     points(year_data$Elevat, year_data$sig_upplim, pch = ".", col = "red")
#     cat('\n\n')
#
#
#     plot(year_data$Date, year_data$CHP1value, pch = 19, cex = .5,
#          main = paste("CHP1 signal ", YYYY ),
#          xlab = "Elevation",
#          ylab = "CHP1 signal" )
#     points(year_data$Date, year_data$sig_lowlim, pch = ".", col = "red")
#     points(year_data$Date, year_data$sig_upplim, pch = ".", col = "red")
#     # abline(v=signal_physical_limits$Date)
#     cat('\n\n')
#
#
#
#     plot(year_data$Elevat, year_data$CHP1sd,    pch = 19, cex = .5,
#          main = paste("CHP1 signal SD", YYYY ),
#          xlab = "Elevation",
#          ylab = "CHP1 signal Standard Deviations")
#     abline( h = yearlims[ an == "CHP1sd", low], col = "red")
#     abline( h = yearlims[ an == "CHP1sd", upe], col = "red")
#     cat('\n\n')
#
#
#     par(mar = c(2,4,2,1))
#     month_vec <- strftime(  year_data$Date, format = "%m")
#     dd        <- aggregate( year_data[,c("CHP1value", "CHP1sd", "Elevat", "Azimuth")],
#                             list(month_vec), FUN = summary, digits = 6 )
#
#
#     # cat("\n\n### CHP 1 measurements, monthly aggregation\n")
#     # cat("\n\n")
#     # cat(pander(dd$CHP1value))
#     # cat("\n\n")
#     #
#     # cat("\n\n### CHP 1 standard deviation, monthly aggregation\n")
#     # cat(pander(dd$CHP1sd))
#     #
#     # cat("\n\n### Sun Elevation\n")
#     # cat(pander(dd$Elevat))
#     #
#     # cat("\n\n### Sun Azimuth\n")
#     # cat(pander(dd$Azimuth))
#
#     boxplot(year_data$CHP1value ~ month_vec )
#     title(main = paste("CHP1value by month", YYYY) )
#     cat('\n\n')
#
#     boxplot(year_data$CHP1sd ~ month_vec )
#     title(main = paste("CHP1sd by month", YYYY) )
#     cat('\n\n')
#
#     boxplot(year_data$Elevat ~ month_vec )
#     title(main = paste("Elevation by month", YYYY) )
#     cat('\n\n')
#
#     boxplot(year_data$Azimuth ~ month_vec )
#     title(main = paste("Azimuth by month", YYYY) )
#     cat('\n\n')
#








#' **END**
#+ include=T, echo=F
myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
