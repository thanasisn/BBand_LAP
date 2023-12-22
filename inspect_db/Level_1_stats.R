#!/usr/bin/env Rscript
# /* Copyright (C) 2017 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:  "Create meta statistics for the broadband measurements."
#' author:
#' - Natsis Athanasios^[Laboratory of Atmospheric Physics, Physics Department, Aristotle University of Thessaloniki, Greece.]
#' - Fountoulakis Ilias^[Laboratory of Atmospheric Physics, Physics Department, Aristotle University of Thessaloniki, Greece.]
#' abstract:
#'     " Create meta statistics for the broadband measurements, looking at daily statistics, gathered
#'       on level 1 process.
#'     "
#' keywords:      "Thessaloniki"
#' date:          "`r format(Sys.time(), '%B %d, %Y')`"
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




#+ echo=F, include=T
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_db/Level_1_stats.R"
renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2,    warn.conflicts = FALSE, quietly = TRUE)


#'
#' Do some aggregation over the daily stats and export data object to be used on reports
#'
#+ echo=F, include=T

## Date range to run
START_day <- as.POSIXct("1993-04-12") ## start of cm21
UNTIL_day <- as.POSIXct(Sys.Date())


## Variables for stats
vars <- c("GLB_wpsm", "DIR_wpsm", "GLB_SD_wpsm", "DIR_SD_wpsm")



BB <- opendata()


# BB |> filter(!is.na(GLB_SD_wpsm)) |> summarise(min(Date)) |> collect()

##  Yearly statistics  ---------------------------------------------------------
stats_yearly <- BB            |>
    filter(Elevat > 0)        |>
    filter(Date >= START_day) |>
    filter(Date <= UNTIL_day) |>
    group_by(year)            |>
    summarise(
        across(
            all_of(vars),
            list(mean   = ~ mean(  .x, na.rm = TRUE),
                 median = ~ median(.x, na.rm = TRUE),
                 min    = ~ min(   .x, na.rm = TRUE),
                 max    = ~ max(   .x, na.rm = TRUE),
                 N      = ~ sum(!is.na(.x))
            ),
            .names = "{.col}.{.fn}"
        )
    )                         |>
    arrange(year)             |>
    collect()                 |>
    data.table()
## create proper date
stats_yearly[, Date := as.Date(paste(year, "1", "1"), format = "%Y %m %d")]


##  Daily statistics  ----------------------------------------------------------
stats_daily <- BB |>
    filter(Elevat > 0)           |>
    filter(Date >= START_day)    |>
    filter(Date <= UNTIL_day)    |>
    mutate(Date = as.Date(Date)) |>
    group_by(Date)               |>
    summarise(
        across(
            all_of(vars),
            list(mean   = ~ mean(  .x, na.rm = TRUE),
                 median = ~ median(.x, na.rm = TRUE),
                 min    = ~ min(   .x, na.rm = TRUE),
                 max    = ~ max(   .x, na.rm = TRUE),
                 N      = ~ sum(!is.na(.x))
            ),
            .names = "{.col}.{.fn}"
        )
    )                            |>
    arrange(Date)                |>
    collect()                    |>
    data.table()


##  Monthly statistics  --------------------------------------------------------
stats_monthly <- BB           |>
    filter(Elevat > 0)        |>
    filter(Date >= START_day) |>
    filter(Date <= UNTIL_day) |>
    group_by(year, month)     |>
    summarise(
        across(
            all_of(vars),
               list(mean   = ~ mean(  .x, na.rm = TRUE),
                    median = ~ median(.x, na.rm = TRUE),
                    min    = ~ min(   .x, na.rm = TRUE),
                    max    = ~ max(   .x, na.rm = TRUE),
                    N      = ~ sum(!is.na(.x))
               ),
               .names = "{.col}.{.fn}"
            )
    ) |>
    arrange(year, month)      |>
    collect()                 |>
    data.table()
## create proper date
stats_monthly[, Date := as.Date(paste(year, month, "15"), format = "%Y %m %d")]



save(list = ls(pattern = "stats_"),
     file = "~/BBand_LAP/SIDE_DATA/BB_Statistics.Rda")








## Find some negative days

#'
#' Days with negative global
#'
#+ echo=F, include=T
BB |> filter(GLB_wpsm < -17) |> select(Date) |> mutate(Date = as.Date(Date)) |> collect() |> unique()


#'
#' Days with negative direct
#'
#+ echo=F, include=T
BB |> filter(DIR_wpsm < -3) |> select(Date) |> mutate(Date = as.Date(Date)) |> collect() |> unique()




## some plots

datas <- ls(pattern = "stats_")

for (dbn in datas) {
    DB     <- get(dbn)
    wecare <- grep("year|month|Date", names(DB), value = T, invert = T)
    for (avar in wecare) {
        ## skip empty
        if (all(!DB[[avar]] %in% c(NA, NaN, 0))) next()

        ## basic plot
        p <- ggplot() +
            aes(x = DB$Date, y = DB[[avar]]) +
            geom_point() +
            labs(title = paste(tr_var(avar),  sub(".*\\.", "", avar), sub(".*\\_", "", dbn)),
                 x = "",
                 y = avar) +
            theme_bw()
        suppressWarnings(print(p))
        # theme(
        #     panel.background = element_rect(fill='transparent'), #transparent panel bg
        #     plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        #     panel.grid.major = element_blank(), #remove major gridlines
        #     panel.grid.minor = element_blank(), #remove minor gridlines
        #     legend.background = element_rect(fill='transparent'), #transparent legend bg
        #     legend.box.background = element_rect(fill='transparent') #transparent legend panel
        # )
        cat(" \n \n")
    }
}



## OUTPUTS
aggr_years   <- "/home/athan/DATA/Broad_Band/aggregated_years.Rda"
aggr_logbook <- "/home/athan/DATA/Broad_Band/aggregated_logbook.Rda"







#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
# cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
#     file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

