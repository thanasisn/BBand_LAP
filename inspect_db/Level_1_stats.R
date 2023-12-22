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

#
# Do some aggregation over the daily stats and export data object to be used on reports
#


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
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )


## Date range to run
START_day <- as.POSIXct("2016-01-01")
UNTIL_day <- as.POSIXct(Sys.Date())


## Variables for stats
vars <- c("GLB_wpsm", "DIR_wpsm", "GLB_SD_wpsm", "DIR_SD_wpsm")



BB <- opendata()


##  Yearly statistics  ---------------------------------------------------------

stats_yearly <- BB     |>
    filter(Elevat > 0) |>
    group_by(year)     |>
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
    arrange(year) |>
    collect() |> data.table()




##  Daily statistics  ----------------------------------------------------------

stats_daily <- BB |>
    filter(Elevat > 0)          |>
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
    ) |>
    arrange(Date) |>
    collect()     |> data.table()



##  Monthly statistics  --------------------------------------------------------


stats_monthly <- BB |>
    filter(Elevat > 0)          |>
    group_by(year, month)       |>
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
    arrange(year, month) |>
    collect() |> data.table()

stats_monthly[, Date := as.Date(paste(year, month, "15"), format = "%Y %m %d")]



save(list = ls(pattern = "stats_"),
     file = "~/BBand_LAP/SIDE_DATA/BB_Statistics.Rda")


## Find days
BB |> filter(GLB_wpsm < -18) |> select(Date) |> mutate(Date = as.Date(Date)) |>  collect() |> unique()








stop()


## OUTPUTS
aggr_years   <- "/home/athan/DATA/Broad_Band/aggregated_years.Rda"
aggr_logbook <- "/home/athan/DATA/Broad_Band/aggregated_logbook.Rda"


## VARIABLES
elevation_limit  <-       -2
cleaness_idx_lim <- c( -4, 4   )
diffuse_frac_lim <- c(  0, 1.1 )

tag <- paste0("Natsis Athanasios LAP AUTH ",
              strftime(Sys.time(), format = "%b %Y" ))

LBch <- data.frame()
LBtm <- data.frame()
LBcm <- data.frame()
## loop years and gather data
for (YY in yearSTA:yearEND) {

    cat( paste("Year", YY, " \n" ) )

    ####  CHP-1 data  ####
    year_file <- paste0(CHP1_L0FL, YY, "_logbook", ".Rds")
    stopifnot( file.exists(year_file) )
    CHP1_L0   <- readRDS(year_file)
    LBch      <- rbind(LBch, CHP1_L0[ CHP1_L0$removed_count > 0, ])
    ####  CHP-1 temperature  ####
    year_file <- paste0(CHP1_L0FL, YY, "_logbook_temp", ".Rds")
    stopifnot( file.exists(year_file) )
    CHP1_L0   <- readRDS(year_file)
    LBtm      <- rbind(LBtm, CHP1_L0[ CHP1_L0$removed_count > 0, ])
    ####  CM-21 data  ####
    year_file <- paste0(CM21_L0FL, YY, "_logbook", ".Rds")
    stopifnot( file.exists(year_file) )
    CM21_L0   <- readRDS(year_file)
    LBcm      <- rbind(LBcm, CM21_L0[CM21_L0$removed_count > 0, ] )

    rm(CHP1_L0, CM21_L0)
}


sum()








#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
# cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
#     file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

