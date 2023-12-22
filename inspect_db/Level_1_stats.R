#!/usr/bin/env Rscript
# /* Copyright (C) 2017 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:  "Create meta statistics for the broadband measurements."
#' author:
#' - Natsis Athanasios^[Laboratory of Atmospheric Physics, Physics Department, Aristotle University of Thessaloniki, Greece.]
#' - Fountoulakis Ilias^[Laboratory of Atmospheric Physics, Physics Department, Aristotle University of Thessaloniki, Greece.]
#' thanks: "Replication files are available on the  (http://gitub.com/smle).
#'          **Current version**: `r format(Sys.time(), '%B %d, %Y')`;
#'          ."
#' abstract:
#'     " Create meta statistics for the broadband measurements, looking at daily statistics, gathered
#'       on level 1 process.
#'     "
#' keywords:      "AOD, Brewer, Cimel, Lidar, Thessaloniki"
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



library(lubridate)



## date range to run
# project_start = PROJECT_START
project_start <- as.POSIXct("2016-01-01")
today         <- as.POSIXct(Sys.Date())

## get year range
yearSTA <- as.numeric( format(project_start, format = "%Y") )
yearEND <- as.numeric( format(x = today, format = "%Y")     )



## INPUTS
CHP1_BASE    <- "/home/athan/DATA/Broad_Band/LAP_CHP1_L1_stats_"
CM21_BASE    <- "/home/athan/DATA/Broad_Band/LAP_CM21_H_L1_stats_"
CHP1_L0FL    <- "/home/athan/DATA/Broad_Band/LAP_CHP1_L0_"
CM21_L0FL    <- "/home/athan/DATA/Broad_Band/LAP_CM21_H_L0_"


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

LBch_Ysum      <- RAerosols::aggregate_CF2(LBch[1], dates_vec = LBch$lower, FUN = sum, unit = "year" )
LBch_Ysum$Date <- as.numeric(strftime(LBch_Ysum$Group.1, "%Y"))
LBch_Ysum      <- subset(LBch_Ysum, select = -Group.1)

LBtm_Ysum      <- RAerosols::aggregate_CF2(LBtm[1], dates_vec = LBtm$lower, FUN = sum, unit = "year" )
LBtm_Ysum$Date <- as.numeric(strftime(LBtm_Ysum$Group.1, "%Y"))
LBtm_Ysum      <- subset(LBtm_Ysum, select = -Group.1)

LBcm_Ysum      <- RAerosols::aggregate_CF2(LBcm[1], dates_vec = LBcm$lower, FUN = sum, unit = "year" )
LBcm_Ysum$Date <- as.numeric(strftime(LBcm_Ysum$Group.1, "%Y"))
LBcm_Ysum      <- subset(LBcm_Ysum, select = -Group.1)



ST <- data.frame()
## loop years and gather data
for (YY in yearSTA:yearEND) {

    cat( paste("Year", YY, "\n" ) )

    ####  Load CHP-1 data  ####
    year_file <- paste0(CHP1_BASE, YY, ".Rds")
    stopifnot( file.exists(year_file) )
    CHP1_L1   <- readRDS(year_file)

    ####  Load CM-21 data  ####
    year_file <- paste0(CM21_BASE, YY, ".Rds")
    stopifnot( file.exists(year_file))
    CM21_L1   <- readRDS(year_file)

    ## mark days with data
    CHP1_L1$chp1_act <- CHP1_L1$cCHP1_meas > 0
    CM21_L1$cm21_act <- CM21_L1$cCM21_meas > 0

    ## merge all data to plot
    STp <- merge(CM21_L1, CHP1_L1, all = T )
    ST  <- rbind(ST,STp)

    ### yearly data availability
    CHP1_L1$chp1_nar <- CHP1_L1$cCHP1_NA - CHP1_L1$async
    CHP1_L1$chp1_nar[ CHP1_L1$chp1_nar == 0 ] <- NA

    plot(CHP1_L1$Date,CHP1_L1$cSun, ylim = range(CHP1_L1$cSun,CHP1_L1$async))
    points(CHP1_L1$Date,CHP1_L1$cCHP1_meas, col = "blue")
    points(CHP1_L1$Date,CHP1_L1$async,      col = "red")
    points(CHP1_L1$Date,CHP1_L1$chp1_nar,   col = "magenta")

    plot(  CM21_L1$Date, CM21_L1$cSun)
    points(CM21_L1$Date, CM21_L1$cCM21_meas, col = "green")
    points(CM21_L1$Date, CM21_L1$cCM21_NA,   col = "red")

    # stop()
    rm(CM21_L1,CHP1_L1,STp)
}



## yearly aggregation
ST_Ysum       <- RAerosols::aggregate_CF2(ST[,!unlist(lapply(ST[1,], is.POSIXct))], dates_vec = ST$Date,
                                          FUN = sum, unit = "year", na.rm = T)
ST_Ysum$Date  <- as.numeric(strftime(ST_Ysum$Group.1, "%Y"))
ST_Ysum       <- subset(ST_Ysum, select = -Group.1)

ST_Ymax       <- RAerosols::aggregate_CF2(ST, dates_vec = ST$Date, FUN = max , unit = "year", na.rm = T)
ST_Ymax$Date  <- as.numeric(strftime(ST_Ymax$Group.1, "%Y"))
ST_Ymax       <- subset(ST_Ymax, select = -Group.1)

ST_Ymin       <- RAerosols::aggregate_CF2(ST, dates_vec = ST$Date, FUN = min , unit = "year", na.rm = T)
ST_Ymin$Date  <- as.numeric(strftime(ST_Ymin$Group.1, "%Y"))
ST_Ymin       <- subset(ST_Ymin, select = -Group.1)

ST_Ymean      <- RAerosols::aggregate_CF2(ST, dates_vec = ST$Date, FUN = mean , unit = "year", na.rm = T)
ST_Ymean$Date <- as.numeric(strftime(ST_Ymean$Group.1, "%Y"))
ST_Ymean      <- subset(ST_Ymean, select = -Group.1)



#'
#' ## CHP 1 async stats
#'

chp1_nar <- ST$cCHP1_NA - ST$async
chp1_nar[ chp1_nar == 0 ] <- NA

plot(  ST$Date, ST$cSun,
       ylim = range(ST$cSun,0, na.rm = T),
       pch = 19, cex = 0.8, xlab = "", ylab = "Number of data points" )
points(ST$Date, ST$cCHP1_meas, col = "blue",    pch = 19, cex = 0.6)
points(ST$Date, ST$async,      col = "red",     pch = 19, cex = 0.6)
points(ST$Date, chp1_nar,      col = "magenta", pch = 19, cex = 0.6)
title("CHP1 data count")


plot(  ST$Date, ST$cSun,
       ylim = range(ST$cSun,0, na.rm = T),
       pch=19, cex=0.8, xlab = "", ylab = "Number of data points" )
points(ST$Date, ST$cCM21_meas, col = "green", pch = 19, cex = 0.6)
points(ST$Date, ST$cCM21_NA,   col = "red",   pch = 19, cex = 0.6)
title("CM21 data count")


##TODO plot consecutive async!!


##TODO plot consecutive NA!!



#### SAVE DATA OBJECTS ####
save( ST_Ymax, ST_Ymin, ST_Ysum, ST_Ymean,  file = aggr_years   )
save( LBch_Ysum, LBtm_Ysum, LBcm_Ysum,      file = aggr_logbook )

#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

