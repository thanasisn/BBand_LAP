# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Inspect raw CHP-1 data **SIG** "
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
#'
#' params:
#'   CLEAN: TRUE
#'
#' ---

#'
#'  **SIG**
#'
#' **Source code: [github.com/thanasisn/BBand_LAP](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [thanasisn.netlify.app/3-data_display/](https://thanasisn.netlify.app/3-data_display/)**
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
Script.Name <- "~/BBand_LAP/process/Legacy_CHP1_L0_export.R"

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")
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
datayears <- opendata() |> filter(year >= 2016) |> select(year) |> unique() |> collect() |> pull() |> sort()

BB_meta   <- read_parquet(DB_META_fl)
BB        <- opendata()

for (YYYY in datayears) {
    data_part <- BB |>
        filter(year == YYYY) |>
        select(c("Date", "CHP1_sig", "CHP1_sig_sd","Async_step_count",
                 "Async_tracker", "Azimuth", "Elevat", "chp1_temperature",
                 "chp1_temperature_SD", "chp1_temp_UNC")) |>
        collect()
    data_part <- data.table(data_part)

    setorder(data_part, Date)

    ## use the old names for output
    names(data_part)[names(data_part) == "Date"]                <- "Date30"
    names(data_part)[names(data_part) == "CHP1_sig"]            <- "CHP1value"
    names(data_part)[names(data_part) == "CHP1_sig_sd"]         <- "CHP1sd"
    names(data_part)[names(data_part) == "Async_step_count"]    <- "AsynStep"
    names(data_part)[names(data_part) == "Async_tracker"]       <- "Async"
    names(data_part)[names(data_part) == "chp1_temperature"]    <- "CHP1temp"
    names(data_part)[names(data_part) == "chp1_temperature_SD"] <- "CHP1tempSD"
    names(data_part)[names(data_part) == "chp1_temp_UNC"]       <- "CHP1tempUNC"

    ## write data to old file format
    write_RDS(data_part,
              paste0("~/DATA/Broad_Band/Legacy_L0_CHP1_", YYYY, ".Rds"),
              clean = TRUE)
}



## Old format of CHP1 L0
# 'data.frame':	383040 obs. of  11 variables:
# $ Date       : POSIXct, format: "2016-01-22 00:00:00" "2016-01-22 00:01:00" "2016-01-22 00:02:00" "2016-01-22 00:03:00" ...
# $ CHP1value  : num  NA NA NA NA NA NA NA NA NA NA ...
# $ CHP1sd     : num  NA NA NA NA NA NA NA NA NA NA ...
# $ AsynStep   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ Async      : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ Azimuth    : num  45.8 46.2 46.6 47 47.4 ...
# $ Elevat     : num  -63 -62.8 -62.7 -62.6 -62.4 ...
# $ CHP1temp   : num  NA NA NA NA NA NA NA NA NA NA ...
# $ CHP1tempSD : num  NA NA NA NA NA NA NA NA NA NA ...
# $ CHP1tempUNC: num  NA NA NA NA NA NA NA NA NA NA ...
# $ Date30     : POSIXct, format: "2016-01-22 00:00:30" "2016-01-22 00:01:30" "2016-01-22 00:02:30" "2016-01-22 00:03:30" ...





## do a data check -----------------------

listlegacy <- list.files(path   = "~/DATA/Broad_Band/",
                         pattern = "Legacy_L0_CHP1_[0-9]{4}\\.Rds",
                         full.names = TRUE, ignore.case = TRUE)

for (alf in listlegacy) {
    legacy <- readRDS(alf)
    yyyy   <- unique(year(legacy$Date30))[1]

    paste0("c")



}







#' **END**
#+ include=T, echo=F
# myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
