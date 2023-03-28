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
source("~/BBand_LAP/functions/Functions_CHP1.R")
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

dddddd <- BB |> filter(CHP1_sig <= -5 ) |> collect()

## export legacy files
for (YYYY in datayears) {
    ## legacy filename
    legacyout <- paste0("~/DATA/Broad_Band/Legacy_L0_CHP1_", YYYY, ".Rds")
    ## get data from DB
    year_data <- BB |>
        filter(year == YYYY) |>
        select(c("Date", "CHP1_sig", "CHP1_sig_sd","Async_step_count",
                 "Async_tracker", "Azimuth", "Elevat", "chp1_temperature",
                 "chp1_temperature_SD", "chp1_temp_UNC",
                 "chp1_bad_data")) |>
        collect()
    year_data <- data.table(year_data)

    if (!file.exists(legacyout) |
        file.mtime(legacyout) < max(BB_meta$cm21_bad_data_flagged, na.rm = T) |
        file.mtime(legacyout) < max(BB_meta$cm21_parsed, na.rm = T)) {
        cat("Will export ", legacyout, "\n")
    } else {
        cat("SKIPPING ", legacyout, "\n")
        next()
    }

    ## Create physical Recording limits ----------------------------------------
    year_data[, sig_lowlim := chp1_signal_lower_limit(Date)]
    year_data[, sig_upplim := chp1_signal_upper_limit(Date)]

    ## Apply some filtering ----------------------------------------------------
    year_data[!is.na(CHP1_sig), .N]
    cat("\nRemove bad data regions\n")
    cat(year_data[!is.na(chp1_bad_data), .N], year_data[!is.na(CHP1_sig), .N], "\n\n")
    year_data$CHP1_sig   [!is.na(year_data$chp1_bad_data)] <- NA
    year_data$CHP1_sig_sd[!is.na(year_data$chp1_bad_data)] <- NA

    cat("\nRemove tracker async cases\n")
    cat(year_data[Async_tracker == TRUE, .N], year_data[!is.na(CHP1_sig), .N], "\n\n")
    year_data$CHP1_sig   [year_data$Async_tracker == TRUE] <- NA
    year_data$CHP1_sig_sd[year_data$Async_tracker == TRUE] <- NA

    # cat("\nRemove data above physical limits\n")
    # cat(year_data[CHP1_sig > sig_upplim, .N], year_data[!is.na(CHP1_sig), .N], "\n\n")
    # year_data$CHP1_sig[year_data$CHP1_sig > year_data$sig_upplim] <- NA
    # year_data$CHP1_sig[year_data$CHP1_sig > year_data$sig_upplim] <- NA
    #
    # cat("\nRemove data below physical limits\n")
    # cat(year_data[CHP1_sig < sig_lowlim, .N], year_data[!is.na(CHP1_sig), .N], "\n\n")
    # year_data$CHP1_sig[year_data$CHP1_sig < year_data$sig_lowlim] <- NA
    # year_data$CHP1_sig[year_data$CHP1_sig < year_data$sig_lowlim] <- NA

    year_data$sig_lowlim    <- NULL
    year_data$sig_upplim    <- NULL
    year_data$chp1_bad_data <- NULL

    ## Clean temperature data --------------------------------------------------
    CHP_TEMP_MIN       <- -20    # Drop temperatures below this value
    CHP_TEMP_MAX       <-  50    # Drop temperatures above this value
    CHP_TEMP_STD_LIM   <-  10    # Drop temperatures with standard deviation above this value

    year_data$chp1_temperature_SD[ year_data$chp1_temperature > CHP_TEMP_MAX] <- NA
    year_data$chp1_temp_UNC      [ year_data$chp1_temperature > CHP_TEMP_MAX] <- NA
    year_data$chp1_temperature   [ year_data$chp1_temperature > CHP_TEMP_MAX] <- NA

    year_data$chp1_temperature_SD[ year_data$chp1_temperature < CHP_TEMP_MIN] <- NA
    year_data$chp1_temp_UNC      [ year_data$chp1_temperature < CHP_TEMP_MIN] <- NA
    year_data$chp1_temperature   [ year_data$chp1_temperature < CHP_TEMP_MIN] <- NA

    year_data$chp1_temperature   [ year_data$chp1_temperature_SD > CHP_TEMP_STD_LIM] <- NA
    year_data$chp1_temp_UNC      [ year_data$chp1_temperature_SD > CHP_TEMP_STD_LIM] <- NA
    year_data$chp1_temperature_SD[ year_data$chp1_temperature_SD > CHP_TEMP_STD_LIM] <- NA


    setorder(year_data, Date)

    ## Use the old names for output --------------------------------------------
    names(year_data)[names(year_data) == "Date"]                <- "Date30"
    names(year_data)[names(year_data) == "CHP1_sig"]            <- "CHP1value"
    names(year_data)[names(year_data) == "CHP1_sig_sd"]         <- "CHP1sd"
    names(year_data)[names(year_data) == "Async_step_count"]    <- "AsynStep"
    names(year_data)[names(year_data) == "Async_tracker"]       <- "Async"
    names(year_data)[names(year_data) == "chp1_temperature"]    <- "CHP1temp"
    names(year_data)[names(year_data) == "chp1_temperature_SD"] <- "CHP1tempSD"
    names(year_data)[names(year_data) == "chp1_temp_UNC"]       <- "CHP1tempUNC"

    ## Write data to old file format  ------------------------------------------
    write_RDS(year_data,
              legacyout,
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





## Do a data comparison --------------------------------------------------------

listlegacy <- list.files(path   = "~/DATA/Broad_Band/",
                         pattern = "Legacy_L0_CHP1_[0-9]{4}\\.Rds",
                         full.names = TRUE, ignore.case = TRUE)

gather <- data.table()
#+ echo=F, include=T, results="asis"
for (alf in listlegacy) {
    legacy <- readRDS(alf)
    yyyy   <- unique(year(legacy$Date30))[1]
    baseDT <- data.table(readRDS(paste0("~/DATA/Broad_Band/LAP_CHP1_L0_",yyyy,".Rds")))

    cat(paste("\n\n##", yyyy, "\n\n"))

    baseDT$Date <- NULL

    baseDT$Azimuth <- NULL
    legacy$Azimuth <- NULL
    baseDT$Elevat  <- NULL
    legacy$Elevat  <- NULL
    baseDT$CHP1tempSD <- NULL
    legacy$CHP1tempSD <- NULL
    baseDT$CHP1tempUNC <- NULL
    legacy$CHP1tempUNC <- NULL

    legacy[Async == FALSE, Async:=NA]
    baseDT[Async == FALSE, Async:=NA]

    legacy$Date30 <- as.POSIXct(legacy$Date30, tz = "UTC")
    baseDT$Date30 <- as.POSIXct(baseDT$Date30, tz = "UTC")

    baseDT <- baseDT[!is.na(CHP1value)]
    legacy <- legacy[!is.na(CHP1value)]


    setorder(baseDT, Date30)
    setorder(legacy, Date30)
stop()

    sss <- merge(baseDT, legacy, by = "Date30", all = T)

    vec <- sss[CHP1value.x == CHP1value.y]
    sss[vec, CHP1value.x := NA ]
    sss[vec, CHP1value.y := NA ]
    plot(  sss$Date30, sss$CHP1value.x, col = "red")
    points(sss$Date30, sss$CHP1value.y, col = "blue")

    vec <- sss[CHP1sd.x == CHP1sd.y]
    sss[vec, CHP1sd.x := NA ]
    sss[vec, CHP1sd.y := NA ]
    plot(  sss$Date30, sss$CHP1sd.x, col = "red")
    points(sss$Date30, sss$CHP1sd.y, col = "blue")

    vec <- sss[CHP1temp.x == CHP1temp.y]
    sss[vec, CHP1temp.x := NA ]
    sss[vec, CHP1temp.y := NA ]
    # plot(  sss$Date30, sss$CHP1temp.y, col = "red")
    # points(sss$Date30, sss$CHP1temp.x, col = "blue")


    vec <- sss[Async.x == Async.y]
    sss[vec, Async.x := NA ]
    sss[vec, Async.y := NA ]
    plot(  sss$Date30, sss$Async.y, col = "red")
    points(sss$Date30, sss$Async.x, col = "blue")


    sss <- sss[apply(sss, MARGIN = 1, function(x) sum(is.na(x))) < 10]

    gather <- rbind(gather,sss, fill=T)

    cat("\n\n")
    cat(pander(summary(sss)))
    cat("\n\n")

    ss <- arsenal::comparedf(legacy, baseDT,
                    by = "Date30",
                    int.as.num = TRUE)




    dd <- compareDF::compare_df(legacy, baseDT,
                          group_col = "Date30",
                          tolerance = 0.00001)

    print(summary(ss))
    cat("\n\n")

}







#' **END**
#+ include=T, echo=F
# myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
