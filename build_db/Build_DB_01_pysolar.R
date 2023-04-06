#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Read PySolar files `sun_path_.*.dat.gz`
#'
#' Populates:
#'  - Date
#'  - Azimuth
#'  - Elevat
#'  - SZA
#'  - preNoon
#'

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_db/Build_DB_01_pysolar.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)

TEST <- FALSE
# TEST <- TRUE

cat("\n Initialize DB or import  PySolar  Sun data\n\n")

##  Initialize meta data file  -------------------------------------------------
if (file.exists(DB_META_fl)) {
    BB_meta <- read_parquet(DB_META_fl)
    BB_meta <- merge(BB_meta,
                     data.table(day = seq(from = min(max(BB_meta$day),        ## start of meta data table
                                                     as_date(DB_start_date)), ## start of project
                                          to   = (Sys.Date() - 1),            ## don't include today
                                          by   = "day")),
                     by = "day",
                     all = TRUE)
    stopifnot(sum(duplicated(BB_meta$day)) == 0)
} else {
    warning("STARTING NEW DB!!")
    BB_meta <- data.table(day = seq(as_date(DB_start_date), Sys.Date(), by = "day"))
    ## add general columns
    BB_meta$pysolar_basename      <- as.character(NA)
    BB_meta$pysolar_mtime         <- as.POSIXct(NA)
    BB_meta$pysolar_parsed        <- as.POSIXct(NA)
    BB_meta$pysolar_basename      <- as.character(BB_meta$pysolar_basename)
    BB_meta$pysolar_mtime         <- as.POSIXct(BB_meta$pysolar_mtime )
    BB_meta$pysolar_parsed        <- as.POSIXct(BB_meta$pysolar_parsed)
    ## For CM-21 meta data
    BB_meta$cm21_Daily_dark       <- as.numeric(NA)
    BB_meta$cm21_bad_data_flagged <- as.POSIXct(NA)
    BB_meta$cm21_basename         <- as.character(NA)
    BB_meta$cm21_dark_Eve_avg     <- as.numeric(NA)
    BB_meta$cm21_dark_Eve_cnt     <- as.integer(NA)
    BB_meta$cm21_dark_Eve_end     <- as.POSIXct(NA)
    BB_meta$cm21_dark_Eve_med     <- as.numeric(NA)
    BB_meta$cm21_dark_Eve_sta     <- as.POSIXct(NA)
    BB_meta$cm21_dark_Mor_avg     <- as.numeric(NA)
    BB_meta$cm21_dark_Mor_cnt     <- as.integer(NA)
    BB_meta$cm21_dark_Mor_end     <- as.POSIXct(NA)
    BB_meta$cm21_dark_Mor_med     <- as.numeric(NA)
    BB_meta$cm21_dark_Mor_sta     <- as.POSIXct(NA)
    BB_meta$cm21_dark_computed    <- as.POSIXct(NA)
    BB_meta$cm21_dark_flag        <- as.character(NA)
    BB_meta$cm21_md5sum           <- as.character(NA)
    BB_meta$cm21_mtime            <- as.POSIXct(NA)
    BB_meta$cm21_parsed           <- as.POSIXct(NA)
    BB_meta$cm21_sig_NAs          <- as.integer(NA)
    BB_meta$cm21_sig_sd_NAs       <- as.integer(NA)
    ## Fro CHP-1 meta data
    BB_meta$chp1_Daily_dark       <- as.numeric(NA)
    BB_meta$chp1_bad_data_flagged <- as.POSIXct(NA)
    BB_meta$chp1_bad_temp_flagged <- as.POSIXct(NA)
    BB_meta$chp1_basename         <- as.character(NA)
    BB_meta$chp1_dark_Eve_avg     <- as.numeric(NA)
    BB_meta$chp1_dark_Eve_cnt     <- as.integer(NA)
    BB_meta$chp1_dark_Eve_end     <- as.POSIXct(NA)
    BB_meta$chp1_dark_Eve_med     <- as.numeric(NA)
    BB_meta$chp1_dark_Eve_sta     <- as.POSIXct(NA)
    BB_meta$chp1_dark_Mor_avg     <- as.numeric(NA)
    BB_meta$chp1_dark_Mor_cnt     <- as.integer(NA)
    BB_meta$chp1_dark_Mor_end     <- as.POSIXct(NA)
    BB_meta$chp1_dark_Mor_med     <- as.numeric(NA)
    BB_meta$chp1_dark_Mor_sta     <- as.POSIXct(NA)
    BB_meta$chp1_dark_computed    <- as.POSIXct(NA)
    BB_meta$chp1_dark_flag        <- as.character(NA)
    BB_meta$chp1_md5sum           <- as.character(NA)
    BB_meta$chp1_mtime            <- as.POSIXct(NA)
    BB_meta$chp1_parsed           <- as.POSIXct(NA)
    BB_meta$chp1_sig_NAs          <- as.integer(NA)
    BB_meta$chp1_sig_sd_NAs       <- as.integer(NA)
}



##  Get PySolar files  ---------------------------------------------------------
inp_filelist <- list.files(path       = SUN_FOLDER,
                           pattern    = "sun_path_.*.dat.gz",
                           recursive  = TRUE,
                           full.names = TRUE)
inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, basename := basename(fullname)]
inp_filelist$day <- as.Date(
    strptime(
        sub("\\.dat\\.gz", "",
            sub("sun_path_", "",
                inp_filelist$basename)),
        format = "%F"))
setorder(inp_filelist, day)
cat("\n**Found:",paste(nrow(inp_filelist), "PySolar files**\n"))

## only new files in the date range
inp_filelist <- inp_filelist[!inp_filelist$basename %in% BB_meta$pysolar_basename]
inp_filelist <- inp_filelist[inp_filelist$day %in% BB_meta$day]

cat("\n**Parse:",paste(nrow(inp_filelist), "PySolar files**\n\n"))

## test random
if (TEST) {
    cat("\nTEST MODE IS ON!!  ", Script.Name, "\n\n")
    inp_filelist <- unique(rbind(
        inp_filelist[ 1:100 ],
        inp_filelist[sample(1:nrow(inp_filelist), 100)],
        NULL
    ))
    setorder(inp_filelist, day)
}



##  Import PySolar files  ------------------------------------------------------
for (YYYY in unique(year(inp_filelist$day))) {
    subyear <- inp_filelist[year(day) == YYYY]
    ## months to do
    for (mm in subyear[, unique(month(day))]) {
        submonth <- subyear[month(day) == mm]
        ## export file name and hive dir
        filedir <- paste0(DB_DIR, "/", YYYY, "/", mm, "/" )
        dir.create(filedir, recursive = TRUE, showWarnings = FALSE)
        partfile <- paste0(filedir, "/part-0.parquet")
        ## init data collector
        if (file.exists(partfile)) {
            cat(" Load: ", partfile, "\n")
            gather <- read_parquet(partfile)
            ## columns may be missing while repacking dataset
            gather$year  <- year(gather$Date)
            gather$month <- month(gather$Date)
        } else {
            cat("* NEW: ", partfile, "\n")
            gather <- data.table()
        }

        ##  read this month set files
        gathermeta <- data.table()
        for (ad in submonth$day) {
            ss <- submonth[day == ad]
            ## Read sun data file  ---------------------------------------------
            sun_temp <- fread(ss$fullname, na.strings = "None")
            names(sun_temp)[names(sun_temp) == "DATE"] <- "Date"
            names(sun_temp)[names(sun_temp) == "AZIM"] <- "Azimuth"
            names(sun_temp)[names(sun_temp) == "ELEV"] <- "Elevat"
            sun_temp[, DIST  := NULL]
            sun_temp[, SZA   := 90 - Elevat]
            sun_temp[, year  := year( Date)]
            sun_temp[, month := month(Date)]
            sun_temp[, doy   := yday( Date)]
            ## Get metadata for each sun file ----------------------------------
            sun_meta <- data.table(day              = as_date(ad),
                                   pysolar_basename = basename(ss$fullname),
                                   pysolar_mtime    = file.mtime(ss$fullname),
                                   pysolar_parsed   = Sys.time())
            ## Here we can init more variables of the database!! ---------------
            sun_temp[Azimuth <= 180, preNoon := TRUE ]
            sun_temp[Azimuth >  180, preNoon := FALSE]

            ## Init DB variables for next processes ----------------------------
            ## For CM-21
            sun_temp[, CM21_sig                := as.numeric(NA)  ]
            sun_temp[, CM21_sig_sd             := as.numeric(NA)  ]
            sun_temp[, CM21_sig_wo_dark        := as.numeric(NA)  ]
            sun_temp[, cm21_bad_data_flag      := as.character(NA)]
            ## For CHP-1
            sun_temp[, Async_step_count        := as.integer(NA)  ]
            sun_temp[, Async_tracker_flag      := TRUE            ]
            sun_temp[, CHP1_sig                := as.numeric(NA)  ]
            sun_temp[, CHP1_sig_sd             := as.numeric(NA)  ]
            sun_temp[, CHP1_sig_wo_dark        := as.numeric(NA)  ]
            sun_temp[, chp1_R_SD_therm         := as.numeric(NA)  ]
            sun_temp[, chp1_R_meas_ERR         := as.numeric(NA)  ]
            sun_temp[, chp1_R_therm            := as.numeric(NA)  ]
            sun_temp[, chp1_bad_data_flag      := as.character(NA)]
            sun_temp[, chp1_temp_UNC           := as.numeric(NA)  ]
            sun_temp[, chp1_temp_bad_data_flag := as.character(NA)]
            sun_temp[, chp1_temperature        := as.numeric(NA)  ]
            sun_temp[, chp1_temperature_SD     := as.numeric(NA)  ]
            sun_temp[, chp1_bad_temp_flag      := as.character(NA)]
            ## For TOT
            sun_temp[, tot_glb                 := as.numeric(NA)  ]
            sun_temp[, tot_glb_sd              := as.numeric(NA)  ]
            sun_temp[, lap_sza                 := as.numeric(NA)  ]
            ## Radiation
            sun_temp[, DIR_wpsm                := as.numeric(NA)  ]
            sun_temp[, DIR_SD_wpsm             := as.numeric(NA)  ]
            sun_temp[, GLB_wpsm                := as.numeric(NA)  ]
            sun_temp[, GLB_SD_wpsm             := as.numeric(NA)  ]
            sun_temp[, HOR_wpsm                := as.numeric(NA)  ]
            sun_temp[, HOR_SD_wpsm             := as.numeric(NA)  ]
            ## Sun
            sun_temp[, Sun_Dist_Astropy        := as.numeric(NA)  ]
            sun_temp[, TSI_TOA                 := as.numeric(NA)  ]
            sun_temp[, TSI_1au                 := as.numeric(NA)  ]
            sun_temp[, TSI_source              := as.character(NA)]


            ## gather data
            if (nrow(gather) == 0) {
                ## this inits the database table!!
                gather     <- as_tibble(sun_temp)
            } else {
                gather     <- rows_upsert(gather, sun_temp, by = "Date")
            }
            gathermeta <- rbind(gathermeta, sun_meta)
            rm(sun_temp, sun_meta, ss)
        }

        BB_meta <- rows_update(BB_meta, gathermeta, by = "day")

        setorder(gather, Date)

        ## store this month / set data
        write_parquet(gather,  partfile)
        write_parquet(BB_meta, DB_META_fl)
        rm(gather, gathermeta, submonth)
    }
    rm(subyear)
}
rm(inp_filelist)



myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
