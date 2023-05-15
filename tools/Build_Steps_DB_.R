#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Read PySolar files `sun_path_.*.dat.gz`
#'
#' This also initializes a lot of columns in the dataset and meta data.
#'
#' Populates:
#'  - Date
#'  - Azimuth
#'
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
#'
#+ echo=F, include=T


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
Script.Name <- "~/BBand_LAP/build_db/Build_DB.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
# source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)

DB_Steps_META_fl <- "~/DATA/Broad_Band/CHP1_Tracker_steps.parquet"
trSTP_DIR        <- "~/DATA_RAW/tracker_chp1/Tracker_STEP/"
DB_Steps_DIR     <- "~/DATA/Broad_Band/CHP1_Tracker_steps/"

cat("\n Initialize DB and import  Tracker steps files\n\n")

##  Initialize meta data file  -------------------------------------------------
if (file.exists(DB_Steps_META_fl)) {
    BB_meta <- read_parquet(DB_Steps_META_fl)
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
    BB_meta <- data.table(day = seq(as_date("2016-04-01"), Sys.Date(), by = "day"))
    ## For tracker step files
    BB_meta$Steps_basename     <- as.character(NA)
    BB_meta$Steps_mtime        <- as.POSIXct(NA)
    BB_meta$Steps_parsed       <- as.POSIXct(NA)
    ## For tracker async files
    BB_meta$Async_basename     <- as.character(NA)
    BB_meta$Async_mtime        <- as.POSIXct(NA)
    BB_meta$Async_parsed       <- as.POSIXct(NA)
}


##  Get Tracker steps files  ---------------------------------------------------
inp_filelist <- list.files(path       = trSTP_DIR,
                           pattern    = "sun_tracker_.*.stp",
                           recursive  = TRUE,
                           full.names = TRUE)
inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, basename := basename(fullname)]

inp_filelist$day <- as.Date(
    strptime(
        sub("\\.stp", "",
            sub("sun_tracker_", "",
                inp_filelist$basename)),
        format = "%F"))
setorder(inp_filelist, day)
cat("\n**Found:", paste(nrow(inp_filelist), "Tracker steps files**\n"))

## only new files in the date range
inp_filelist <- inp_filelist[!inp_filelist$basename %in% BB_meta$pysolar_basename]
inp_filelist <- inp_filelist[inp_filelist$day %in% BB_meta$day]

cat("\n**Parse:",paste(nrow(inp_filelist), "Tracker steps files**\n\n"))



##  Import PySolar files  ------------------------------------------------------
for (YYYY in unique(year(inp_filelist$day))) {
    subyear <- inp_filelist[year(day) == YYYY]
    ## export file name and hive dir
    filedir <- paste0(DB_Steps_DIR, "/", YYYY, "/")
    dir.create(filedir, recursive = TRUE, showWarnings = FALSE)
    partfile <- paste0(filedir, "/part-0.parquet")
    ## init data collector
    if (file.exists(partfile)) {
        cat("99 Load: ", partfile, "\n")
        gather <- read_parquet(partfile)
        ## columns may be missing while repacking dataset
        gather$year  <- year(gather$Date)
        gather$month <- month(gather$Date)
    } else {
        cat("99  NEW: ", partfile, "\n")
        gather <- data.table()
    }

    ##  read this years set files
    gathermeta <- data.table()
    for (ad in subyear$day) {
        ss <- subyear[day == ad]
        ## Read sun data file  ---------------------------------------------
        step_temp <- fread(ss$fullname, na.strings = "None")
    stop()
        names(step_temp)[names(step_temp) == "V1"] <- "Date"
        names(step_temp)[names(step_temp) == "V2"] <- "Axis"
        names(step_temp)[names(step_temp) == "V3"] <- "AxisNum"
        names(step_temp)[names(step_temp) == "V3"] <- "AxisNum"
        step_temp[, DIST  := NULL]
        step_temp[, SZA   := 90 - Elevat]
        step_temp[, year  := year( Date)]
        step_temp[, month := month(Date)]
        step_temp[, doy   := yday( Date)]

        ## Get metadata for each sun file ----------------------------------
        sun_meta <- data.table(day              = as_date(ad),
                               pysolar_basename = basename(ss$fullname),
                               pysolar_mtime    = file.mtime(ss$fullname),
                               pysolar_parsed   = Sys.time())

        ## Here we can init more variables of the database -----------------
        step_temp[Azimuth <= 180, preNoon := TRUE ]
        step_temp[Azimuth >  180, preNoon := FALSE]

        ## Init DB variables for next processes ----------------------------
        ## For CM-21
        step_temp[, CM21_sig                := as.numeric(NA)  ]
        step_temp[, CM21_sig_sd             := as.numeric(NA)  ]
        step_temp[, CM21_sig_wo_dark        := as.numeric(NA)  ]
        step_temp[, cm21_bad_data_flag      := as.character(NA)]
        ## For INCLINED CM-21
        step_temp[, CM21INC_sig             := as.numeric(NA)  ]
        step_temp[, CM21INC_sig_sd          := as.numeric(NA)  ]
        step_temp[, CM21INC_sig_wo_dark     := as.numeric(NA)  ]
        step_temp[, cm21INC_bad_data_flag   := as.character(NA)]
        ## For CHP-1
        step_temp[, Async_step_count        := as.integer(NA)  ]
        step_temp[, Async_tracker_flag      := TRUE            ]
        step_temp[, CHP1_sig                := as.numeric(NA)  ]
        step_temp[, CHP1_sig_sd             := as.numeric(NA)  ]
        step_temp[, CHP1_sig_wo_dark        := as.numeric(NA)  ]
        step_temp[, chp1_R_SD_therm         := as.numeric(NA)  ]
        step_temp[, chp1_R_meas_ERR         := as.numeric(NA)  ]
        step_temp[, chp1_R_therm            := as.numeric(NA)  ]
        step_temp[, chp1_bad_data_flag      := as.character(NA)]
        step_temp[, chp1_bad_temp_flag      := as.character(NA)]
        step_temp[, chp1_t_cor_factor       := as.numeric(NA)  ]
        step_temp[, chp1_temp_UNC           := as.numeric(NA)  ]
        step_temp[, chp1_temperature        := as.numeric(NA)  ]
        step_temp[, chp1_temperature_SD     := as.numeric(NA)  ]
        step_temp[, Sun_Dist_Astropy        := as.numeric(NA)  ]
        step_temp[, TSI_TOA                 := as.numeric(NA)  ]
        step_temp[, TSI_1au                 := as.numeric(NA)  ]
        step_temp[, TSI_source              := as.character(NA)]
        ## Pressure
        step_temp[, Pressure                := as.numeric(NA)  ]
        step_temp[, Pressure_source         := as.character(NA)]

        ## gather data
        if (nrow(gather) == 0) {
            ## this inits the database table!!
            gather     <- as_tibble(step_temp)
        } else {
            gather     <- rows_upsert(gather, step_temp, by = "Date")
        }
        gathermeta <- rbind(gathermeta, sun_meta)
        rm(step_temp, sun_meta, ss)
    }


    stop()
    BB_meta <- rows_update(BB_meta, gathermeta, by = "day")

    setorder(gather, Date)

    ## store this month / set data
    write_parquet(gather,  partfile)
    write_parquet(BB_meta, DB_META_fl)
    rm(gather, gathermeta, submonth)
}
rm(subyear)

rm(inp_filelist)








# ## Sycfile deffinition
#
# str(now)
# str(step)
# str(response)
# str(az_count)
# str(freq_az)
# str(freq_main)
# with open(SYNCFILE, "a+") as myfile:
#     myfile.write(text)
#
#
#
# ## Step file definition
#
# if response == step:
# str(now)
#  \ta
#  \t1\t" + \
# str(step) + "\t" + \
# str(sun_azimuth) + "\t" + \
# str(az_count) + "\t" + \
# str(tracker_azi_ang) + "\n"



# myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
