#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#' 
#' Reads temperature data for CHP-1 into the database
#' 
#'  - Reads raw resistance data
#'  - Converts resistance to temperature
#'

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_db/Build_DB_05_chp1_TMP.R"

source("~/CHP_1_DIR/Functions_CHP1.R")
source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)


TEST <- FALSE
# TEST <- TRUE

cat("\n Import  CHP-1 temperature data\n\n")

##  Initialize meta data file  -------------------------------------------------
if (file.exists(DB_META_fl)) {
    BB_meta <- read_parquet(DB_META_fl)
    BB_meta <- merge(BB_meta,
                     data.table(day = seq(max(BB_meta$day), Sys.Date(),
                                          by = "day")),
                     by = "day",
                     all = TRUE)
    stopifnot(sum(duplicated(BB_meta$day)) == 0)
    ## new columns
    var <- "chp1_temp_basename"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.character(BB_meta[[var]])
    }
    var <- "chp1_temp_md5sum"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.character(BB_meta[[var]])
    }
    var <- "chp1_temp_mtime"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.POSIXct(BB_meta[[var]])
    }
    var <- "chp1_temp_parsed"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.POSIXct(BB_meta[[var]])
    }
} else {
    stop("STAR A NEW DB!!")
}



##  Get tracker sync files  --------------------------------------------------------
inp_filelist <- list.files(path        = CHPTMP_DIR,
                           recursive   = TRUE,
                           pattern     = "sun_tracker_.*.therm$",,
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:",paste(length(inp_filelist), "CHP-1 temperature files**\n"))




inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, chp1_temp_basename := basename(fullname)]
stopifnot( all(duplicated(sub("\\..*", "", inp_filelist$chp1_temp_basename))) == FALSE )


inp_filelist$day <- as.Date(parse_date_time(
    sub("\\.therm", "", sub("sun_tracker_", "", inp_filelist$chp1_temp_basename)),
    "Ymd"))
setorder(inp_filelist, day)
cat("\n**Found:",paste(nrow(inp_filelist), "CHP-1 temperature files**\n"))

## only new files in the date range
inp_filelist <- inp_filelist[!inp_filelist$chp1_temp_basename %in% BB_meta$chp1_temp_basename]
inp_filelist <- inp_filelist[inp_filelist$day %in% BB_meta$day]

cat("\n**Parse:",paste(nrow(inp_filelist), "CHP-1 temperature files**\n\n"))

## test random
if (TEST) {
    cat("\nTEST MODE IS ON!!  ", Script.Name, "\n\n")
    inp_filelist <- unique(rbind(
        inp_filelist[ 1:30 ],
        inp_filelist[sample(1:nrow(inp_filelist), 30)],
        NULL
    ))
    setorder(inp_filelist, day)
}


##  Import CHP-1 files  ------------------------------------------------------
for (YYYY in unique(year(inp_filelist$day))) {
    subyear <- inp_filelist[year(day) == YYYY]
    ## months to do
    for (mm in subyear[, unique(month(day))]) {
        submonth <- subyear[month(day) == mm]
        ## export file name and hive dir
        filedir <- paste0(DB_DIR, "/", YYYY, "/", mm, "/")
        dir.create(filedir, recursive = TRUE, showWarnings = FALSE)
        partfile <- paste0(filedir, "/part-0.parquet")
        ## init data collector
        if (file.exists(partfile)) {
            cat(" Load: ", partfile, "\n")
            gather <- read_parquet(partfile)
            ## add columns for this set
            var <- "chp1_R_therm"
            if (!any(names(gather) == var)) {
                gather[[var]] <- NA
                gather[[var]] <- as.numeric(gather[[var]])
            }
            var <- "chp1_R_SD_therm"
            if (!any(names(gather) == var)) {
                gather[[var]] <- NA
                gather[[var]] <- as.numeric(gather[[var]])
            }
            var <- "chp1_R_meas_ERR"
            if (!any(names(gather) == var)) {
                gather[[var]] <- NA
                gather[[var]] <- as.numeric(gather[[var]])
            }
            var <- "chp1_temperature"
            if (!any(names(gather) == var)) {
                gather[[var]] <- NA
                gather[[var]] <- as.numeric(gather[[var]])
            }
            var <- "chp1_temperature_SD"
            if (!any(names(gather) == var)) {
                gather[[var]] <- NA
                gather[[var]] <- as.numeric(gather[[var]])
            }
            var <- "chp1_temp_UNC"
            if (!any(names(gather) == var)) {
                gather[[var]] <- NA
                gather[[var]] <- as.numeric(gather[[var]])
            }
            var <- "year"
            if (!any(names(gather) == var)) {
                gather[[var]] <- NA
                gather[[var]] <- as.integer(gather[[var]])
            }
            var <- "month"
            if (!any(names(gather) == var)) {
                gather[[var]] <- NA
                gather[[var]] <- as.integer(gather[[var]])
            }
        } else {
            cat("Skipping new rows data inport", partfile, "\n")
            next()
            ## This can work, but there is no need for it.
            ## Sun script should initialize the DB rows.
        }

        ##  read this month set files
        gathermeta <- data.table()
        for (ad in submonth$day) {
            ss <- submonth[day == ad]

            ## __  Read CHP-1 temperature file  --------------------------------
            temp_temp    <- read.table(ss$fullname, sep = "\t", as.is = TRUE)
            temp_temp$V1 <- as.POSIXct( temp_temp$V1 )
            temp_temp$V1 <- as.POSIXct( format( temp_temp$V1, format = "%F %R" ) )
            temp_temp$V1 <- temp_temp$V1 + 30
            temp_temp$V3[ temp_temp$V3 == 0 ] <- NA

            temp_temp    <- data.table(temp_temp)
            temp_temp    <- temp_temp[, .( V2 = mean(V2, na.rm = T),
                                           V3 = mean(V3, na.rm = T) ), by = V1 ]

            temp_temp[V2 < 0, V2 := NA]
            temp_temp[V2 < 0, V3 := NA]

            day_data <- data.frame(Date                = temp_temp$V1,
                                   year                = year(temp_temp$V1),
                                   month               = month(temp_temp$V1),
                                   chp1_R_therm        = temp_temp$V2,
                                   chp1_R_SD_therm     = temp_temp$V3,
                                   chp1_R_meas_ERR     = Protek_506_R_error(    temp_temp$V2),
                                   chp1_temperature    = CHP_thermistor_R_to_T( temp_temp$V2),
                                   chp1_temperature_SD = CHP_thermistor_ResUnc_to_TempUnc(temp_temp$V2, temp_temp$V3))
            day_data$chp1_temp_UNC <- CHP_thermistor_ResUnc_to_TempUnc(temp_temp$V2, day_data$chp1_R_meas_ERR)

            ## get file metadata
            file_meta <- data.table(day                = as_date(ad),
                                    chp1_temp_basename = basename(ss$fullname),
                                    chp1_temp_mtime    = file.mtime(ss$fullname),
                                    chp1_temp_parsed   = Sys.time(),
                                    chp1_temp_md5sum   = as.vector(md5sum(ss$fullname)))

            # gather <- rows_patch(gather, day_data, by = "Date")
            gather     <- rows_upsert(gather, day_data, by = "Date")
            gathermeta <- rbind(gathermeta, file_meta)
            rm(day_data, file_meta, ss)
            rm(temp_temp)
        }

        BB_meta <- rows_update(BB_meta, gathermeta, by = "day")
        # BBdaily <- rows_patch(BBdaily, gathermeta, by = "day", unmatched = "ignore")

        setorder(gather, Date)

        ## store this month / set data
        write_parquet(gather,  partfile)
        write_parquet(BB_meta, DB_META_fl)
        rm(gather, gathermeta, submonth)
    }
    rm(subyear)
}
rm(inp_filelist)






on.exit(myunlock(DB_lock))
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
