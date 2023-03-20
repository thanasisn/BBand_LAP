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
#'

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({funr::sys.script()},
                        error = function(e) {
                            cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("Buid_DB_01_")
                        })

source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)
on.exit(myunlock(DB_lock))

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

cat("\n Initialize DB or import  PySolar  Sun data\n\n")

##  Initialize meta data file  -------------------------------------------------
if (file.exists(DB_META_fl)) {
    BB_meta <- read_parquet(DB_META_fl)
    BB_meta <- merge(BB_meta,
                     data.table(day = seq(max(BB_meta$day), Sys.Date(),
                                          by = "day")),
                     by = "day",
                     all = TRUE)
    stopifnot(sum(duplicated(BB_meta$day)) == 0)
} else {
    warning("STARTING NEW DB!!")
    BB_meta <- data.table(day = seq(as_date(DB_start_date), Sys.Date(), by = "day"))
    ## add columns
    BB_meta$pysolar_basename <- NA
    BB_meta$pysolar_mtime    <- NA
    BB_meta$pysolar_parsed   <- NA
    BB_meta$pysolar_basename <- as.character(BB_meta$pysolar_basename)
    BB_meta$pysolar_mtime    <- as.POSIXct(BB_meta$pysolar_mtime )
    BB_meta$pysolar_parsed   <- as.POSIXct(BB_meta$pysolar_parsed)
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
        } else {
            cat("* NEW: ", partfile, "\n")
            gather <- data.table()
        }

        ##  read this month set files
        gathermeta <- data.table()
        for (ad in submonth$day) {
            ss <- submonth[day == ad]
            ## read sun data file
            sun_temp <- fread(ss$fullname, na.strings = "None")
            names(sun_temp)[names(sun_temp) == "DATE"] <- "Date"
            names(sun_temp)[names(sun_temp) == "AZIM"] <- "Azimuth"
            names(sun_temp)[names(sun_temp) == "ELEV"] <- "Elevat"
            sun_temp[, DIST := NULL]
            sun_temp[, SZA  := 90 - Elevat]
            sun_temp[, year  := year( Date)]
            sun_temp[, month := month(Date)]
            sun_temp[, doy   := yday( Date)]
            ## get metadata for each file
            sun_meta <- data.table(day              = as_date(ad),
                                   pysolar_basename = basename(ss$fullname),
                                   pysolar_mtime    = file.mtime(ss$fullname),
                                   pysolar_parsed   = Sys.time())
            ## aggregate data
            if (nrow(gather) == 0) {
                gather     <- rbind(gather, sun_temp, fill = TRUE )
            } else {
                gather     <- rows_upsert(gather, sun_temp, by = "Date" )
            }
            gathermeta <- rbind(gathermeta, sun_meta)
            rm(sun_temp, sun_meta, ss)
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



# myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
