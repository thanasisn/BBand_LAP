#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Reads tracker synchronization data form `sun_tracker_.*.snc$`
#'
#' Populates:
#'  - Async_tracker_flag
#'
#' There are more data but are parsed here
#'

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_db/Build_DB_04_chp1_SNC.R"

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
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

cat("\n Import  CHP-1  data\n\n")

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
    var <- "chp1_sync_basename"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.character(BB_meta[[var]])
    }
    var <- "chp1_sync_md5sum"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.character(BB_meta[[var]])
    }
    var <- "chp1_sync_mtime"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.POSIXct(BB_meta[[var]])
    }
    var <- "chp1_sync_parsed"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.POSIXct(BB_meta[[var]])
    }
} else {
    stop("STAR A NEW DB!!")
}



##  Get tracker sync files  ----------------------------------------------------
inp_filelist <- list.files(path        = trSYNC_DIR,
                           recursive   = TRUE,
                           pattern     = "sun_tracker_.*.snc$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:",paste(length(inp_filelist), "tracker sync files**\n"))

inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, chp1_sync_basename := basename(fullname)]
stopifnot( all(duplicated(sub("\\..*", "", inp_filelist$chp1_sync_basename))) == FALSE )

inp_filelist$day <- as.Date(parse_date_time(
    sub("\\.snc", "", sub("sun_tracker_", "", inp_filelist$chp1_sync_basename)),
    "Ymd"))
setorder(inp_filelist, day)
cat("\n**Found:",paste(nrow(inp_filelist), "tracker sync files**\n"))


## days with a sync file
syncfldates  <- inp_filelist$day

## only new files in the date range
inp_filelist <- inp_filelist[!inp_filelist$chp1_sync_basename %in% BB_meta$chp1_sync_basename]
inp_filelist <- inp_filelist[inp_filelist$day %in% BB_meta$day]

cat("\n**Parse:",paste(nrow(inp_filelist), "tracker sync files**\n\n"))

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



##  Import CHP-1 files  --------------------------------------------------------
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
            ## add columns for this set
            var <- "Async_tracker_flag"
            if (!any(names(gather) == var)) {
                ## init with tracker as async
                gather[[var]] <- TRUE
                # gather[[var]] <- as.logical(gather[[var]])
            }
            var <- "Async_step_count"
            if (!any(names(gather) == var)) {
                gather[[var]] <- NA
                gather[[var]] <- as.integer(gather[[var]])
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
            ## get file info
            ss <- submonth[day == ad]

            async    <- rep(FALSE, 1440)  # The snc file exist, so start with all not async
            asyncstp <- rep(NA,    1440)  # Async magnitude (steps missed)

            ## Recreate time stamp for all minutes of day starting from zero!!!
            D_minutes <- seq(from       = as.POSIXct(paste(as_date(ad), "00:00:00 UTC")),
                             length.out = 1440,
                             by         = "min")

            ## __  Read tracker sync file  -------------------------------------
            ## TODO we should use step files as more reliable to detect async events!!!
            syc_temp    <- read.table(ss$fullname, sep = "\t", as.is = TRUE, na.strings = "None")
            ## get dates from file
            syc_temp$V1 <- as.POSIXct(syc_temp$V1)
            ## round to start of each minute
            async_minu  <- as.POSIXct(format(syc_temp$V1,format = "%F %R"))  ## async end
            ## get minutes with async
            uniq_async  <- unique(async_minu)
            ## async time distance
            syc_temp$timeDist <- apply(syc_temp[, c('V7', 'V8')], MARGIN = 1, FUN = max, na.rm = T)

            for (amin in uniq_async) {
                min_ind <- async_minu == amin
                stepgo  <- syc_temp$V4[min_ind]
                stepis  <- syc_temp$V5[min_ind]
                stepout <- suppressWarnings(max(abs( stepgo - stepis ), na.rm = TRUE))
                if (is.finite(stepout)) {
                    # Async magnitude (count steps missed)
                    asyncstp[ which( D_minutes == amin ) ] <- stepout
                }
            }

            ## set async from time back
            syc_temp$async_start <- syc_temp$V1 - syc_temp$timeDist
            syc_temp$async_start <- as.POSIXct(format(syc_temp$async_start, format = "%F %R"))
            syc_temp$async_end   <- as.POSIXct(format(syc_temp$V1,          format = "%F %R"))
            ## create vector of asyncs
            for (ik in 1:nrow(syc_temp)) {
                async[ which( D_minutes <= syc_temp$async_end[   ik ] &
                              D_minutes >= syc_temp$async_start[ ik ]  ) ] <- TRUE   ## !!
            }

            ## Move dates to the center of each minute, as the rest of DB
            D_minutes <- D_minutes + 30

            day_data <- data.frame(Date               = D_minutes,
                                   year               = year(D_minutes),
                                   month              = month(D_minutes),
                                   Async_tracker_flag = async,
                                   Async_step_count   = asyncstp)

            ## get file metadata
            file_meta <- data.table(day                = as_date(ad),
                                    chp1_sync_basename = basename(ss$fullname),
                                    chp1_sync_mtime    = file.mtime(ss$fullname),
                                    chp1_sync_parsed   = Sys.time(),
                                    chp1_sync_md5sum   = as.vector(md5sum(ss$fullname)))

            # gather <- rows_patch(gather, day_data, by = "Date")
            gather     <- rows_upsert(gather, day_data, by = "Date")
            gathermeta <- rbind(gathermeta, file_meta)
            rm(day_data, file_meta, ss)
            rm(async, async_minu, syc_temp, uniq_async, stepgo, stepis, min_ind)
        }

        BB_meta <- rows_update(BB_meta, gathermeta, by = "day")
        # BBdaily <- rows_patch(BBdaily, gathermeta, by = "day", unmatched = "ignore")

        ## mark all days without a sync file as Async cases
        gather$Async_tracker_flag[!as.Date(gather$Date) %in% syncfldates] <- FALSE

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
