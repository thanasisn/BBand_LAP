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
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_db/Build_DB.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_Steps_lock)

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)



cat("\n Initialize DB and import  Tracker steps files\n\n")

##  Initialize meta data file  -------------------------------------------------
if (file.exists(DB_Steps_META_fl)) {
    BB_meta <- read_parquet(DB_Steps_META_fl)
    BB_meta <- merge(BB_meta,
                     data.table(day = seq(from = min(max(BB_meta$day),     ## start of meta data table
                                                     DB_Steps_start_date), ## start of project
                                          to   = (Sys.Date() - 1),         ## don't include today
                                          by   = "day")),
                     by = "day",
                     all = TRUE)
    stopifnot(sum(duplicated(BB_meta$day)) == 0)
} else {
    warning("STARTING NEW DB!!")
    BB_meta <- data.table(day = seq(DB_Steps_start_date, Sys.Date(), by = "day"))
    ## For tracker step files
    BB_meta$chp1_Steps_basename <- as.character(NA)
    BB_meta$chp1_Steps_mtime    <- as.POSIXct(NA)
    BB_meta$chp1_Steps_parsed   <- as.POSIXct(NA)
    BB_meta$chp1_Steps_md5sum   <- as.character(NA)
    ## For tracker async files
    BB_meta$chp1_Async_basename <- as.character(NA)
    BB_meta$chp1_Async_mtime    <- as.POSIXct(NA)
    BB_meta$chp1_Async_parsed   <- as.POSIXct(NA)
    BB_meta$chp1_Async_md5sum   <- as.character(NA)
}


##  Get Tracker steps files  ---------------------------------------------------
inp_filelist <- list.files(path       = trSTEP_DIR,
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
inp_filelist <- inp_filelist[!inp_filelist$basename %in% BB_meta$chp1_Steps_basename]
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

        ## Read sun data file  -------------------------------------------------
        step_temp <- fread(ss$fullname, na.strings = "None")
        names(step_temp)[names(step_temp) == "V1"] <- "Date"
        names(step_temp)[names(step_temp) == "V2"] <- "Axis"
        names(step_temp)[names(step_temp) == "V3"] <- "Num"
        names(step_temp)[names(step_temp) == "V4"] <- "Step"
        names(step_temp)[names(step_temp) == "V5"] <- "Sun"
        names(step_temp)[names(step_temp) == "V6"] <- "StepsTaken"
        names(step_temp)[names(step_temp) == "V7"] <- "Tracker"

        ## reshape data --------------------------------------------------------
        step_temp$Num <- NULL
        step_temp[Axis == "a", Axis := "Azim"]
        step_temp[Axis == "z", Axis := "Elev"]
        dt_azim      <- step_temp[Axis == "Azim"]
        dt_elev      <- step_temp[Axis == "Elev"]
        dt_azim$Axis <- NULL
        dt_elev$Axis <- NULL

        wecare <- grep("Date|Taken", names(dt_azim),
                       value = TRUE, ignore.case = TRUE, invert = TRUE)

        for (av in wecare) {
            names(dt_azim)[names(dt_azim) == av] <- paste0(av, "_Azim")
            names(dt_elev)[names(dt_elev) == av] <- paste0(av, "_Elev")
        }

        step_temp <- merge(dt_azim, dt_elev, all = TRUE)
        step_temp[, year  := year( Date)]
        step_temp[, month := month(Date)]
        step_temp[, doy   := yday( Date)]

        ## Get metadata for steps file  ----------------------------------------
        step_meta <- data.table(day                 = as_date(ad),
                                chp1_Steps_basename = basename(ss$fullname),
                                chp1_Steps_mtime    = file.mtime(ss$fullname),
                                chp1_Steps_parsed   = Sys.time(),
                                chp1_Steps_md5sum   = as.vector(md5sum(ss$fullname)))

        ## Init DB variables for next processes --------------------------------
        step_temp[, Async_step_count   := as.integer(NA)]
        step_temp[, Async_tracker_flag := TRUE          ]
        step_temp <- unique(step_temp)

        ## gather data
        if (nrow(gather) == 0) {
            ## this inits the database table!!
            gather <- as_tibble(step_temp)
        } else {
            ## Append allow duplicate dates!!!!!
            gather <- rows_append(gather, step_temp)
        }

        gathermeta <- rbind(gathermeta, step_meta)
        rm(step_temp, step_meta, ss)
    }

    BB_meta <- rows_update(BB_meta, gathermeta, by = "day")

    setorder(gather, Date)

    ## store this month / set data
    write_parquet(gather,  partfile)
    write_parquet(BB_meta, DB_Steps_META_fl)
    cat("99 Save: ", partfile, "\n")
    rm(gather, gathermeta, submonth)
}
rm(subyear, inp_filelist)
gc()



################################################################################




stop()









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
            cat("04 Load: ", partfile, "\n")
            gather <- read_parquet(partfile)

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
        cat("04 Save: ", partfile, "\n")
        rm(gather, gathermeta, submonth)
    }
    rm(subyear)
}
rm(inp_filelist)









# ## Sycfile deffinition
#
# str(now)
# str(step)
# str(response)
# str(az_count)
# str(freq_az)
# str(freq_main)
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



# myunlock(DB_Steps_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
