#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Read *.stp and *.snc  files
#'
#' This also initializes dataset and meta data for tracker.
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
Script.Name <- "~/BBand_LAP/build_db/Build_chp1_tracker_DB.R"

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


## STEP files import -----------------------------------------------------------


cat("\n Initialize DB and import  Tracker steps files\n\n")

## _ Initialize meta data file  ------------------------------------------------
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


# TEST
BB_meta$chp1_Async_basename <- as.character(NA)

## _ Get Tracker steps files  --------------------------------------------------
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



## _ Init db import data  ------------------------------------------------------
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

    ## read this year files
    gathermeta <- data.table()
    for (ad in subyear$day) {
        ss <- subyear[day == ad]

        ## _ Read sync data file  ----------------------------------------------
        step_temp <- fread(ss$fullname, na.strings = "None")
        names(step_temp)[names(step_temp) == "V1"] <- "Date"
        names(step_temp)[names(step_temp) == "V2"] <- "Axis"
        names(step_temp)[names(step_temp) == "V3"] <- "Num"
        names(step_temp)[names(step_temp) == "V4"] <- "Step"
        names(step_temp)[names(step_temp) == "V5"] <- "Sun"
        names(step_temp)[names(step_temp) == "V6"] <- "StepsTaken"
        names(step_temp)[names(step_temp) == "V7"] <- "Tracker"

        ## reshape data
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
        step_temp[, year          := year( Date)]
        step_temp[, month         := month(Date)]
        step_temp[, doy           := yday( Date)]
        step_temp[, Tracker_event := "Step"]

        ## _ Get metadata for steps file  --------------------------------------
        step_meta <- data.table(day                 = as_date(ad),
                                chp1_Steps_basename = basename(ss$fullname),
                                chp1_Steps_mtime    = file.mtime(ss$fullname),
                                chp1_Steps_parsed   = Sys.time(),
                                chp1_Steps_md5sum   = as.vector(md5sum(ss$fullname)))

        ## _ Init DB variables for next processes ------------------------------
        step_temp[, Tracker_freq        := as.numeric(NA)]
        step_temp[, Step_Should_Azim    := as.integer(NA)]
        step_temp[, Step_Response_Azim  := as.integer(NA)]
        step_temp[, Axis_step_Azim      := as.numeric(NA)]
        step_temp[, Axis_freq_Azim      := as.numeric(NA)]
        step_temp[, Step_Should_Elev    := as.integer(NA)]
        step_temp[, Step_Response_Elev  := as.integer(NA)]
        step_temp[, Axis_step_Elev      := as.numeric(NA)]
        step_temp[, Axis_freq_Elev      := as.numeric(NA)]

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
    rm(gather, gathermeta, subyear)
}
rm(subyear, inp_filelist)
gc()






## ASYNC files import ----------------------------------------------------------


## _ Get tracker Async files  ---------------------------------------------------
inp_filelist <- list.files(path        = trSYNC_DIR,
                           recursive   = TRUE,
                           pattern     = "sun_tracker_.*.snc$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:",paste(length(inp_filelist), "tracker sync files**\n"))

inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, chp1_Async_basename := basename(fullname)]
stopifnot( all(duplicated(sub("\\..*", "", inp_filelist$chp1_Async_basename))) == FALSE )

inp_filelist$day <- as.Date(parse_date_time(
    sub("\\.snc", "", sub("sun_tracker_", "", inp_filelist$chp1_Async_basename)),
    "Ymd"))
setorder(inp_filelist, day)
cat("\n**Found:",paste(nrow(inp_filelist), "tracker sync files**\n"))


## days with a sync file
syncfldates  <- inp_filelist$day

## only new files in the date range
inp_filelist <- inp_filelist[!inp_filelist$chp1_Async_basename %in% BB_meta$chp1_Async_basename]
inp_filelist <- inp_filelist[inp_filelist$day %in% BB_meta$day]

cat("\n**Parse:",paste(nrow(inp_filelist), "tracker sync files**\n\n"))




## _ Import Async files  -------------------------------------------------------
for (YYYY in unique(year(inp_filelist$day))) {
    subyear <- inp_filelist[year(day) == YYYY]

    ## export file name and hive dir
    filedir <- paste0(DB_Steps_DIR, "/", YYYY, "/")
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
    }

    ##  read this year files
    gathermeta <- data.table()
    for (ad in subyear$day) {
        ## get file info
        ss <- subyear[day == ad]

        ## _ Read tracker sync file  -------------------------------------------
        sync_temp    <- fread(ss$fullname, na.strings = "None")
        ## get dates from file
        sync_temp$V1 <- as.POSIXct(sync_temp$V1)

        names(sync_temp)[names(sync_temp) == "V1"] <- "Date"
        names(sync_temp)[names(sync_temp) == "V2"] <- "Axis"
        names(sync_temp)[names(sync_temp) == "V3"] <- "Num"
        names(sync_temp)[names(sync_temp) == "V4"] <- "Step_Should"
        names(sync_temp)[names(sync_temp) == "V5"] <- "Step_Response"
        names(sync_temp)[names(sync_temp) == "V6"] <- "Axis_step"
        names(sync_temp)[names(sync_temp) == "V7"] <- "Axis_freq"
        names(sync_temp)[names(sync_temp) == "V8"] <- "Tracker_freq"

        ## reshape data
        sync_temp$Num <- NULL
        sync_temp[Axis == "a", Axis := "Azim"]
        sync_temp[Axis == "z", Axis := "Elev"]
        dt_azim      <- sync_temp[Axis == "Azim"]
        dt_elev      <- sync_temp[Axis == "Elev"]
        dt_azim$Axis <- NULL
        dt_elev$Axis <- NULL



        wecare <- grep("Date|Tracker_freq", names(dt_azim),
                       value = TRUE, ignore.case = TRUE, invert = TRUE)

        for (av in wecare) {
            names(dt_azim)[names(dt_azim) == av] <- paste0(av, "_Azim")
            names(dt_elev)[names(dt_elev) == av] <- paste0(av, "_Elev")
        }

        sync_temp <- merge(dt_azim, dt_elev, all = TRUE)
        sync_temp[, year          := year( Date)]
        sync_temp[, month         := month(Date)]
        sync_temp[, doy           := yday( Date)]
        sync_temp[, Tracker_event := "Async"]

        names(sync_temp)[!names(sync_temp) %in% names(gather)]

        ## get file metadata
        file_meta <- data.table(day                 = as_date(ad),
                                chp1_Async_basename = basename(ss$fullname),
                                chp1_Async_mtime    = file.mtime(ss$fullname),
                                chp1_Async_parsed   = Sys.time(),
                                chp1_Async_md5sum   = as.vector(md5sum(ss$fullname)))

        # append!! allow duplicate time stamps
        gather     <- rows_append(gather, sync_temp)
        gathermeta <- rbind(gathermeta, file_meta)
        rm(day_data, file_meta, ss)
        rm(async, async_minu, sync_temp, uniq_async, stepgo, stepis, min_ind)
    }

    BB_meta <- rows_update(BB_meta, gathermeta, by = "day")

    ## mark all days without a sync file as Async cases
    gather$Async_tracker_flag[!as.Date(gather$Date) %in% syncfldates] <- FALSE

    setorder(gather, Date)
stop()
    ## store this month / set data
    write_parquet(gather,  partfile)
    write_parquet(BB_meta, DB_Steps_META_fl)
    cat("04 Save: ", partfile, "\n")
    rm(gather, gathermeta, submonth)
}
rm(subyear)
rm(inp_filelist)
gc()



## Find async cases to really remove!

ss <- data.table(open_dataset(DB_Steps_DIR) |> filter(year == 2020) |> collect())


ss[!is.na(Async_step_count)]

sss <- ss[doy==327]





# myunlock(DB_Steps_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
