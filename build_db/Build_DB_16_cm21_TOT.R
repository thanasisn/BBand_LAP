#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Reads Global radiation from sirena TOT files into the database
#'
#'  - Reads raw resistance data
#'  - Converts resistance to temperature
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
Script.Name <- "~/BBand_LAP/build_db/Build_DB_06_cm21_TOT.R"

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

cat("\n Import  CM-21  TOT  data\n\n")

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
    var <- "tot_cm21_basename"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.character(BB_meta[[var]])
    }
    var <- "tot_cm21_md5sum"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.character(BB_meta[[var]])
    }
    var <- "tot_cm21_mtime"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.POSIXct(BB_meta[[var]])
    }
    var <- "tot_cm21_parsed"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.POSIXct(BB_meta[[var]])
    }
    var <- "tot_cm21_glb_NAs"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.integer(BB_meta[[var]])
    }
    var <- "tot_cm21_glb_sd_NAs"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.integer(BB_meta[[var]])
    }
} else {
    stop("STAR A NEW DB!!")
}



##  Get TOT CM-21 files  ------------------------------------------------------
inp_filelist <- list.files(path        = SIRENA_TOT,
                           recursive   = TRUE,
                           pattern     = "[0-9]*\\TOT.*.dat",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:",paste(length(inp_filelist), "TOT CM-21 files from Sirena**\n"))
## just in case, there are nested folders with more lap files in Sirens
inp_filelist <- grep("OLD", inp_filelist, ignore.case = T, invert = T, value = T )

inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, tot_cm21_basename := basename(fullname)]
stopifnot( all(duplicated(sub("\\..*", "", inp_filelist$tot_cm21_basename))) == FALSE )

inp_filelist$day <- as.Date(parse_date_time(
    sub("\\.dat", "", sub("TOT", "", inp_filelist$tot_cm21_basename, ignore.case = T), ignore.case = T),
    "jy"))
setorder(inp_filelist, day)
cat("\n**Found:",paste(nrow(inp_filelist), "TOT CM-21 files**\n"))


## only new files in the date range
inp_filelist <- inp_filelist[!inp_filelist$tot_cm21_basename %in% BB_meta$tot_cm21_basename]
inp_filelist <- inp_filelist[inp_filelist$day %in% BB_meta$day]

cat("\n**Parse:",paste(nrow(inp_filelist), "TOT CM-21 files**\n\n"))

## test random
if (TEST) {
    cat("\nTEST MODE IS ON!!  ", Script.Name, "\n\n")
    inp_filelist <- unique(rbind(
        inp_filelist[ 1:60 ],
        inp_filelist[sample(1:nrow(inp_filelist), 60)],
        NULL
    ))
    setorder(inp_filelist, day)
}


##  Import TOT CM-21 files  ----------------------------------------------------
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
            # var <- "tot_glb"
            # if (!any(names(gather) == var)) {
            #     gather[[var]] <- NA
            #     gather[[var]] <- as.numeric(gather[[var]])
            # }
            # var <- "tot_glb_sd"
            # if (!any(names(gather) == var)) {
            #     gather[[var]] <- NA
            #     gather[[var]] <- as.numeric(gather[[var]])
            # }
            # var <- "lap_sza"
            # if (!any(names(gather) == var)) {
            #     gather[[var]] <- NA
            #     gather[[var]] <- as.numeric(gather[[var]])
            # }
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

            ## __  Read TOT file  ----------------------------------------------
            temp <- fread(ss$fullname, na.strings = "-9")

            if (dim(temp)[1] != 1440) {
                cat("** Can not parse file: ", basename(ss$fullname), "**\n")
                warning("Can not parse file: ", basename(ss$fullname))
                next()
            }

            dateess   <- paste(ss$day, temp$TIME_UT %/% 1, round((temp$TIME_UT %% 1) * 60))
            ## use the 30 second times stamp
            temp$Date <- as.POSIXct(strptime(dateess, "%F %H %M")) - 30
            temp[, TIME_UT := NULL]
            ## rename columns
            names(temp)[names(temp) == "SZA"]     <- "lap_sza"
            names(temp)[names(temp) == "[W.m-2]"] <- "tot_glb"
            names(temp)[names(temp) == "st.dev"]  <- "tot_glb_sd"
            ## deal with NAs
            temp[tot_glb    < -8, tot_glb    := NA]
            temp[tot_glb_sd < -8, tot_glb_sd := NA]

            stopifnot(is.numeric(temp$tot_glb))
            stopifnot(is.numeric(temp$tot_glb_sd))
            stopifnot(dim(temp)[1] == 1440)

            temp[, year  := as.integer(year(Date))]
            temp[, month := as.integer(month(Date))]

            ## get metadata
            file_meta <- data.table(day                 = as_date(ad),
                                    tot_cm21_basename   = basename(ss$fullname),
                                    tot_cm21_mtime      = file.mtime(ss$fullname),
                                    tot_cm21_parsed     = Sys.time(),
                                    tot_cm21_glb_NAs    = sum(is.na(temp$tot_glb)),
                                    tot_cm21_glb_sd_NAs = sum(is.na(temp$tot_glb_sd)),
                                    tot_cm21_md5sum     = as.vector(md5sum(ss$fullname)))

            # gather <- rows_patch(gather, day_data, by = "Date")
            # gather     <- rows_update(gather, day_data, by = "Date")
            gather     <- rows_upsert(gather, temp, by = "Date")
            gathermeta <- rbind(gathermeta, file_meta)
            rm(temp, file_meta, ss )
        }

        if (nrow(gathermeta) == 0) {
            cat("\nNo new files to import\n\n")
            next()
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






myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
