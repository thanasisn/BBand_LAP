#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Read CHP-1 signal from `[0-9]*03.LAP$`
#'
#' Populates:
#'  - CHP1_sig
#'  - CHP1_sig_sd
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
knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(fig.pos    = '!h'     )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_db/Build_DB_03_chp1.R"

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
    # var <- "chp1_basename"
    # if (!any(names(BB_meta) == var)) {
    #     BB_meta[[var]] <- NA
    #     BB_meta[[var]] <- as.character(BB_meta[[var]])
    # }
} else {
    stop("STAR A NEW DB!!")
}



##  Get CHP-1 files  --------------------------------------------------------
inp_filelist <- list.files(path        = SIRENA_DIR,
                           recursive   = TRUE,
                           pattern     = "[0-9]*03.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:",paste(length(inp_filelist), "CHP-1 files from Sirena**\n"))
## just in case, there are nested folders with more lap files in Sirens
inp_filelist <- grep("OLD", inp_filelist, ignore.case = T, invert = T, value = T)


inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, chp1_basename := basename(fullname)]
stopifnot( all(duplicated(sub("\\..*", "", inp_filelist$chp1_basename))) == FALSE)

inp_filelist$day <- as.Date(parse_date_time(
    sub("03\\..*", "", inp_filelist$chp1_basename),
    "dmy"))
setorder(inp_filelist, day)
cat("\n**Found:",paste(nrow(inp_filelist), "CHP-1 files**\n"))

## only new files in the date range
inp_filelist <- inp_filelist[!inp_filelist$chp1_basename %in% BB_meta$chp1_basename]
inp_filelist <- inp_filelist[inp_filelist$day %in% BB_meta$day]

cat("\n**Parse:",paste(nrow(inp_filelist), "CHP-1 files**\n\n"))





##  Import CHP-1 files  00------------------------------------------------------
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
            # var <- "CHP1_sig"
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

            suppressWarnings(rm(D_minutes))
            D_minutes <- seq(from       = as.POSIXct(paste(as_date(ad), "00:00:30 UTC")),
                             length.out = 1440,
                             by         = "min")

            ## __  Read LAP file  ----------------------------------------------
            lap   <- fread(ss$fullname, na.strings = "-9")
            lap$V1 <- as.numeric(lap$V1)
            lap$V2 <- as.numeric(lap$V2)
            stopifnot(is.numeric(lap$V1))
            stopifnot(is.numeric(lap$V2))
            stopifnot(dim(lap)[1] == 1440)
            lap[V1 <= -5, V1 := NA]
            lap[V2 <= -5, V2 := NA]

            ## get data
            day_data <- data.table(Date        = D_minutes,      # Date of the data point
                                   year        = year(D_minutes),
                                   month       = month(D_minutes),
                                   CHP1_sig    = lap$V1,         # Raw value for CHP1
                                   CHP1_sig_sd = lap$V2)         # Raw SD value for CHP1

            ## get metadata
            file_meta <- data.table(day             = as_date(ad),
                                    chp1_basename   = basename(ss$fullname),
                                    chp1_mtime      = file.mtime(ss$fullname),
                                    chp1_parsed     = Sys.time(),
                                    chp1_sig_NAs    = sum(is.na(day_data$CHP1_sig)),
                                    chp1_sig_sd_NAs = sum(is.na(day_data$CHP1_sig)),
                                    chp1_md5sum     = as.vector(md5sum(ss$fullname)))

            # gather <- rows_patch(gather, day_data, by = "Date")
            # gather     <- rows_update(gather, day_data, by = "Date")
            gather     <- rows_upsert(gather, day_data, by = "Date")
            gathermeta <- rbind(gathermeta, file_meta)
            rm(day_data, file_meta, ss, lap)
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
