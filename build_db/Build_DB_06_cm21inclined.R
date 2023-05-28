#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Read INCLINED CM-21 signal from `[0-9]*01.LAP$`
#'
#' Populates:
#'  - CM21INC_sig
#'  - CM21INC_sig_sd
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
Script.Name <- "~/BBand_LAP/build_db/Build_DB_02_cm21.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)



cat("\n Import  INCLINED CM-21  data\n\n")

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
    stop("STAR A NEW DB!!")
}



##  Get CM-21 files  -----------------------------------------------------------
inp_filelist <- list.files(path        = SIRENA_INC,
                           recursive   = TRUE,
                           pattern     = "[0-9]*01.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE)
cat("\n**Found:", paste(length(inp_filelist), "INCLINED CM-21 files from Sirena**\n"))
## just in case, there are nested folders with more lap files in Sirens
inp_filelist <- grep("OLD", inp_filelist, ignore.case = T, invert = T, value = T)
inp_filelist <- grep("Horizontal", inp_filelist, ignore.case = T, invert = T, value = T)


inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, cm21inc_basename := basename(fullname)]
stopifnot(all(duplicated(sub("\\..*", "", inp_filelist$cm21inc_basename))) == FALSE)

inp_filelist$day <- as.Date(parse_date_time(
    sub("01\\..*", "", inp_filelist$cm21inc_basename),
    "dmy"))
setorder(inp_filelist, day)
cat("\n**Found:", paste(nrow(inp_filelist), "INCLINED CM-21 files**\n"))

## only new files in the date range
inp_filelist <- inp_filelist[!inp_filelist$cm21inc_basename %in% BB_meta$cm21inc_basename]
inp_filelist <- inp_filelist[inp_filelist$day %in% BB_meta$day]
cat("\n**Parse:", paste(nrow(inp_filelist), "INCLINED CM-21 files**\n\n"))



##  Import INCLINED CM-21 files  -----------------------------------------------
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
            # cat("06 Load: ", partfile, "\n")
            gather <- read_parquet(partfile)
            ## add columns for this set
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

            ## __  Read LAP file  --------------------------------------------------
            if (nrow(ss) > 1) {stop("Multiple input files!!")}
            lap    <- fread(ss$fullname, na.strings = "-9")
            lap$V1 <- as.numeric(lap$V1)
            lap$V2 <- as.numeric(lap$V2)
            stopifnot(is.numeric(lap$V1))
            stopifnot(is.numeric(lap$V2))
            stopifnot(dim(lap)[1] == 1440)
            lap[V1 < -8, V1 := NA]
            lap[V2 < -8, V2 := NA]

            ## get data
            day_data <- data.table(Date           = D_minutes,      # Date of the data point
                                   year           = year(D_minutes),
                                   month          = month(D_minutes),
                                   CM21INC_sig    = lap$V1,         # Raw value for INCLINED CM21
                                   CM21INC_sig_sd = lap$V2)         # Raw SD value for INCLINED CM21

            ## get metadata
            file_meta <- data.table(day                = as_date(ad),
                                    cm21inc_basename   = basename(ss$fullname),
                                    cm21inc_mtime      = file.mtime(ss$fullname),
                                    cm21inc_parsed     = Sys.time(),
                                    cm21inc_sig_NAs    = sum(is.na(day_data$CM21INC_sig)),
                                    cm21inc_sig_sd_NAs = sum(is.na(day_data$CM21INC_sig)),
                                    cm21inc_md5sum     = as.vector(md5sum(ss$fullname)))

            ## insert new data
            gather     <- rows_upsert(gather, day_data, by = "Date")
            gathermeta <- rbind(gathermeta, file_meta)
            rm(day_data, file_meta, ss, lap)
        }
        ## insert new meta data
        BB_meta <- rows_update(BB_meta, gathermeta, by = "day")

        setorder(gather, Date)

        ## store this month / set data
        cat("06 Save: ", partfile, "\n")
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
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
