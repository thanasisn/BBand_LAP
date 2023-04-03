#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' TODO:
#'
#' - Darck and radiation for CHP-1 and CM-21
#'
#'
#+ include=T, echo=F

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/settname.R"

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/BBand_LAP/functions/Functions_dark_calculation.R")
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
library(pander,     warn.conflicts = TRUE, quietly = TRUE)



##  Initialize meta data file  -------------------------------------------------
if (file.exists(DB_META_fl)) {
    BB_meta <- read_parquet(DB_META_fl)
    ## add more days
    BB_meta <- merge(BB_meta,
                     data.table(day = seq(max(BB_meta$day),
                                          Sys.Date(),
                                          by = "day")),
                     by = "day",
                     all = TRUE)
    stopifnot(sum(duplicated(BB_meta$day)) == 0)
    ## new columns
    var <- "chp1_dark_flag"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- as.character(NA)
    }
} else {
    stop("NO METADATA FILE!!")
}



##  Dark calculations on dataset  ----------------------------------------------

## list data base files
filelist <- data.table(
    names = list.files(DB_DIR,
                       pattern = "*.parquet",
                       recursive  = TRUE,
                       full.names = TRUE))
dd      <- dirname(filelist$names)
dd      <- tstrsplit(dd, "/")

filelist$flmonth <- as.numeric(unlist(dd[length(dd)]))
filelist$flyear  <- as.numeric(unlist(dd[length(dd)-1]))

## list data set files to touch
todosets <- unique(rbind(
    BB_meta[is.na(chp1_dark_flag),
            .(month = month(day), year = year(day))]
))


## TODO check done

## select what to touch
filelist <- filelist[todosets, on = .(flmonth = month, flyear = year)]

BB_meta[is.na(chp1_dark_flag)]





rm(todosets, dd)

stop()

## loop data base files computing black for CHP-1
for (af in filelist$names) {
    datapart <- data.table(read_parquet(af))
    datapart[, month := month(Date)]
    datapart[, year  := year(Date) ]
    cat("Load: ", af, "\n")

    ## Ignore bad and missing data
    usedata <- datapart[is.na(chp1_bad_data_flag) & !is.na(CHP1_sig) ]
    if (nrow(usedata) == 0) {
        cat("\nNo usefull CHP-1 data in this file\n\n")
        next()
    }

    ## loop days
    for (aday in unique(as.Date(usedata$Date))) {
        daydata <- usedata[ as.Date(Date) == aday ]

        ## __ Compute dark values for day  -------------------------------------
        dark_day <- dark_calculations_2(
            dates      = daydata$Date,
            values     = daydata$CHP1_sig,
            elevatio   = daydata$Eleva,
            nightlimit = DARK_ELEV,
            dstretch   = DSTRETCH
        )

        ## __ Resolve problematic dark calculations ----------------------------
        if ( !((!is.na(dark_day$dark_Mor_med) & dark_day$dark_Mor_cnt >= DCOUNTLIM) |
               (!is.na(dark_day$dark_Eve_med) & dark_day$dark_Eve_cnt >= DCOUNTLIM))) {
            # cat("Can not apply dark\n")
            todays_dark_correction <- as.numeric(NA)
            dark_flag              <- "MISSING"
            missingdark            <- as.numeric(NA)

            cat("skip day\n")
            next()

            stop("gdsgsdg")
            ## get dark from pre-computed file
            if (exists("construct")) {
                ## can not find date
                if (!aday %in% construct$Date) {
                    todays_dark_correction <- as.numeric(NA)
                    dark_flag              <- "MISSING"
                    missingdark            <- as.numeric(NA)
                } else {
                    ## get data from recomputed dark database
                    todays_dark_correction <- construct[ Date == aday, DARK]
                    dark_flag              <- "CONSTRUCTED"
                }
            }
        } else {
            ## __ Dark Correction function for non missing  --------------------
            dark_generator <- dark_function_2(dark_day    = dark_day,
                                              DCOUNTLIM   = DCOUNTLIM,
                                              type        = "median",
                                              missingdark = missingdark )
            ## Create dark signal for every minute
            todays_dark_correction <- dark_generator(daydata$Date)
            dark_flag              <- "COMPUTED"
        }

        ## __ Apply dark correction for the day  -------------------------------
        daydata[, CHP1_sig_wo_dark := CHP1_sig - todays_dark_correction ]

        ## __ Day stats --------------------------------------------------------
        names(dark_day) <- paste0("chp1_", names(dark_day))
        meta_day <- data.frame(day                = as.Date(aday),
                               chp1_Daily_dark    = mean(todays_dark_correction, na.rm = T),
                               chp1_dark_flag     = dark_flag,
                               dark_day,
                               chp1_dark_computed = Sys.time()
        )

        ## import new data
        BB_meta  <- rows_update(BB_meta, meta_day, by = "day")
        datapart <- rows_update(datapart, daydata, by = "Date")
        rm(daydata, meta_day)
    }

    ## store actual data
    write_parquet(x = datapart, sink = af)
    write_parquet(BB_meta, DB_META_fl)
    cat("Save: ", af, "\n\n")
    ## clean
    rm(datapart, meta_day)

}











myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
