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
# mylock(DB_lock)

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
                                          Sys.Date() - 1,
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

## select what to touch
filelist <- filelist[todosets, on = .(flmonth = month, flyear = year)]
rm(todosets, dd)


## loop data base files computing black
for (af in filelist$names) {
    datapart <- data.table(read_parquet(af))
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

        dark_day <- dark_calculations(dates      = daydata$Date,
                                      values     = daydata$CM21_sig,
                                      elevatio   = daydata$Eleva,
                                      nightlimit = DARK_ELEV,
                                      dstretch   = DSTRETCH)

    }



    ## ignore data!!
    grep("chp1", names(usedata), ignore.case = TRUE, value = TRUE)


    stop()

}

# ####    Calculate Dark signal   ########################################
# dark_day <- dark_calculations( dates      = daydata$Date,
#                                values     = daydata$CM21value,
#                                elevatio   = daydata$Eleva,
#                                nightlimit = DARK_ELEV,
#                                dstretch   = DSTRETCH)
#
#
#
# # if ( is.na(dark_day$Mmed) & is.na(dark_day$Emed) ) {
# if ( ! ((!is.na(dark_day$Mmed) & dark_day$Mcnt >= DCOUNTLIM) |
#         (!is.na(dark_day$Emed) & dark_day$Ecnt >= DCOUNTLIM)) ) {
#     # cat("Can not apply dark\n")
#     todays_dark_correction <- NA
#     dark_flag              <- "MISSING"
#     missingdark            <- NA
#
#     ## get dark from pre-computed file
#     if (exists("construct")) {
#         ## can not find date
#         if (! theday %in% construct$Date) {
#             todays_dark_correction <- NA
#             dark_flag              <- "MISSING"
#             missingdark            <- NA
#         } else {
#             ## get data from recomputed dark database
#             todays_dark_correction <- construct[ Date == theday, DARK]
#             dark_flag              <- "CONSTRUCTED"
#         }
#     }
# } else {
#     ####    Dark Correction function   #################################
#     dark_generator <- dark_function(dark_day    = dark_day,
#                                     DCOUNTLIM   = DCOUNTLIM,
#                                     type        = "median",
#                                     adate       = theday ,
#                                     test        = test,
#                                     missfiles   = missfiles,
#                                     missingdark = missingdark )
#
#     ####    Create dark signal for correction    #######################
#     todays_dark_correction <- dark_generator(daydata$Date)
#     dark_flag              <- "COMPUTED"
# }
#
# ####    Apply dark correction    #######################################
# daydata[, CM21valueWdark := CM21value - todays_dark_correction ]









myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
