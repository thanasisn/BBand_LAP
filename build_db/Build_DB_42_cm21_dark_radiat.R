#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Compute or construct dark signal offset for CM-21.
#'
#' Fills:
#' - `CM21_sig_wo_dark` when it's appropriate
#'
#' On the second run will replace 'MISSING' dark with 'CONSTRUCTED' dark.
#'
#' TODO
#' - print dark type on graphs from metadata
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
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"  )
knitr::opts_chunk$set(fig.align  = "center")
knitr::opts_chunk$set(fig.pos    = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_db/Build_DB_42_cm21_dark.R"

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_dark_calculation.R")
source("~/BBand_LAP/functions/Functions_CM21.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)


##  Initialize meta data file  -------------------------------------------------
if (file.exists(DB_META_fl)) {
    BB_meta <- read_parquet(DB_META_fl)
    ## add more days
    BB_meta <- merge(BB_meta,
                     data.table(day = seq(max(BB_meta$day),
                                          Sys.Date(),
                                          by = "day")),
                     by  = "day",
                     all = TRUE)
    stopifnot(sum(duplicated(BB_meta$day)) == 0)
    ## avoid creating new columns
    # var <- "cm21_dark_flag"
    # if (!any(names(BB_meta) == var)) {
    #     BB_meta[[var]] <- as.character(NA)
    # }
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
dark_to_do <- BB_meta[cm21_dark_flag %in% c(NA, "MISSING") & !is.na(cm21_basename) & cm21_sig_NAs != 1440]
cat("There are ", nrow(dark_to_do), "days with missing dark\n\n")
cat(format(dark_to_do$day), " ")
cat("\n")

todosets <- unique(rbind(
    dark_to_do[, .(month = month(day), year = year(day))]
))

## select what dataset files to touch
filelist <- filelist[todosets, on = .(flmonth = month, flyear = year)]



## Create a dark construct!  ---------------------------------------------------

## create construct if are available data
if (BB_meta[!is.na(cm21_dark_flag), .N] > 100) {
    test <- BB_meta[, .(day, cm21_dark_flag, cm21_dark_Eve_med, cm21_dark_Mor_med, cm21_Daily_dark) ]
    ## will use mean Daily dark
    cm21EVEdark   <- approxfun(test$day, test$cm21_dark_Eve_med)
    cm21MORdark   <- approxfun(test$day, test$cm21_dark_Mor_med)
    cm21DAILYdark <- approxfun(test$day, test$cm21_Daily_dark  )
    ## get dark missing days
    missingdays <- BB_meta[!is.na(cm21_basename) &
                           cm21_dark_flag %in% c("MISSING", "CONSTRUCTED"), day]
    ## should we use missing only?

    ## Create missing dark
    construct <- data.table(
        Date = missingdays,
        DARK = cm21DAILYdark(missingdays)
    )
    # plot(test$day, test$cm21_dark_Eve_med)
    # plot(test$day, test$cm21_dark_Mor_med)
    plot(test$day, test$cm21_Daily_dark,
         main = "Constructed Dark values for CM-21")
    points(construct$Date, construct$DARK, col = "red")
}


rm(todosets, dd, test)



## loop data base files computing black for CM-21
for (af in filelist$names) {
    datapart <- data.table(read_parquet(af))
    datapart[, month := as.integer(month(Date))]
    datapart[, year  := as.integer(year(Date)) ]

    ## use only valid data for dark
    data_use <- datapart[is.na(cm21_bad_data_flag) &
                         !is.na(CM21_sig) &
                         is.na(CM21_sig_wo_dark)]

    cat("42 Load: ", af, "\n")

    ## Ignore bad and missing data
    if (nrow(data_use) == 0) {
        cat("\nNo usefull CM-21 data in this file\n\n")
        next()
    }

    ## loop days
    for (aday in unique(as.Date(data_use$Date))) {
        daydata <- data_use[ as.Date(Date) == aday ]

        if (any(is.na(daydata$Elevat))) {
            cat("The day is not initialized:", format(as.Date(aday)),"\n")
            next()
        }

        ## __ Compute dark values for day  -------------------------------------
        dark_day <- dark_calculations_2(
            dates      = daydata$Date,
            values     = daydata$CM21_sig,
            elevatio   = daydata$Eleva,
            nightlimit = DARK_ELEV,
            dstretch   = DSTRETCH
        )

        ## __ Resolve problematic dark calculations ----------------------------

        ## no data to use
        if (all(is.na(daydata$CM21_sig))) {
            dark_flag              <- "NO SIGNAL DATA"
            todays_dark_correction <- as.numeric(NA)
            missingdark            <- as.numeric(NA)
        ## data to compute dark are missing
        } else if ( !((!is.na(dark_day$dark_Mor_med) & dark_day$dark_Mor_cnt >= DCOUNTLIM) |
                      (!is.na(dark_day$dark_Eve_med) & dark_day$dark_Eve_cnt >= DCOUNTLIM))) {
            todays_dark_correction <- as.numeric(NA)
            dark_flag              <- "MISSING"
            missingdark            <- as.numeric(NA)

            ## get dark from pre-computed file
            if (exists("construct")) {
                ## can not find date
                if (!aday %in% construct$Date) {
                    todays_dark_correction <- as.numeric(NA)
                    dark_flag              <- "MISSING"
                    missingdark            <- as.numeric(NA)
                } else {
                    ## get data from recomputed dark database
                    todays_dark_correction <- construct[Date == aday, DARK]
                    dark_flag              <- "CONSTRUCTED"
                }
            } else {
                cat("Need to constract dark:", format(as.Date(aday)),"\n")
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
        daydata[, CM21_sig_wo_dark := CM21_sig - todays_dark_correction ]

        ## __ Convert signal to radiation --------------------------------------
        daydata[, GLB_wpsm    := CM21_sig    * cm21factor(Date)]
        daydata[, GLB_SD_wpsm := CM21_sig_sd * cm21factor(Date)]

        # ## sanity check!
        # test <- daydata[GLB_wpsm > 2000 | GLB_wpsm < 100]
        # if (nrow(test) > 0) {
        #     cat(paste(format(unique(as.Date(test$Date))), collapse = " "),"\n")
        #     stop("Sanity check failed: Global out of range!!")
        # }

        ## __ Day stats --------------------------------------------------------
        names(dark_day) <- paste0("cm21_", names(dark_day))
        meta_day <- data.frame(day                = as.Date(aday),
                               cm21_Daily_dark    = mean(todays_dark_correction, na.rm = T),
                               cm21_dark_flag     = dark_flag,
                               dark_day,
                               cm21_dark_computed = Sys.time()
        )

        ## import new data
        BB_meta  <- rows_update(BB_meta, meta_day, by = "day")
        ## update initial data with valid only
        datapart <- rows_update(datapart, daydata, by = "Date")
        rm(daydata, meta_day, dark_day)
    }

    ## store actual data
    write_parquet(x = datapart, sink = af)
    write_parquet(BB_meta, DB_META_fl)
    cat("42 Save: ", af, "\n\n")
    ## clean
    rm(datapart, meta_day)

}


print(
    table(BB_meta[,(cm21_dark_flag)])
)



myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
