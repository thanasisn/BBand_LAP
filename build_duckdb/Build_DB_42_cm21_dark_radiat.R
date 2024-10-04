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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_42_cm21_dark_radiat.R"
# renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_dark_calculation.R")
source("~/BBand_LAP/functions/Functions_CM21.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,     warn.conflicts = FALSE, quietly = TRUE)


##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_DUCK))


##
make_new_column(con, "META", "cm21_dark_flag", "character")


## days to loop
dayslist <- tbl(con, "META") |> filter(is.na(cm21_dark_flag) & !is.na(cm21_basename)) |> select(Day) |> pull()

## go find dark and radiation
for (ad in dayslist) {
  ad <- as.Date(ad, origin = origin)
  tbl(con, "LAP") |>
    filter(Day == ad)                 |>  ## this day only
    filter(!is.na(CM21_sig))          |>  ## valid measurements
    filter(is.na(cm21_bad_data_flag)) |>  ## not bad data
    collect() |> data.table()


  ## use only valid data for dark
  data_use <- datapart[is.na(cm21_bad_data_flag) &
                         !is.na(CM21_sig) &
                         is.na(CM21_sig_wo_dark)]

}




# remove_column(con, "META", "cm21_dark_flag")


tbl(con, "META") |> glimpse()
tbl(con, "LAP")  |> glimpse()

tbl(con, "LAP") |> filter(!is.na(cm21_bad_data_flag))
tbl(con, "LAP") |> filter(!is.na(cm21_bad_data_flag))

stop()






##  Dark calculations on dataset  ----------------------------------------------


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
            cat("The day is not initialized:", format(as.Date(aday, origin = "1970-01-01")),"\n")
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
                                              missingdark = missingdark)
            ## Create dark signal for every minute
            todays_dark_correction <- dark_generator(daydata$Date)
            dark_flag              <- "COMPUTED"
        }

        ## __ Apply dark correction for the day  -------------------------------
        daydata[, CM21_sig_wo_dark := CM21_sig - todays_dark_correction]

        ## __ Convert signal to radiation --------------------------------------
        daydata[, GLB_wpsm    := CM21_sig_wo_dark * cm21factor(Date)]
        daydata[, GLB_SD_wpsm := CM21_sig_sd      * cm21factor(Date)]

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
}


print(
    table(BB_meta[,(cm21_dark_flag)])
)



tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

