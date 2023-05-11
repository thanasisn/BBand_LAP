#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Apply flags on data
#'
#' - CHP-1 bad data ranges
#' - CHP-1 temperature bad data ranges
#' - CM-21 bad data ranges
#'
#'
#' TODO
#'
#' - Implement a pure arrow table method?
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
Script.Name <- "~/BBand_LAP/build_db/Build_DB_30_exclude_ranges.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/BBand_LAP/functions/Functions_CM21.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)
library(pander,     warn.conflicts = TRUE, quietly = TRUE)




## Load CHP-1 exclusions -------------------------------------------------------
chp1_exclude_mtime <- file.mtime(CHP1_EXCLUDE)
ranges_CHP1        <- read.table(CHP1_EXCLUDE,
                                 sep          = ";",
                                 colClasses   = "character",
                                 strip.white  = TRUE,
                                 header       = TRUE,
                                 comment.char = "#" )
ranges_CHP1$From  <- as.POSIXct(strptime(ranges_CHP1$From,  format = "%F %H:%M", tz = "UTC"))
ranges_CHP1$Until <- as.POSIXct(strptime(ranges_CHP1$Until, format = "%F %H:%M", tz = "UTC"))

## check negative ranges
if (!all((ranges_CHP1$Until - ranges_CHP1$From) >= 1)) {
    pander(ranges_CHP1[ !ranges_CHP1$From < ranges_CHP1$Until, ])
    stop("Inverted ranges in ", CHP1_EXCLUDE, "!!!")
}
## capitalize
ranges_CHP1$Comment <- sub("(.)", "\\U\\1", ranges_CHP1$Comment, perl = TRUE)
ranges_CHP1$Comment[ranges_CHP1$Comment == ""] <- "NO DESCRIPTION"
## compute time span
ranges_CHP1$HourSpan <- (as.numeric(ranges_CHP1$Until) - as.numeric(ranges_CHP1$From)) / 3600


#'
#' Check time ranges span in hours
#'
#+ include=T, echo=F
hist(ranges_CHP1$HourSpan)
cat('\n\n')

temp <- ranges_CHP1[ ranges_CHP1$HourSpan > 20 , ]
row.names(temp) <- NULL
cat('\n\n\\footnotesize\n\n')
pander(temp)
cat('\n\n\\normalsize\n\n')

cat('\n\n\\footnotesize\n\n')
pander(data.table(table(ranges_CHP1$Comment)))
cat('\n\n\\normalsize\n\n')




## Load CHP-1 temperature exclusions -------------------------------------------
chp1_temp_exclude_mtime <- file.mtime(CHP1_TEMP_EX)
ranges_CHP1_temp        <- read.table(CHP1_TEMP_EX,
                                sep          = ";",
                                colClasses   = "character",
                                strip.white  = TRUE,
                                header       = TRUE,
                                comment.char = "#" )
ranges_CHP1_temp$From  <- as.POSIXct(strptime(ranges_CHP1_temp$From,  format = "%F %H:%M", tz = "UTC"))
ranges_CHP1_temp$Until <- as.POSIXct(strptime(ranges_CHP1_temp$Until, format = "%F %H:%M", tz = "UTC"))

## check negative ranges
if (!all((ranges_CHP1_temp$Until - ranges_CHP1_temp$From) >= 1)) {
    pander(ranges_CHP1_temp[ !ranges_CHP1_temp$From < ranges_CHP1_temp$Until, ])
    stop("Inverted ranges in ", CHP1_TEMP_EX, "!!!")
}
## capitalize
ranges_CHP1_temp$Comment <- sub("(.)", "\\U\\1", ranges_CHP1_temp$Comment, perl = TRUE)
ranges_CHP1_temp$Comment[ranges_CHP1_temp$Comment == ""] <- "NO DESCRIPTION"
## compute time span
ranges_CHP1_temp$HourSpan <- (as.numeric(ranges_CHP1_temp$Until) - as.numeric(ranges_CHP1_temp$From)) / 3600


#'
#' Check time ranges span in hours
#'
#+ include=T, echo=F
hist(ranges_CHP1_temp$HourSpan)
cat('\n\n')

temp <- ranges_CHP1_temp[ ranges_CHP1_temp$HourSpan > 20 , ]
row.names(temp) <- NULL
cat('\n\n\\footnotesize\n\n')
pander(temp)
cat('\n\n\\normalsize\n\n')
rm(temp)

cat('\n\n\\footnotesize\n\n')
pander(data.table(table(ranges_CHP1_temp$Comment)))
cat('\n\n\\normalsize\n\n')




## Load CM-21 exclusions -------------------------------------------------------
cm21_exclude_mtime <- file.mtime(CM21_EXCLUDE)
ranges_CM21        <- read.table(CM21_EXCLUDE,
                                 sep          = ";",
                                 colClasses   = "character",
                                 strip.white  = TRUE,
                                 header       = TRUE,
                                 comment.char = "#" )
ranges_CM21$From  <- as.POSIXct(strptime(ranges_CM21$From,  format = "%F %H:%M", tz = "UTC"))
ranges_CM21$Until <- as.POSIXct(strptime(ranges_CM21$Until, format = "%F %H:%M", tz = "UTC"))

## check negative ranges
if (!all((ranges_CM21$Until - ranges_CM21$From) >= 1)) {
    pander(ranges_CM21[ !ranges_CM21$From < ranges_CM21$Until, ])
    stop("Inverted ranges in ", CM21_EXCLUDE, "!!!")
}
## capitalize
ranges_CM21$Comment <- sub("(.)", "\\U\\1", ranges_CM21$Comment, perl = TRUE)
ranges_CM21$Comment[ranges_CM21$Comment == ""] <- "NO DESCRIPTION"
## compute time span
ranges_CM21$HourSpan <- (as.numeric(ranges_CM21$Until) - as.numeric(ranges_CM21$From)) / 3600

#'
#' Check time ranges span in hours
#'
#+ include=T, echo=F

hist(ranges_CM21$HourSpan)
cat('\n\n')

temp <- ranges_CM21[ ranges_CM21$HourSpan > 12 , ]
row.names(temp) <- NULL
cat('\n\n\\footnotesize\n\n')
pander( temp )
cat('\n\n\\normalsize\n\n')
rm(temp)

cat('\n\n\\footnotesize\n\n')
pander(data.table(table(ranges_CM21$Comment)))
cat('\n\n')
cat('\n\n\\normalsize\n\n')



# ##  Load data just to check the columns
# BB <- opendata()




##  Initialize meta data file  -------------------------------------------------
if (file.exists(DB_META_fl)) {
    BB_meta <- read_parquet(DB_META_fl)
    ## add more days
    BB_meta <- merge(BB_meta,
                     data.table(day = seq(max(BB_meta$day), Sys.Date(),
                                          by = "day")),
                     by = "day",
                     all = TRUE)
    stopifnot(sum(duplicated(BB_meta$day)) == 0)
    ## new columns
} else {
    stop("HAVE TO STAR A NEW DB!!")
}







## Flag exclusions file by file  -----------------------------------------------
## FIXME should find a better method through arrow dataset

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

## list data set to be touched
todosets <- unique(rbind(
    BB_meta[is.na(chp1_bad_data_flagged),
            .(month = month(day), year = year(day))],
    BB_meta[is.na(cm21_bad_data_flagged),
            .(month = month(day), year = year(day))]
))

## select what to touch
filelist <- filelist[todosets, on = .(flmonth = month, flyear = year)]
rm(todosets, dd)



for (af in na.omit(filelist$names)) {
    datapart <- read_parquet(af)
    ## add columns for this set
    datapart[["year"]]  <- as.integer(year( datapart$Date))
    datapart[["month"]] <- as.integer(month(datapart$Date))

    cat("30 Load: ", af, "\n")

    ## CHP-1 flag bad data -----------------------------------------------------
    for (i in 1:nrow(ranges_CHP1)) {
        lower  <- ranges_CHP1$From[   i]
        upper  <- ranges_CHP1$Until[  i]
        comme  <- ranges_CHP1$Comment[i]
        tempex <- data.table(Date = seq(lower + 30, upper - 60 + 30, by = "min"),
                             chp1_bad_data_flag = comme)

        ## mark bad regions of data
        datapart <- rows_update(datapart, tempex, by = "Date", unmatched = "ignore")
        rm(tempex)
    }

    ## CHP-1 flag physical limits anomalies  -----------------------------------
    datapart <- data.table(datapart)
    datapart[CHP1_sig < chp1_signal_lower_limit(Date) & !is.na(chp1_bad_data_flag),
             chp1_bad_data_flag := "Abnormal LOW signal"]
    datapart[CHP1_sig > chp1_signal_upper_limit(Date) & !is.na(chp1_bad_data_flag),
             chp1_bad_data_flag := "Abnormal HIGH signal"]
    datapart <- as_tibble(datapart)

    ## CHP-1 flag temperature --------------------------------------------------
    for (i in 1:nrow(ranges_CHP1_temp)) {
        lower  <- ranges_CHP1_temp$From[   i]
        upper  <- ranges_CHP1_temp$Until[  i]
        comme  <- ranges_CHP1_temp$Comment[i]
        tempex <- data.table(Date = seq(lower + 30, upper - 60 + 30, by = "min"),
                             chp1_bad_temp_flag = comme)

        ## mark bad regions of data
        datapart <- rows_update(datapart, tempex, by = "Date", unmatched = "ignore")
        rm(tempex)
    }

    ## CHP-1 flag temperature physical limits ----------------------------------
    datapart <- data.table(datapart)
    datapart[chp1_temperature > CHP1_TEMP_MAX        & !is.na(chp1_bad_temp_flag),
             chp1_bad_temp_flag := "Abnormal HIGH temperature"]
    datapart[chp1_temperature < CHP1_TEMP_MIN        & !is.na(chp1_bad_temp_flag),
             chp1_bad_temp_flag := "Abnormal LOW temperature"]
    datapart[chp1_temperature_SD > CHP1_TEMP_STD_LIM & !is.na(chp1_bad_temp_flag),
             chp1_bad_temp_flag := "Abnormal temperature SD"]
    datapart <- as_tibble(datapart)



    ## CM-21 flag data ---------------------------------------------------------
    for (i in 1:nrow(ranges_CM21)) {
        lower  <- ranges_CM21$From[   i]
        upper  <- ranges_CM21$Until[  i]
        comme  <- ranges_CM21$Comment[i]
        tempex <- data.table(Date = seq(lower + 30, upper - 60 + 30, by = "min"),
                             cm21_bad_data_flag = comme)

        ## mark bad regions of data
        datapart <- rows_update(datapart, tempex, by = "Date", unmatched = "ignore")
        # datapart[Date >= lower & Date < upper, cm21_bad_data_flag := comme]
        rm(tempex)
    }

    ## CM-21 flag physical limits anomalies  -----------------------------------
    datapart <- data.table(datapart)
    datapart[CM21_sig < cm21_signal_lower_limit(Date) & !is.na(cm21_bad_data_flag),
             cm21_bad_data_flag := "Abnormal LOW signal"]
    datapart[CM21_sig > cm21_signal_upper_limit(Date) & !is.na(cm21_bad_data_flag),
             cm21_bad_data_flag := "Abnormal HIGH signal"]
    datapart <- as_tibble(datapart)



    ## Store data --------------------------------------------------------------
    chg_days <- unique(as.Date(datapart$Date))

    ## save flagged metadata
    BB_meta[day %in% chg_days, cm21_bad_data_flagged := cm21_exclude_mtime     ]
    BB_meta[day %in% chg_days, chp1_bad_temp_flagged := chp1_temp_exclude_mtime]
    BB_meta[day %in% chg_days, chp1_bad_data_flagged := chp1_exclude_mtime     ]
    ## store actual data
    write_parquet(x = datapart, sink = af)
    write_parquet(BB_meta, DB_META_fl)
    cat("30 Save: ", af, "\n\n")
    rm(datapart)
}

## clean
rm(BB_meta)
rm(filelist)
rm(ranges_CHP1)
rm(ranges_CM21)


## Show some info for the dataset flags
BB <- opendata()

wecare <- c("cm21_bad_data_flag", "chp1_bad_data_flag", "chp1_bad_temp_flag")

for (acol in wecare) {
    stats <- BB |> select(all_of(acol)) |> collect() |> table(useNA = "always")
    pander(data.frame(stats))
}



myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
