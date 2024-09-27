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
#+ echo=F, include=T


#+ echo=F, include=F
## __ Document options __ ------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )


## __ Set environment __ -------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_30_exclude_ranges.R"
Script.ID   <- "30"
# renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries __ --------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/BBand_LAP/functions/Functions_CM21.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")

## __ Execution lock __ --------------------------------------------------------


library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,     warn.conflicts = FALSE, quietly = TRUE)



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
## Capitalize comments
ranges_CHP1$Comment <- sub("(.)", "\\U\\1", ranges_CHP1$Comment, perl = TRUE)
ranges_CHP1$Comment[ranges_CHP1$Comment == ""] <- "NO DESCRIPTION"
## Compute time span of exclusions
ranges_CHP1$HourSpan <- (as.numeric(ranges_CHP1$Until) - as.numeric(ranges_CHP1$From)) / 3600

#'
#' Check time ranges span in hours
#'
#+ include=T, echo=F
hist(ranges_CHP1$HourSpan)
cat("\n \n")

temp <- ranges_CHP1[ ranges_CHP1$HourSpan > 20 , ]
row.names(temp) <- NULL
cat("\n\n\\footnotesize\n\n")
pander(temp)
cat("\n\n\\normalsize\n\n")

cat("\n\n\\footnotesize\n\n")
pander(data.table(table(ranges_CHP1$Comment)))
cat("\n\n\\normalsize\n\n")



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
## Capitalize comments
ranges_CHP1_temp$Comment <- sub("(.)", "\\U\\1", ranges_CHP1_temp$Comment, perl = TRUE)
ranges_CHP1_temp$Comment[ranges_CHP1_temp$Comment == ""] <- "NO DESCRIPTION"
## Compute time span of exclusions
ranges_CHP1_temp$HourSpan <- (as.numeric(ranges_CHP1_temp$Until) - as.numeric(ranges_CHP1_temp$From)) / 3600

#'
#' Check time ranges span in hours
#'
#+ include=T, echo=F
hist(ranges_CHP1_temp$HourSpan)
cat(" \n \n")

temp <- ranges_CHP1_temp[ ranges_CHP1_temp$HourSpan > 20 , ]
row.names(temp) <- NULL
cat("\n\n\\footnotesize\n\n")
pander(temp)
cat("\n\n\\normalsize\n\n")
rm(temp)

cat("\n\n\\footnotesize\n\n")
pander(data.table(table(ranges_CHP1_temp$Comment)))
cat("\n\n\\normalsize\n\n")



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
## Capitalize comments
ranges_CM21$Comment <- sub("(.)", "\\U\\1", ranges_CM21$Comment, perl = TRUE)
ranges_CM21$Comment[ranges_CM21$Comment == ""] <- "NO DESCRIPTION"
## Compute time span of exclusions
ranges_CM21$HourSpan <- (as.numeric(ranges_CM21$Until) - as.numeric(ranges_CM21$From)) / 3600

#'
#' Check time ranges span in hours
#'
#+ include=T, echo=F

hist(ranges_CM21$HourSpan)
cat(" \n \n")

temp <- ranges_CM21[ ranges_CM21$HourSpan > 12 , ]
row.names(temp) <- NULL
cat("\n\\footnotesize\n\n")
pander(temp)
cat("\n\\normalsize\n\n")
rm(temp)

cat("\n\\footnotesize\n\n")
pander(data.table(table(ranges_CM21$Comment)))
cat("\n\n\\normalsize\n\n")



##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_DUCK))


## add all missing days in meta data
minday <- tbl(con, "META") |>
  select(Day) |>
  to_arrow() |>
  summarise(min(Day)) |>
  collect() |>
  pull()
alldays <- data.table(Day = seq(minday, Sys.Date(),
                                by = "day"))

missingdays <- anti_join(
  alldays,
  tbl(con, "META") |> select(Day),
  by   = "Day",
  copy = TRUE
)

rows_insert(x = tbl(con, "META"),
            y = missingdays,
            copy = TRUE,
            conflict = "ignore",
            in_place = T)




ranges_CM21$HourSpan <- NULL

# apply(ranges_CM21, 1,
#       function(x){
#         data.table(
#           Date = seq.POSIXt(
#             from = x[1] + 30,
#             to   = x[2] - 60 + 30,
#             by   = "min")) }
# )




## _ CM-21 flag data -------------------------------------------------------

##  create exclusion table
temp_flag <- data.table()
for (i in 1:nrow(ranges_CM21)) {
  lower  <- ranges_CM21$From[   i]
  upper  <- ranges_CM21$Until[  i]
  comme  <- ranges_CM21$Comment[i]
  tempex <- data.table(Date = seq(lower + 30, upper - 60 + 30, by = "min"),
                       cm21_bad_data_flag = comme)
  temp_flag <- rbind(temp_flag, tempex)
  rm(tempex)
}


## FIXME test
temp_flag <- temp_flag[Date >= "2023-01-01"]

temp_flag$Epoch <- as.integer(temp_flag$Date)
temp_flag$Date  <- NULL


## apply bad data ranges
make_empty_column(con, "LAP", "cm21_bad_data_flag", "character")
update_table(con, temp_flag, "LAP", "Epoch")




tbl(con, "LAP") |> filter(!is.na(cm21_bad_data_flag)) |> select(cm21_bad_data_flag)





stop()


dbDisconnect(con)

##  Flag exclusions file by file  ----------------------------------------------
##
##  FIXME should find a better method through arrow dataset to mark everything
##  at once and update files as needed.
##


## _ List data set to be touched  ----------------------------------------------
##
##
##  Allow some wiggle room in the comparison (1sec). Either wise it always
##  update everything.
##
todosets <- unique(rbind(
    BB_meta[is.na(chp1_bad_data_flagged)                            |
                chp1_bad_data_flagged < chp1_exclude_mtime      - 1 |
                chp1_bad_temp_flagged < chp1_temp_exclude_mtime - 1 ,
            .(month = month(day), year = year(day))],
    BB_meta[is.na(cm21_bad_data_flagged) |
                cm21_bad_data_flagged < cm21_exclude_mtime - 1 ,
            .(month = month(day), year = year(day))]
))

## Select files to touch
filelist <- filelist[todosets, on = .(flmonth = month, flyear = year)]
rm(todosets, dd)





##  Apply exclusion ranges to DB  ----------------------------------------------
for (af in na.omit(filelist$names)) {
    datapart <- read_parquet(af)
    ## add columns for this set
    datapart[["year"]]  <- as.integer(year( datapart$Date))
    datapart[["month"]] <- as.integer(month(datapart$Date))

    # cat(paste0(Script.ID, " Load: "), af, "\n")

    ## _ CHP-1 flag bad data ---------------------------------------------------
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

    ## _ CHP-1 flag physical limits anomalies  ---------------------------------
    datapart <- data.table(datapart)
    datapart[CHP1_sig < chp1_signal_lower_limit(Date) & !is.na(chp1_bad_data_flag),
             chp1_bad_data_flag := "Abnormal LOW signal"]
    datapart[CHP1_sig > chp1_signal_upper_limit(Date) & !is.na(chp1_bad_data_flag),
             chp1_bad_data_flag := "Abnormal HIGH signal"]
    datapart <- as_tibble(datapart)

    ## _ CHP-1 flag temperature ------------------------------------------------
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

    ## _ CHP-1 flag temperature physical limits --------------------------------
    datapart <- data.table(datapart)
    datapart[chp1_temperature > CHP1_TEMP_MAX        & !is.na(chp1_bad_temp_flag),
             chp1_bad_temp_flag := "Abnormal HIGH temperature"]
    datapart[chp1_temperature < CHP1_TEMP_MIN        & !is.na(chp1_bad_temp_flag),
             chp1_bad_temp_flag := "Abnormal LOW temperature"]
    datapart[chp1_temperature_SD > CHP1_TEMP_STD_LIM & !is.na(chp1_bad_temp_flag),
             chp1_bad_temp_flag := "Abnormal temperature SD"]
    datapart <- as_tibble(datapart)





    ## Save flagged metadata
    BB_meta[day %in% chg_days, cm21_bad_data_flagged := cm21_exclude_mtime     ]
    BB_meta[day %in% chg_days, chp1_bad_temp_flagged := chp1_temp_exclude_mtime]
    BB_meta[day %in% chg_days, chp1_bad_data_flagged := chp1_exclude_mtime     ]

}



## __ Show some info for the flags __ ------------------------------------------

wecare <- c("cm21_bad_data_flag", "chp1_bad_data_flag", "chp1_bad_temp_flag")

for (acol in wecare) {
    stats <- tbl(con, "LAP") |> select(all_of(acol)) |> collect() |> table(useNA = "always")
    pander(data.frame(stats))
}

acol <- "cm21_bad_data_flag"
tbl(con, "LAP") |>
  filter(!!acol == 0)   |>
  filter(!is.na(cm21_bad_data_flag)) |>
  group_by(cm21_bad_data_flag)       |> tally()

  select(all_of("cm21_bad_data_flag")) |> collect() |> tally()


## __ Execution Unlock __ ------------------------------------------------------
myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
