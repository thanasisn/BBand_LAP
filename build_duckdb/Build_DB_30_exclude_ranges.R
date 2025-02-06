#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Apply flags on data
#'
#' - CHP-1 bad data ranges
#' - CHP-1 temperature bad data ranges
#' - CM-21 bad data ranges
#'
#' TODO
#'
#' - Implement a pure arrow table method?
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#+ include=F

## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.cap   = " empty caption ")
knitr::opts_chunk$set(fig.pos   = '!h'    )
knitr::opts_chunk$set(tidy = TRUE,
                      tidy.opts = list(
                        indent       = 4,
                        blank        = FALSE,
                        comment      = FALSE,
                        args.newline = TRUE,
                        arrow        = TRUE)
)

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_30_exclude_ranges.R"
Script.ID   <- "30"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/BBand_LAP/functions/Functions_CM21.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)

#+ include=T, echo=F
## Load CHP-1 exclusions  ------------------------------------------------------
chp1_exclude_mtime <- file.mtime(CHP1_EXCLUDE)
ranges_CHP1        <- read.table(CHP1_EXCLUDE,
                                 sep          = ";",
                                 colClasses   = "character",
                                 strip.white  = TRUE,
                                 header       = TRUE,
                                 comment.char = "#")
ranges_CHP1$From  <- as.POSIXct(strptime(ranges_CHP1$From,  format = "%F %H:%M", tz = "UTC"))
ranges_CHP1$Until <- as.POSIXct(strptime(ranges_CHP1$Until, format = "%F %H:%M", tz = "UTC"))

## check negative exclution ranges
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

temp <- ranges_CHP1[ ranges_CHP1$HourSpan > 20, ]
row.names(temp) <- NULL

pander(temp)
cat(" /n /n")

pander(data.table(table(ranges_CHP1$Comment)))
cat(" /n /n")

# temp_flag <- data.table()
# for (i in 1:nrow(ranges_CHP1)) {
#   lower  <- ranges_CHP1$From[   i]
#   upper  <- ranges_CHP1$Until[  i]
#   comme  <- ranges_CHP1$Comment[i]
#   tempex <- data.table(Date = seq(lower + 30, upper - 60 + 30, by = "min"),
#                        chp1_bad_data_flag = comme)
#   temp_flag <- rbind(temp_flag, tempex)
#   rm(tempex)
# }
#
# ll <- ranges_CHP1[1, ]
#
# exrange <- function(x){
#         data.table(
#           Date = seq.POSIXt(
#             from = x$From  + 30,
#             to   = x$Until - 60 + 30,
#             by   = "min"),
#           Comment = x$Comment
#           )
#   }
#
# exrange(ll)



##  Load CHP-1 temperature exclusions  -----------------------------------------
chp1_temp_exclude_mtime <- file.mtime(CHP1_TEMP_EX)
ranges_CHP1_temp        <- read.table(CHP1_TEMP_EX,
                                      sep          = ";",
                                      colClasses   = "character",
                                      strip.white  = TRUE,
                                      header       = TRUE,
                                      comment.char = "#")
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

temp <- ranges_CHP1_temp[ranges_CHP1_temp$HourSpan > 20, ]
row.names(temp) <- NULL

pander(temp)
cat(" /n /n")

rm(temp)

pander(data.table(table(ranges_CHP1_temp$Comment)))
cat(" /n /n")

##  Load CM-21 exclusions  -----------------------------------------------------
cm21_exclude_mtime <- file.mtime(CM21_EXCLUDE)
ranges_CM21        <- read.table(CM21_EXCLUDE,
                                 sep          = ";",
                                 colClasses   = "character",
                                 strip.white  = TRUE,
                                 header       = TRUE,
                                 comment.char = "#")
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

temp <- ranges_CM21[ ranges_CM21$HourSpan > 12 , ]
row.names(temp) <- NULL

pander(temp)
cat(" /n /n")

rm(temp)

pander(data.table(table(ranges_CM21$Comment)))
cat(" /n /n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_BROAD))

## fill all days in meta data until today
{
  minday <- tbl(con, "META") |>
    select(Day) |>
    summarise(min(Day, na.rm = TRUE)) |>
    collect() |>
    pull()
  alldays <- data.table(Day = seq(minday, Sys.Date(), by = "day"))

  missingdays <- anti_join(
    alldays,
    tbl(con, "META") |> select(Day),
    by   = "Day",
    copy = TRUE
  )

  rows_insert(x        = tbl(con, "META"),
              y        = missingdays,
              copy     = TRUE,
              conflict = "ignore",
              in_place = T)
}


ranges_CM21$HourSpan <- NULL

# apply(ranges_CM21, 1,
#       function(x){
#         data.table(
#           Date = seq.POSIXt(
#             from = x[1] + 30,
#             to   = x[2] - 60 + 30,
#             by   = "min")) }
# )

## use dplyr::reframe() to create the data?


## _ CM-21 flag data  ----------------------------------------------------------

##  create exclusion table
##  FIXME make it vectorized somehow
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

##  Create categorical column
categories <- unique(c("empty", temp_flag$cm21_bad_data_flag))

## remove existing flags
remove_column(con, "LAP", "cm21_bad_data_flag")

## create categorical if not existing
make_categorical_column("cm21_bad_data_flag", categories, con, "LAP")

## apply bad data ranges
##  Remove any previous flags
# make_null_column(con, "LAP", "cm21_bad_data_flag", "character")
##  Apply flags
update_table(con, temp_flag, "LAP", "Date")
rm(temp_flag)


## _ CHP-1 flag data  ----------------------------------------------------------

##  create exclusion table
##  FIXME make it vectorized somehow
temp_flag <- data.table()
for (i in 1:nrow(ranges_CHP1)) {
  lower  <- ranges_CHP1$From[   i]
  upper  <- ranges_CHP1$Until[  i]
  comme  <- ranges_CHP1$Comment[i]
  tempex <- data.table(Date = seq(lower + 30, upper - 60 + 30, by = "min"),
                       chp1_bad_data_flag = comme)
  temp_flag <- rbind(temp_flag, tempex)
  rm(tempex)
}

##  Create categorical column
categories <- unique(c("empty", temp_flag$chp1_bad_data_flag))

## remove existing flags
remove_column(con, "LAP", "chp1_bad_data_flag")

## create categorical if not existing
make_categorical_column("chp1_bad_data_flag", categories, con, "LAP")

## apply bad data ranges
##  Remove any previous flags
# make_null_column(con, "LAP", "chp1_bad_data_flag", "character")
##  Apply flags
update_table(con, temp_flag, "LAP", "Date")
rm(temp_flag)


## _ CHP-1 flag temperature data  ----------------------------------------------

##  create exclusion table
##  FIXME make it vectorized somehow
temp_flag <- data.table()
for (i in 1:nrow(ranges_CHP1_temp)) {
  lower  <- ranges_CHP1_temp$From[   i]
  upper  <- ranges_CHP1_temp$Until[  i]
  comme  <- ranges_CHP1_temp$Comment[i]
  tempex <- data.table(Date = seq(lower + 30, upper - 60 + 30, by = "min"),
                       chp1_bad_temp_flag = comme)
  temp_flag <- rbind(temp_flag, tempex)
  rm(tempex)
}

##  Create categorical column
categories <- unique(c("empty", temp_flag$chp1_bad_temp_flag))

## remove existing flags
remove_column(con, "LAP", "chp1_bad_temp_flag")

## create categorical if not existing
make_categorical_column("chp1_bad_temp_flag", categories, con, "LAP")

## apply bad data ranges
##  Remove any previous flags
# make_null_column(con, "LAP", "chp1_bad_temp_flag", "character")
##  Apply flags
update_table(con, temp_flag, "LAP", "Date")
rm(temp_flag)


# left_join(
#   tbl(con, "LAP") |> select(Date),
#   temp_flag,
#   copy = T
# ) |> select(cm21_bad_data_flag) |> distinct()

tbl(con, "LAP") |>
  group_by(cm21_bad_data_flag) |>
  tally()

tbl(con, "LAP") |>
  group_by(chp1_bad_data_flag) |>
  tally()



tbl(con, "LAP") |>
  group_by(cm21_sig_limit_flag) |>
  tally()

tbl(con, "LAP") |>
  group_by(chp1_sig_limit_flag) |>
  tally()


dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
