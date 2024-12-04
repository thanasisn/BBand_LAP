#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Populates:
#'  - TSI from NOAA
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )

## __ Set environment  ---------------------------------------------------------
# closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/TSI/Create_LAP_TSI.R"
Script.ID   <- "0B"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
  sink(file = paste0("~/BBand_LAP/REPORTS/LOGs/",    basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(RNetCDF,    warn.conflicts = FALSE, quietly = TRUE)

cat("\n Create TSI data for LAP\n\n")

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_TSI))
sun <- dbConnect(duckdb(dbdir = DB_LAP, read_only = TRUE))

##  Select Astropy data  -------------------------------------------------------
SUN <- tbl(sun, "params") |>
  filter(!is.na(AsPy_Elevation) & Date >= DB_start_date) |>
  rename(Elevat           = "AsPy_Elevation")  |>
  filter(Elevat > -5)                          |> ## Don't need night LAP
  select(Date)                                 |>
  arrange(Date)

##  Add new dates to main table  -----------------------------------------------
TABLE <- "LAP_TSI"
if (!dbExistsTable(con, TABLE)) {
  ## Create new table
  cat("\n Initialize table", TABLE, "\n\n")
  ## create table and date variable with pure SQL call
  dbExecute(con, paste("CREATE TABLE", TABLE, "(Date TIMESTAMP)"))
  ## fill table
  res <- insert_table(con,
                      SUN |> select(Date) |> arrange(Date),
                      TABLE, "Date")
} else {
  ## Append new data
  cat("\n Add more dates to", TABLE, "\n\n")
  SUN <- anti_join(SUN |>
                     select(Date),
                   tbl(con, TABLE) |>
                     select(Date)  |>
                     filter(!is.na(Date)),
                   by   = "Date",
                   copy = TRUE)

  cat(paste(Script.ID, ":",
            SUN |> tally() |> pull(),
            "New rows"), "\n")

  res <- insert_table(con,
                      SUN |> select(Date) |> arrange(Date),
                      TABLE, "Date")
}


## Fill with TSI DATA  ---------------------------------------------------------
#'
#'  Insert raw NOAA values to the main table
#'

## TODO detect new data
tbl(con, "TSI_NOAA") |> summarise(max(file_Creation, na.rm = T))
tbl(con, "TSI_NOAA")   |>
  filter(prelimi == T) |> summarise(min(Time, na.rm = T))
tbl(con, "LAP_TSI")

## ADD row values for LAP
RAW <- tbl(con, "TSI_NOAA")                |>
  mutate(Source = "NOAA_RAW")              |>
  select(Time, TSI, Source, file_Creation) |>
  rename(Updated = "file_Creation")        |>
  rename(Date = "Time")

## Add raw values
update_table(con, RAW, TABLE, "Date")

## Fill raw with interpolation
NEW <- tbl(con, TABLE)

NEW |> filter(is.na(TSI)) |> summarise(min(Date), max(Date))
NEW |> filter(!is.na(TSI)) |> summarise(min(Date), max(Date))


ff <- NEW |> filter(Date > "2024-06-29") |> collect()

### Create interpolation function
tt <- NEW |> filter(Source == "NOAA_RAW") |>
  collect() |> data.table()
tsi_fun <- approxfun(
  x      = tt$Date,
  y      = tt$TSI,
  method = "linear",
  rule   = 1,
  ties   = mean
)

## Fill with interpolated data
yearstofill <- NEW           |>
  filter(is.na(TSI))         |>
  mutate(year = year(Date))  |>
  select(year) |> distinct() |> pull()

for (ay in yearstofill) {
  some <- NEW |> filter(year(Date) == ay) |>
    filter(is.na(TSI)) |> select(Date) |> collect() |> data.table()
  some[, TSI    := tsi_fun(Date)]
  some[, Source := "NOAA_INTERP"]
  ## write only when needed
  some <- some[!is.na(TSI)]
  if (nrow(some) > 0) {
    cat(paste(Script.ID, ":",
              "Interpolate TSI for", ay), "\n")
    res <- update_table(con, some, TABLE, "Date")
  }
}


## Fill TOA and LAP ground
make_new_column(con = con, table = TABLE, "TSI_TOA")
make_new_column(con = con, table = TABLE, "TSI_GRN")
NEW <- tbl(con, TABLE)

yearstofill <- NEW |>
  filter(is.na(TSI_TOA) | is.na(TSI_GRN)) |>
  mutate(year = year(Date))  |>
  select(year) |> distinct() |> pull()

for (ay in yearstofill) {
  systime <- Sys.time()
  some <- NEW |> filter(year(Date) == ay) |>
    filter(is.na(TSI_TOA) | is.na(TSI_GRN)) |>
    select(Date, TSI) |>
    mutate(Updated = systime)

  SUN <- tbl(sun, "params") |>
    filter(year(Date) == ay) |>
    filter(!is.na(AsPy_Elevation) & Date >= DB_start_date) |>
    rename(Sun_Dist_Astropy = "AsPy_Dist")       |>
    rename(Elevat           = "AsPy_Elevation")  |>
    mutate(SZA              = 90 - Elevat)       |>
    select(Date, Sun_Dist_Astropy, SZA)

  ADD <- left_join(some, SUN, copy = T) |>
    mutate(
      TSI_TOA = TSI / Sun_Dist_Astropy^2,  ## TSI on LAP TOA
      TSI_GRN = TSI_TOA * cos(SZA*pi/180)  ## TSI on LAP ground
    ) |>
    select(Date, TSI_TOA, TSI_GRN) |>
    filter(!is.na(TSI_TOA) & !is.na(TSI_GRN))

  ## write only when needed
  if (ADD |> tally() |> pull() > 0) {
    cat(paste(Script.ID, ":",
              "TOA and Ground TSI for", ay), "\n")
    res <- update_table(con, ADD, TABLE, "Date")
  }
}


# test <- NEW |> filter(year(Date) == 1993) |> collect() |> data.table()
#
# plot(test$Date, test$TSI)
# points(test[Source == "NOAA_RAW", TSI, Date], col = "red")
#
# plot(test$Date, test$TSI_TOA)
# plot(test$Date, test$TSI_GRN)



## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
