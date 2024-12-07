#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:  "Pressure data combination from multiple source."
#' author: "Natsis Athanasios"
#' ---
#'
#' Populates pressure for LAP
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#+ include=F

## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = "!h"    )
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
Script.Name <- "~/BBand_LAP/parameters/weather/10_Create_LAP_pressure.R"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)



## INPUTS
davis_elect <- "/home/athan/DATA/WEATHER/Eyryma.Rds"
DIithess2_f <- "/home/athan/DATA/WEATHER/iama.Rds"
davis_lap   <- "/home/athan/DATA/WEATHER/lap.Rds"
WUithess2_f <- "/home/athan/DATA/Wunderground/ITHESSAL2.Rds"

#+ include=T, echo=F, results="asis"
cat("\n# Create pressure data for LAP\n\n")

file.mtime(davis_lap)

#+ include=F
## load all data
DAVI <- readRDS(davisroof_f)
ITHE <- readRDS(WUithess2_f)
DITH <- readRDS(DIithess2_f)
DILA <- readRDS(DIlap_f)

stop()
##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_PRESSURE))

##  Add new dates to main table  -----------------------------------------------
#'
#' ## Fill the dates we care to fill with TSI data
#'
start_date   <- DB_start_date - 1  ## go back one day to fill with interpolation
sun_up_limit <- -3                 ## limit data creation to daylight only

SUN <- tbl(sun, "params")                             |>
  filter(!is.na(AsPy_Elevation) & Date >= start_date) |>
  rename(Elevat           = "AsPy_Elevation")         |>
  filter(Elevat > sun_up_limit)                       |>
  select(Date)

if (!dbExistsTable(con, "LAP_TSI")) {
  ## Create new table
  cat("\n Initialize table", "LAP_TSI", "\n\n")
  ## create table and date variable with pure SQL call
  dbExecute(con, paste("CREATE TABLE", "LAP_TSI", "(Date TIMESTAMP)"))
  ## fill table
  res <- insert_table(con,
                      SUN |> select(Date) |> arrange(Date),
                      "LAP_TSI", "Date")
} else {
  ## Append new data if needed
  SUN <- anti_join(SUN |>
                     select(Date),
                   tbl(con, "LAP_TSI") |>
                     select(Date)  |>
                     filter(!is.na(Date)),
                   by   = "Date",
                   copy = TRUE)

  if (SUN |> tally() |> pull() > 0) {
    cat("\n Add more dates to", "LAP_TSI", "\n\n")
    cat(paste(Script.ID, ":",
              SUN |> tally() |> pull(),
              "New rows"), "\n")

    res <- insert_table(con,
                        SUN |> select(Date) |> arrange(Date),
                        "LAP_TSI", "Date")
  }
}

##  Fill LAP TSI DATA  ---------------------------------------------------------

## __ Insert raw NOAA  ---------------------------------------------------------
#'
#' ## Insert raw NOAA values to the main table
#'

## ADD row values for LAP
RAW <- tbl(con, "TSI_NOAA")                |>
  mutate(Source = "NOAA_RAW")              |>
  select(Time, TSI, Source, file_Creation) |>
  rename(Updated = "file_Creation")        |>
  rename(Date = "Time")

if (!any(tbl(con, "LAP_TSI") |> colnames() %in% "Source")) {
  cat("Inialiaze table", "LAP_TSI", "\n\n")
  ## Add raw values
  res <- update_table(con, RAW, "LAP_TSI", "Date")
}

TEST <- tbl(con, "LAP_TSI")        |>
  filter(Source == "NOAA_RAW") |>
  select(Date, TSI)

##  Update with newer data
if (anti_join(TEST, RAW) |> tally() |> pull() == 0) {
  cat("No new data from NOAA\n\n")
} else {
  cat("New data from NOAA\n\n")
  remove_column(con, "LAP_TSI", "TSI")
  remove_column(con, "LAP_TSI", "TSI_TOA")
  remove_column(con, "LAP_TSI", "TSI_LAP")
  ## Add raw values
  res <- update_table(con, RAW, "LAP_TSI", "Date")
}

## __ Interpolate NOAA  ---------------------------------------------------------
#'
#' ## Fill TSI with interpolated values from raw NOAA.
#'
#'  Create a function than can fill any date in the date range
#'

##  Create interpolation function
tt <- tbl(con, "LAP_TSI") |> filter(Source == "NOAA_RAW") |>
  collect() |> data.table()
tsi_fun <- approxfun(
  x      = tt$Date,
  y      = tt$TSI,
  method = "linear",
  rule   = 1,
  ties   = mean
)

## Fill raw with interpolation
NEW <- tbl(con, "LAP_TSI")

## Fill with interpolated data
yearstofill <- NEW           |>
  filter(is.na(TSI))         |>
  mutate(year = year(Date))  |>
  select(year) |> distinct() |> pull()

for (ay in yearstofill) {
  some <- NEW |> filter(year(Date) == ay) |>
    filter(is.na(TSI)) |> select(Date) |> collect() |> data.table()
  some[, TSI     := tsi_fun(Date)]
  some[, Source  := "NOAA_INTERP"]
  some[, Updated := Sys.time()   ]
  ## write only when needed
  some <- some[!is.na(TSI)]
  if (nrow(some) > 0) {
    cat(paste(Script.ID, ":",
              "Interpolate TSI for", ay), "\n")
    res <- update_table(con, some, "LAP_TSI", "Date")
  }
}

## __ Calculate other TSI values for LAP  --------------------------------------
#'
#' ## Create values of TSI at TOA and LAP ground for all
#'

## Fill TOA and LAP ground
make_new_column(con = con, table = "LAP_TSI", "TSI_TOA")
make_new_column(con = con, table = "LAP_TSI", "TSI_LAP")
NEW <- tbl(con, "LAP_TSI")

yearstofill <- NEW |>
  filter(Date > DB_start_date)            |>
  filter(is.na(TSI_TOA) | is.na(TSI_LAP)) |>
  mutate(year = year(Date))               |>
  select(year) |> distinct() |> pull()

for (ay in yearstofill) {
  some <- NEW |> filter(year(Date) == ay)   |>
    filter(is.na(TSI_TOA) | is.na(TSI_LAP)) |>
    select(Date, TSI)

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
      TSI_LAP = TSI_TOA * cos(SZA*pi/180)  ## TSI on LAP ground
    ) |>
    select(Date, TSI_TOA, TSI_LAP) |>
    filter(!is.na(TSI_TOA) & !is.na(TSI_LAP))

  ## write only when needed
  if (ADD |> tally() |> pull() > 0) {
    cat(paste(Script.ID, ":",
              "TOA and Ground TSI for", ay), "\n")
    res <- update_table(con, ADD, "LAP_TSI", "Date")
  }
}

#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
