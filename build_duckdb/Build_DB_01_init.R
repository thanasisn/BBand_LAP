#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Read Astropy
#'
#' This also initializes a lot of columns in the dataset and meta data.
#'
#' Populates:
#'  - Date
#'  - Azimuth
#'  - Elevat
#'  - SZA
#'  - year
#'  - month
#'  - doy
#'  - preNoon
#'
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
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
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_01_sun.R"
Script.ID   <- "01"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)

cat("\n Initialize DB and/or import Sun data\n\n")

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_DUCK))
sun <- dbConnect(duckdb(dbdir = DB_LAP, read_only = TRUE))


## TEST
con <- dbConnect(duckdb(dbdir = "~/ZHOST/test.duckdb"))
##  Get Astropy data  ----------------------------------------------------------
SUN <- tbl(sun, "params") |>
  filter(!is.na(AsPy_Elevation) & Date >= DB_start_date) |>
  select(Date, AsPy_Azimuth, AsPy_Elevation, AsPy_Dist)  |>
  rename(Azimuth          = "AsPy_Azimuth")              |>
  rename(Sun_Dist_Astropy = "AsPy_Dist")                 |>
  rename(Elevat           = "AsPy_Elevation")

## select what is missing to add
if (dbExistsTable(con, "LAP")) {
  SUN <- anti_join(SUN,
                   tbl(con, "LAP")        |>
                     select(Date)         |>
                     filter(!is.na(Date)) |>
                     collect(),
                   by = "Date")
}

##  Create some useful variables
SUN <- SUN |> mutate(
  month   = as.integer(month(Date)),
  year    = as.integer(year(Date)),
  doy     = as.integer(yday(Date)),
  Day     = as.Date(Date),
  SZA     = 90 - Elevat,
  preNoon = case_when(
    Azimuth <= 180 ~ TRUE,
    Azimuth >  180 ~ FALSE
  ) |>
    arrange(Date)
)



##  Info display
cat(paste(
  Script.ID,
  ":",
  SUN |> distinct(Day) |> tally() |> pull(),
  "New days"),
  sep = "\n")

##  Add data  ------------------------------------------------------------------
if (!dbExistsTable(con, "LAP")) {
  ## Create new table
  cat("\n Initialize table 'LAP' \n\n")
  dbWriteTable(con, "LAP", SUN)
  ## indexing will block drop of columns for now!!
  # db_create_index(con, "LAP", columns = "Date", unique = TRUE)
} else {
  ## Append new data
  cat("\n Add data to 'LAP' \n\n")
  dbWriteTable(con, "LAP", SUN, append = TRUE)
}

##  Create all corresponding metadata days
if (!dbExistsTable(con, "META")) {

  ## FIXME should be done in pure SQL
  init <- tbl(con, "LAP") |> select(Day) |> distinct() |> collect() |> data.table()

  dbWriteTable(con, "META", init)
} else {
  ##  Add new dates
  rows_insert(
    tbl(con, "META"),
    tbl(con, "LAP") |> select(Day) |> distinct(),
    by = "Day",
    conflict = "ignore",
    in_place = TRUE
  )
}





stop()
##  Get Astropy files  ---------------------------------------------------------
SUN <- data.table(readRDS(ASTROPY_FL))
names(SUN)[names(SUN) == "Dist"]      <- "Sun_Dist_Astropy"
names(SUN)[names(SUN) == "Elevation"] <- "Elevat"
setorder(SUN, Date)
stopifnot(length(unique(SUN$Date)) == nrow(SUN))
SUN <- SUN[as.Date(Date) >= DB_start_date, ]

### FIXME this is for TEST
{
  start_test <- as.Date("2020-01-01")
  test_cnt   <- "~/ZHOST/.test_counter.Rds"
  if (!file.exists(test_cnt)) {
    sscn <- 1
    saveRDS(sscn, test_cnt)
  } else {
    sscn <- readRDS(test_cnt) + 1
    saveRDS(sscn, test_cnt)
  }
  end_test <- start_test + 360 + sscn * 1
  SUN <- SUN[Date < end_test & Date > start_test]
}

## Use date as integer
SUN[, Date := round_date(Date, unit = "second")]

## select what is missing to add
if (dbExistsTable(con, "LAP")) {
  SUN <- anti_join(SUN,
                   tbl(con, "LAP")        |>
                     select(Date)         |>
                     filter(!is.na(Date)) |>
                     collect(),
                   by = "Date")
}

##  Create some useful variables
names(SUN)[names(SUN) == "Dist"] <- "Sun_Dist_Astropy"
SUN <- SUN |> relocate(Date) |> data.table()
SUN[, month := month(  as.POSIXct(SUN$Date, origin = "1970-01-01"))]
SUN[, year  := year(   as.POSIXct(SUN$Date, origin = "1970-01-01"))]
SUN[, doy   := yday(   as.POSIXct(SUN$Date, origin = "1970-01-01"))]
SUN[, Day   := as.Date(as.POSIXct(SUN$Date, origin = "1970-01-01"))]
SUN[, SZA   := 90 - Elevat]
SUN[Azimuth <= 180, preNoon := TRUE ]
SUN[Azimuth >  180, preNoon := FALSE]

##  Info display
cat(paste(
  Script.ID,
  ":",
  unique(as.Date(SUN$Date)
  )),
  sep = "\n")

##  Add data  ------------------------------------------------------------------
if (!dbExistsTable(con, "LAP")) {
  ## Create new table
  cat("\n Initialize table 'LAP' \n\n")
  dbWriteTable(con, "LAP", SUN)
  ## indexing will block drop of columns for now!!
  # db_create_index(con, "LAP", columns = "Date", unique = TRUE)
} else {
  ## Append new data
  cat("\n Add data to 'LAP' \n\n")
  dbWriteTable(con, "LAP", SUN, append = TRUE)
}

##  Create all corresponding metadata days
if (!dbExistsTable(con, "META")) {

  ## FIXME should be done in pure SQL
  init <- tbl(con, "LAP") |> select(Day) |> distinct() |> collect() |> data.table()

  dbWriteTable(con, "META", init)
} else {
  ##  Add new dates
  rows_insert(
    tbl(con, "META"),
    tbl(con, "LAP") |> select(Day) |> distinct(),
    by = "Day",
    conflict = "ignore",
    in_place = TRUE
  )
}

##  Checks  --------------------------------------------------------------------
stopifnot(tbl(con, "LAP") |> filter(is.na(Date))    |> collect() |> nrow() == 0)
stopifnot(tbl(con, "LAP") |> filter(is.na(Elevat))  |> collect() |> nrow() == 0)
stopifnot(tbl(con, "LAP") |> filter(is.na(Azimuth)) |> collect() |> nrow() == 0)

if (all(tbl(con, "LAP") |> select(Date) |> collect() |> pull() |> diff() == 1)) {
  cat("Dates are sorted and regular\n\n")
} else {
  stop("DATES NOT SORTED OR NOT REGULAR\n\n")
}

## Info display
if (interactive()) {
  tbl(con, "LAP") |> tally()
  tbl(con, "LAP") |> glimpse()

  tbl(con, "LAP")  |> select(Date) |> collect() |> pull() |> range()
  tbl(con, "META") |> select(Day)  |> collect() |> pull() |> range()

  tbl(con, "LAP")  |> select(Day) |> distinct() |> tally()
  tbl(con, "META") |> select(Day) |> distinct() |> tally()
}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()
dbDisconnect(sun, shutdown = TRUE); rm("sun"); closeAllConnections()


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
