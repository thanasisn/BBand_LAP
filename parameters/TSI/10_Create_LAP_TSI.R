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
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/TSI/Create_LAP_TSI.R"
Script.ID   <- "0B"

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
library(RNetCDF,    warn.conflicts = FALSE, quietly = TRUE)

cat("\n Create TSI data for LAP\n\n")

sun_up_limit <- -3  ## limit data creation to daylight only

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_TSI))
sun <- dbConnect(duckdb(dbdir = DB_LAP, read_only = TRUE))

##  Add new dates to main table  -----------------------------------------------
#'
#' Get the dates we care to fill with TSI data
#'
start_date <- DB_start_date - 1
SUN <- tbl(sun, "params")                             |>
  filter(!is.na(AsPy_Elevation) & Date >= start_date) |>
  rename(Elevat           = "AsPy_Elevation")         |>
  filter(Elevat > sun_up_limit)                       |>
  select(Date)

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
  ## Append new data if needed
  SUN <- anti_join(SUN |>
                     select(Date),
                   tbl(con, TABLE) |>
                     select(Date)  |>
                     filter(!is.na(Date)),
                   by   = "Date",
                   copy = TRUE)

  if (SUN |> tally() |> pull() > 0) {
    cat("\n Add more dates to", TABLE, "\n\n")
    cat(paste(Script.ID, ":",
              SUN |> tally() |> pull(),
              "New rows"), "\n")

    res <- insert_table(con,
                        SUN |> select(Date) |> arrange(Date),
                        TABLE, "Date")
  }
}


##  Fill LAP TSI DATA  ---------------------------------------------------------


## __ Insert raw NOAA  ---------------------------------------------------------
#'
#'  Insert raw NOAA values to the main table
#'
#+ echo=T

## ADD row values for LAP
RAW <- tbl(con, "TSI_NOAA")                |>
  mutate(Source = "NOAA_RAW")              |>
  select(Time, TSI, Source, file_Creation) |>
  rename(Updated = "file_Creation")        |>
  rename(Date = "Time")


if (!any(tbl(con, TABLE) |> colnames() %in% "Source")) {
  cat("Inialiaze table", TABLE, "\n\n")
  ## Add raw values
  res <- update_table(con, RAW, TABLE, "Date")
}

TEST <- tbl(con, TABLE)        |>
  filter(Source == "NOAA_RAW") |>
  select(Date, TSI)

##  Update with newer data
if (anti_join(TEST, RAW) |> tally() |> pull() == 0) {
  cat("No new data from NOAA\n\n")
} else {
  cat("New data from NOAA\n\n")
  ## Add raw values
  res <- update_table(con, RAW, TABLE, "Date")
}




## __ Interpolate NOAA  ---------------------------------------------------------
#'
#'  Fill TSI with interpolated values from raw NOAA.
#'
#'  Create a function than can fill any date
#'
#+ echo=T

### Create interpolation function
tt <- tbl(con, TABLE) |> filter(Source == "NOAA_RAW") |>
  collect() |> data.table()
tsi_fun <- approxfun(
  x      = tt$Date,
  y      = tt$TSI,
  method = "linear",
  rule   = 1,
  ties   = mean
)

## Fill raw with interpolation
NEW <- tbl(con, TABLE)

NEW |> filter(Source == "NOAA_RAW")    |> summarise(max(Updated, na.rm = T)) |> pull() >
  NEW |> filter(Source == "NOAA_INTERP") |> summarise(max(Updated, na.rm = T)) |> pull()



## TODO check updated
warning("This will not detecte")


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
    res <- update_table(con, some, TABLE, "Date")
  }
}


## __ Calculate other TSI values for LAP  --------------------------------------
#'
#'  Create values of TSI at TOA and LAP for all TSIs at 1 au.
#'
#+ echo=T

## Fill TOA and LAP ground
make_new_column(con = con, table = TABLE, "TSI_TOA")
make_new_column(con = con, table = TABLE, "TSI_LAP")
NEW <- tbl(con, TABLE)

# dd <- NEW |> filter(is.na(TSI)) |> collect() |> data.table()

yearstofill <- NEW |>
  filter(Date > DB_start_date)            |>
  filter(is.na(TSI_TOA) | is.na(TSI_LAP)) |>
  mutate(year = year(Date))               |>
  select(year) |> distinct() |> pull()

for (ay in yearstofill) {
  some <- NEW |> filter(year(Date) == ay) |>
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
    res <- update_table(con, ADD, TABLE, "Date")
  }
}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con)

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
