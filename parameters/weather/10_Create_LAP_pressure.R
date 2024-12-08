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
require(ggplot2,    warn.conflicts = FALSE, quietly = TRUE)



## INPUTS
davis_elect <- "/home/athan/DATA/WEATHER/Eyryma.Rds"
DIithess2_f <- "/home/athan/DATA/WEATHER/iama.Rds"
davis_lap   <- "/home/athan/DATA/WEATHER/lap.Rds"
WUithess2_f <- "/home/athan/DATA/Wunderground/ITHESSAL2.Rds"

#+ include=T, echo=F, results="asis"
cat("\n# Create pressure data for LAP\n\n")

#+ include=T, echo=F
pressure_limit_low  <- 970


##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_PRESSURE))

if (!dbExistsTable(con, "PRESSURE_RAW")) {
  cat("\n Initialize table", "PRESSURE_RAW", "\n\n")
  ## create table and date variable with pure SQL call
  res <- dbExecute(con, paste("CREATE TABLE", "PRESSURE_RAW", "(Date TIMESTAMP)"))

  ##  Add static data  ---------------------------------------------------------

  ## __ WUithess2  -------------------------------------------------------------
  ## Wunderground a nearby station
  ## data stopped being free access
  ITHE <- readRDS(WUithess2_f)

  cat(
    "\nWUithess2: Ignoring",
    ITHE[PressuremB < pressure_limit_low, .N],
    "pressure points below", pressure_limit_low,
    "hPa\n\n"
  )

  ITHE <- ITHE |>
    select(contains(c("Date", "Pressure"))) |>
    rename(Date               = DateUTC,
           Pressure_WUithess2 = PressuremB) |>
    filter(Pressure_WUithess2 > pressure_limit_low) |>
    arrange(Date)

  stopifnot(all(duplicated(ITHE$Date)) == FALSE)

  res <- insert_table(con, ITHE, "PRESSURE_RAW", "Date")
  rm(ITHE); d <- gc()


  ## __ DIithess2  -------------------------------------------------------------

  ## Direct access to ithess2 data?
  ## Can access under request
  DITH <- readRDS(DIithess2_f)
  DITH <- DITH[!is.na(barometer)]

  cat(
    "\nDIithess2: Ignoring",
    DITH[barometer < pressure_limit_low, .N],
    "pressure points below", pressure_limit_low,
    "hPa\n\n"
  )

  DITH <- DITH |>
    select(Date, barometer) |>
    rename(Pressure_DIithess2 = barometer) |>
    filter(Pressure_DIithess2 > pressure_limit_low) |>
    arrange(Date)

  stopifnot(all(duplicated(DITH$Date)) == FALSE)

  res <- insert_table(con, DITH, "PRESSURE_RAW", "Date")
  rm(DITH); d <- gc()


  ## __ DIithess2  -------------------------------------------------------------

  ## Data from roof Davis of electronics department

  DAVI <- data.table(readRDS(davis_elect))
  DAVI <- DAVI[, .(Date, Bar)]

  cat(
    "\ndavis_elect: Ignoring",
    DAVI[Bar < pressure_limit_low, .N],
    "pressure points below", pressure_limit_low,
    "hPa\n\n"
  )

  DAVI <- DAVI |>
    rename(Pressure_davis_elect = Bar) |>
    filter(Pressure_davis_elect > pressure_limit_low) |>
    arrange(Date)

  stopifnot(all(duplicated(DAVI$Date)) == FALSE)

  res <- insert_table(con, DAVI, "PRESSURE_RAW", "Date")
  rm(DAVI); d <- gc()
}

##  Add active data  -----------------------------------------------------------

##  This will work after a davis export

if (dbExistsTable(con, "PRESSURE_RAW") &
    file.mtime(davis_lap) > file.mtime(DB_PRESSURE)) {

  ## __ LAP davis on the roof  -------------------------------------------------

  DILA <- readRDS(davis_lap)
  DILA <- DILA[, Date, pressure]
  DILA <- DILA[!is.na(pressure)]

  cat(
    "\ndavis_lap: Ignoring",
    DILA[pressure < pressure_limit_low, .N],
    "pressure points below", pressure_limit_low,
    "hPa\n\n"
  )

  DILA <- DILA |>
    rename(Pressure_davis_lap = pressure) |>
    filter(Pressure_davis_lap > pressure_limit_low) |>
    arrange(Date)

  stopifnot(all(duplicated(DILA$Date)) == FALSE)

  res <- insert_table(con, DILA, "PRESSURE_RAW", "Date")
  rm(DILA); d <- gc()
}


RAW <- tbl(con, "PRESSURE_RAW")
RAW |> tally()
RAW |> summarise(min(Date, na.rm = TRUE), max(Date, na.rm = TRUE))

RAW |>
  ggplot() +
  geom_point(aes(x = Date, y = Pressure_WUithess2),   color = 2, size = 0.5) +
  geom_point(aes(x = Date, y = Pressure_DIithess2),   color = 3, size = 0.5) +
  geom_point(aes(x = Date, y = Pressure_davis_elect), color = 4, size = 0.5) +
  geom_point(aes(x = Date, y = Pressure_davis_lap),   color = 5, size = 0.5)



##  Create composite pressure  -------------------------------------------------
#'
#' ## Fill the composite pressure form raw pressure
#'


# ##  Fill LAP TSI DATA  ---------------------------------------------------------
#
# ## __ Insert raw NOAA  ---------------------------------------------------------
# #'
# #' ## Insert raw NOAA values to the main table
# #'
#
# ## ADD row values for LAP
# RAW <- tbl(con, "TSI_NOAA")                |>
#   mutate(Source = "NOAA_RAW")              |>
#   select(Time, TSI, Source, file_Creation) |>
#   rename(Updated = "file_Creation")        |>
#   rename(Date = "Time")
#
# if (!any(tbl(con, "LAP_TSI") |> colnames() %in% "Source")) {
#   cat("Inialiaze table", "LAP_TSI", "\n\n")
#   ## Add raw values
#   res <- update_table(con, RAW, "LAP_TSI", "Date")
# }
#
# TEST <- tbl(con, "LAP_TSI")        |>
#   filter(Source == "NOAA_RAW") |>
#   select(Date, TSI)
#
# ##  Update with newer data
# if (anti_join(TEST, RAW) |> tally() |> pull() == 0) {
#   cat("No new data from NOAA\n\n")
# } else {
#   cat("New data from NOAA\n\n")
#   remove_column(con, "LAP_TSI", "TSI")
#   remove_column(con, "LAP_TSI", "TSI_TOA")
#   remove_column(con, "LAP_TSI", "TSI_LAP")
#   ## Add raw values
#   res <- update_table(con, RAW, "LAP_TSI", "Date")
# }
#
# ## __ Interpolate NOAA  ---------------------------------------------------------
# #'
# #' ## Fill TSI with interpolated values from raw NOAA.
# #'
# #'  Create a function than can fill any date in the date range
# #'
#
# ##  Create interpolation function
# tt <- tbl(con, "LAP_TSI") |> filter(Source == "NOAA_RAW") |>
#   collect() |> data.table()
# tsi_fun <- approxfun(
#   x      = tt$Date,
#   y      = tt$TSI,
#   method = "linear",
#   rule   = 1,
#   ties   = mean
# )
#
# ## Fill raw with interpolation
# NEW <- tbl(con, "LAP_TSI")
#
# ## Fill with interpolated data
# yearstofill <- NEW           |>
#   filter(is.na(TSI))         |>
#   mutate(year = year(Date))  |>
#   select(year) |> distinct() |> pull()
#
# for (ay in yearstofill) {
#   some <- NEW |> filter(year(Date) == ay) |>
#     filter(is.na(TSI)) |> select(Date) |> collect() |> data.table()
#   some[, TSI     := tsi_fun(Date)]
#   some[, Source  := "NOAA_INTERP"]
#   some[, Updated := Sys.time()   ]
#   ## write only when needed
#   some <- some[!is.na(TSI)]
#   if (nrow(some) > 0) {
#     cat(paste(Script.ID, ":",
#               "Interpolate TSI for", ay), "\n")
#     res <- update_table(con, some, "LAP_TSI", "Date")
#   }
# }
#
# ## __ Calculate other TSI values for LAP  --------------------------------------
# #'
# #' ## Create values of TSI at TOA and LAP ground for all
# #'
#
# ## Fill TOA and LAP ground
# make_new_column(con = con, table = "LAP_TSI", "TSI_TOA")
# make_new_column(con = con, table = "LAP_TSI", "TSI_LAP")
# NEW <- tbl(con, "LAP_TSI")
#
# yearstofill <- NEW |>
#   filter(Date > DB_start_date)            |>
#   filter(is.na(TSI_TOA) | is.na(TSI_LAP)) |>
#   mutate(year = year(Date))               |>
#   select(year) |> distinct() |> pull()
#
# for (ay in yearstofill) {
#   some <- NEW |> filter(year(Date) == ay)   |>
#     filter(is.na(TSI_TOA) | is.na(TSI_LAP)) |>
#     select(Date, TSI)
#
#   SUN <- tbl(sun, "params") |>
#     filter(year(Date) == ay) |>
#     filter(!is.na(AsPy_Elevation) & Date >= DB_start_date) |>
#     rename(Sun_Dist_Astropy = "AsPy_Dist")       |>
#     rename(Elevat           = "AsPy_Elevation")  |>
#     mutate(SZA              = 90 - Elevat)       |>
#     select(Date, Sun_Dist_Astropy, SZA)
#
#   ADD <- left_join(some, SUN, copy = T) |>
#     mutate(
#       TSI_TOA = TSI / Sun_Dist_Astropy^2,  ## TSI on LAP TOA
#       TSI_LAP = TSI_TOA * cos(SZA*pi/180)  ## TSI on LAP ground
#     ) |>
#     select(Date, TSI_TOA, TSI_LAP) |>
#     filter(!is.na(TSI_TOA) & !is.na(TSI_LAP))
#
#   ## write only when needed
#   if (ADD |> tally() |> pull() > 0) {
#     cat(paste(Script.ID, ":",
#               "TOA and Ground TSI for", ay), "\n")
#     res <- update_table(con, ADD, "LAP_TSI", "Date")
#   }
# }

#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
