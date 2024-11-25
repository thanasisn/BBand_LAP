#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Compute sun vector for LAP with standard tools
#'
#' Populates:
#'  - Date
#'  - AsPy_Azimuth
#'  - AsPy_Elevatation
#'  - AsPy_Dist
#'  - PySo_Azimuth
#'  - PySo_Elevation
#'  - LAP_SZA_start
#'  - LAP_SZA_middle
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
Script.Name <- "~/BBand_LAP/parameters/TSI/Read_TSI_data.R"
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
library(reticulate, warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(RNetCDF,    warn.conflicts = FALSE, quietly = TRUE)

cat("\n Initialize params DB and/or import TSI data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_TSI))

##  TSI from NOAA  -------------------------------------------------------------


## __ Get data  ----------------------------------------------------------------
if (Sys.info()["nodename"] == "sagan") {
  system(paste("wget -N -r -np -nd -nH -A .nc -P", DEST_NOAA, FROM_NOAA))
}

## __ Check available files  ---------------------------------------------------
ncfiles <- list.files(path       = DEST_NOAA,
                      pattern    = "_daily_s.*.nc",
                      recursive  = T,
                      full.names = T )

ncfiles <- data.table(
  file    = ncfiles,
  start   = strptime(regmatches(ncfiles, regexpr("s[0-9]+", ncfiles)), "s%Y%m%d"),
  end     = strptime(regmatches(ncfiles, regexpr("e[0-9]+", ncfiles)), "e%Y%m%d"),
  created = strptime(regmatches(ncfiles, regexpr("c[0-9]+", ncfiles)), "c%Y%m%d"),
  version = regmatches(ncfiles, regexpr("v[0-9]+r[0-9]+", ncfiles)),
  prelimi = grepl("preliminary", ncfiles)
)

test <- ncfiles[, max(created) , by = .(start, end)]





stop()

TABLE <- "TSI_NOAA"
if (!dbExistsTable(con, TABLE)) {
  cat("Initialize table\n")


  stop()
  ## create table and date variable with pure SQL call
  dbExecute(con, paste("CREATE TABLE", TABLE,  "(Date TIMESTAMP)")) ## this is better than TIMESTAMP_S
  ## Create all dates
  DT <- data.table(Date = seq(start_date, end_date, by = "mins"))
  DT[ , Date := round_date(Date, unit = "second")]
  setorder(DT, Date)
  res <- insert_table(con, DT, "params", "Date")
} else {
  stop()
  ## Extend days, add from last date
  start_date <- tbl(con, "params") |> summarise(max(Date, na.rm = T)) |> pull()

  ## temporary fix
  if (end_date < start_date) { end_date <- start_date}

  DT <- data.table(Date = seq(start_date, end_date, by = "mins"))
  DT[ , Date := round_date(Date, unit = "second")]
  setorder(DT, Date)
  if (nrow(DT) > 1) {
    cat(Script.ID, ": Dates", paste(range(DT$Date)), "\n")
    res <- insert_table(con, DT, "params", "Date")
  } else {
    cat("No new dates to add\n")
  }
}


## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
