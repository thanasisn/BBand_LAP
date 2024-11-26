#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Download and import TSI from NOAA
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
Script.Name <- "~/BBand_LAP/parameters/TSI/Read_raw_TSI_NOAA.R"
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

cat("\n Initialize params DB and/or import TSI data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_TSI))

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
  start   = as.POSIXct(strptime(regmatches(ncfiles, regexpr("s[0-9]+", ncfiles)), "s%Y%m%d")),
  end     = as.POSIXct(strptime(regmatches(ncfiles, regexpr("e[0-9]+", ncfiles)), "e%Y%m%d")),
  created = as.POSIXct(strptime(regmatches(ncfiles, regexpr("c[0-9]+", ncfiles)), "c%Y%m%d")),
  version = regmatches(ncfiles, regexpr("v[0-9]+r[0-9]+", ncfiles)),
  prelimi = grepl("preliminary", ncfiles)
)

## Check if preliminary are old and remove them
prelimin <- ncfiles[prelimi == T,  ]
for (af in 1:nrow(prelimin)) {
  ll <- prelimin[af]
  if (nrow(ncfiles[prelimi != TRUE & ll$start >= start & ll$end <= end ]) != 0) {
    cat("Remove old preliminary file", ll$file)
    file.remove(ll$file)
    ncfiles <- ncfiles[ file != ll$file]
  }
}

## Get the last version of files only
ncfiles <- ncfiles[version == last(sort(unique(ncfiles$version)))]

## __ Parse data  --------------------------------------------------------------
gather <- data.table()
for (af in 1:nrow(ncfiles)) {
  ll <- ncfiles[af]

  anc  <- open.nc(ll$file, write  = FALSE)
  data <- read.nc(anc, unpack = TRUE )
  # print.nc(anc)

  data <- data.table(TSI      = as.vector(data$TSI),
                     Time     = as.vector(data$time),
                     TSI_UNC  = as.vector(data$TSI_UNC),
                     time_low = as.vector(data$time_bnds[1,]),
                     time_upp = as.vector(data$time_bnds[2,]) )
  ## check sanity
  dateorigin <- "1610-01-01 00:00:00"
  stopifnot(grepl(dateorigin, att.get.nc(anc, "time",      "units")))
  stopifnot(grepl(dateorigin, att.get.nc(anc, "time",      "units")))
  stopifnot(grepl(dateorigin, att.get.nc(anc, "time_bnds", "units")))
  ## format data
  data$Time          <- as.Date(    data$Time,     origin = dateorigin)
  data$Time          <- as.POSIXct( data$Time ) + 12 * 3600 + 30 ## shift to match LAP
  data$time_low      <- as.Date(    data$time_low, origin = dateorigin)
  data$time_upp      <- as.Date(    data$time_upp, origin = dateorigin)
  # data$file          <- ll$file
  data$file_Version  <- ll$version
  data$file_Creation <- ll$created
  data$prelimi       <- ll$prelimi
  # data$file_mtime    <- file.mtime(ll$file)

  gather <- rbind(gather, data)
}

## Drop non meaningful data
if (length(unique(gather$time_low - gather$time_upp)) == 1 &
    length(unique(gather$Time - as.POSIXct(gather$time_upp))) == 1) {
  gather[, time_low := NULL]
  gather[, time_upp := NULL]
}





TABLE <- "TSI_NOAA"
if (!dbExistsTable(con, TABLE)) {
  cat("Initialize table\n")
  ## create table and date variable with pure SQL call
  dbExecute(con, paste("CREATE TABLE", TABLE,  "(Time TIMESTAMP)")) ## this is better than TIMESTAMP_S
  setorder(gather, Time)
  res <- insert_table(con, gather, TABLE, "Time")
} else {
  ## always drop preliminar data from DB
  CCC <- tbl(con, TABLE) |>
    filter(prelimi == T) |>
    mutate(TSI           = NA,
           TSI_UNC       = NA,
           file_Version  = NA,
           file_Creation = NA,
           prelimi       = NA)
  res <- update_table(con, CCC, TABLE, "Time")

  ## Keep most recent data every time
  DT   <- tbl(con, TABLE) |> collect() |> data.table()
  Keep <- full_join(
    DT,
    gather) |>
    group_by(Time) |>
    slice(which.max(file_Creation))
  setorder(Keep, Time)
  cat(Script.ID, ": Update", nrow(Keep), "rows of raw", TABLE, "\n")
  res <- update_table(con, Keep, TABLE, "Time")
}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
