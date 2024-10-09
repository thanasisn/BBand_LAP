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
Script.Name <- "~/BBand_LAP/parameters/sun/create_sun_data.R"
Script.ID   <- "0A"

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
library(foreach,    warn.conflicts = FALSE, quietly = TRUE)
library(doMC,       warn.conflicts = FALSE, quietly = TRUE)
use_python("/usr/bin/python3")

cat("\n Initialize params DB and/or import Sun data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_LAP))

##  Initialize table with dates to fill  ---------------------------------------
start_date <- as.POSIXct("1992-01-01") + 30  ## start before time
memlimit   <- 9999                           ## add data in batches to save memory
end_date   <- ceiling_date(Sys.time(),
                           unit = "week") +
  30 * 60 * 60 + 30                          ## until the near future

##  Create all dates  ----------------------------------------------------------
if (!dbExistsTable(con, "params")) {
  cat("Initialize table\n")
  ## create table and date variable with pure SQL call
  dbExecute(con, "CREATE TABLE params (Date TIMESTAMP)") ## this is better than TIMESTAMP_S
  ## Create all dates
  DT <- data.table(Date = seq(start_date, end_date, by = "mins"))
  DT[ , Date := round_date(Date, unit = "second")]
  setorder(DT, Date)
  res <- insert_table(con, DT, "params", "Date")
} else {
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

##  Compute Astropy data  ------------------------------------------------------
source_python("~/BBand_LAP/parameters/sun/sun_vector_astropy_p3.py")
## Call pythons Astropy for sun distance calculation
sunR_astropy <- function(date) {
    # sun_vector( format(date) )
    cbind(t(sun_vector(date)), date)
}

if (dbExistsTable(con, "params") &
    any(dbListFields(con, "params") %in% "AsPy_Elevation")) {
  ## fill all dates
  while (tbl(con, "params") |> filter(is.na(AsPy_Elevation)) |> tally() |> pull() > 0) {
    ## batch to fill
    dates_to_do <- tbl(con, "params") |> filter(is.na(AsPy_Elevation)) |> select(Date) |> pull() |> sort()
    dates_to_do <- data.table::first(dates_to_do, memlimit)

    cat(Script.ID, ": Astropy", paste(range(dates_to_do)), "\n")

    ##  Calculate sun vector
    sss   <- data.frame(t(sapply(dates_to_do, sunR_astropy )))
    ##  reshape data
    month <- data.frame(AsPy_Azimuth   = unlist(sss$X1),
                        AsPy_Elevation = unlist(sss$X2),
                        AsPy_Dist      = unlist(sss$X3),
                        Date           = as.POSIXct(unlist(sss$X4),
                                                    origin = "1970-01-01"))
    ##  Put in the data base
    update_table(con, month, "params", "Date")
  }
} else {
  ##  fill some days to initialize table
  dates_to_do <- tbl(con, "params") |> select(Date) |> pull() |> sort()
  dates_to_do <- data.table::first(dates_to_do, memlimit)

  cat(Script.ID, ": Astropy", paste(range(dates_to_do), collapse = " -- "), "\n")

  ##  Calculate sun vector
  sss   <- data.frame(t(sapply(dates_to_do, sunR_astropy )))
  ##  reshape data
  month <- data.frame(AsPy_Azimuth   = unlist(sss$X1),
                      AsPy_Elevation = unlist(sss$X2),
                      AsPy_Dist      = unlist(sss$X3),
                      Date           = as.POSIXct(unlist(sss$X4),
                                                  origin = "1970-01-01"))
  ##  Put in the data base
  update_table(con, month, "params", "Date")
}
rm("sun_vector")

##  Compute Pysolar data  ------------------------------------------------------
source_python("~/BBand_LAP/parameters/sun/sun_vector_pysolar_p3.py")
## Call pythons Astropy for sun distance calculation
sunR_pysolar <- function(date) {
  # sun_vector( format(date) )
  cbind(t(sun_vector(date)), date)
}

if (dbExistsTable(con, "params") &
    any(dbListFields(con, "params") %in% "PySo_Elevation")) {
  ## fill all dates
  while (tbl(con, "params") |> filter(is.na(PySo_Elevation)) |> tally() |> pull() > 0) {
    ## batch to fill
    dates_to_do <- tbl(con, "params") |> filter(is.na(PySo_Elevation)) |> select(Date) |> pull() |> sort()
    dates_to_do <- data.table::first(dates_to_do, memlimit)

    cat(Script.ID, ": Pysolar", paste(range(dates_to_do), collapse = " -- "), "\n")

    ##  Calculate sun vector
    sss   <- data.frame(t(sapply(dates_to_do, sunR_astropy )))
    ##  reshape data
    month <- data.frame(PySo_Azimuth   = unlist(sss$X1),
                        PySo_Elevation = unlist(sss$X2),
                        Date           = as.POSIXct(unlist(sss$X4),
                                                    origin = "1970-01-01"))
    ##  Put in the data base
    update_table(con, month, "params", "Date")
  }
} else {
  ##  fill some days to initialize table
  dates_to_do <- tbl(con, "params") |> select(Date) |> pull() |> sort()
  dates_to_do <- data.table::first(dates_to_do, memlimit)

  cat(Script.ID, ": Pysolar", paste(range(dates_to_do), collapse = " -- "), "\n")

  ##  Calculate sun vector
  sss   <- data.frame(t(sapply(dates_to_do, sunR_pysolar )))
  ##  reshape data
  month <- data.frame(PySo_Azimuth   = unlist(sss$X1),
                      PySo_Elevation = unlist(sss$X2),
                      Date           = as.POSIXct(unlist(sss$X4),
                                                  origin = "1970-01-01"))
  ##  Put in the data base
  update_table(con, month, "params", "Date")
}
rm("sun_vector")

##  Compute LAP sun vector data  -----------------------------------------------

## Sun position algorithm as the other broadband
zenangle <- function(year, min, doy){
  as.numeric(
    system(
      paste("~/BBand_LAP/parameters/sun/zenangle64 ", year, min, doy, " 40.634 -22.956"),
      intern = T)
  )
}

## Parallelize zenangle
registerDoMC()
pzen <- function(year, min = 1:1440, doy) {
  foreach(min = min, .combine = 'c') %dopar% zenangle(year = year, min = min, doy = doy)
}


for (ad in missza) {
  new_sza <- pzen(yyyy, 1:1440, ad)
  DATA[doy == ad, lap_sza := new_sza]
  cat("\nFilled LAP SZA for doy:", ad, "\n")
}

if (dbExistsTable(con, "params") &
    any(dbListFields(con, "params") %in% "Lap_SZA_start")) {
  stop()
} else {
  ##  fill some days to initialize table
  dates_to_do <- tbl(con, "params") |> select(Date) |> pull() |> sort()
  dates_to_do <- data.table::first(dates_to_do, memlimit)

  cat(Script.ID, ": zenangle", paste(range(dates_to_do), collapse = " -- "), "\n")

  ##  Calculate sun vector
  for (aday in dates_to_do) {
    year <- year(aday)
    doy  <- yday(aday)

    LAP_SZA_start  <- pzen(year,   1:1440  , doy)
    LAP_SZA_middle <- pzen(year, 1.5:1440.5, doy)
  }
  stop()
}



## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
