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
Script.Name <- "~/BBand_LAP/parameters/sun/create_sun_data.R"
Script.ID   <- "0A"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
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
use_python("/usr/bin/python3")

cat("\n Initialize params DB and/or import Sun data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_LAP))

##  Initialize table with dates to fill  ---------------------------------------
start_date <- as.POSIXct("1992-01-01") + 30
memlimit   <- 6666   ## add data in batches
end_date   <- ceiling_date(Sys.time(), unit = "month")

if (!dbExistsTable(con, "params")) {
  cat("Initialize dates\n")
  ## Create all dates
  DT <- data.table(Date = seq(start_date, end_date, by = "mins"))
  DT[ , Date := round_date(Date, unit = "second")]
  cat(Script.ID, ": Dates", paste(range(DT$Date)), "\n")
  dbWriteTable(con, "params", DT)
} else {
  ## Extend days, add from last date
  start_date <- tbl(con, "params") |> summarise(max(Date, na.rm = T)) |> pull()
  DT <- data.table(Date = seq(start_date, end_date, by = "mins"))
  DT[ , Date := round_date(Date, unit = "second")]
  cat(Script.ID, ": Dates", paste(range(DT$Date)), "\n")
  insert_table(con, DT, "params", "Date")
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
    dates_to_do <- tbl(con, "params") |> filter(is.na(AsPy_Elevation)) |> select(Date) |> pull()
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
  dates_to_do <- tbl(con, "params") |> select(Date) |> pull()
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
rm(sun_vector())

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
    dates_to_do <- tbl(con, "params") |> filter(is.na(PySo_Elevation)) |> select(Date) |> pull()
    dates_to_do <- data.table::first(dates_to_do, memlimit)

    cat(Script.ID, ": Pysolar", paste(range(dates_to_do)), "\n")

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
  dates_to_do <- tbl(con, "params") |> select(Date) |> pull()
  dates_to_do <- data.table::first(dates_to_do, memlimit)

  cat(Script.ID, ": Pysolar", paste(range(dates_to_do)), "\n")

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
rm(sun_vector())

## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
