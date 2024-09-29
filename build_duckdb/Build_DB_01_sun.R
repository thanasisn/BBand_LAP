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
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_01_sun.R"
Script.ID   <- "01"
# renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)


cat("\n Initialize DB and/or import Sun data\n\n")


##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_DUCK))

##  Get Astropy files  ---------------------------------------------------------
SUN <- data.table(readRDS(ASTROPY_FL))
names(SUN)[names(SUN) == "Dist"]      <- "Sun_Dist_Astropy"
names(SUN)[names(SUN) == "Elevation"] <- "Elevat"
setorder(SUN, Date)
stopifnot(length(unique(SUN$Date)) == nrow(SUN))
SUN <- SUN[as.Date(Date) >= DB_start_date, ]


### FIXME TEST
SUN <- SUN[Date < "2024-01-01" & Date > "2023-01-01"]
# SUN <- SUN[Date > "2023-01-01"]

## Use epoch as key
SUN$Epoch <- as.integer(SUN$Date)
SUN$Date  <- NULL


## drop existing dates
if (dbExistsTable(con, "LAP")) {
  SUN <- anti_join(SUN,
            tbl(con, "LAP") |>
              select(Epoch) |>
              filter(!is.na(Epoch)) |>
              collect(),
            by = "Epoch")
}



## create some nice vars
names(SUN)[names(SUN) == "Dist"] <- "Sun_Dist_Astropy"
SUN <- SUN |> relocate(Epoch) |> data.table()
SUN[, month := month(  as.POSIXct(SUN$Epoch, origin = "1970-01-01"))]
SUN[, year  := year(   as.POSIXct(SUN$Epoch, origin = "1970-01-01"))]
SUN[, doy   := yday(   as.POSIXct(SUN$Epoch, origin = "1970-01-01"))]
SUN[, Day   := as.Date(as.POSIXct(SUN$Epoch, origin = "1970-01-01"))]
SUN[, SZA   := 90 - Elevat]
SUN[Azimuth <= 180, preNoon := TRUE ]
SUN[Azimuth >  180, preNoon := FALSE]


if (!dbExistsTable(con, "LAP")) {
  ## Create new table
  cat("\n Initialize table 'LAP' \n\n")
  dbWriteTable(con, "LAP", SUN)
  ## index will block drop of columns for now!!
  # db_create_index(con, "LAP", columns = "Epoch", unique = TRUE)
  # db_create_index(con, "LAP", columns = "Date", unique = TRUE)
} else {
  ## Append new data
  cat("\n Add data to 'LAP' \n\n")
  dbWriteTable(con, "LAP", SUN, append = TRUE)
}


## Info
tbl(con, "LAP") |> tally()
tbl(con, "LAP") |> glimpse()
SUN             |> tally()
SUN             |> glimpse()


dbDisconnect(con)
rm(con)

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
