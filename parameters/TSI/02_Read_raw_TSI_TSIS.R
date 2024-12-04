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
Script.Name <- "~/BBand_LAP/parameters/TSI/02_Read_raw_TSI_TSIS.R"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
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

## __ Import all data  ---------------------------------------------------------
SIS <- readRDS(DATA_TSIS)

SIS <- SIS |>
  rename(Time = "Date",
         TSI  = "tsi_1au") |>
  mutate(Time = Time + 30)

## safe names
names(SIS) <- gsub("[ ]+", "_" , gsub("[ ]+$", "" , gsub("\\)|\\("," ", names(SIS))))

## this doesn't fit in the current scheme
SIS <- SIS |> select(-avg_measurement_date_Julian_Date)

#'
#' Insert new data as needed
#'
#+ echo=T
TABLE <- "TSI_TSIS"
if (!dbExistsTable(con, TABLE)) {
  cat("Initialize table\n")
  ## create table and date variable with pure SQL call
  dbExecute(con, paste("CREATE TABLE", TABLE,  "(Time TIMESTAMP)")) ## this is better than TIMESTAMP_S
  setorder(SIS, Time)
  res <- insert_table(con, SIS, TABLE, "Time")
} else {
  ## Check for new data
  new <- anti_join(SIS,
                   tbl(con, TABLE),
                   by = c("Time", "TSI"),
                   copy = TRUE)
  if (nrow(new) > 0) {
    cat("New data for import\n")
    setorder(SIS, Time)
    cat(" Update", nrow(SIS), "rows of raw", TABLE, "\n")
    res <- update_table(con, SIS, TABLE, "Time")
  }
}
#+ echo=F

## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
