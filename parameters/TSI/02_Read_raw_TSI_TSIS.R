#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Download and import TSI from TSIS
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#+ include=F

## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = "!ht"   )
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
Script.Name <- "~/BBand_LAP/parameters/TSI/02_Read_raw_TSI_TSIS.R"

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

#+ include=T, echo=F, results="asis"
cat("\n# Initialize params DB and/or import TSI data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_TSI))

## __ Import all data  ---------------------------------------------------------
SIS <- readRDS(DATA_TSIS)

SIS <- SIS |>
  rename(Time = "Date",
         TSI  = "tsi_1au") |>
  mutate(Time = Time + 30)    ## shift to middle of the minute

## safe names
names(SIS) <- gsub("[ ]+", "_", gsub("[ ]+$", "", gsub("\\)|\\(", " ", names(SIS))))

## this number doesn't fit in the current duckdb scheme
SIS <- SIS |> select(-avg_measurement_date_Julian_Date)

#'
#' Insert new data as needed
#'

if (!dbExistsTable(con, "TSI_TSIS")) {
  cat("Initialize table\n")
  ## create table and date variable with pure SQL call
  dbExecute(con, paste("CREATE TABLE", "TSI_TSIS",  "(Time TIMESTAMP)")) ## this is better than TIMESTAMP_S
  setorder(SIS, Time)
  res <- insert_table(con, SIS, "TSI_TSIS", "Time")
} else {
  ## Check for new data
  new <- anti_join(SIS,
                   tbl(con, "TSI_TSIS"),
                   by = c("Time", "TSI"),
                   copy = TRUE)
  if (nrow(new) > 0) {
    cat("New data for import\n")
    setorder(SIS, Time)
    cat(" Update", nrow(SIS), "rows of raw", "TSI_TSIS", "\n")
    res <- update_table(con, SIS, "TSI_TSIS", "Time")
  }
}

#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
