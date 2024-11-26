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

cat("\n Create TSI data for LAP\n\n")

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_TSI))
sun <- dbConnect(duckdb(dbdir = DB_LAP, read_only = TRUE))

##  Select Astropy data  -------------------------------------------------------
SUN <- tbl(sun, "params") |>
  filter(!is.na(AsPy_Elevation) & Date >= DB_start_date) |>
  # rename(Sun_Dist_Astropy = "AsPy_Dist")       |>
  # rename(Elevat           = "AsPy_Elevation")  |>
  # mutate(SZA              = 90 - Elevat)       |>
  select(Date)          |>
  arrange(Date)

## TEST
SUN <- SUN |> filter(Date < "2023-01-02")



# print(SUN |> summarise(min(Date), max(Date)))

##  Add Dates  -----------------------------------------------------------------
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
  ## Append new data
  cat("\n Add more dates to", TABLE, "\n\n")
  SUN <- anti_join(SUN |>
                     select(Date),
                   tbl(con, TABLE) |>
                     select(Date)  |>
                     filter(!is.na(Date)),
                   by   = "Date",
                   copy = TRUE)

  cat(paste(Script.ID, ":",
            SUN |> tally() |> pull(),
            "New rows"), "\n")

  res <- insert_table(con,
                      SUN |> select(Date) |> arrange(Date),
                      TABLE, "Date")
}

stop()
## Fill with TSI DATA  ---------------------------------------------------------

## ADD row values for LAP
RAW <- tbl(con, "TSI_NOAA")   |>
  mutate(Source = "RAW_NOAA") |>
  select(Time, TSI, Source)   |>
  rename(Date = "Time")

## Add row values
update_table(con, RAW, TABLE, "Date")


## Fill values with interpolation
library(tidyverse)
left_join(
  tibble(x = seq(min(data.raw$x), max(data.raw$x))),
  data.raw) %>%
  mutate(S = if_else(is.na(S), approx(x, S, x)$y, S))

NEW |>
  mutate(TSI = if_else(is.na(TSI), approx(Date, TSI, Date)$y, TSI))


NEW <- tbl(con, TABLE)
NEW |> filter(!is.na(TSI))


SUN <- tbl(sun, "params") |>
  filter(!is.na(AsPy_Elevation) & Date >= DB_start_date) |>
  rename(Sun_Dist_Astropy = "AsPy_Dist")       |>
  rename(Elevat           = "AsPy_Elevation")  |>
  mutate(SZA              = 90 - Elevat)       |>
  select(Date, SZA, Sun_Dist_Astropy)          |>
  arrange(Date)


dbExecute(con,
paste("
SELECT
Date,
COALESCE(
  TSI,
  (
    SELECT
    prev.TSI + ( (next.TSI - prev.TSI) * (parent.Date - prev.Date) / (next.Date - prev.Date) )
    FROM
    ( SELECT Date, TSI, ROW_NUMBER() OVER (ORDER BY Date DESC) as rn FROM df WHERE Date <= parent.Date and TSI is not null ) AS prev
    CROSS JOIN
    ( SELECT Date, TSI, ROW_NUMBER() OVER (ORDER BY Date ASC) as rn FROM df WHERE Date >= parent.Date and TSI is not null ) AS next
    WHERE
    prev.rn = next.rn
  )
) AS TSI
FROM
LAP_TSI parent
")
)

stop()







stop()
## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
