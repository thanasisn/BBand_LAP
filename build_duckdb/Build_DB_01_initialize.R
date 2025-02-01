#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Radiation processing for LAP"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#'
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#'
#' link-citations:  yes
#' colorlinks:      yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   html_document:
#'     toc:        true
#'     fig_width:  9
#'     fig_height: 4
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     keep_md:          no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        4
#'     fig_width:        8
#'     fig_height:       5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---
#+ include=F

#'
#' # Initialize main data base
#'
#' This also creates some columns in the dataset and meta data.
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
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'

#+ include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.cap   = " - empty caption - ")
knitr::opts_chunk$set(fig.pos   = '!h'    )
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_01_initialize.R"
Script.ID   <- "01"
memlimit    <- 66666

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
cat("\n Initialize DB and/or import Sun data\n\n")

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_DUCK))
sun <- dbConnect(duckdb(dbdir = DB_LAP, read_only = TRUE))

##  Select Astropy data  -------------------------------------------------------
SUN <- tbl(sun, "params") |>
  filter(!is.na(AsPy_Elevation) & Date >= DB_start_date) |>
  select(Date,
         AsPy_Azimuth,
         AsPy_Elevation,
         AsPy_Dist)                                      |>
  rename(Azimuth          = "AsPy_Azimuth")              |>
  rename(Sun_Dist_Astropy = "AsPy_Dist")                 |>
  rename(Elevat           = "AsPy_Elevation")

##  Create some useful variables
SUN <- SUN |> mutate(
  month   = as.integer(month(Date)),
  year    = as.integer(year(Date)),
  doy     = as.integer(yday(Date)),
  Day     = as.Date(Date),
  SZA     = 90 - Elevat,
  preNoon = case_when(
    Azimuth <= 180 ~ TRUE,
    Azimuth >  180 ~ FALSE
  )
)

##  Add data  ------------------------------------------------------------------
if (!dbExistsTable(con, "LAP")) {
  ## Create new table
  cat("\n Initialize table 'LAP' \n\n")
  ## create table and date variable with pure SQL call
  dbExecute(con, "CREATE TABLE LAP (Date TIMESTAMP)")

  yto <- SUN |> summarise(min(year, na.rm = T)) |> pull()
  ADD <- SUN |> filter(year == yto) |> collect() |> data.table() |> arrange(Date)
  cat(paste(Script.ID, ":",
            ADD |> distinct(Day) |> tally() |> pull(),
            "New days",
            paste(ADD |> reframe(range(Date)) |> pull(), collapse = " -- ")
  ), sep = "\n")
  res <- insert_table(con, ADD, "LAP", "Date")
} else {
  ## Append new data
  cat("\n Add more data to 'LAP' \n\n")
  SUN <- anti_join(SUN,
                   tbl(con, "LAP") |> select(Date) |> filter(!is.na(Date)) |> collect(),
                   by = "Date",
                   copy = TRUE)

  ytodo <- SUN |> select(year) |> distinct() |> pull()
  for (yto in sort(ytodo)) {
    ADD <- SUN |> filter(year == yto) |> collect() |> data.table() |> arrange(Date)
    cat(paste(Script.ID, ":",
              ADD |> distinct(Day) |> tally() |> pull(),
              "New days",
              paste(ADD |> reframe(range(Date)) |> pull(), collapse = " -- ")
    ), sep = "\n")
    res <- insert_table(con, ADD, "LAP", "Date")
  }
}

##  Create all corresponding metadata days
if (!dbExistsTable(con, "META")) {
  cat("\n Initialize table 'META' \n\n")
  ## FIXME should be done in pure SQL
  init <- tbl(con, "LAP") |> select(Day) |> distinct() |> collect() |> data.table()
  dbWriteTable(con, "META", init)
} else {
  ##  Add new dates
  cat("\n Extend table 'META' \n\n")
  rows_insert(
    tbl(con, "META"),
    tbl(con, "LAP") |> select(Day) |> distinct(),
    by = "Day",
    conflict = "ignore",
    in_place = TRUE
  )
}


##  Create decimal date variable  ----------------------------------------------
#'
#' This steps will consume a lot of memory at first run, it can be moved at start
#'
LAP <- tbl(con, "LAP")
## make sure the column exist in order to query for NAs
make_new_column(con, "LAP", "Decimal_date")

ADD <- LAP |>
  filter(is.na(Decimal_date)) |>
  select(Date)                |>
  collect()                   |>
  data.table()                |>
  mutate(Decimal_date := decimal_date(Date))

res <- update_table(con, ADD, "LAP", "Date")


##  Checks  --------------------------------------------------------------------
stopifnot(tbl(con, "LAP") |> filter(is.na(Date))    |> collect() |> nrow() == 0)
stopifnot(tbl(con, "LAP") |> filter(is.na(Elevat))  |> collect() |> nrow() == 0)
stopifnot(tbl(con, "LAP") |> filter(is.na(Azimuth)) |> collect() |> nrow() == 0)

if (all(tbl(con, "LAP") |> select(Date) |> arrange(Date) |> collect() |> pull() |> diff() == 1)) {
  cat("Dates are regular.\n\n")
} else {
  # stop("DATES NOT REGULAR\n\n")
  warning("DATES NOT REGULAR\n\n")
}

##  Do some inspection  --------------------------------------------------------
if (interactive()) {

  fs::file_size(DB_DUCK)

  tbl(con, "LAP") |> tally()
  tbl(con, "LAP") |> glimpse()

  tbl(con, "LAP")  |> select(Date) |> collect() |> pull() |> range()
  tbl(con, "META") |> select(Day)  |> collect() |> pull() |> range()

  tbl(con, "LAP")  |> select(Day) |> distinct() |> tally()
  tbl(con, "META") |> select(Day) |> distinct() |> tally()
}

#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)
dbDisconnect(sun, shutdown = TRUE); rm(sun)

#+ results="asis", echo=FALSE
goodbye()
