# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title: "*Identification of Periods of Clear Sky Irradiance in
#'  Time Series of GHI Measurements* Matthew J. Reno and Clifford W. Hansen."
#' author: "Natsis Athanasios"
#' institute: "AUTH"
#' date: "`r format(Sys.time(), '%F')`"
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
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     toc_depth:        4
#'     latex_engine:     xelatex
#'     toc:              yes
#'     fig_width:        7
#'     fig_height:       4.5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#' ---
#+ include=F

#'
#' ## DETECTION OF CLEAR PERIODS IN GHI AND DNI MEASUREMENTS
#'
#' Export flags
#'


#+ include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.cap   = " empty caption ")
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
Script.Name    <- "~/BBand_LAP/process/CSid_RenoHansen/Clear_sky_id_Reno-Hansen_export_v14.2_legacy.R"
Script.Version <- "14.2"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/Aerosols/RAerosols/R/statistics.R")
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")
source("~/BBand_LAP/parameters/theory/Air_mass_models.R")
source("~/BBand_LAP/parameters/theory/Clear_sky_irradiance_models.R")
source("~/BBand_LAP/parameters/theory/Extraterrestrial_radiation_models.R")
source("~/BBand_LAP/parameters/theory/Linke_turbidity_models.R")
source("~/CODE/R_myRtools/myRtools/R/trigonometric.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")

library(RColorBrewer, warn.conflicts = FALSE, quietly = TRUE)
library(caTools,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table,   warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,        warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,       warn.conflicts = FALSE, quietly = TRUE)
library(janitor,      warn.conflicts = FALSE, quietly = TRUE)
library(pander,       warn.conflicts = FALSE, quietly = TRUE)
library(scales,       warn.conflicts = FALSE, quietly = TRUE)
library(yardstick,    warn.conflicts = FALSE, quietly = TRUE)
library(rlang,        warn.conflicts = FALSE, quietly = TRUE)
library(tidyr,        warn.conflicts = FALSE, quietly = TRUE)


##  Load all data from duckdb  -------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))

DATA <- tbl(con, "LAP")

##  Set flag for sky conditions  -----------------------------------------------
##  This is also set in Trends_A_00_raw_data
DATA <- DATA |>
  mutate(
    SKY := case_when(
      CSRHv14_2_flag == 0 ~ "Clear",
      CSRHv14_2_flag != 0 ~ "Cloud",
      .default = as.character(NA)
    ),
  )

## Fix data version to use!!!
EXP  <- DATA |> select(Date, year, starts_with("CSRHv14_2") & contains("flag"), SKY)

yearstoexp <- EXP |> select(year) |> distinct() |> pull()
yearstoexp <- sort(yearstoexp)

for (ay in yearstoexp) {

  ## export to csv
  tmp <- EXP |> filter(year == ay) |> select(-year) |> collect() |> data.table()
  setorder(tmp, Date)
  efile <- paste0("/home/athan/BBand_LAP/REPORTS/EXPORTS/", "CSRHv14_2_", ay)
  write_dat(tmp, efile)

  ## export to matrix
  ## FOR RADMON
  tmp[, DOY    := yday(Date)]
  tmp[, minute := 60 * hour(Date) + minute(Date) + 1 ]
  tmp   <- tmp[, .(DOY, minute, SKY)]

  ##  Change flags to factors
  tmp[SKY == "Clear", SKY := 0]
  tmp[SKY == "Cloud", SKY := 1]
  tmp[is.na(SKY),     SKY := 9]

  exp   <- dcast(tmp, minute ~ DOY, value.var = "SKY")
  efile <- paste0("/media/raddata_cloud_flags/", "CSRHv14_2_", ay, ".csv")
  write.table(format(exp), efile, col.names = T, row.names = F, sep = ", ", quote = F)
}

#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
goodbye()
