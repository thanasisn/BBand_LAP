#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_run.R"

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/QCRad_LongShi/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
output_format <- bookdown::pdf_document2()

library(rmarkdown)

## Run every nth day
run_days <- 10

## Get shell arguments
args <- commandArgs( trailingOnly = TRUE )
## Override run condition from shell
FORCE <- FALSE
if (length(args) > 0) {
  if (any(args == "FORCERUN")) {
    FORCE <- TRUE
    cat("\n * * * FORCED TO RUN NOW * * *\n\n")
  }
}

##  Run tests  -----------------------------------------------------------------
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T01_v10.R")
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R")
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T03_v10.R")
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T04_v10.R")
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T05_v10.R")
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T06_v10.R")


if (FORCE || as.numeric(Sys.Date()) %% run_days == 0) {

  try({
    render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T01_v10.R",
           params      = list(CLEAN = TRUE),
           output_file = "QCRad_LongShi_T01_v10",
           output_format = output_format,
           output_dir  = output_dir)
  })
  try({
    render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R",
           params      = list(CLEAN = TRUE),
           output_file = "QCRad_LongShi_T02_v10",
           output_format = output_format,
           output_dir  = output_dir)
  })
  try({
    render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T03_v10.R",
           params      = list(CLEAN = TRUE),
           output_file = "QCRad_LongShi_T03_v10",
           output_format = output_format,
           output_dir  = output_dir)
  })
  try({
    render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T04_v10.R",
           params      = list(CLEAN = TRUE),
           output_file = "QCRad_LongShi_T04_v10",
           output_format = output_format,
           output_dir  = output_dir)
  })
  try({
    render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T05_v10.R",
           params      = list(CLEAN = TRUE),
           output_file = "QCRad_LongShi_T05_v10",
           output_format = output_format,
           output_dir  = output_dir)
  })
  try({
    render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T06_v10.R",
           params      = list(CLEAN = TRUE),
           output_file = "QCRad_LongShi_T06_v10",
           output_format = output_format,
           output_dir  = output_dir)
  })

  cat("\n\nEND of QCRad LongShi \n\n")
} else {
  cat("\n\nNot time to run QCRad LongShi yet \n\n")
}
