#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
# closeAllConnections()
# rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name <- "/home/athan/BBand_LAP/process/Cloud_enchancements_A/Enchanc_A_run.R"

output_dir <- "/home/athan/BBand_LAP/REPORTS/REPORTS/Cloud_enchancements_A/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
output_format <- bookdown::pdf_document2()

library(rmarkdown)

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

## run on the first of each month to include all of the previous
if (FORCE || as.numeric(strftime(Sys.Date(), "%d")) == 1) {

  render(
         input         = "/home/athan/BBand_LAP/process/Cloud_enchancements_A/Enchanc_A_00_raw_data.R",
         # output_file   = "/home/athan/BBand_LAP/REPORTS/REPORTS/Trends_A/Trends_A_00_raw_data",
         output_format = output_format,
         # clean         = TRUE,
         output_dir    = output_dir
  )

#  render(
#         input         = "~/BBand_LAP/process/Trends_A/Trends_A_01_daily_data.R",
#         output_format = output_format,
#         # clean         = TRUE,
#         output_dir    = output_dir
#  )
#


} else {
  cat("\nIt's not time to run", Script.Name, "\n\n")
}
