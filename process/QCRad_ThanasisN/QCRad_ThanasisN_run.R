#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name <- "~/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_run.R"

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/QCRad_ThanasisN/"
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


if (FORCE | as.numeric(Sys.Date()) %% run_days == 0)
{

  # source("~/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_T07_v10.R")
  # try({
  #   rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_T07_v10.R",
  #                     params      = list(CLEAN = TRUE),
  #                     output_file = "QCRad_ThanasisN_T07_v10",
  #                     output_format = output_format,
  #                     output_dir    = output_dir)
  # })

  source("~/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_T08_v10.R")
  try({
    rmarkdown::render(
      input         = "~/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_T08_v10.R",
      output_file   = "QCRad_ThanasisN_T08_v10",
      output_format = output_format,
      output_dir    = output_dir
    )
  })

}



cat("\n\nEND of QCRad ThanasisN \n\n")

