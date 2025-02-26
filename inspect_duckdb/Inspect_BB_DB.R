#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' This run post process on the data.
#' - Create data checks reports
#' - Creates overview of previous processes
#' - Creates daily plots
#'

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/Inspect/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
output_format <- bookdown::pdf_document2()

library(rmarkdown)

## Run every nth day
run_days <- 3

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


if (FORCE || as.numeric(Sys.Date()) %% run_days == 0)
{

  ## __ Check inputs  ------------------------------------------------------------
  try({
    cat("\n\n 0. Check files\n")
    render(input         = "~/BBand_LAP/inspect_duckdb/00_Check_input_files.R",
           output_format = output_format,
           output_dir    = output_dir)
  })

  try({
    cat("\n\n 1a. Inspect CHP-1 signal CLEAN\n")
    render(input       = "~/BBand_LAP/inspect_duckdb/01_Inspect_CHP1_sig_snc_temp.R",
           params      = list(CLEAN = TRUE),
           output_file = "01_Inspect_CHP1_sig_snc_temp_CLEAN.pdf",
           output_dir  = output_dir)
  })
  try({
    cat("\n\n 1b. Inspect CHP-1 signal DIRTY\n")
    render(input       = "~/BBand_LAP/inspect_duckdb/01_Inspect_CHP1_sig_snc_temp.R",
           params      = list(CLEAN = FALSE),
           output_file = "01_Inspect_CHP1_sig_snc_temp_DIRTY.pdf",
           output_dir  = output_dir)
  })


  try({
    cat("\n\n 2a. Inspect CM-21 signal CLEAN\n")
    render(input       = "~/BBand_LAP/inspect_duckdb/02_Inspect_CM21_sig.R",
           params      = list(CLEAN = TRUE),
           output_file = "02_Inspect_CM21_sig_CLEAN.pdf",
           output_dir  = output_dir)
  })
  try({
    cat("\n\n 2b. Inspect CM-21 signal DIRTY\n")
    render(input       = "~/BBand_LAP/inspect_duckdb/02_Inspect_CM21_sig.R",
           params      = list(CLEAN = FALSE),
           output_file = "02_Inspect_CM21_sig_DIRTY.pdf",
           output_dir  = output_dir)
  })

  ## No point for clean yet
  try({
    cat("\n\n 3b. Inspect INCLINED CM-21 signal DIRTY\n")
    render(input       = "~/BBand_LAP/inspect_duckdb/03_Inspect_CM21INC_sig.R",
           params      = list(CLEAN = FALSE),
           output_file = "03_Inspect_CM21INC_sig_DIRTY.pdf",
           output_dir  = output_dir)
  })

  if (as.numeric(Sys.Date()) %% run_days == 0)
  {
    try({
      cat("\n\n 4b. Inspect ERPPLAY PIR signal DIRTY\n")
      render(input       = "~/BBand_LAP/inspect_duckdb/04_Inspect_PIR_sig.R",
             params      = list(CLEAN = FALSE),
             output_file = "04_Inspect_PIR_sig_DIRTY.pdf",
             output_dir  = output_dir)
    })
  }

  ## __ Plot daily signal  -------------------------------------------------------
  try({
    cat("\n\n 10. Plot daily CHP-1 signals\n")
    source("~/BBand_LAP/inspect_duckdb/10_Plot_daily_CHP1_sig.R")
  })
  try({
    cat("\n\n 11. Plot daily CM-21 signals\n")
    source("~/BBand_LAP/inspect_duckdb/11_Plot_daily_CM21_sig.R")
  })

  # try({
  #     cat("\n\n 12. Plot daily PIR signals\n")
  #     source("~/BBand_LAP/inspect_duckdb/12_Plot_daily_PIR_sig.R")
  # })


  ## __ Check Radiation  ---------------------------------------------------------
  try({
    cat("\n\n 20. Inspect CHP-1 radiation\n")
    render(input         = "~/BBand_LAP/inspect_duckdb/20_Inspect_CHP1_rad_temp.R",
           output_format = output_format,
           output_dir    = output_dir)
  })

  try({
    cat("\n\n 21. Inspect CM-21 radiation\n")
    render(input         = "~/BBand_LAP/inspect_duckdb/21_Inspect_CM21_rad.R",
           output_format = output_format,
           output_dir    = output_dir)
  })


  ## __ Plot daily radiation  ----------------------------------------------------
  try({
    cat("\n\n 30. Plot daily radiation\n")
    render(input         = "~/BBand_LAP/inspect_duckdb/30_Plot_daily_CHP1_L1.R",
           output_format = output_format,
           output_dir    = output_dir)
  })
  try({
    cat("\n\n 31. Plot daily radiation\n")
    render(input         = "~/BBand_LAP/inspect_duckdb/31_Plot_daily_CM21_L1.R",
           output_format = output_format,
           output_dir    = output_dir)
  })


  if (as.numeric(Sys.Date()) %% run_days == 0)
  {
    try({
      cat("\n\n 60. Inspect CM-21/TOT radiation\n")
      render(input         = "~/BBand_LAP/inspect_duckdb/60_Inspect_TOT_GLB.R",
             output_format = output_format,
             output_dir    = output_dir)
    })

    try({
      cat("\n\n 70. Inspect Level 1 data statistics\n")
      render(input         = "~/BBand_LAP/inspect_duckdb/70_Level_1_stats.R",
             output_format = output_format,
             output_dir    = output_dir)
    })

    try({
      cat("\n\n 71. Inspect Level 2 data statistics\n")
      render(input         = "~/BBand_LAP/inspect_duckdb/71_Level_2_stats.R",
             output_format = output_format,
             output_dir    = output_dir)
    })
  }
}

cat("\n\nEND of inspecting the DB\n\n")
i
