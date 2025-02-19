#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name <- "~/BBand_LAP/process/CSid_RenoHansen/CSid_RenoHansen_run.R"
# renv::load("~/BBand_LAP")

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/CSid_RenoHansen/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

library(rmarkdown)

## Run every nth day
run_days <- 10

if (as.numeric(Sys.Date()) %% run_days == 0)
{
  try({
    render(input      = "~/BBand_LAP/process/CSid_RenoHansen/Clear_sky_id_Reno-Hansen_apply_v14.2_legacy.R",
           output_dir = output_dir)
  })



  #; ##  Run training  --------------------------------------------------------------
  #; source("~/BBand_LAP/process/CSid_RenoHansen/CSid_RenoHansen_01_alldata_optim_v15.R")
  #; try({
  #;   rmarkdown::render(input      = "~/BBand_LAP/process/CSid_RenoHansen/CSid_RenoHansen_01_alldata_optim_v15.R",
  #;                     output_dir = output_dir)
  #; })

  ##  Run tests  -----------------------------------------------------------------

  # source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R")
  # try({
  #   rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R",
  #                     params      = list(CLEAN = TRUE),
  #                     output_file = "QCRad_LongShi_T02_v10_B",
  #                     output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
  # })

  cat("\n\nEND of CSid RenoHansen \n\n")

} else {
  cat("\n\nNot time to run CSid RenoHansen yet \n\n")
}
