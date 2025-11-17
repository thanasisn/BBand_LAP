#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name <- "~/BBand_LAP/process/CSid_RenoHansen/CSid_RenoHansen_run.R"

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/CSid_RenoHansen/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

library(rmarkdown)

## Run every nth day
run_days <- 5

## Get shell arguments
args <- commandArgs(trailingOnly = TRUE)
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
  try({
    render(input      = "~/BBand_LAP/process/CSid_RenoHansen/Clear_sky_id_Reno-Hansen_apply_v14.2_legacy.R",
           output_dir = output_dir)
  })

  try({
    render(input      = "~/BBand_LAP/process/CSid_RenoHansen/Clear_sky_id_Reno-Hansen_export_v14.2_legacy.R",
           output_dir = output_dir)
  })

#   try({
#     source("~/BBand_LAP/process/CSid_RenoHansen/Clear_sky_id_Reno-Hansen_apply_v14.2_legacy.R")
#   })
# 
#   try({
#     source("~/BBand_LAP/process/CSid_RenoHansen/Clear_sky_id_Reno-Hansen_export_v14.2_legacy.R")
#   })
 
  system("~/BBand_LAP/process/CSid_RenoHansen/clear_sky_lap_export_workaroound.sh")

  ##  Run tests  -----------------------------------------------------------------

  # source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R")
  # try({
  #   rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R",
  #                     output_file = "QCRad_LongShi_T02_v10_B",
  #                     output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
  # })


} else {
  cat("\n\nNot time to run CSid RenoHansen yet \n\n")
}




# ## run on the first of each month to include all of the previous
# if (FORCE || as.numeric(strftime(Sys.Date(), "%d")) == 1) {
#
#   ##  Run training  ------------------------------------------------------------
#   source("~/BBand_LAP/process/CSid_RenoHansen/CSid_RenoHansen_01_alldata_optim_v15.R")
#   try({
#     rmarkdown::render(input      = "~/BBand_LAP/process/CSid_RenoHansen/CSid_RenoHansen_01_alldata_optim_v15.R",
#                       output_dir = output_dir)
#   })
#
# }
