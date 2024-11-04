#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name <- "~/BBand_LAP/process/CSid_RenoHansen/CSid_RenoHansen_run.R"
renv::load("~/BBand_LAP")


##  Run training  --------------------------------------------------------------

# make each filter a script with parameters
# use the set of filters for training and application of CS


##  Run tests  -----------------------------------------------------------------

# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R")
# try({
#   rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R",
#                     params      = list(CLEAN = TRUE),
#                     output_file = "QCRad_LongShi_T02_v10_B",
#                     output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
# })




cat("\n\nEND of CSis RenoHansen \n\n")

