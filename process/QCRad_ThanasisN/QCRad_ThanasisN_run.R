#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name <- "~/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_run.R"
# renv::load("~/BBand_LAP")


##  Run tests  -----------------------------------------------------------------
# source("~/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_T07_v10.R")
# try({
#   rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_T07_v10.R",
#                     params      = list(CLEAN = TRUE),
#                     output_file = "QCRad_ThanasisN_T07_v10",
#                     output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
# })

source("~/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_T08_v10.R")
try({
  rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_T08_v10.R",
                    params      = list(CLEAN = TRUE),
                    output_file = "QCRad_ThanasisN_T08_v10",
                    output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
})





cat("\n\nEND of QCRad ThanasisN \n\n")

