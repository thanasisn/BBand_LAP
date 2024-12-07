#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_run.R"
renv::load("~/BBand_LAP", quiet = TRUE)

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/QCRad_LongShi/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

##  Run tests  -----------------------------------------------------------------
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T01_v10.R")
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R")
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T03_v10.R")
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T04_v10.R")
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T05_v10.R")


try({
  rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T01_v10.R",
                    params      = list(CLEAN = TRUE),
                    output_file = "QCRad_LongShi_T01_v10",
                    output_dir  = output_dir)
})
try({
  rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R",
                    params      = list(CLEAN = TRUE),
                    output_file = "QCRad_LongShi_T02_v10",
                    output_dir  = output_dir)
})
try({
  rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T03_v10.R",
                    params      = list(CLEAN = TRUE),
                    output_file = "QCRad_LongShi_T03_v10",
                    output_dir  = output_dir)
})
try({
  rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T04_v10.R",
                    params      = list(CLEAN = TRUE),
                    output_file = "QCRad_LongShi_T04_v10",
                    output_dir  = output_dir)
})
try({
  rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T05_v10.R",
                    params      = list(CLEAN = TRUE),
                    output_file = "QCRad_LongShi_T05_v10",
                    output_dir  = output_dir)
})

# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T06_v10.R")
try({
  rmarkdown::render(input       = "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T06_v10.R",
                    params      = list(CLEAN = TRUE),
                    output_file = "QCRad_LongShi_T06_v10_B",
                    output_dir  = output_dir)
})

cat("\n\nEND of QCRad LongShi \n\n")
