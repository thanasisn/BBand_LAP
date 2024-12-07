#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2024 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/weather/Build_weather.R"
renv::load("~/BBand_LAP", quiet = TRUE)

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/Parameters_weather/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

##  Parse data from Davis  -----------------------------------------------------
try({
  cat("\n\n 01. Read LAP davis \n")
  rmarkdown::render(input      = "~/BBand_LAP/parameters/weather/01_Read_LAP_davis.R",
                    output_dir = output_dir)
})

##  Create pressure for LAP  ---------------------------------------------------
source("~/BBand_LAP/parameters/weather/10_Create_LAP_pressure.R" )
# try({
#   cat("\n\n 02. Read raw TSI TSIS \n")
#   rmarkdown::render(input      = "~/BBand_LAP/parameters/TSI/02_Read_raw_TSI_TSIS.R",
#                     output_dir = output_dir)
# })


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
