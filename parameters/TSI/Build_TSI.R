#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/TSI/Build_TSI.R"
renv::load("~/BBand_LAP", quiet = TRUE)

output_dir <- "~/BBand_LAP/REPORTS/REPORTS_duck/"


##  Get TSI data  --------------------------------------------------------------
source("~/BBand_LAP/parameters/TSI/00_download_TSI.R"      )

##  Parse TSI to data base  ----------------------------------------------------
# source("~/BBand_LAP/parameters/TSI/01_Read_raw_TSI_NOAA.R" )
# source("~/BBand_LAP/parameters/TSI/02_Read_raw_TSI_TSIS.R" )
try({
  cat("\n\n 01. Read raw TSI NOAA \n")
  rmarkdown::render(input      = "~/BBand_LAP/parameters/TSI/01_Read_raw_TSI_NOAA.R",
                    output_dir = output_dir)
})
try({
  cat("\n\n 02. Read raw TSI TSIS \n")
  rmarkdown::render(input      = "~/BBand_LAP/parameters/TSI/02_Read_raw_TSI_TSIS.R",
                    output_dir = output_dir)
})


##  Create TSI for LAP  --------------------------------------------------------
# source("~/BBand_LAP/parameters/TSI/10_Create_LAP_TSI.R" )
# source("~/BBand_LAP/parameters/TSI/11_Extend_LAP_TSI.R" )
try({
  cat("\n\n 10. Create LAP TSI \n")
  rmarkdown::render(input      = "~/BBand_LAP/parameters/TSI/10_Create_LAP_TSI.R",
                    output_dir = output_dir)
})
try({
  cat("\n\n 11. Extend LAP TSI \n")
  rmarkdown::render(input      = "~/BBand_LAP/parameters/TSI/11_Extend_LAP_TSI.R",
                    output_dir = output_dir)
})


##  This will be removed  ------------------------------------------------------
source("~/BBand_LAP/parameters/TSI/30_Export_LAP_TSI_legacy.R")


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
