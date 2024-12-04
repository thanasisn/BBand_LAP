#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/TSI/Build_TSI.R"
renv::load("~/BBand_LAP")

##  Get NOAA TSI data  ---------------------------------------------------------
source("~/BBand_LAP/parameters/TSI/00_download_TSI_NOAA.R" )
##  Get and parse NOAA TSI  ----------------------------------------------------
source("~/BBand_LAP/parameters/TSI/01_Read_raw_TSI_NOAA.R" )
##  Create TSI for LAP  --------------------------------------------------------
source("~/BBand_LAP/parameters/TSI/10_Create_LAP_TSI.R"    )


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
