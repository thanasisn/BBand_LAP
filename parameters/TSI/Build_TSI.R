#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/TSI/Build_TSI.R"
renv::load("~/BBand_LAP", quiet = TRUE)

##  Get TSI data  --------------------------------------------------------------
source("~/BBand_LAP/parameters/TSI/00_download_TSI.R"      )
##  Parse TSI to data base  ----------------------------------------------------
source("~/BBand_LAP/parameters/TSI/01_Read_raw_TSI_NOAA.R" )
source("~/BBand_LAP/parameters/TSI/02_Read_raw_TSI_TSIS.R" )
##  Create TSI for LAP  --------------------------------------------------------
source("~/BBand_LAP/parameters/TSI/10_Create_LAP_TSI.R"    )
source("~/BBand_LAP/parameters/TSI/11_Extend_LAP_TSI.R"    )


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
