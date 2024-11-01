#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_run.R"
renv::load("~/BBand_LAP")


##  Create some new variables  -------------------------------------------------
# source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T00_v10.R"      )

##  Run test  ------------------------------------------------------------------
source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T01_v10.R"      )
source("~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R"      )



cat("\n\nEND of QCRad LongShi \n\n")

