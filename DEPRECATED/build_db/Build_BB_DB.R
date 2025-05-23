#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_db/Build_DB.R"
renv::load("~/BBand_LAP")


## Build tracker database ------------------------------------------------------
## TODO: Under development
try(
  source("~/BBand_LAP/build_db/Build_chp1_tracker_DB.R"     )
)

## Import raw data from instruments --------------------------------------------
source("~/BBand_LAP/build_db/Build_DB_01_pysolar.R"         )
source("~/BBand_LAP/build_db/Build_DB_02_cm21.R"            )
source("~/BBand_LAP/build_db/Build_DB_03_chp1.R"            )
source("~/BBand_LAP/build_db/Build_DB_04_chp1_SNC.R"        )
source("~/BBand_LAP/build_db/Build_DB_05_chp1_TMP.R"        )
source("~/BBand_LAP/build_db/Build_DB_06_cm21inclined.R"    )

source("~/BBand_LAP/build_db/Build_DB_16_cm21_TOT.R"        )

## Flag bad data ---------------------------------------------------------------
source("~/BBand_LAP/build_db/Build_DB_30_exclude_ranges.R"  )


## Raw to actual data ----------------------------------------------------------

## __ Apply dark offset and convert to radiation -------------------------------
source("~/BBand_LAP/build_db/Build_DB_42_cm21_dark_radiat.R")
source("~/BBand_LAP/build_db/Build_DB_43_chp1_dark_radiat.R")

## Second pass to construct missing dark !
source("~/BBand_LAP/build_db/Build_DB_42_cm21_dark_radiat.R")
source("~/BBand_LAP/build_db/Build_DB_43_chp1_dark_radiat.R")

## __ Extra process for CHP-1 temperature --------------------------------------
source("~/BBand_LAP/build_db/Build_DB_44_chp1_temp_correc.R")

## Add data from other sources -------------------------------------------------
source("~/BBand_LAP/build_db/Import_50_TSI.R"               )
source("~/BBand_LAP/build_db/Import_51_Pressure.R"          )



cat("\n\nEND of Building the DB\n\n")

## __ Describe environment -----------------------------------------------------
source("~/BBand_LAP/tools/List_dependencies.R")


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
