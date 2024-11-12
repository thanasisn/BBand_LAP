#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB.R"
renv::load("~/BBand_LAP")

# ## Update input files  -------------------------------------------------------
# system("~/BBand_LAP/tools/Get_data_from_sirena.sh"          )
#
# ## Build tracker database  ---------------------------------------------------
# ## TODO: Under development
# try(
#     source("~/BBand_LAP/build_duckdb/Build_chp1_tracker_DB.R"       )
# )

##  Prepare sun data for the database  -----------------------------------------
source("~/BBand_LAP/parameters/sun/create_sun_data.R"           )
# source("~/BBand_LAP/parameters/sun/export_sun_data.R"           )

##  Import raw data from instruments  ------------------------------------------
source("~/BBand_LAP/build_duckdb/Build_DB_01_initialize.R"      )
source("~/BBand_LAP/build_duckdb/Build_DB_02_cm21.R"            )
source("~/BBand_LAP/build_duckdb/Build_DB_03_chp1.R"            )
source("~/BBand_LAP/build_duckdb/Build_DB_04_chp1_SNC.R"        )
source("~/BBand_LAP/build_duckdb/Build_DB_05_chp1_TMP.R"        )
source("~/BBand_LAP/build_duckdb/Build_DB_06_cm21inclined.R"    )
source("~/BBand_LAP/build_duckdb/Build_DB_07_pir.R"             )
source("~/BBand_LAP/build_duckdb/Build_DB_16_cm21_TOT.R"        )

##  Flag bad data  -------------------------------------------------------------
source("~/BBand_LAP/build_duckdb/Build_DB_30_exclude_ranges.R"  )


##  Data processing  -----------------------------------------------------------

## __ Apply dark offset and convert to radiation  ------------------------------
source("~/BBand_LAP/build_duckdb/Build_DB_42_cm21_dark_radiat.R")
source("~/BBand_LAP/build_duckdb/Build_DB_43_chp1_dark_radiat.R")

## __ Second pass to construct missing dark  -----------------------------------
# source("~/BBand_LAP/build_duckdb/Build_DB_42_cm21_dark_radiat.R")
# source("~/BBand_LAP/build_duckdb/Build_DB_43_chp1_dark_radiat.R")

## __ Extra process for CHP-1 temperature  -------------------------------------
# source("~/BBand_LAP/build_duckdb/Build_DB_44_chp1_temp_correc.R")

## Add data from other sources  ------------------------------------------------
source("~/BBand_LAP/build_duckdb/Build_DB_50_Import_TSI.R"      )
source("~/BBand_LAP/build_duckdb/Build_DB_51_Import_Pressure.R" )

## __ Create some new variables  -----------------------------------------------
source("~/BBand_LAP/build_duckdb/Build_DB_60_Create_strict.R"   )

## __ Keep some stats on data bases  -------------------------------------------
source("~/BBand_LAP/inspect_duckdb/Duckdb_save_stats.R"         )

cat("\n\nEND of Building the DB\n\n")

## __ Describe environment -----------------------------------------------------
source("~/BBand_LAP/tools/List_dependencies.R")

