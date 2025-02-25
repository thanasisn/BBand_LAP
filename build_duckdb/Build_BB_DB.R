#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2024 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/Build/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
output_format <- bookdown::pdf_document2()

library(rmarkdown)

# ## Build tracker database  ---------------------------------------------------
# ## TODO: Under development
# try(
#     source("~/BBand_LAP/build_duckdb/Build_chp1_tracker_DB.R"       )
# )

##  Prepare sun data for the database  -----------------------------------------
source("~/BBand_LAP/parameters/sun/create_sun_data.R"           )
source("~/BBand_LAP/parameters/sun/export_sun_data.R"           )

##  Import raw data from instruments  ------------------------------------------
# source("~/BBand_LAP/build_duckdb/Build_DB_01_initialize.R"      )
cat("\n\n 01. Build DB initialize\n")
render(input         = "~/BBand_LAP/build_duckdb/Build_DB_01_initialize.R",
       output_format = output_format,
       output_dir    = output_dir)

# source("~/BBand_LAP/build_duckdb/Build_DB_02_cm21.R"            )
cat("\n\n 02. Build DB cm21\n")
render(input         = "~/BBand_LAP/build_duckdb/Build_DB_02_cm21.R",
       output_format = output_format,
       output_dir    = output_dir)

# source("~/BBand_LAP/build_duckdb/Build_DB_03_chp1.R"            )
cat("\n\n 03. Build DB chp1\n")
render(input         = "~/BBand_LAP/build_duckdb/Build_DB_03_chp1.R",
       output_format = output_format,
       output_dir    = output_dir)


# source("~/BBand_LAP/build_duckdb/Build_DB_04_chp1_SNC.R"        )
cat("\n\n 04. Build DB chp1 SNC\n")
render(input         = "~/BBand_LAP/build_duckdb/Build_DB_04_chp1_SNC.R",
       output_format = output_format,
       output_dir    = output_dir)

# source("~/BBand_LAP/build_duckdb/Build_DB_05_chp1_TMP.R"        )
cat("\n\n 05. Build DB chp1 TMP\n")
render(input         = "~/BBand_LAP/build_duckdb/Build_DB_05_chp1_TMP.R",
       output_format = output_format,
       output_dir    = output_dir)

# source("~/BBand_LAP/build_duckdb/Build_DB_06_cm21inclined.R"    )
cat("\n\n 06. Build DB 06 cm21inclined\n")
render(input         = "~/BBand_LAP/build_duckdb/Build_DB_06_cm21inclined.R",
       output_format = output_format,
       output_dir    = output_dir)

# source("~/BBand_LAP/build_duckdb/Build_DB_07_pir.R"             )
cat("\n\n 07. Build DB pir\n")
render(input         = "~/BBand_LAP/build_duckdb/Build_DB_07_pir.R",
       output_format = output_format,
       output_dir    = output_dir)

# source("~/BBand_LAP/build_duckdb/Build_DB_16_cm21_TOT.R"        )
cat("\n\n 16. Build DB cm21 TOT\n")
render(input         = "~/BBand_LAP/build_duckdb/Build_DB_16_cm21_TOT.R",
       output_format = output_format,
       output_dir    = output_dir)

##  Flag bad data  -------------------------------------------------------------
# source("~/BBand_LAP/build_duckdb/Build_DB_30_exclude_ranges.R"  )
cat("\n\n 30. Build DB exclude ranges\n")
render(input         = "~/BBand_LAP/build_duckdb/Build_DB_30_exclude_ranges.R",
       output_format = output_format,
       output_dir    = output_dir)


##  Data processing  -----------------------------------------------------------

## __ Apply dark offset and convert to radiation  ------------------------------
# source("~/BBand_LAP/build_duckdb/Build_DB_42_cm21_dark_radiat.R")
cat("\n\n 42. Build DB cm21 dark radiat\n")
render(input         = "~/BBand_LAP/build_duckdb/Build_DB_42_cm21_dark_radiat.R",
       output_format = output_format,
       output_dir    = output_dir)

# source("~/BBand_LAP/build_duckdb/Build_DB_43_chp1_dark_radiat.R")
cat("\n\n 43. Build DB chp1 dark radiat\n")
render(input         = "~/BBand_LAP/build_duckdb/Build_DB_43_chp1_dark_radiat.R",
       output_format = output_format,
       output_dir    = output_dir)

## __ Second pass to construct missing dark  -----------------------------------
# source("~/BBand_LAP/build_duckdb/Build_DB_42_cm21_dark_radiat.R")
# source("~/BBand_LAP/build_duckdb/Build_DB_43_chp1_dark_radiat.R")

## __ Extra process for CHP-1 temperature  -------------------------------------
# source("~/BBand_LAP/build_duckdb/Build_DB_44_chp1_temp_correc.R")

## Add data from other sources  ------------------------------------------------
# source("~/BBand_LAP/build_duckdb/Build_DB_50_Import_TSI.R"      )
try({
  cat("\n\n 50. Build DB Import TSI\n")
  render(input         = "~/BBand_LAP/build_duckdb/Build_DB_50_Import_TSI.R",
         output_format = output_format,
         output_dir    = output_dir)
})
# source("~/BBand_LAP/build_duckdb/Build_DB_51_Import_Pressure.R" )
try({
  cat("\n\n 51. Build DB Import Pressure\n")
  render(input         = "~/BBand_LAP/build_duckdb/Build_DB_51_Import_Pressure.R",
         output_format = output_format,
         output_dir    = output_dir)
})


## __ Create some new variables  -----------------------------------------------
# source("~/BBand_LAP/build_duckdb/Build_DB_60_Create_strict.R"   )
try({
  cat("\n\n 60. Build DB Create strict\n")
  render(input         = "~/BBand_LAP/build_duckdb/Build_DB_60_Create_strict.R",
         output_format = output_format,
         output_dir    = output_dir)
})

source("~/BBand_LAP/build_duckdb/Build_chp1_tracker_DB.R")



cat("\n\nEND of Building the DB\n\n")

## __ Describe environment -----------------------------------------------------
source("~/BBand_LAP/tools/List_dependencies.R")

