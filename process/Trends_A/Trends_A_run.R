#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
Script.Name <- "~/BBand_LAP/process/Trends/Trends_A_run.R"
# renv::load("~/BBand_LAP", quiet = TRUE)

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/Trends_A/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
output_format <- bookdown::pdf_document2()

library(rmarkdown)


render(input         = "~/BBand_LAP/process/Trends_A/Trends_A_00_raw_data.R",
       output_format = output_format,
       clean         = TRUE,
       output_dir    = output_dir)

render(input         = "~/BBand_LAP/process/Trends_A/Trends_A_01_daily_data.R",
       output_format = output_format,
       clean         = TRUE,
       output_dir    = output_dir)

render(input         = "~/BBand_LAP/process/Trends_A/Trends_A_02_monthly_data.R",
       output_format = output_format,
       clean         = TRUE,
       output_dir    = output_dir)

render(input         = "~/BBand_LAP/process/Trends_A/Trends_A_03_SZA_data.R",
       output_format = output_format,
       clean         = TRUE,
       output_dir    = output_dir)

render(input         = "~/BBand_LAP/process/Trends_A/Trends_A_10_raw_data_analysis.R",
       output_format = output_format,
       clean         = TRUE,
       output_dir    = output_dir)

render(input         = "~/BBand_LAP/process/Trends_A/Trends_A_11_daily_data_analysis.R",
       output_format = output_format,
       clean         = TRUE,
       output_dir    = output_dir)

render(input         = "~/BBand_LAP/process/Trends_A/Trends_A_12_monthly_data_analysis.R",
       output_format = output_format,
       clean         = TRUE,
       output_dir    = output_dir)

render(input         = "~/BBand_LAP/process/Trends_A/Trends_A_13_by_season_analysis.R",
       output_format = output_format,
       clean         = TRUE,
       output_dir    = output_dir)
