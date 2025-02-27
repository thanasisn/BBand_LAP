#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")

library(rmarkdown)

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


## This should be the last things to run on the BBand_LAP

## __ Keep some stats on data bases  -------------------------------------------
try({
  source("~/BBand_LAP/tools/Duckdb_save_stats.R")
})

try({
  source("~/BBand_LAP/tools/List_dependencies.R")
})

try({
  render(input         = "~/BBand_LAP/tools/Duckdb_plot_stats.R",
         output_dir    = "~/BBand_LAP/REPORTS/REPORTS/")
})

try({
  render(input         = "~/BBand_LAP/tools/99_Self_evaluation.R",
         output_dir    = "~/BBand_LAP/REPORTS/REPORTS/")
})

