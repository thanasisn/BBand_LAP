#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
renv::load("~/BBand_LAP")

library(rmarkdown)

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/"
dir.create(output_dir, showWarnings = F, recursive = T)




## This should be the last things to run on the BBand_LAP


## __ Keep some stats on data bases  -------------------------------------------
try({
  source("~/BBand_LAP/tools/Duckdb_save_stats.R")
})

try({
  render(input         = "~/BBand_LAP/tools/Duckdb_plot_stats.R",
         output_dir    = output_dir)
})

try({
  render(input         = "~/BBand_LAP/tools/99_Self_evaluation.R",
         output_dir    = output_dir)
})

