#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")


library(rmarkdown)

output_dir <- "~/BBand_LAP/REPORTS/REPORTS/Exports/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


try({
  render(input      = "~/BBand_LAP/process/Exports/Export_CM21_GHI_WRDC_duckdb.R",
         output_dir = output_dir)
})


try({
  render(input      = "~/BBand_LAP/process/Exports/Export_CHP1_DIR.R",
         output_dir = output_dir)
})


# try({
#     render(input         = "~/BBand_LAP/process/Export_CM21_TOT.R",
#            output_format = " bookdown::pdf_document2",
#            output_dir    = "~/BBand_LAP/REPORTS/REPORTS")
# })
# try({
#     render(input         = "~/BBand_LAP/inspect_db/80_Inspect_CM21_TOT_export.R",
#            output_format = " bookdown::pdf_document2",
#            output_dir    = "~/BBand_LAP/REPORTS/REPORTS")
# })



