#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' This run post process on the data.
#' - Create data checks reports
#' - Creates overview of previous processes
#' - Creates daily plots
#'

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_duckdb/Inspect_BB_DB.R"
renv::load("~/BBand_LAP")

output_dir <- "~/BBand_LAP/REPORTS/REPORTS_duck/"

cat("\n\nwrite it in shell to parallelize\n\n")


try({
    cat("\n\n 0. Check files\n")
    rmarkdown::render(input       = "~/BBand_LAP/inspect_duckdb/00_Check_input_files.R",
                      params      = list(CLEAN = TRUE),
                      output_dir  = output_dir)
})

try({
    cat("\n\n 1a. Inspect CHP-1 signal CLEAN\n")
    rmarkdown::render(input       = "~/BBand_LAP/inspect_duckdb/01_Inspect_CHP1_sig_snc_temp.R",
                      params      = list(CLEAN = TRUE),
                      output_file = "01_Inspect_CHP1_sig_snc_temp_CLEAN.pdf",
                      output_dir  = output_dir)
})
try({
    cat("\n\n 1b. Inspect CHP-1 signal DIRTY\n")
    rmarkdown::render(input       = "~/BBand_LAP/inspect_duckdb/01_Inspect_CHP1_sig_snc_temp.R",
                      params      = list(CLEAN = FALSE),
                      output_file = "01_Inspect_CHP1_sig_snc_temp_DIRTY.pdf",
                      output_dir  = output_dir)
})




try({
    cat("\n\n 2a. Inspect CM-21 signal CLEAN\n")
    rmarkdown::render(input       = "~/BBand_LAP/inspect_duckdb/02_Inspect_CM21_sig.R",
                      params      = list(CLEAN = TRUE),
                      output_file = "02_Inspect_CM21_sig_CLEAN.pdf",
                      output_dir  = output_dir)
})
try({
    cat("\n\n 2b. Inspect CM-21 signal DIRTY\n")
    rmarkdown::render(input       = "~/BBand_LAP/inspect_duckdb/02_Inspect_CM21_sig.R",
                      params      = list(CLEAN = FALSE),
                      output_file = "02_Inspect_CM21_sig_DIRTY.pdf",
                      output_dir  = output_dir)
})




# try({
#     cat("\n\n 3a. Inspect INCLINED CM-21 signal CLEAN\n")
#     rmarkdown::render(input       = "~/BBand_LAP/inspect_duckdb/03_Inspect_CM21INC_sig.R",
#                       params      = list(CLEAN = TRUE),
#                       output_file = "03_Inspect_CM21INC_sig_CLEAN.pdf",
#                       output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
# })
# try({
#     cat("\n\n 3b. Inspect INCLINED CM-21 signal DIRTY\n")
#     rmarkdown::render(input       = "~/BBand_LAP/inspect_duckdb/03_Inspect_CM21INC_sig.R",
#                       params      = list(CLEAN = FALSE),
#                       output_file = "03_Inspect_CM21INC_sig_DIRTY.pdf",
#                       output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
# })




try({
    cat("\n\n 10. Plot daily CHP-1 signals\n")
    source("~/BBand_LAP/inspect_duckdb/10_Plot_daily_CHP1_sig.R")
})
try({
    cat("\n\n 11. Plot daily CM-21 signals\n")
    source("~/BBand_LAP/inspect_duckdb/11_Plot_daily_CM21_sig.R")
})




try({
    cat("\n\n 20. Inspect CHP-1 radiation\n")
    rmarkdown::render(input      = "~/BBand_LAP/inspect_duckdb/20_Inspect_CHP1_rad_temp.R",
                      # params     = list(CLEAN = FALSE),
                      output_dir  = output_dir)
})

try({
    cat("\n\n 21. Inspect CM-21 radiation\n")
    rmarkdown::render(input      = "~/BBand_LAP/inspect_duckdb/21_Inspect_CM21_rad.R",
                      # params     = list(CLEAN = FALSE),
                      output_dir  = output_dir)
})




try({
    cat("\n\n 30. Plot daily radiation\n")
    source("~/BBand_LAP/inspect_duckdb/30_Plot_daily_CHP1_L1.R")
})
try({
    cat("\n\n 31. Plot daily radiation\n")
    source("~/BBand_LAP/inspect_duckdb/31_Plot_daily_CM21_L1.R")
})



try({
    cat("\n\n 60. Inspect CM-21/TOT radiation\n")
    rmarkdown::render(input      = "~/BBand_LAP/inspect_duckdb/60_Inspect_TOT_GLB.R",
                      output_dir  = output_dir)
})


try({
    cat("\n\n 80. Inspect duckdbs\n")
    rmarkdown::render(input      = "~/BBand_LAP/inspect_duckdb/80_Inspect_duckdb.R",
                      output_dir  = output_dir)
})



tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
