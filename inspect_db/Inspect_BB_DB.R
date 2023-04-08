#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_db/Inspect_BB_DB.R"


library(rmarkdown)

source("~/BBand_LAP/DEFINITIONS.R")


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


cat("\n\nCheck files\n")
render(input       = "~/BBand_LAP/inspect_db/Check_input_files.R",
       params      = list(CLEAN = TRUE),
       output_dir  = "~/BBand_LAP/REPORTS/REPORTS")



cat("\n\nInspect CHP-1 signal CLEAN\n")
render(input       = "~/BBand_LAP/inspect_db/Inspect_CHP1_sig_snc_temp.R",
       params      = list(CLEAN = TRUE),
       output_file = "Inspect_CHP1_sig_snc_temp_CLEAN.pdf",
       output_dir  = "~/BBand_LAP/REPORTS/REPORTS")

cat("\n\nInspect CHP-1 signal DIRTY\n")
render(input       = "~/BBand_LAP/inspect_db/Inspect_CHP1_sig_snc_temp.R",
       params      = list(CLEAN = FALSE),
       output_file = "Inspect_CHP1_sig_snc_temp_DIRTY.pdf",
       output_dir  = "~/BBand_LAP/REPORTS/REPORTS")



cat("\n\nInspect CM-21 signal CLEAN\n")
render(input       = "~/BBand_LAP/inspect_db/Inspect_CM21_sig.R",
       params      = list(CLEAN = TRUE),
       output_file = "Inspect_CM21_sig_CLEAN.pdf",
       output_dir  = "~/BBand_LAP/REPORTS/REPORTS")

cat("\n\nInspect CM-21 signal DIRTY\n")
render(input       = "~/BBand_LAP/inspect_db/Inspect_CM21_sig.R",
       params      = list(CLEAN = FALSE),
       output_file = "Inspect_CM21_sig_DIRTY.pdf",
       output_dir  = "~/BBand_LAP/REPORTS/REPORTS")


cat("\nPlot daily signals\n\n")
source("~/BBand_LAP/inspect_db/Plot_daily_CHP1_sig.R")
source("~/BBand_LAP/inspect_db/Plot_daily_CM21_sig.R")



cat("\n\nInspect CHP-1 radiation\n")
render(input       = "~/BBand_LAP/inspect_db/Inspect_CHP1_rad_temp.R",
       params      = list(CLEAN = FALSE),
       output_dir  = "~/BBand_LAP/REPORTS/REPORTS")


cat("\nPlot daily radiation\n\n")
source("~/BBand_LAP/inspect_db/Plot_daily_CHP1_L1.R")
# source("~/BBand_LAP/inspect_db/Plot_daily_CM21_sig.R")



tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
