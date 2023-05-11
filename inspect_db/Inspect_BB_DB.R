#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_db/Inspect_BB_DB.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

## __ Load libraries  ----------------------------------------------------------
library(rmarkdown, warn.conflicts = TRUE, quietly = TRUE)



try({
    cat("\n\n 1. Check files\n")
    render(input       = "~/BBand_LAP/inspect_db/Check_input_files.R",
           params      = list(CLEAN = TRUE),
           output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
})



try({
    cat("\n\n 2. Inspect CHP-1 signal CLEAN\n")
    render(input       = "~/BBand_LAP/inspect_db/Inspect_CHP1_sig_snc_temp.R",
           params      = list(CLEAN = TRUE),
           output_file = "Inspect_CHP1_sig_snc_temp_CLEAN.pdf",
           output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
})
try({
    cat("\n\n 3. Inspect CHP-1 signal DIRTY\n")
    render(input       = "~/BBand_LAP/inspect_db/Inspect_CHP1_sig_snc_temp.R",
           params      = list(CLEAN = FALSE),
           output_file = "Inspect_CHP1_sig_snc_temp_DIRTY.pdf",
           output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
})



try({
    cat("\n\n 4. Inspect CM-21 signal CLEAN\n")
    render(input       = "~/BBand_LAP/inspect_db/Inspect_CM21_sig.R",
           params      = list(CLEAN = TRUE),
           output_file = "Inspect_CM21_sig_CLEAN.pdf",
           output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
})
try({
    cat("\n\n 5. Inspect CM-21 signal DIRTY\n")
    render(input       = "~/BBand_LAP/inspect_db/Inspect_CM21_sig.R",
           params      = list(CLEAN = FALSE),
           output_file = "Inspect_CM21_sig_DIRTY.pdf",
           output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
})





try({
    cat("\n\n 6. Plot daily CHP-1 signals\n")
    source("~/BBand_LAP/inspect_db/Plot_daily_CHP1_sig.R")
})
try({
    cat("\n\n 7. Plot daily CM-21 signals\n")
    source("~/BBand_LAP/inspect_db/Plot_daily_CM21_sig.R")
})



try({
    cat("\n\n 8. Inspect CHP-1 radiation\n")
    render(input       = "~/BBand_LAP/inspect_db/Inspect_CHP1_rad_temp.R",
           # params      = list(CLEAN = FALSE),
           output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
})

try({
    cat("\n\n 9. Inspect CM-21 radiation\n")
    render(input       = "~/BBand_LAP/inspect_db/Inspect_CM21_rad.R",
           # params      = list(CLEAN = FALSE),
           output_dir  = "~/BBand_LAP/REPORTS/REPORTS")
})




try({
    cat("\n\n 10. Plot daily radiation\n")
    source("~/BBand_LAP/inspect_db/Plot_daily_CHP1_L1.R")
})
try({
    cat("\n\n 11. Plot daily radiation\n")
    source("~/BBand_LAP/inspect_db/Plot_daily_CM21_L1.R")
})



tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
