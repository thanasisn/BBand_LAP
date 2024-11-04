#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/process/Process_BB_DB.R"
renv::load("~/BBand_LAP")


library(rmarkdown)


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


cat("\n\nCheck legacy export\n")


try({
    render(input      = "~/BBand_LAP/process/Legacy_CHP1_L1_export.R",
           output_dir = "~/BBand_LAP/REPORTS/REPORTS")
})


try({
    render(input      = "~/BBand_LAP/process/Legacy_CM21_R60_export.R",
           output_dir = "~/BBand_LAP/REPORTS/REPORTS")
})


try({
    render(input         = "~/BBand_LAP/process/QCRad_LongShi_v9.R",
           output_format = " bookdown::pdf_document2",
           output_dir    = "~/BBand_LAP/REPORTS/REPORTS")
})


## This should be the last thing to run on the BBand_LAP
try({
    render(input         = "~/BBand_LAP/inspect_db/99_Self_evaluation.R",
           output_format = " bookdown::pdf_document2",
           output_dir    = "~/BBand_LAP/REPORTS/REPORTS")
})


system("$HOME/BBand_LAP/process/Upload_reports.sh")


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
