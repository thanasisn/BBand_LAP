#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
renv::load("~/BBand_LAP")

library(rmarkdown)

output_dir <- "~/BBand_LAP/REPORTS/PROCESS"
dir.create(output_dir, showWarnings = F, recursive = T)





# try({
#     render(input      = "~/BBand_LAP/process/Export_CM21_TOT.R",
#            output_dir = "~/BBand_LAP/REPORTS/REPORTS")
# })

# try({
#     render(input         = "~/BBand_LAP/process/QCRad_LongShi_v9.R",
#            output_format = " bookdown::pdf_document2",
#            output_dir    = "~/BBand_LAP/REPORTS/REPORTS")
# })


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
