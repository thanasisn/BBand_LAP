#!/usr/bin/env Rscript
# /* Copyright (C) 2019 Athanasios Natsis <natsisthanasis@gmail.com> */

rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n")
                            return("RAD_QC_") })

library(rmarkdown)
library(knitr)

## Work in this dir only
setwd("~/RAD_QC/")



####  render scripts  ####


render("./QCRad_LongShi_v8_id_CM21_CHP1.R",
       clean                = TRUE,
       output_dir           = "./REPORTS/")


render("./QCRad_LongShi_v8_apply_CM21_CHP1.R",
       clean                = TRUE,
       output_dir           = "./REPORTS/")






cat(paste("\n\n IS YOU SEE THIS: \n", Script.Name," GOT TO THE END!! \n"))
cat(paste("\n EVERYTHING SHOULD BE FINE \n"))

## END ##
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
