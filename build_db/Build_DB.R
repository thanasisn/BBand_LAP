#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_db/Build_DB.R"


source("~/BBand_LAP//DEFINITIONS.R")


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


source("~/BBand_LAP/build_db/Build_DB_01_pysolar.R")
source("~/BBand_LAP/build_db/Build_DB_02_cm21.R")
source("~/BBand_LAP/build_db/Build_DB_03_chp1.R")
source("~/BBand_LAP/build_db/Build_DB_04_chp1_SNC.R")
source("~/BBand_LAP/build_db/Build_DB_05_chp1_TMP.R")
source("~/BBand_LAP/build_db/Build_DB_06_cm21_TOT.R")


stop()



# library(arrow)
# BB_meta <- read_parquet(DB_META_fl)
# BB_meta$chp1_temp_basename <- NULL
# write_parquet(BB_meta, DB_META_fl)


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
