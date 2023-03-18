#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({funr::sys.script()},
                        error = function(e) {
                            cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("CHP1_001_")
                        })


source("~/BBand_LAP//DEFINITIONS.R")


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


source("~/BBand_LAP/build_db/Build_DB_01_pysolar.R")
source("~/BBand_LAP/build_db/Build_DB_02_cm21.R")
source("~/BBand_LAP/build_db/Build_DB_03_chp1.R")


stop()
BB <- arrow::open_dataset(DB_DIR,
                          unify_schemas = T,
                          hive_style = FALSE,
                          partitioning = c("year", "month"))

BB %>% glimpse()

# arrow::write_dataset(dataset = BB, path = DB_DIR,
#                      format       = "parquet",
#                      partitioning = c("year", "month"),
#                      hive_style   = FALSE)


BB %>% filter(!is.na(SZA))      %>% nrow()
BB %>% filter(!is.na(CM21_sig)) %>% nrow()
BB %>% filter(!is.na(CHP1_sig)) %>% nrow()


BB <- BB %>% mutate(newcol = CM21_sig + CHP1_sig ) %>% collect()

# library(data.table)

# BB <- BB %>% mutate(newcol = CM21_sig + CHP1_sig) %>% compute()
# BB %>% select(Date) %>% as_datetime()
BB <- BB %>% mutate(newcol = CM21_sig + CHP1_sig) %>% compute()

BB %>% arrow::write_dataset(path = DB_DIR,
                            format       = "parquet",
                            partitioning = c("year", "month"),
                            hive_style   = FALSE)



tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
