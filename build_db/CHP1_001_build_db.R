#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({funr::sys.script()},
                        error = function(e) {
                            cat(paste("\nUnresolved script name: ", e),"\n")
                            return("CHP1_001_")
                        })

source("~/BBand_LAP/DEFINITIONS.R")
# dblock <- flock::lock(paste0(DB_DIR, ".lock"), exclusive = TRUE)

## create a db of data
## allowing sparce file filling





stop()

source("~/CHP_1_DIR/CHP1_R11_db_build_pysolar.R")

source("~/CHP_1_DIR/CHP1_R12_db_build_cm21.R")

source("~/CHP_1_DIR/CHP1_R13_db_build_chp1.R")



BB <- arrow::open_dataset(DB_DIR, unify_schemas = T)
arrow::write_dataset(dataset = BB, path = DB_DIR,
                     format       = "parquet",
                     partitioning = c("year", "month"),
                     hive_style   = FALSE)
#
#
#
# BB %>% filter(!is.na(SZA))       %>% nrow()
# BB %>% filter(!is.na(CM21_sig))  %>% nrow()
# BB %>% filter(!is.na(CHP1_sig))  %>% nrow()



# flock::unlock(dblock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
