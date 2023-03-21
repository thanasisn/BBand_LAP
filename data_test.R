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
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)
on.exit(myunlock(DB_lock))



library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## read
BB <- arrow::open_dataset(DB_DIR,
                          unify_schemas = T,
                          hive_style = FALSE,
                          partitioning = c("year", "month"))


BB %>% summarise(min(Date)) %>% collect()

dd <- BB %>% filter(year == 1992) %>% select(Date, tot_glb) %>% collect()

stop()
BB %>% glimpse()

write_dataset(dataset = BB, path = DB_DIR,
              format       = "parquet",
              partitioning = c("year", "month"),
              hive_style   = FALSE)


BB %>% filter(!is.na(SZA))      %>% nrow()
BB %>% filter(!is.na(CM21_sig)) %>% nrow()
BB %>% filter(!is.na(CHP1_sig)) %>% nrow()


# BB <- BB %>% mutate(newcol = CM21_sig + CHP1_sig) %>% collect()
# BB <- BB %>% mutate(newcol = CM21_sig + CHP1_sig) %>% compute()
# BB %>% select(Date) %>% as_datetime()
BB <- BB %>% mutate(newcol = CM21_sig + CHP1_sig) %>% compute()



BB %>% write_dataset(path = DB_DIR,
                     format       = "parquet",
                     partitioning = c("year", "month"),
                     hive_style   = FALSE)



tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
