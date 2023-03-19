#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({funr::sys.script()},
                        error = function(e) {
                            cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("CHP1_resistance_to_tem")
                        })

source("~/BBand_LAP/DEFINITIONS.R")
source("~/CHP_1_DIR/Functions_CHP1.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)




## read
BB <- arrow::open_dataset(DB_DIR,
                          unify_schemas = T,
                          hive_style = FALSE,
                          partitioning = c("year", "month"))

stop()


BB %>% select(chp1_R_therm) %>% collect() %>% summary()



BB %>% glimpse()






stop()



# day_data <- data.frame(Date         = temp_temp$V1,
#                        CHP1RmeasERR = Protek_506_R_error(    temp_temp$V2),
#                        CHP1temp     = CHP_thermistor_R_to_T( temp_temp$V2),
#                        CHP1tempSD   = CHP_thermistor_ResUnc_to_TempUnc(temp_temp$V2, temp_temp$V3))
# day_data$CHP1tempUNC      <- CHP_thermistor_ResUnc_to_TempUnc(temp_temp$V2, day_data$CHP1RmeasERR )
# day_data$CHP1Resistance   <- temp_temp$V2
# day_data$CHP1ResistanceSD <- temp_temp$V3


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


myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
