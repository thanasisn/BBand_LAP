#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/tools/Read_steps_tracker.R"


source("~/BBand_LAP//DEFINITIONS.R")

library(data.table)


trSTP_DIR   <- "~/DATA_RAW/tracker_chp1/Tracker_STEP/"


filesin <- list.files(trSTP_DIR,
                      pattern = "sun_tracker_.*.stp",
                      full.names = TRUE)

gather <- data.table()
for (af in filesin) {
    tmp <- fread(af)
    tmp$file <- af
    gather <- rbind(gather, tmp)

}

any(duplicated(gather))
any(duplicated(gather$Date))




tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
