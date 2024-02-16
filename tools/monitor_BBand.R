#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */


#### Monitor Broadband acquisition and issue warnings.
## Depends on "~/CODE/conky/scripts/broadband_data_sub.py"

rm(list = (ls()[ls() != ""]))
Script.Name <- "~/BBand_LAP/tools/monitor_BBand.R"

d <- filelock::lock(paste0("/dev/shm/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
Sys.setenv(TZ = "UTC")
tic <- Sys.time()

library(reticulate)
library(data.table, warn.conflicts = F, quietly = T)


NOTIFY <- 0

## paths
trkr_pth    <- "~/DATA_RAW/tracker_chp1/"
data_pth    <- "~/DATA_RAW/Raddata/"


## raddata files are from yesterday
lap_date <- format(Sys.Date() - 1, "%d%m%y")
## tracker files are concurrent
iso_date <- format(Sys.Date(), "%F")


cat("\n")
message <- ""



## Check tracker files
tracker_files <- list.files(trkr_pth,
                            recursive = T,
                            pattern = paste0("_", iso_date))
if (length(tracker_files) == 3) {
    text    <- "Tracker files are correct.\n"
    message <- paste0(message, text)
    cat(text)
} else {
    text    <- "<b>MISSING</b> today`s tracker files!!\n"
    message <- paste0(message, text)
    cat(text)
    NOTIFY  <- NOTIFY + 1
}



## Check data files
data_files <- list.files(data_pth,
                         recursive = T,
                         pattern = lap_date)
if (length(data_files) > 0) {
    text    <- "Yesterday`s data files exist.\n"
    message <- paste0(message, text)
    cat(text)
} else {
    text    <- "<b>NO DATA</b> yesterday from radmon!\n"
    message <- paste0(message, text)
    cat(message)
    NOTIFY  <- NOTIFY + 1
}



## Check tracker link
Sys.setenv(RETICULATE_PYTHON = "/usr/bin/python3")

## run python script to get the data
use_python("/usr/bin/python3")
source_python("~/CODE/conky/scripts/broadband_data_sub.py")


## Override notification function
options(error = function() {
        system(paste("notify-send -u critical -t 30000 'BROAD BAND NOTIFICATION FAILED'", Script.Name))
})



## Prepare radiation
lap_dt        <- fread(text = lapfile, na.strings = "-9", data.table = F  )
nam           <- expand.grid(c("CH_dt_", "CH_sd_"), 1:12)
names(lap_dt) <- c("Time", paste0(nam$Var1, nam$Var2))
lap_dt$Time   <- NULL

## count data
## only first 8 channels are operational
res <- colSums(!is.na(lap_dt[,1:16]))

## values only
val <- res[(1:16 %% 2) == 1]

active_channels <- sum(val > 1)

if (active_channels > 6) {
    text    <- "All channels working now.\n"
    message <- paste0(message, text)
    cat(text)
} else {
    text    <- paste0("<b>ONLY ", active_channels, "</b> channels working now!\n")
    message <- paste0(message, text)
    cat(message)
    NOTIFY  <- NOTIFY + 1
}

## received data
print(val)

# TEST
# NOTIFY <- 1

if (NOTIFY == 1) {
    system(
        paste0("notify-send",
               " -u low 'Broadband' '",
               sub("^[ ]+", "", gsub("\n", "<br>",message)), "'"))

} else if (NOTIFY == 2) {
    system(
        paste0("notify-send",
               " -u normal 'Broadband' '",
               sub("^[ ]+", "", gsub("\n", "<br>",message)), "'"))

} else if (NOTIFY == 3) {
    system(
        paste0("notify-send",
               " -u critical 'Broadband' '",
               sub("^[ ]+", "", gsub("\n", "<br>",message)), "'"))

} else if (NOTIFY == 0 ) {
    cat("\nOPERATION IS NORMAL\n\n")
}







#' **END**
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
