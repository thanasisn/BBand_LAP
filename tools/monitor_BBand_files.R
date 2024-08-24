#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */


#### Monitor Broadband acquisition and issue warnings.
## Depends on "~/CODE/conky/scripts/broadband_data_sub.py"

rm(list = (ls()[ls() != ""]))
.libPaths(c(.libPaths(), "/home/athan/.R/x86_64-pc-linux-gnu-library/4.2.3/"))

Script.Name <- "~/BBand_LAP/tools/monitor_BBand.R"

Sys.setenv(TZ = "UTC")

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





## Override notification function
options(error = function() {
        system(paste("notify-send -u critical -t 30000 'BROAD BAND NOTIFICATION FAILED'", Script.Name))
})


# TEST
# NOTIFY <- 1

if (NOTIFY >= 1) {
    system(
        paste0("notify-send",
               " -u low 'Broadband' '",
               sub("^[ ]+", "", gsub("\n", "<br>",message)), "'"))

} else if (NOTIFY >= 2) {
    system(
        paste0("notify-send",
               " -u normal 'Broadband' '",
               sub("^[ ]+", "", gsub("\n", "<br>",message)), "'"))

} else if (NOTIFY == 0 ) {
    cat("\nOPERATION IS NORMAL\n\n")
}

#' **END**
