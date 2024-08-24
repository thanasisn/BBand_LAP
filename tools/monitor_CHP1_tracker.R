#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */

#### Monitor Broadband acquisition and issue warnings.
## Depends on "~/CODE/conky/scripts/broadband_data_sub.py"

rm(list = (ls()[ls() != ""]))
.libPaths(c(.libPaths(), "/home/athan/.R/x86_64-pc-linux-gnu-library/4.2.3/"))

Script.Name <- "~/BBand_LAP/tools/monitor_CHP1_tracker.R"

Sys.setenv(TZ = "UTC")

library(reticulate)
library(data.table, warn.conflicts = F, quietly = T)

NOTIFY <- FALSE

cat("\n")
message <- ""

## Check tracker tcp link
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
times         <- lap_dt$Time
lap_dt$Time   <- NULL

## count data
## only first 8 channels are operational
res <- colSums(!is.na(lap_dt[,1:16]))

## values only
val <- res[(1:16 %% 2) == 1]

active_channels <- sum(val > 1)

if (active_channels > 6) {
    text    <- "All channels have data.\n"
    message <- paste0(message, text)
    cat(text)
} else {
    text    <- paste0("<b>ONLY ", active_channels, "</b> channels working now!\n")
    message <- paste0(message, text)
    cat(message)
    NOTIFY  <- TRUE
}

last_record <- strptime(paste(Sys.Date(), last(times)), "%F %H.%M", tz = "UTC")
delay       <- as.numeric(Sys.time()) - as.numeric(last_record)
duration    <- difftime(Sys.time(), last_record)
# delay <- 10000

if (delay < 3600) {
    text  <- "Recording is recent.\n"
    message <- paste0(message, text)
    cat(text)
} else {
    text    <- paste0("<b>Last recording was before: ", round(duration[[1]], 2), " ",  units(duration), "</b> \n")
    message <- paste0(message, text)
    cat(message)
    NOTIFY  <- TRUE
}


## received data
print(val)

if (NOTIFY) {
    system(
        paste0("notify-send",
               " -u critical 'Broadband' '",
               sub("^[ ]+", "", gsub("\n", "<br>",message)), "'"))

} else {
    cat("\nOPERATION IS NORMAL\n\n")
}

#' **END**
