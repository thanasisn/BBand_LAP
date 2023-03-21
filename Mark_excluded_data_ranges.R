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
mylock(DB_lock)
on.exit(myunlock(DB_lock))

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)
library(pander)


## CHP-1 exclusions ------------------------------------------------------------

ranges_CHP1       <- read.table(CHP1_EXCLUDE,
                                sep          = ";",
                                colClasses   = "character",
                                strip.white  = TRUE,
                                header       = TRUE,
                                comment.char = "#" )
ranges_CHP1$From  <- as.POSIXct(strptime(ranges_CHP1$From,  format = "%F %H:%M", tz = "UTC"))
ranges_CHP1$Until <- as.POSIXct(strptime(ranges_CHP1$Until, format = "%F %H:%M", tz = "UTC"))

## check negative ranges
if (!all((ranges_CHP1$Until - ranges_CHP1$From) >= 1)) {
    pander(ranges_CHP1[ !ranges_CHP1$From < ranges_CHP1$Until, ])
    stop("Inverted ranges in ", CHP1_EXCLUDE, "!!!")
}
## capitalize
ranges_CHP1$Comment <- sub("(.)", "\\U\\1", ranges_CHP1$Comment, perl = TRUE)
ranges_CHP1$Comment[ranges_CHP1$Comment == ""] <- "NO DESCRIPTION"
## compute time span
ranges_CHP1$HourSpan <- as.numeric(ranges_CHP1$Until - ranges_CHP1$From) / 3600


#'
#' Check time ranges span in hours
#'
#+ include=T, echo=F

hist(ranges_CHP1$HourSpan)
cat('\n\n')

temp <- ranges_CHP1[ ranges_CHP1$HourSpan > 12 , ]
row.names(temp) <- NULL
pander( temp )

cat('\n\n')
pander(data.table(table(ranges_CHP1$Comment)))
cat('\n\n')





## todo
## - create a factor column
## - fill column with data
## - try to fill by year / month?
## - if works move column creation elsewhere


BB <- arrow::open_dataset(DB_DIR,
                          unify_schemas = TRUE,
                          hive_style    = FALSE,
                          partitioning  = c("year", "month"))




## create new column
var <- "chp1_bad_data"
if (any(names(BB) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.character(BB_meta[[var]])
}

BB[["Ff"]] <- "dd"

BB$create("dd")


yearstodo <- unique(year(c(ranges_CHP1$From, ranges_CHP1$Until)))













tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
