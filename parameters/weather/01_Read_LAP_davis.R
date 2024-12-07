#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:  "Pressure data combination from multiple source."
#' author: "Natsis Athanasios"
#' ---
#'
#' This read and export direct ITHESSAL2 data to RDS data files.
#' Should be run manual when new data become available
#'
#' BE-CAREFUL there are some erroneous records. there are cases that
#' the measurement is stack at one constant value for long periods.
#' This is corrected with omitting multiple consecutive values above
#' a number of occurrences.
#'
#+ include=F

## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = "!h"    )
knitr::opts_chunk$set(tidy = TRUE,
                      tidy.opts = list(
                        indent       = 4,
                        blank        = FALSE,
                        comment      = FALSE,
                        args.newline = TRUE,
                        arrow        = TRUE)
                      )


## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/weather/01_Read_LAP_davis.R"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/CODE/R_myRtools/myRtools/R/physics_conv.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")
source("~/CODE/FUNCTIONS/R/data.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)

## stop on warnings to fix bad input files
options(warn = 2)

## INPUTS
data_dir         <- "~/DATA_RAW/LAPWeath/LAP_roof/LAP_AUTH_davis.csv"

## OUTPUTS
direct_data      <- "~/DATA/WEATHER/lap"
pdf_base         <- "~/BBand_LAP/REPORTS/REPORTS/"

#+ include=T, echo=T, results="asis"
cat("\n# Parce data from LAP Davis\n\n")

## FILTERS
PRESSURE_LOW_LIM = 950
TEMPERAT_HIG_LIM = 100
RHUMIDIT_HIG_LIM = 100

## DATA FLOW FREEZE LIMITS
TEMPERAT_FRE_LIM = 11
PRESSURE_FRE_LIM = 40
RHUMIDIT_FRE_LIM = 40

## read and drop empty data
data <- fread(data_dir)
data <- rm.cols.NA.DT(data)

## This is a strange error, no solar sensor exist
data[, maxSolarRad := NULL ]
data[, radiation   := NULL ]

## Proper dates
data[, Date := as.POSIXct(dateTime, origin = "1970-01-01" )]
data[, dateTime := NULL]

## check for variables
stopifnot(all(data$usUnits == 1 ))

## prepare temperature
data[, outTemp := fahrenheit_to_celsius(outTemp) ]
hist(data$outTemp, breaks = 100)
data$outTemp[ data$outTemp >  TEMPERAT_HIG_LIM ] <- NA
names(data)[names(data) == "outTemp"] <- "temperature"
hist(data$temperature, breaks = 100)

## prepare pressure
data[, pressure := InHg_to_mbar( pressure ) ]
hist(data$pressure, breaks = 100)
data$pressure[ data$pressure   <  PRESSURE_LOW_LIM ] <- NA
hist(data$pressure, breaks = 100)

## prepare humidity
hist( data$outHumidity,breaks = 100)
names(data)[names(data) == "outHumidity"] <- "humidity"

## Export data as is
data <- data[order(data$Date), ]
write_RDS(data, direct_data)

## plot params
par(mar = c(2, 3, 1.25, .5))
years_vec <- format(data$Date, "%Y")
years     <- unique(years_vec)

## ignore constants from plots
data <- rm.cols.dups.DT(data)

## plot all data

#+ results="asis", echo=F
for (col in grep(pattern = "Date", x =  colnames( data ), invert = T, value = T )) {
  ## go through years
  for (yy in years) {
    subdata = data[ years_vec == yy, ]
    if (all(is.na(subdata[[col]]))) next
    plot(  subdata$Date, subdata[[col]], type = 'l' )
    title( main = paste(yy,col) )
  }
}

#+ results="asis", echo=F
for (col in grep(pattern = "Date", x =  colnames( data ), invert = T, value = T )) {
  ## go through years
  for (yy in years) {
    subdata = data[ years_vec == yy, ]
    if (all(is.na(subdata[[col]]))) next
    hist(  subdata[[col]], breaks = 50, main = paste(yy,col) )
  }
}



#+ results="asis", echo=FALSE
tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
