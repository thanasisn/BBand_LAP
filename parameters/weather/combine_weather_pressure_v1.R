#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:       "Pressure data combination from multiple source."
#' author:      "Natsis Athanasios"
#' ---
#'
#'  Create a composite of pressure data
#'
#+ include=F

## _ Document options  ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"  )
knitr::opts_chunk$set(fig.align  = "center")
knitr::opts_chunk$set(fig.pos   = "!ht"   )
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
Script.Name <- "~/BBand_LAP/parameters/weather/combine_weather_pressure_v1.R"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

source("~/CODE/R_myRtools/myRtools/R/write_.R")
source("~/Aerosols/RAerosols/R/statistics.R")

library(caTools)
library(zoo)
library(pander)
library(data.table)
library(janitor)

panderOptions("table.style", 'rmarkdown')
panderOptions("table.split.table", 100 )
panderOptions("table.continues", '')
panderOptions("graph.fontsize", 9)
panderOptions("table.alignment.default", "left")

pressure_limit_low  <- 975
gap_reduction_limit <- 3600 * 4  # if greater than this use ITHESS data to fill DAVIS [seconds]

#' ## Work on original data ##
#+ include=F

#' ### Load data from each weather data source ###
# Upper date limit used to subset data. We start when CHP1 becomes operational.

startdt     <- as.POSIXct("2016-04-01")

## INPUTS
davisroof_f <- "/home/athan/DATA/WEATHER/Eyryma.Rds"
DIithess2_f <- "/home/athan/DATA/WEATHER/iama.Rds"
DIlap_f     <- "/home/athan/DATA/WEATHER/lap.Rds"
WUithess2_f <- "/home/athan/DATA/Wunderground/ITHESSAL2.Rds"

## OUTPUTS
tempe_f     <- "/home/athan/DATA/WEATHER/Pressure_all_sources"
tempe_f_M1  <- "/home/athan/DATA/WEATHER/Pressure_M1"

#+ include=F
## load all data
DAVI <- data.table(readRDS(davisroof_f))
ITHE <- data.table(readRDS(WUithess2_f))
DITH <- data.table(readRDS(DIithess2_f))
DILA <- data.table(readRDS(DIlap_f))

# #' #### Keep only data after this date
# DAVI <- DAVI[ DAVI$Date    > startdt, ]
# ITHE <- ITHE[ ITHE$DateUTC > startdt, ]
# DITH <- DITH[ DITH$Date    > startdt, ]
# DILA <- DILA[ DILA$Date    > startdt, ]

#+ include=F
## drop some columns
ITHE <- subset(ITHE, select = c( -WindDirection) )
DAVI <- subset(DAVI, select = c( -WindDir, -THSWIndex, -HiDir))

## rename
names(ITHE)[names(ITHE) == "DateUTC"] <- "Date"

## drop na columns
DAVI <- remove_empty(DAVI, "cols")
ITHE <- remove_empty(ITHE, "cols")
DITH <- remove_empty(DITH, "cols")
DILA <- remove_empty(DILA, "cols")

## order data by date just to be sure
ITHE <- ITHE[order(ITHE$Date), ]
DAVI <- DAVI[order(DAVI$Date), ]
DITH <- DITH[order(DITH$Date), ]
DILA <- DILA[order(DILA$Date), ]

hist(ITHE$PressuremB, breaks = 100)
hist(DAVI$Bar,        breaks = 100)
hist(DITH$barometer,  breaks = 100)
hist(DILA$pressure,   breaks = 100)

## remove extreme values
ITHE <- ITHE[PressuremB > pressure_limit_low]
DAVI <- DAVI[Bar        > pressure_limit_low]
DITH <- DITH[barometer  > pressure_limit_low]
DILA <- DILA[pressure   > pressure_limit_low]

## Drop empty
ITHE <- ITHE[!is.na(PressuremB)]
DAVI <- DAVI[!is.na(Bar       )]
DITH <- DITH[!is.na(barometer )]
DILA <- DILA[!is.na(pressure  )]

hist(ITHE$PressuremB, breaks = 100)
hist(DAVI$Bar,        breaks = 100)
hist(DITH$barometer,  breaks = 100)
hist(DILA$pressure,   breaks = 100)


#' ### Estimate temporal resolution of each data set

#' #### Davis on roof data
#'
#' Maximum time step `r max(diff(DAVI$Date))`
#'
#' Minimum time step `r min(diff(DAVI$Date))`
#'
#' Davis **Time resolution:** `r median(diff(DAVI$Date))`

#' #### IThessal2 at "Kamara" data
#'
#' Maximum time step `r max(diff(ITHE$Date))`
#'
#' Minimum time step `r min(diff(ITHE$Date))`
#'
#' IThessal2 **Time resolution:** `r median(diff(ITHE$Date))`

#' #### IThessal2 at "Kamara" Direct data
#'
#' Maximum time step `r max(diff(DITH$Date))`
#'
#' Minimum time step `r min(diff(DITH$Date))`
#'
#' IThessal2 **Time resolution:** `r median(diff(DITH$Date))`

#' #### IThessal16 at "Roof" Direct data
#'
#' Maximum time step `r max(diff(DILA$Date))`
#'
#' Minimum time step `r min(diff(DILA$Date))`
#'
#' IThessal2 **Time resolution:** `r median(diff(DILA$Date))`

#' ### Summary of DAVI
#'
#+ include=T, echo=F
pander(summary(DAVI))

#' ### Summary of ITHE
#'
#+ include=T, echo=F
pander(summary(ITHE))

#' ### Summary of DITH
#'
#+ include=T, echo=F
pander(summary(DITH))

#' ### Summary of DILA
#'
#+ include=T, echo=F
pander(summary(DILA))

## compare sources of ITHESSAL
#'
#' ### We have two sources of ITHESSAL data to work with.
#'
#' We will clean the data "by hand" using a list of exclusions after inspection.
#'
#+ include=T, echo=F

par(mar = c(2,4,.5,.5))
yrange <- range(DITH$barometer,
                ITHE$PressuremB,
                DAVI$Bar,
                DILA$pressure, na.rm = T)
xrange <- range(DITH$Date,
                ITHE$Date,
                DAVI$Date,
                DILA$Date )

plot(  DITH$Date, DITH$barometer,  pch = 19, cex = .2, col = "blue",
       ylab = "ITHESS Pressure", ylim = yrange, xlim = xrange)
points(ITHE$Date, ITHE$PressuremB, pch = 19, cex = .2, col = "red")

plot( DITH$Date, DITH$barometer,  type = 'l', col = "blue" , ylim = yrange, xlim = xrange)
lines(ITHE$Date, ITHE$PressuremB, type = 'l', col = "green")
lines(DAVI$Date, DAVI$Bar,        type = 'l', col = "red"  )
lines(DILA$Date, DILA$pressure,   type = 'l', col = "cyan" )

#'
#' ### Remove DAVIS data when it seems that the measurement is stacked.
#'
#' Will find cases of the save value to be repeated for consecutive measurements.
#' We will remove this data as problematic. The limit is set to:
consecutive_limit = 15
#+ include=F, echo=F

sum(is.na(DAVI$Bar))
problems <- find_freezed_measurements(DAVI$Bar, consecutive_limit )
hist(problems$lengths)
## Inspect problems
# y  = DAVI$Bar[  problems$starts ]
# x1 = DAVI$Date[ problems$starts ]
# x2 = DAVI$Date[ problems$ends ]
# plot( range(x1,x2), range(y),type="n")
# segments( x1, y, x2, y )

#'
#' We found `r length(problems$starts)`
#' cases with more than `r consecutive_limit`
#' consecutive values
#'

for (i in 1:length(problems$starts)) {
    ## show data span of the problem
    # print(data$Date[problems$ends[i]]-data$Date[problems$starts[i]])

    ## keep the first and drop all other data
    # print(data$temperature[ problems$starts[i]     :problems$ends[i] ])
    DAVI$Bar[(problems$starts[i] + 1):problems$ends[i] ] <- NA
    # print(data$temperature[ problems$starts[i]     :problems$ends[i] ])
}
sum(is.na(DAVI$Bar))

#'
#' ### Remove ITHESS data when it seems that the measures is stacked.
#'
#' Will find cases of the save value to be repeated for consecutive measurements.
#' We will remove this data as problematic. The limit is set to:
consecutive_limit = 15
#+ include=F, echo=F

sum(is.na(ITHE$PressuremB))
problems <- find_freezed_measurements(ITHE$PressuremB, consecutive_limit )
hist(problems$lengths)
## Inspect problems
# y  = DAVI$Bar[  problems$starts ]
# x1 = DAVI$Date[ problems$starts ]
# x2 = DAVI$Date[ problems$ends ]
# plot( range(x1,x2), range(y),type="n")
# segments( x1, y, x2, y )

#' We found `r length(problems$starts)`
#' cases with more than `r consecutive_limit`
#' consecutive values

for (i in 1:length(problems$starts)) {
    ## show data span of the problem
    # print(data$Date[problems$ends[i]]-data$Date[problems$starts[i]])

    ## keep the first and drop all other data
    # print(data$temperature[ problems$starts[i]     :problems$ends[i] ])
    ITHE$PressuremB[ (problems$starts[i] + 1):problems$ends[i] ] <- NA
    # print(data$temperature[ problems$starts[i]     :problems$ends[i] ])
}
sum(is.na(ITHE$PressuremB))


#'
#' ### Remove Direct ITHESS data when it seems that the measurement is stacked
#'
#' Will find cases of the save value to be repeated for consecutive measurements.
#' We will remove this data as problematic. The limit is set to:
consecutive_limit = 15
#+ include=F, echo=F

sum(is.na(DITH$barometer))
problems <- find_freezed_measurements(DITH$barometer, consecutive_limit )
hist(problems$lengths)
## Inspect problems
# y  = DAVI$Bar[  problems$starts ]
# x1 = DAVI$Date[ problems$starts ]
# x2 = DAVI$Date[ problems$ends ]
# plot( range(x1,x2), range(y),type="n")
# segments( x1, y, x2, y )

#' We found `r length(problems$starts)`
#' cases with more than `r consecutive_limit`
#' consecutive values

for (i in 1:length(problems$starts)) {
    ## show data span of the problem
    # print(data$Date[problems$ends[i]]-data$Date[problems$starts[i]])

    ## keep the first and drop all other data
    # print(data$temperature[ problems$starts[i]     :problems$ends[i] ])
    DITH$barometer[ (problems$starts[i] + 1):problems$ends[i] ] <- NA
    # print(data$temperature[ problems$starts[i]     :problems$ends[i] ])
}
sum(is.na(DITH$barometer))

par(mar = c(2,4,.5,.5))
yrange <- range(DITH$barometer,
                ITHE$PressuremB,
                DAVI$Bar,
                DILA$pressure,
                na.rm = T)
xrange <- range(DITH$Date,
                ITHE$Date,
                DAVI$Date,
                DILA$Date)
plot(  DITH$Date, DITH$barometer,  pch = 19, cex = .2, col = "blue", ylab = "ITHESS Pressure",
       ylim = yrange, xlim = xrange )
points(ITHE$Date, ITHE$PressuremB, pch = 19, cex = .2, col = "red" )

plot( DITH$Date, DITH$barometer,  type = 'l', col = "blue" ,
      ylim = yrange, xlim = xrange )
lines(ITHE$Date, ITHE$PressuremB, type = 'l', col = "green" )
lines(DAVI$Date, DAVI$Bar,        type = 'l', col = "red"   )
lines(DILA$Date, DILA$pressure,   type = 'l', col = "cyan"  )

## find and remove some bad data

## will remove some bad points
excludedataithess = as.POSIXct(c(
    "2016-01-25 10:23:00 UTC", "2016-01-25 10:24:00 UTC"
    ))
## drop bad points
DITH <- DITH[!Date %in% excludedataithess]

DITH[!is.na(barometer)]

## will remove some bad points
excludedataithess = as.POSIXct(c(
    "2016-01-25 08:24:00 UTC", "2016-01-25 08:29:00 UTC",
    "2016-01-25 08:34:00 UTC", "2016-01-25 08:39:00 UTC",
    "2016-01-25 08:44:00 UTC", "2016-01-25 08:49:00 UTC",
    "2016-01-25 08:54:00 UTC", "2016-01-25 08:59:00 UTC",
    "2016-01-25 09:04:00 UTC", "2016-01-25 09:09:00 UTC",
    "2016-01-25 09:19:00 UTC"
))
## drop bad points
ITHE <- ITHE[!Date %in% excludedataithess]


par(mar = c(2,4,.5,.5))
yrange <- range(DITH$barometer,
                ITHE$PressuremB,
                DAVI$Bar,
                DILA$pressure,
                na.rm = T)
xrange <- range(DITH$Date,
                ITHE$Date,
                DAVI$Date,
                DILA$Date)
plot(  DITH$Date, DITH$barometer,  pch = 19, cex = .2, col = "blue", ylab = "ITHESS Pressure",
       ylim = yrange, xlim = xrange )
points(ITHE$Date, ITHE$PressuremB, pch = 19, cex = .2, col = "red" )

plot( DITH$Date, DITH$barometer ,  type = 'l', col = "blue" ,
      ylim = yrange, xlim = xrange )
lines(ITHE$Date, ITHE$PressuremB, type = 'l', col = "green" )
lines(DAVI$Date, DAVI$Bar,        type = 'l', col = "red"   )
lines(DILA$Date, DILA$pressure,   type = 'l', col = "cyan"  )

## Create a composite for ITHESS
# Use Davis as basis and add missing data from ITHESS ignore Direct ITHESS for
# now

ITHE_valid_pressur <- ITHE[!is.na(ITHE$PressuremB), c("Date", "PressuremB")]
DAVI_valid_pressur <- DAVI[!is.na(DAVI$Bar),        c("Date", "Bar"       )]
DITH_valid_pressur <- DITH[!is.na(DITH$barometer),  c("Date", "barometer" )]
DILA_valid_pressur <- DILA[!is.na(DILA$pressure) ,  c("Date", "pressure"  )]
rm(ITHE,DAVI,DITH,DILA)

## LAP  davis common
cdl  <- range(DILA_valid_pressur$Date)
true <- mean(DILA_valid_pressur$pressure)
move <- mean(DITH_valid_pressur[ DITH_valid_pressur$Date >= cdl[1] & DITH_valid_pressur$Date <= cdl[2], barometer ])

## correct Direct Iama data by our Davis
DITH_valid_pressur$barometer <- DITH_valid_pressur$barometer - (move - true)
DITH_valid_pressur$Source    <- "iama_corrected"
DITH_valid_pressur           <- unique(DITH_valid_pressur)

## combine the two davis
DILA_valid_pressur$Source <- "LAP_Davis"
DAVI_valid_pressur$Source <- "roof_Davis"
names(DAVI_valid_pressur)[names(DAVI_valid_pressur) == "Bar"] <- "pressure"
DAVI_valid_pressur        <- unique(DAVI_valid_pressur)

comp <- rbind(DILA_valid_pressur, DAVI_valid_pressur)
rm(DILA_valid_pressur, DAVI_valid_pressur)

plot(comp$Date, comp$pressure, col = factor(comp$Source), pch = ".")

base_dates_vec <- comp$Date
fill_dates_vec <- DITH_valid_pressur$Date

range(base_dates_vec)
range(fill_dates_vec)

closeness_date <- function(a_test_date) {
    base_dates_vec <- comp$Date

    ## next     available date
    after <- base_dates_vec[min(which(base_dates_vec >= a_test_date), na.rm = T)]
    ## previous available date
    befor <- base_dates_vec[max(which(base_dates_vec <= a_test_date), na.rm = T)]

    da <- difftime(a_test_date, after, units = "sec")
    db <- difftime(a_test_date, befor, units = "sec")

    nearest <- min(abs(c(da, db)), na.rm = T)
    return(nearest)
}

closeness_date <- function(a_test_date) {
    base_dates_vec <- comp$Date

    ## TODO use data.table
    ## next     available date
    after <- base_dates_vec[which.min(base_dates_vec >= a_test_date)]
    da    <- difftime(a_test_date, after, units = "sec")

    ## previous available date
    befor <- base_dates_vec[which.max(base_dates_vec <= a_test_date)]
    db    <- difftime(a_test_date, befor, units = "sec")

    nearest <- min(abs(c(da, db)), na.rm = T )
    return(nearest)
}

tdifff <- unlist(lapply(DITH_valid_pressur$Date , closeness_date))

yrange <- range(tdifff/3600)
plot(  DITH_valid_pressur$Date[tdifff < gap_reduction_limit],
       tdifff[tdifff < gap_reduction_limit]/3600, ylim = yrange,
       pch = 19, cex = 0.3, col = "green", ylab = "time gap from davis [h]")
points(DITH_valid_pressur$Date[tdifff > gap_reduction_limit],
       tdifff[tdifff > gap_reduction_limit]/3600,
       pch = 19, cex = 0.3, col = "red")
legend("topright", c("uses DAVIS data", "uses ITHESS data"), col = c("green", "red"), pch = 19, bty = "n")

## use data from Direct Iama to fill Davis
append <- DITH_valid_pressur[tdifff > gap_reduction_limit, ]
names(append)[names(append) == "barometer"] <- "pressure"
append <- unique(append)

composite <- rbind(comp, append)
composite <- composite[order(composite$Date), ]

## plot combined data
plot(composite$Date, composite$pressure,
     pch  = ".",
     col  = as.numeric(factor(composite$Source)) + 1 ,
     ylab = "Pressure [mB]")

#'
#' ### Composite Pressure data
#'
#' Maximum time step `r max(diff(composite$Date))`
#' Minimum time step `r min(diff(composite$Date))`
#' IThessal2 **Time resolution:** `r median(diff(composite$Date))` `r mean(diff(composite$Date))`

summary(composite)
if ( !all((as.numeric( composite$Date ) %% 60) == 0) ) {
    stop("Date not multiple of whole minute")
}

##  Export original temperature data without adjustment  -----------------------
write_RDS(composite, tempe_f)


##  Extend to all minutes  -----------------------------------------------------
pressaprx <- approxfun(x      = composite$Date,
                       y      = composite$pressure,
                       method = "linear",
                       rule   = 2,
                       f      = 0,
                       ties   = mean)

## fill all minutes
alldates  <- data.frame(Date = seq(min(composite$Date), max(composite$Date), by = "min"))
composite <- merge(alldates, composite, all = T)
composite <- unique(composite)

tofill <- is.na(composite$pressure)

composite$pressure[tofill] <- pressaprx(composite$Date[tofill])
composite$Source[tofill]   <- "approximation"

## plot combined data
plot(composite$Date, composite$pressure, pch = ".",
     col  = as.numeric(factor(composite$Source)) + 1 ,
     ylab = "Pressure [mB]")

write_RDS(composite, tempe_f_M1)

## TODO Create pressure climatology for sun tracking


#+ results="asis", echo=FALSE
tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
