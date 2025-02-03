# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */

#### Variables of this project.

## date range to export for TOT and WRDC
EXPORT_START <- as.POSIXct("2006-01-01 00:00:00 UTC")
EXPORT_STOP  <- as.POSIXct("2023-01-01 00:00:00 UTC")


####    Input paths    #########################################################
SUN_FOLDER <- "~/DATA_RAW/SUN/PySolar_LAP/"
SIRENA_DIR <- "~/DATA_RAW/Bband/AC21_LAP.GLB/"
RADMON_DIR <- "~/DATA_RAW/Raddata/6"

####    Output paths    ########################################################
DAILYgrDIR <- paste0(BASED, "/REPORTS/DAILY/")
REPORT_DIR <- paste0(BASED, "/REPORTS/")
SIGNAL_DIR <- "~/DATA/Broad_Band/CM21_H_signal/"
GLOBAL_DIR <- "~/DATA/Broad_Band/CM21_H_global/"
EXPORT_DIR <- "~/DATA/Broad_Band/CM21_H_exports/"
TOT_EXPORT <- "~/DATA/cm21_data_validation/AC21_lap.GLB_NEW/"


####    Parametric files    ####################################################

## Date ranges to exclude, after manual inspection
BAD_RANGES  <- paste0(BASED,      "/PARAMS/Skip_ranges_CM21.dat")
## Storage of dark signal details for all days
DARKSTORE   <- paste0(SIGNAL_DIR, "/LAP_CM21_Dark_data_S0.Rds")
## Dark signal details constructed for uncomputable days
DARKCONST   <- paste0(SIGNAL_DIR, "/LAP_CM21_Dark_construction_S0.Rds")
## Logging of missing lap input files
MISSING_INP <- paste0(BASED,      "/PARAMS/Missing_lap_files.dat")

####  Filtering Variables  ####

## break between 2014-02-04 and 2014-02-05
BREAKDATE      <- as.POSIXct("2014-02-05 00:00:00")


## Lower global limit any Global value below this should be erroneous data
GLB_LOW_LIM_01 <- -15      ## before break-date
GLB_LOW_LIM_02 <- -7       ## after break-data

####   Dark Calculations and definitions    ####################################

## Extend of dark signal for morning and evening of the same day
DSTRETCH    <- 3 * 3600
## Number of valid measurements/minutes to compute dark
DCOUNTLIM   <- round((DSTRETCH/60) * 0.20, 0)
## Start dark computation when Sun is bellow elevation
DARK_ELEV   <- -10

MINLIMnight <- -15         ## Lower radiation limit  when dark       (R20) -> ToolowDark
MAXLIMnight <- +15         ## Higher radiation limit when dark       (R20) -> ToohigDark

LOWrangeNight <- 0.3       ## Limit the signal for dark calculation inside the lower % part of physical possible limit


####    Negative radiation when sun is up   ####################################
SUN_ELEV    <- +0         ## When sun is above that           (R50)
MINglbSUNup <- -0.3       ## Exclude signal values below that (R50) above that will set to zero

####    Positive radiation when sun is down   ##################################
SUN_TOO_LOW <- -5         ## When sun is down  (R50)
ERROR_GLOBA <-  5         ## Positive radiation in the night (R50)

####    Drop night time data    ################################################
NIGHT_DROP  <- -5         ## Drop when sun is below that (R60)
