#'
#' Variables for the Broab Band project
#'

####    Project root folder    #################################################
BASED      <- "~/BBand_LAP/"


##  Input paths  ---------------------------------------------------------------
SUN_FOLDER   <- "~/DATA_RAW/SUN/PySolar_LAP/"
SIRENA_GLB   <- "~/DATA_RAW/Bband/AC21_LAP.GLB/"
SIRENA_DIR   <- "~/DATA_RAW/Bband/CHP1_lap.DIR/"
trSYNC_DIR   <- "~/DATA_RAW/tracker_chp1/tracker_SYNC"
CHPTMP_DIR   <- "~/DATA_RAW/tracker_chp1/Tracker_THERM"
SIRENA_TOT   <- "~/DATA/cm21_data_validation/AC21_lap.GLB_TOT"
RADMON_DIR   <- "~/DATA_RAW/Raddata/3"
RADMON_GLB   <- "~/DATA_RAW/Raddata/6"

##  Other data input  ----------------------------------------------------------
COMP_TSI     <- "~/DATA/SUN/TSI_COMPOSITE.Rds"
COMP_PRES    <- "~/DATA/WEATHER/Pressure_M1.Rds"


##  Parameters files  ----------------------------------------------------------
CM21_EXCLUDE <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CM21.dat"
CHP1_EXCLUDE <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CHP1.dat"
CHP1_TEMP_EX <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CHP1_Temp.dat"



##  DB specifications  ---------------------------------------------------------
DB_DIR        <- "~/DATA/Broad_Band/Broad_Band_DB"
DB_lock       <- "~/DATA/Broad_Band/Broad_Band_DB.stopfile"
DB_META_fl    <- "~/DATA/Broad_Band/Broad_Band_DB_metadata.parquet" ## Always append to this file
DB_HASH_fl    <- "~/DATA_RAW/Bband/Broad_Band_DB_hash_table.parquet"
DB_start_date <- as.Date("1993-01-01")   ## ~ start in 1993-04-19??
# DB_start_date <- as.Date("2016-01-01")   ## ~ start of chp1
# DB_start_date <- as.Date("2022-01-01")   ##  For testing
DB_compress_codec <- "lz4"
DB_compress_level <- 8


## __ DB test paths  -----------------------------------------------------------
test_DB_DIR     <- "~/ZHOST/Broad_Band_DB"
test_DB_lock    <- "~/ZHOST/Broad_Band_DB.stopfile"
test_DB_META_fl <- "~/ZHOST/Broad_Band_DB_metadata.parquet"
test_DB_HASH_fl <- "~/ZHOST/Broad_Band_DB_hash_table.paquet"



##  CHP-1 variables  -----------------------------------------------------------
CHP1_TEMP_MIN     <- -20    ## Drop temperatures below this value
CHP1_TEMP_MAX     <-  60    ## Drop temperatures above this value
CHP1_TEMP_STD_LIM <-   5    ## Drop temperatures with standard deviation above this value
CHP1_TEMP_UNC_LIM <-   0.8  ## Drop temperatures with uncertainty above this value
CHP1_MINnightLIM  <- -15    ## Lower  radiation limit when dark   (R20) -> ToolowDark
CHP1_MAXnightLIM  <- +15    ## Higher radiation limit when dark   (R20) -> ToohigDark


##  CM-21 variables  -----------------------------------------------------------
CM21_MINnightLIM  <- -15    ## Lower  radiation limit when dark   (R20) -> ToolowDark
CM21_MAXnightLIM  <- +15    ## Higher radiation limit when dark   (R20) -> ToohigDark



##  Dark Calculations and definitions  -----------------------------------------

## Extend of dark signal for morning and evening of the same day
DSTRETCH    <- 3 * 3600
## Number of valid measurements/minutes to compute dark
DCOUNTLIM   <- round((DSTRETCH/60) * 0.20, 0)
## Start dark computation when Sun is bellow elevation
DARK_ELEV   <- -10




