#'
#' Variables for the Broad Band project
#'

BASED      <- "~/BBand_LAP/" ## Project root folder
Main.Host  <- "sagan"        ## This is the main host used to build the DB

##  Input paths  ---------------------------------------------------------------
SUN_FOLDER   <- "~/DATA_RAW/SUN/PySolar_LAP/"
SIRENA_DIR   <- "~/DATA_RAW/Bband/CHP1_lap.DIR/"
SIRENA_GLB   <- "~/DATA_RAW/Bband/AC21_LAP.GLB/"
SIRENA_INC   <- "~/DATA_RAW/Bband/CM21_LAP.INC/"
SIRENA_EKO   <- "~/DATA_RAW/Bband/EKO_LAP.GLB/"
trSYNC_DIR   <- "~/DATA_RAW/tracker_chp1/tracker_SYNC/"
trSTEP_DIR   <- "~/DATA_RAW/tracker_chp1/Tracker_STEP/"
CHPTMP_DIR   <- "~/DATA_RAW/tracker_chp1/Tracker_THERM/"
SIRENA_TOT   <- "~/DATA/cm21_data_validation/AC21_lap.GLB_TOT/"
RADMON_DIR   <- "~/DATA_RAW/Raddata/3/"
RADMON_GLB   <- "~/DATA_RAW/Raddata/6/"
RADMON_INC   <- "~/DATA_RAW/Raddata/1/"
RADMON_PIR   <- "~/DATA_RAW/Raddata/2/"


##  Other data input  ----------------------------------------------------------
COMP_TSI     <- "~/DATA/SUN/TSI_COMPOSITE.Rds"
COMP_PRES    <- "~/DATA/WEATHER/Pressure_M1.Rds"


##  Parameters files  ----------------------------------------------------------
CM21_EXCLUDE <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CM21.dat"
CHP1_EXCLUDE <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CHP1.dat"
CHP1_TEMP_EX <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CHP1_Temp.dat"


##  DB specifications  ---------------------------------------------------------
# DB_LAP              <- "~/DATA_RAW/LAP/LAP_parameters.duckdb"
DB_LAP              <- "~/DATA_RAW/SUN/LAP_SUN.duckdb"
DB_BROAD             <- "~/DATA/Broad_Band/Broad_Band_LAP.duckdb"
DB_TSI              <- "~/DATA/SUN/TSI.duckdb"
DB_PRESSURE         <- "~/DATA/WEATHER/Pressure.duckdb"
DB_TRACKER          <- "~/DATA/Broad_Band/CHP1_tracker.duckdb"
DB_DIR              <- "~/DATA/Broad_Band/Broad_Band_DB"
DB_META_fl          <- "~/DATA/Broad_Band/Broad_Band_DB_metadata.parquet"
DB_Steps_DIR        <- "~/DATA/Broad_Band/CHP1_Tracker_steps_DB/"
DB_Steps_META_fl    <- "~/DATA/Broad_Band/CHP1_Tracker_steps_DB_metadata.parquet"
DB_Steps_lock       <- "~/DATA/Broad_Band/CHP1_Tracker_steps_DB.stopfile"
DB_lock             <- "~/DATA/Broad_Band/Broad_Band_DB.stopfile"
DB_HASH_fl          <- "~/DATA_RAW/Bband/Broad_Band_DB_hash_table.parquet"  ## Always append to this file
DB_start_date       <- as.Date("1993-01-01")   ## ~ start in 1993-04-19??
DB_Steps_start_date <- as.Date("2016-04-01")
# DB_compress_codec   <- "lz4"  ## available in minimal arrow
# DB_compress_level   <- 9
DB_compress_codec   <- "brotli"
DB_compress_level   <- 5

## __ DB test paths  -----------------------------------------------------------
test_DB_DIR     <- "~/ZHOST/Broad_Band_DB"
test_DB_lock    <- "~/ZHOST/Broad_Band_DB.stopfile"
test_DB_META_fl <- "~/ZHOST/Broad_Band_DB_metadata.parquet"
test_DB_HASH_fl <- "~/ZHOST/Broad_Band_DB_hash_table.paquet"



##  CHP-1 variables  -----------------------------------------------------------
CHP1_TEMP_MIN      <- -20    ## Drop temperatures below this value
CHP1_TEMP_MAX      <-  60    ## Drop temperatures above this value
CHP1_TEMP_STD_LIM  <-   5    ## Drop temperatures with standard deviation above this value
CHP1_TEMP_UNC_LIM  <-   0.8  ## Drop temperatures with uncertainty above this value
CHP1_MINnightLIM   <-  -1    ## Lower  radiation limit when dark (R20) -> ToolowDark
CHP1_MAXnightLIM   <-  +1    ## Higher radiation limit when dark (R20) -> ToohigDark
CHP1_MAXSDnightLIM <-  +1     ## Radiation SD limit when dark


##  CM-21 variables  -----------------------------------------------------------
CM21_MINnightLIM   <- -14    ## Lower  radiation limit when dark (R20) -> ToolowDark
CM21_MAXnightLIM   <- +14    ## Higher radiation limit when dark (R20) -> ToohigDark
CM21_MAXSDnightLIM <- +10    ## Higher radiation limit when dark (R20) -> ToohigDark



##  Dark Calculations and definitions  -----------------------------------------

## Extend of dark signal for morning and evening of the same day
DSTRETCH    <- 3 * 3600
## Number of valid measurements/minutes to compute dark
DCOUNTLIM   <- round((DSTRETCH/60) * 0.20, 0)
## Start dark computation when Sun is bellow elevation
DARK_ELEV   <- -10

## Drop ALL radiation data when sun is below this point create "strict" data
Sun_elev_MIN <- -2 * 0.103


##  QCRad Long Shi options  ----------------------------------------------------
QCrad_plot_date_min <- as.Date("1992-01-01")
QCrad_plot_date_min <- as.Date("2024-12-31")
QCrad_plot_date_max <- as.Date("2026-01-01")



##  TSI  -----------------------------------------------------------------------

COMP_TSI_legacy <- "~/DATA/SUN/TSI_COMPOSITE_legacy.Rds"

## __ NOAA TSI  ----------------------------------------------------------------
##  https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00828
##  Cite as: Odele Coddington, Judith L. Lean, Doug Lindholm, Peter Pilewskie, Martin Snow, and NOAA CDR Program (2015): NOAA Climate Data Record (CDR) of Total Solar Irradiance (TSI), NRLTSI Version 2. [indicate subset used]. NOAA National Centers for Environmental Information. doi:10.7289/V55B00C1 [access date].
FROM_NOAA <- "https://www.ncei.noaa.gov/data/total-solar-irradiance/access/daily/"
DEST_NOAA <- "~/DATA/SUN/TSI_model_NOAA/"

## __ TSIS TSI from LISIRD  ----------------------------------------------------
##  https://lasp.colorado.edu/lisird/data/tsis_tsi_6hr
##
FROM_TSIS <- "https://lasp.colorado.edu/lisird/latis/dap/tsis_tsi_6hr.csv?&format_time(yyyy-MM-dd'T'HH:mm:ss.SSSZ)"
DEST_TSIS <- "~/DATA/SUN/TSI_tsis_6hr.csv"
DATA_TSIS <- "~/DATA/SUN/TSI_tsis_6hr.Rds"


## __ SORCE TSI from LISIRD  ---------------------------------------------------
FROM_SORCE <- "https://lasp.colorado.edu/lisird/latis/dap/sorce_tsi_6hr_l3.csv?&format_time(yyyy-MM-dd'T'HH:mm:ss.SSSZ)"
DEST_SORCE <- "~/DATA/SUN/TSI_sorce_6hr.csv"
DATA_SORCE <- "~/DATA/SUN/TSI_sorce_6hr.Rds"


## __ CMIC  --------------------------------------------------------------------
CMIC_DIR <- "/home/folder/EUMETSAT/CMIC_output_LAP/"

