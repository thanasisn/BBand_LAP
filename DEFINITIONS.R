#'
#' Here are some variables for this project to build the raw database.
#'

####    Project root folder    #################################################
BASED      <- "~/BBand_LAP/"


####    Input paths    #########################################################
SUN_FOLDER   <- "~/DATA_RAW/SUN/PySolar_LAP/"
SIRENA_GLB   <- "~/DATA_RAW/Bband/AC21_LAP.GLB/"
SIRENA_DIR   <- "~/DATA_RAW/Bband/CHP1_lap.DIR/"
trSYNC_DIR   <- "~/DATA_RAW/tracker_chp1/tracker_SYNC"
CHPTMP_DIR   <- "~/DATA_RAW/tracker_chp1/Tracker_THERM"
SIRENA_TOT   <- "~/DATA/cm21_data_validation/AC21_lap.GLB_TOT"
RADMON_DIR   <- "~/DATA_RAW/Raddata/3"
RADMON_GLB   <- "~/DATA_RAW/Raddata/6"


CM21_EXCLUDE <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CM21.dat"
CHP1_EXCLUDE <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CHP1.dat"
CHP1_TEMP_EX <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CHP1_Temp.dat"



####    DB specifications    ###################################################
DB_DIR        <- "~/DATA/Broad_Band/Broad_Band_DB"
DB_lock       <- "~/DATA/Broad_Band/Broad_Band_DB.stopfile"
DB_META_fl    <- "~/DATA/Broad_Band/Broad_Band_DB_metadata.parquet"
DB_HASH_fl    <- "~/DATA/Broad_Band/Broad_Band_DB_hash_table.paquet"
DB_start_date <- as.Date("1993-01-01")   ## <- start in 1993-04-19??
DB_start_date <- as.Date("2016-01-01")



####    Filters for CHP 1 temperatures    ######################################
CHP_TEMP_MIN      <- -25    ## Drop temperatures below this value
CHP_TEMP_MAX      <-  60    ## Drop temperatures above this value
CHP_TEMP_STD_LIM  <-   2    ## Drop temperatures with standard deviation above this value
CHP_TEMP_UNC_LIM  <-   0.8  ## Drop temperatures with uncertainty above this value

