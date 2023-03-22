#'
#' Here are some variables for this project to build the raw database.
#'

####    Project root folder    #################################################
BASED      <- "~/BBand_LAP/"


####    Input paths    #########################################################
SUN_FOLDER <- "~/DATA_RAW/SUN/PySolar_LAP/"
SIRENA_GLB <- "~/DATA_RAW/Bband/AC21_LAP.GLB/"
SIRENA_DIR <- "~/DATA_RAW/Bband/CHP1_lap.DIR/"
trSYNC_DIR <- "~/DATA_RAW/tracker_chp1/tracker_SYNC"
CHPTMP_DIR <- "~/DATA_RAW/tracker_chp1/Tracker_THERM"
SIRENA_TOT <- "~/DATA/cm21_data_validation/AC21_lap.GLB_TOT"


CM21_EXCLUDE <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CM21.dat"
CHP1_EXCLUDE <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CHP1.dat"
CHP1_TEMP_EX <- "~/Aerosols/source_R/PARAMS/Skip_ranges_CHP1_Temp.dat"

RADMON_DIR <- "~/DATA_RAW/Raddata/3"


####    DB specifications    ###################################################
DB_DIR        <- "~/ZHOST/Broad_Band_BD"
DB_lock       <- "~/ZHOST/Broad_Band_BD.stopfile"
DB_META_fl    <- "~/ZHOST/Broad_Band_metadata.parquet"
DB_start_date <- as.Date("1993-01-01")   ## <- start in 1993-01-19!!!!!
DB_start_date <- as.Date("2020-01-01")





