#'
#' Variables for long term trends of Broad Band project
#'

DB_DUCK   <- "~/DATA/Broad_Band/Broad_Band_LAP.duckdb"

LAST_DAY  <- as.POSIXct("2025-01-01 00:00:00")
FIRST_DAY <- as.POSIXct("1900-01-01 00:00:00")

FIBais_Az_1               <-  58
FIBais_Az_2               <- 120
FIBais_Elev               <-  12
MIN_ELEVA                 <-   5  ##  global low elevation limit
All_daily_ratio_lim       <- 0.5  ##  ration of dayly valid data
