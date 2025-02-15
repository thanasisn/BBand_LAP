#'
#' Variables for long term trends of Broad Band project
#'

##  DATA SELECTION  ------------------------------------------------------------
DB_BROAD  <- "~/DATA/Broad_Band/Broad_Band_LAP.duckdb"
Main.Host <- "sagan"        ## This is the main host used to build the DB

LAST_DAY  <- as.POSIXct("2025-01-01 00:00:00")
FIRST_DAY <- as.POSIXct("1900-01-01 00:00:00")

FIBais_Az_1               <-  58
FIBais_Az_2               <- 120
FIBais_Elev               <-  12
MIN_ELEVA                 <-   5  ##  global low elevation limit
All_daily_ratio_lim       <- 0.5  ##  ration of daily valid data
Monthly_aggegation_N_lim  <-  20  ##  Number of days per month for valid monthly value

## Ratio of characterizations to set as daily characterization
Clear_daily_ratio_lim     <- 0.6  ## keep both the same!!
Cloud_daily_ratio_lim     <- 0.6


##  DATA ANALYSIS  -------------------------------------------------------------
running_mean_window_days   <- 120
running_mean_window_months <-   4
running_mean_window_years  <-   3





##  DATA DISPLAY AND PLOT  -----------------------------------------------------
var_name <- function(type) {
  switch(type,
         DIR_trnd_A                 = "DNI",
         DIR_trnd_A_mean            = "DNI",
         DIR_trnd_A_mean_mean       = "DNI",
         DIR_trnd_A_mean_anom       = "DNI Anomaly",
         DIR_trnd_A_mean_mean_anom  = "DNI Anomaly",
         HOR_trnd_A                 = "Dir. Irrad. horizontal plane",
         HOR_trnd_A_mean            = "Dir. Irrad. horizontal plane",
         HOR_trnd_A_mean_mean       = "Dir. Irrad. horizontal plane",
         HOR_trnd_A_mean_anom       = "Dir. Irrad. horizontal plane Anomaly",
         HOR_trnd_A_mean_mean_anom  = "Dir. Irrad. horizontal plane Anomaly",
         DIFF_trnd_A                = "Diffuse. Irrad.",
         DIFF_trnd_A_mean           = "Diffuse. Irrad.",
         DIFF_trnd_A_mean_mean      = "Diffuse. Irrad.",
         DIFF_trnd_A_mean_anom      = "Diffuse. Irrad. Anomaly",
         DIFF_trnd_A_mean_mean_anom = "Diffuse. Irrad. Anomaly",
         GLB_trnd_A                 = "GHI",
         GLB_trnd_A_mean            = "GHI",
         GLB_trnd_A_mean_mean       = "GHI",
         GLB_trnd_A_mean_anom       = "GHI Anomaly",
         GLB_trnd_A_mean_mean_anom  = "GHI Anomaly",
         tsi1au_att                 = "TSI at 1au",
         near_tcc_des               = "TCCn deseasonalized",
         near_tcc_att               = "TCC nearest",
         bilin_tcc_att              = "TCC bilinear fit",
         near_tcc_zero_N            = "TCCn zero counts",
         near_tcc_TN                = "TCCn total counts",
         near_tcc_NOzero_att        = "TCCn without zeros",
         near_tcc_clear_att         = "TCCn almost completely clear",
         near_tcc_cloud_att         = "TCCn complementary to almost completely clear",
         near_tcc_zero_rel          = "TCCn zeros relative to total",
         ALL                        =    "All sky cond.",
         CLEAR                      =  "Clear sky cond.",
         CLOUD                      = "Cloudy sky cond.",
         Trend_A_DAILY_ALL          =    "All sky cond. daily",
         Trend_A_DAILY_CLEAR        =  "Clear sky cond. daily",
         Trend_A_DAILY_CLOUD        = "Cloudy sky cond. daily",
         Trend_A_MONTHLY_ALL        =    "All sky cond. monthly",
         Trend_A_MONTHLY_CLEAR      =  "Clear sky cond. monthly",
         Trend_A_MONTHLY_CLOUD      = "Cloudy sky cond. monthly",
         type)
}


coldict <- list(
  DIR_trnd   = "#2166ac",
  DIR_strict = "blue",
  HOR_trnd   = "#4244ac",
  DIFF_trnd  = "#9970ab",
  GLB_trnd   = "#1a9850",
  GLB_strict = "green"
)

var_col <- function(x) {
  res <- c()
  for (ax in x) {
    ## get match
    amatch <- as.vector(unlist(
      coldict[stringr::str_detect(ax, names(coldict))]
    ))
    ## return same if not found
    if (is.null(amatch)) {
      amatch <- "#000000"
    }
    ## gather results
    res <- c(res, amatch)
  }
  return(res)
}

