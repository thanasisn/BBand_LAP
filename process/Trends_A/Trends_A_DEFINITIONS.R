#'
#' Variables for long term trends of Broad Band project
#'

DB_BROAD  <- "~/DATA/Broad_Band/Broad_Band_LAP.duckdb"
Main.Host <- "sagan"        ## This is the main host used to build the DB

LAST_DAY  <- as.POSIXct("2025-01-01 00:00:00")
FIRST_DAY <- as.POSIXct("1900-01-01 00:00:00")

FIBais_Az_1               <-  58
FIBais_Az_2               <- 120
FIBais_Elev               <-  12
MIN_ELEVA                 <-   5  ##  global low elevation limit
All_daily_ratio_lim       <- 0.5  ##  ration of daily valid data

## Ratio of characterizations to set as daily characterization
Clear_daily_ratio_lim     <- 0.6  ## keep both the same!!
Cloud_daily_ratio_lim     <- 0.6




#### dictionary ####
dictionary <-
  list(DIR_trnd_A            = "DNI",
       HOR_trnd_A            = "Dir. Irrad. horizontal plane",
       DIFF_trnd_A           = "Diffuse. Irrad.",
       DIFF_trnd_A_mean_anom = "Diffuse. Irrad. Anomaly",
       GLB_trnd_A            = "GHI",
       tsi1au_att          = "TSI at 1au",
       near_tcc_des        = "TCCn deseasonalized",
       near_tcc_att        = "TCC nearest",
       bilin_tcc_att       = "TCC bilinear fit",
       near_tcc_zero_N     = "TCCn zero counts",
       near_tcc_TN         = "TCCn total counts",
       near_tcc_NOzero_att = "TCCn without zeros",
       near_tcc_clear_att  = "TCCn almost completely clear",
       near_tcc_cloud_att  = "TCCn complementary to almost completely clear",
       near_tcc_zero_rel   = "TCCn zeros relative to total",
       ALL                 = "All sky cond.",
       CLEAR               = "Clear sky cond.",
       CLOUD               = "Cloudy sky cond.")

## Function to translate objects names
var_name <- function(x) {
  res <- c()
  for (ax in x) {
    ## get match
    amatch <- as.vector(unlist(
      dictionary[stringr::str_detect(ax, names(dictionary))]
    ))
    ## return same if not found
    if (is.null(amatch)) {
      amatch <- ax
    }
    ## gather results
    res <- c(res, amatch)
  }
  return(res)
}

var_name("DIFF_trnd_A_mean_anom")

dictionary[agrep("DIFF_trnd_mean_anom", (names(dictionary)))]
dictionary[agrep("DIFF_trnd_A_mean_anom", (names(dictionary)))]
dictionary[agrep("DIFF_trnd_A_anom", (names(dictionary)))]



coldict <- list(
  DIR_trnd   = "#2166ac",
  HOR_trnd   = "#4244ac",
  DIFF_trnd  = "#9970ab",
  GLB_trnd   = "#1a9850"
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
