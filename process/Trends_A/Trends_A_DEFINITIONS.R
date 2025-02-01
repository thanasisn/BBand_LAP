#'
#' Variables for long term trends of Broad Band project
#'

DB_BROAD  <- "~/DATA/Broad_Band/Broad_Band_LAP.duckdb"

LAST_DAY  <- as.POSIXct("2025-01-01 00:00:00")
FIRST_DAY <- as.POSIXct("1900-01-01 00:00:00")

FIBais_Az_1               <-  58
FIBais_Az_2               <- 120
FIBais_Elev               <-  12
MIN_ELEVA                 <-   5  ##  global low elevation limit
All_daily_ratio_lim       <- 0.5  ##  ration of daily valid data


#### dictionary ####
dictionary <-
  list(DIR_trnd            = 'DNI',
       HOR_trnd            = 'Dir. Irrad. horizontal plane',
       DIFF_trnd           = 'Diffuse. Irrad.',
       GLB_trnd            = "GHI",
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


coldict <- list(
  DIR   = "#2166ac",
  HOR   = "#4244ac",
  DIFF  = "#9970ab",
  GLB   = "#1a9850"
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
      amatch <- ax
    }
    ## gather results
    res <- c(res, amatch)
  }
  return(res)
}

var_name("DIFF_traf_A")
