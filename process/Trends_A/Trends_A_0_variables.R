
## Variables for project


#### dictionary ####
dict <- list(DIR_att    = 'Dir. Beam Irrad.',
             HOR_att    = 'Dir. Irrad. horizontal plane',
             DIR_transp = 'Transp. for Dir. Irrad.',
             wattGLB    = "Global Radiation raw",
             GLB_att    = "SDR",
             tsi1au_att = "TSI at 1au",
             ALL        = "All sky cond.",
             CLEAR      = "Clear sky cond.",
             CLOUD      = "Cloudy sky cond.")
## function to translate objects names
# translate <- function(...) as.vector(unlist(dict[c(...) == names(dict)]))

translate <- function(x) {
    res <- c()
    for (ax in x) {
        res <- c(res,
                 as.vector(unlist(
                     dict[stringr::str_detect(ax, names(dict))]
                 ))
        )
    }
    return(res)
}




#### Data range ####

#### Paths ####
tag                      <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))
variables_fl             <- "./DHI_GHI_0_variables.R"
data_procsess_fl         <- "./DHI_GHI_0_data_input.R"

#### colors ####
col_DIR_att              <- "#2166ac"
col_HOR_att              <- "#4244ac"
col_DIR_transp           <- "#9970ab"
col_GLB_att              <- "#1a9850"
col_tsi1au_att           <- "#e3e300"


#### parameters ####

## https://www.rapidtables.com/calc/time/days-in-year.html
# Days_of_year             <- 365.25   ## Mean Julian year
Days_of_year              <- 365.2425 ## Mean Gregorian calendar year
pch_am                    <-   1
pch_pm                    <-   2
pch_ampm                  <-  13 ## try 10
pch_daily                 <-  19
running_mean_window_years <-   5
running_mean_window_days  <- running_mean_window_years * Days_of_year

MIN_ELEVA                 <-   5  ##  global low elevation limit
SZA_BIN                   <-   1
MIN_N                     <-   4
SEAS_MIN_N                <-   3

Daily_confidence_limit    <-   0.99
SZA_confidence_limit      <-   0.99
Monthly_confidence_limit  <-   0.99

# Daily_aggregation_N_lim   <-  60 * 3 # minutes in a day
Daily_aggregation_N_lim   <-   0        # replaced with relative daylight filter
Monthly_aggegation_N_lim  <-  20
SZA_aggregation_N_lim     <-   4

All_daily_ratio_lim       <- 0.5
Clear_daily_ratio_lim     <- 0.6 ## keep both the same!!
Cloud_daily_ratio_lim     <- 0.6




