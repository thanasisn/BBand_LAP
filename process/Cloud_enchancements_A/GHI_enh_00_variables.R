
## Project Variables


#### Data range ####
## will not include the last/first day
LAST_DAY                 <- as.POSIXct("2023-12-31 11:59:30") ## set the date of last NOAA TSI data
# FIRST_DAY                <- as.POSIXct("1900-07-01") ## include all available
FIRST_DAY                <- as.POSIXct("1994-01-01") ## Set initial date to whole year


#### Paths ####
CLEARdir                 <- "~/DATA/Broad_Band/CS_id"
tag                      <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))
CS_file_13               <- "./data/Clear_Sky_13.Rds"
CS_file_14               <- "./data/Clear_Sky_14.Rds"
CS_file_14               <- "./data/Clear_Sky_14.Rds"
CS_file_14_2             <- "./data/Clear_Sky_14_2.Rds"
common_data_13           <- "./data/common_data_13.Rda"
common_data_14           <- "./data/common_data_14.Rda"
common_data_14_2         <- "./data/common_data_14_2.Rda"
variables_fl             <- "./GHI_enh_00_variables.R"
raw_input_data           <- "./data/CE_ID_Input.Rds"
Input_data_ID            <- "./data/CE_ID_Input.Rds"
I1_longterm              <- "./data/Input_1_longterm_trends.Rda"
I2_szatrend              <- "./data/Input_2_sza_trends.Rda"
I3_trendsconsist         <- "./data/Input_3_trends_consist.Rda"


#### parameters ####

## https://www.rapidtables.com/calc/time/days-in-year.html
# Days_of_year             <- 365.25   ## Mean Julian year
Days_of_year              <- 365.2425 ## Mean Gregorian calendar year
running_mean_window_years <-   5
running_mean_window_days  <- running_mean_window_years * Days_of_year

MIN_ELEVA                 <-  -10  ##  no global elevation limit
MIN_N                     <-    4
SEAS_MIN_N                <-    3
BIO_ELEVA                 <-   12  ## 11.55479

Daily_confidence_limit    <-   0.99
SZA_confidence_limit      <-   0.99
Monthly_confidence_limit  <-   0.99

# Daily_aggregation_N_lim   <-  60 * 3 # minutes in a day
Daily_aggregation_N_lim   <-   0       # replaced with relative daylight filter
Monthly_aggegation_N_lim  <-  20
SZA_aggregation_N_lim     <-   4

All_daily_ratio_lim       <- 0.3   # Keep days with daylight data available
Clear_daily_ratio_lim     <- 0.5
Cloud_daily_ratio_lim     <- 0.5



## extra skip ranges for this work
SKIP_cm21 <- matrix(
##     From                  Until                  Comment
    c("1996-02-10 00:00:00", "1996-02-29 23:59:00", "Systematic cut off due to gain error",
      NULL),
    byrow = TRUE,
    ncol = 3)

## Format to data frame
SKIP_cm21 <- data.frame(From    = as.POSIXct(  SKIP_cm21[,1]),
                        Until   = as.POSIXct(  SKIP_cm21[,2]),
                        Comment = as.character(SKIP_cm21[,3]))

## __ Variables  -----------------------------------------------------------
ampl               <- 1.05  ## adjusted HAU amplified threshold
SZA_BIN            <- 1


# solar_constant <- 1361.028 ## from NOAA TSI for data
solar_constant <- 1367     ## WRC

