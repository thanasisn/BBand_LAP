
#### Paths ####
tag                      <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))

## https://www.rapidtables.com/calc/time/days-in-year.html
# Days_of_year             <- 365.25   ## Mean Julian year
Days_of_year              <- 365.2425 ## Mean Gregorian calendar year
pch_am                    <-   1
pch_pm                    <-   2
pch_ampm                  <-  13 ## try 10
pch_daily                 <-  19
running_mean_window_years <-   5
running_mean_window_days  <- running_mean_window_years * Days_of_year


Daily_confidence_limit    <-   0.99
SZA_confidence_limit      <-   0.99
Monthly_confidence_limit  <-   0.99

Daily_aggregation_N_lim   <-   0        # replaced with relative daylight filter


