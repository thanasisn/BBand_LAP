
02


##  Daily SZA means ------------------------------------------------------------

## _ daily means  --------------------------------------------------------------


hist(CLEAR_2_daily_mean[, GLB_att_N], breaks = 100)
abline(v = SZA_aggregation_N_lim, col = "red")
hist(CLOUD_2_daily_mean[, GLB_att_N], breaks = 100)
abline(v = SZA_aggregation_N_lim, col = "red")




## _ Seasonal daily ------------------------------------------------------------
ALL_2_daily_seas <-
    ALL_2_daily_mean[,
                     .(
                         DIR_att_seas       = mean(DIR_att,    na.rm = T),
                         HOR_att_seas       = mean(HOR_att,    na.rm = T),
                         GLB_att_seas       = mean(GLB_att,    na.rm = T),
                         DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                         HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                         GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                         GLB_att_N_seas     = sum(!is.na(GLB_att)),
                         HOR_att_N_seas     = sum(!is.na(HOR_att)),
                         DIR_att_N_seas     = sum(!is.na(DIR_att))
                     ),
                     by = .(
                         doy,
                         SZA,
                         preNoon
                     )]

CLEAR_2_daily_seas <-
    CLEAR_2_daily_mean[,
                       .(
                           DIR_att_seas       = mean(DIR_att,    na.rm = T),
                           HOR_att_seas       = mean(HOR_att,    na.rm = T),
                           GLB_att_seas       = mean(GLB_att,    na.rm = T),
                           DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                           HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                           GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                           GLB_att_N_seas     = sum(!is.na(GLB_att)),
                           HOR_att_N_seas     = sum(!is.na(HOR_att)),
                           DIR_att_N_seas     = sum(!is.na(DIR_att))
                       ),
                       by = .(
                           doy,
                           SZA,
                           preNoon
                       )]

CLOUD_2_daily_seas <-
    CLOUD_2_daily_mean[,
                       .(
                           DIR_att_seas       = mean(DIR_att,    na.rm = T),
                           HOR_att_seas       = mean(HOR_att,    na.rm = T),
                           GLB_att_seas       = mean(GLB_att,    na.rm = T),
                           DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                           HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                           GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                           GLB_att_N_seas     = sum(!is.na(GLB_att)),
                           HOR_att_N_seas     = sum(!is.na(HOR_att)),
                           DIR_att_N_seas     = sum(!is.na(DIR_att))
                       ),
                       by = .(
                           doy,
                           SZA,
                           preNoon
                       )]



## _ Daily de-seasonal relative anomaly ----------------------------------------

ALL_2_daily_DESEAS   <- merge(  ALL_2_daily_mean,   ALL_2_daily_seas, by = c("doy", "SZA", "preNoon"), all = T)
CLEAR_2_daily_DESEAS <- merge(CLEAR_2_daily_mean, CLEAR_2_daily_seas, by = c("doy", "SZA", "preNoon"), all = T)
CLOUD_2_daily_DESEAS <- merge(CLOUD_2_daily_mean, CLOUD_2_daily_seas, by = c("doy", "SZA", "preNoon"), all = T)

setorder(  ALL_2_daily_DESEAS, Date)
setorder(CLEAR_2_daily_DESEAS, Date)
setorder(CLOUD_2_daily_DESEAS, Date)

## forget some daily data
rm(  ALL_2_daily_seas,
     CLEAR_2_daily_seas,
     CLOUD_2_daily_seas)
gc()

### Using the % departure from seasonal values

ALL_2_daily_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
ALL_2_daily_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
ALL_2_daily_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
# ALL_2_daily_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLEAR_2_daily_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLEAR_2_daily_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLEAR_2_daily_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
# CLEAR_2_daily_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLOUD_2_daily_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLOUD_2_daily_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLOUD_2_daily_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
# CLOUD_2_daily_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]





##  Monthly SZA means ----------------------------------------------------------

## _ monthly means -------------------------------------------------------------
ALL_2_monthly_mean <-
    ALL_2_daily_mean[,
                     .(
                         DIR_att       = mean(DIR_att,    na.rm = T),
                         HOR_att       = mean(HOR_att,    na.rm = T),
                         GLB_att       = mean(GLB_att,    na.rm = T),
                         # DIR_transp    = mean(DIR_transp, na.rm = T),
                         DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                         HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                         GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                         # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                         GLB_att_N     = sum(!is.na(GLB_att)),
                         HOR_att_N     = sum(!is.na(HOR_att)),
                         DIR_att_N     = sum(!is.na(DIR_att))
                     ),
                     by = .(
                         # SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                         SZA     = SZA,
                         Year    = year(Date),
                         Month   = month(Date),
                         preNoon = preNoon
                     ) ]
ALL_2_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]



CLEAR_2_monthly_mean <-
    CLEAR_2_daily_mean[,
                       .(
                           DIR_att       = mean(DIR_att,    na.rm = T),
                           HOR_att       = mean(HOR_att,    na.rm = T),
                           GLB_att       = mean(GLB_att,    na.rm = T),
                           # DIR_transp    = mean(DIR_transp, na.rm = T),
                           DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                           HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                           GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                           # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                           GLB_att_N     = sum(!is.na(GLB_att)),
                           HOR_att_N     = sum(!is.na(HOR_att)),
                           DIR_att_N     = sum(!is.na(DIR_att))
                       ),
                       by = .(
                           # SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                           SZA     = SZA,
                           Year    = year(Date),
                           Month   = month(Date),
                           preNoon = preNoon
                       ) ]
CLEAR_2_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


CLOUD_2_monthly_mean <-
    CLOUD_2_daily_mean[,
                       .(
                           DIR_att       = mean(DIR_att,    na.rm = T),
                           HOR_att       = mean(HOR_att,    na.rm = T),
                           GLB_att       = mean(GLB_att,    na.rm = T),
                           # DIR_transp    = mean(DIR_transp, na.rm = T),
                           DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                           HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                           GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                           # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                           GLB_att_N     = sum(!is.na(GLB_att)),
                           HOR_att_N     = sum(!is.na(HOR_att)),
                           DIR_att_N     = sum(!is.na(DIR_att))
                       ),
                       by = .(
                           # SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                           SZA     = SZA,
                           Year    = year(Date),
                           Month   = month(Date),
                           preNoon = preNoon
                       ) ]
CLOUD_2_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


hist(CLOUD_2_monthly_mean[, GLB_att_N], breaks = 100)
hist(CLEAR_2_monthly_mean[, GLB_att_N], breaks = 100)
hist(  ALL_2_monthly_mean[, GLB_att_N], breaks = 100)

table(CLOUD_2_monthly_mean[, GLB_att_N])
table(CLEAR_2_monthly_mean[, GLB_att_N])


## _ Aggregation Representation limits?? ---------------------------------------
CLOUD_2_monthly_mean[ GLB_att_N <=2, GLB_att := NA ]
CLEAR_2_monthly_mean[ GLB_att_N <=2, GLB_att := NA ]
ALL_2_monthly_mean[ GLB_att_N <=2, GLB_att := NA ]


## _ Seasonal monthly ----------------------------------------------------------
ALL_2_monthly_seas <-
    ALL_2_monthly_mean[,
                       .(
                           DIR_att_seas       = mean(DIR_att,    na.rm = T),
                           HOR_att_seas       = mean(HOR_att,    na.rm = T),
                           GLB_att_seas       = mean(GLB_att,    na.rm = T),
                           # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                           DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                           HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                           GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                           # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                           GLB_att_N_seas     = sum(!is.na(GLB_att)),
                           HOR_att_N_seas     = sum(!is.na(HOR_att)),
                           DIR_att_N_seas     = sum(!is.na(DIR_att))
                       ),
                       by = .(
                           Month   = month(Date),
                           SZA,
                           preNoon
                       )]

CLEAR_2_monthly_seas <-
    CLEAR_2_monthly_mean[,
                         .(
                             DIR_att_seas       = mean(DIR_att,    na.rm = T),
                             HOR_att_seas       = mean(HOR_att,    na.rm = T),
                             GLB_att_seas       = mean(GLB_att,    na.rm = T),
                             # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                             DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                             HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                             GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                             # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                             GLB_att_N_seas     = sum(!is.na(GLB_att)),
                             HOR_att_N_seas     = sum(!is.na(HOR_att)),
                             DIR_att_N_seas     = sum(!is.na(DIR_att))
                         ),
                         by = .(
                             Month   = month(Date),
                             SZA,
                             preNoon
                         )]

CLOUD_2_monthly_seas <-
    CLOUD_2_monthly_mean[,
                         .(
                             DIR_att_seas       = mean(DIR_att,    na.rm = T),
                             HOR_att_seas       = mean(HOR_att,    na.rm = T),
                             GLB_att_seas       = mean(GLB_att,    na.rm = T),
                             # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                             DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                             HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                             GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                             # DIR_transp_sd_seas = sd(DIR_transp,   na.rm = T),
                             GLB_att_N_seas     = sum(!is.na(GLB_att)),
                             HOR_att_N_seas     = sum(!is.na(HOR_att)),
                             DIR_att_N_seas     = sum(!is.na(DIR_att))
                         ),
                         by = .(
                             Month   = month(Date),
                             SZA,
                             preNoon
                         )]


## _ Monthly de-seasonal relative anomaly --------------------------------------

ALL_2_monthly_DESEAS <- merge(  ALL_2_monthly_mean,   ALL_2_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
CLEAR_2_monthly_DESEAS <- merge(CLEAR_2_monthly_mean, CLEAR_2_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
CLOUD_2_monthly_DESEAS <- merge(CLOUD_2_monthly_mean, CLOUD_2_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)

setorder(  ALL_2_monthly_DESEAS, Date)
setorder(CLEAR_2_monthly_DESEAS, Date)
setorder(CLOUD_2_monthly_DESEAS, Date)


### Using the % departure from seasonal values
ALL_2_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att - GLB_att_seas ) / GLB_att_seas ]
CLEAR_2_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att - GLB_att_seas ) / GLB_att_seas ]
CLOUD_2_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att - GLB_att_seas ) / GLB_att_seas ]



##  Yearly SZA means -----------------------------------------------------------
ALL_2_yearly_mean <-
    ALL_2_monthly_mean[,
                       .(
                           DIR_att       = mean(DIR_att,    na.rm = T),
                           HOR_att       = mean(HOR_att,    na.rm = T),
                           GLB_att       = mean(GLB_att,    na.rm = T),
                           # DIR_transp    = mean(DIR_transp, na.rm = T),
                           DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                           HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                           GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                           # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                           GLB_att_N     = sum(!is.na(GLB_att)),
                           HOR_att_N     = sum(!is.na(HOR_att)),
                           DIR_att_N     = sum(!is.na(DIR_att))
                       ),
                       by = .(
                           SZA     = SZA,
                           Year    = year(Date),
                           preNoon = preNoon
                       ) ]

CLEAR_2_yearly_mean <-
    CLEAR_2_monthly_mean[,
                         .(
                             DIR_att       = mean(DIR_att,    na.rm = T),
                             HOR_att       = mean(HOR_att,    na.rm = T),
                             GLB_att       = mean(GLB_att,    na.rm = T),
                             # DIR_transp    = mean(DIR_transp, na.rm = T),
                             DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                             HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                             GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                             # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                             GLB_att_N     = sum(!is.na(GLB_att)),
                             HOR_att_N     = sum(!is.na(HOR_att)),
                             DIR_att_N     = sum(!is.na(DIR_att))  ),
                         by = .(
                             SZA     = SZA,
                             Year    = year(Date),
                             preNoon = preNoon
                         ) ]

CLOUD_2_yearly_mean <-
    CLOUD_2_monthly_mean[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                             HOR_att       = mean(HOR_att,    na.rm = T),
                             GLB_att       = mean(GLB_att,    na.rm = T),
                             # DIR_transp    = mean(DIR_transp, na.rm = T),
                             DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                             HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                             GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                             # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                             GLB_att_N     = sum(!is.na(GLB_att)),
                             HOR_att_N     = sum(!is.na(HOR_att)),
                             DIR_att_N     = sum(!is.na(DIR_att))  ),
                         by = .(
                             SZA     = SZA,
                             Year    = year(Date),
                             preNoon = preNoon
                         ) ]











##  Season of year SZA ---------------------------------------------------------

## Quarter of year with one month shift to include December in the next years winter
DATA_all[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
DATA_Clear[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
DATA_Cloud[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]

## Flag seasons using quarters
DATA_all[season_Yqrt %% 1 == 0   , Season := "Winter"]
DATA_all[season_Yqrt %% 1 == 0.25, Season := "Spring"]
DATA_all[season_Yqrt %% 1 == 0.50, Season := "Summer"]
DATA_all[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
DATA_Clear[season_Yqrt %% 1 == 0   , Season := "Winter"]
DATA_Clear[season_Yqrt %% 1 == 0.25, Season := "Spring"]
DATA_Clear[season_Yqrt %% 1 == 0.50, Season := "Summer"]
DATA_Clear[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
DATA_Cloud[season_Yqrt %% 1 == 0   , Season := "Winter"]
DATA_Cloud[season_Yqrt %% 1 == 0.25, Season := "Spring"]
DATA_Cloud[season_Yqrt %% 1 == 0.50, Season := "Summer"]
DATA_Cloud[season_Yqrt %% 1 == 0.75, Season := "Autumn"]



## _ Daily mean by season ------------------------------------------------------
ALL_2_bySeason_daily_mean <-
    DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                 HOR_att       = mean(HOR_att,    na.rm = T),
                 GLB_att       = mean(GLB_att,    na.rm = T),
                 # DIR_transp    = mean(DIR_transp, na.rm = T),
                 DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                 HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                 GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                 # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                 doy           = yday(Date),
                 GLB_att_N     = sum(!is.na(GLB_att)),
                 HOR_att_N     = sum(!is.na(HOR_att)),
                 DIR_att_N     = sum(!is.na(DIR_att))  ),
             by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                    Date    = Day,
                    preNoon = preNoon,
                    Yqrt    = season_Yqrt)]

CLEAR_2_bySeason_daily_mean <-
    DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                   HOR_att       = mean(HOR_att,    na.rm = T),
                   GLB_att       = mean(GLB_att,    na.rm = T),
                   # DIR_transp    = mean(DIR_transp, na.rm = T),
                   DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                   HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                   GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                   # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                   doy           = yday(Date),
                   GLB_att_N     = sum(!is.na(GLB_att)),
                   HOR_att_N     = sum(!is.na(HOR_att)),
                   DIR_att_N     = sum(!is.na(DIR_att))  ),
               by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                      Date    = Day,
                      preNoon = preNoon,
                      Yqrt    = season_Yqrt)]

CLOUD_2_bySeason_daily_mean <-
    DATA_Cloud[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                   HOR_att       = mean(HOR_att,    na.rm = T),
                   GLB_att       = mean(GLB_att,    na.rm = T),
                   # DIR_transp    = mean(DIR_transp, na.rm = T),
                   DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                   HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                   GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                   # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                   doy           = yday(Date),
                   GLB_att_N     = sum(!is.na(GLB_att)),
                   HOR_att_N     = sum(!is.na(HOR_att)),
                   DIR_att_N     = sum(!is.na(DIR_att))  ),
               by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                      Date    = Day,
                      preNoon = preNoon,
                      Yqrt    = season_Yqrt)]

## _ Exclude means with less than SZA_aggregation_N_lim data points ------------
ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
ALL_2_bySeason_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
ALL_2_bySeason_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
# ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp    := NA ]
ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
ALL_2_bySeason_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
ALL_2_bySeason_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
# ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_sd := NA ]
ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
ALL_2_bySeason_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
ALL_2_bySeason_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]
# ALL_2_bySeason_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_EM := NA ]

CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
CLEAR_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
CLEAR_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
# CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp    := NA ]
CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
CLEAR_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
CLEAR_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
# CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_sd := NA ]
CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
CLEAR_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
CLEAR_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]
# CLEAR_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_EM := NA ]

CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
CLOUD_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
CLOUD_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
# CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp    := NA ]
CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
CLOUD_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
CLOUD_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
# CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_sd := NA ]
CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
CLOUD_2_bySeason_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
CLOUD_2_bySeason_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]
# CLOUD_2_bySeason_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_transp_EM := NA ]



## _ Monthly mean by season  ---------------------------------------------------
ALL_2_bySeason_monthly_mean <-
    ALL_2_bySeason_daily_mean[,
                              .(
                                  DIR_att       = mean(DIR_att,    na.rm = T),
                                  HOR_att       = mean(HOR_att,    na.rm = T),
                                  GLB_att       = mean(GLB_att,    na.rm = T),
                                  # DIR_transp    = mean(DIR_transp, na.rm = T),
                                  DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                  HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                                  GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                  # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                                  doy           = yday(Date),
                                  GLB_att_N     = sum(!is.na(GLB_att)),
                                  HOR_att_N     = sum(!is.na(HOR_att)),
                                  DIR_att_N     = sum(!is.na(DIR_att))
                              ),
                              by = .(
                                  SZA     = SZA,
                                  Month   = month(Date),
                                  Year    = year(Date),
                                  preNoon = preNoon,
                                  Yqrt    = Yqrt
                              )]
ALL_2_bySeason_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


CLEAR_2_bySeason_monthly_mean <-
    CLEAR_2_bySeason_daily_mean[,
                                .(
                                    DIR_att       = mean(DIR_att,    na.rm = T),
                                    HOR_att       = mean(HOR_att,    na.rm = T),
                                    GLB_att       = mean(GLB_att,    na.rm = T),
                                    # DIR_transp    = mean(DIR_transp, na.rm = T),
                                    DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                    HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                                    GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                    # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                                    doy           = yday(Date),
                                    GLB_att_N     = sum(!is.na(GLB_att)),
                                    HOR_att_N     = sum(!is.na(HOR_att)),
                                    DIR_att_N     = sum(!is.na(DIR_att))
                                ),
                                by = .(
                                    SZA     = SZA,
                                    Month   = month(Date),
                                    Year    = year(Date),
                                    preNoon = preNoon,
                                    Yqrt    = Yqrt
                                )]
CLEAR_2_bySeason_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


CLOUD_2_bySeason_monthly_mean <-
    CLOUD_2_bySeason_daily_mean[,
                                .(
                                    DIR_att       = mean(DIR_att,    na.rm = T),
                                    HOR_att       = mean(HOR_att,    na.rm = T),
                                    GLB_att       = mean(GLB_att,    na.rm = T),
                                    # DIR_transp    = mean(DIR_transp, na.rm = T),
                                    DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                                    HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                                    GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                                    # DIR_transp_sd = sd(  DIR_transp, na.rm = T),
                                    doy           = yday(Date),
                                    GLB_att_N     = sum(!is.na(GLB_att)),
                                    HOR_att_N     = sum(!is.na(HOR_att)),
                                    DIR_att_N     = sum(!is.na(DIR_att))
                                ),
                                by = .(
                                    SZA     = SZA,
                                    Month   = month(Date),
                                    Year    = year(Date),
                                    preNoon = preNoon,
                                    Yqrt    = Yqrt
                                )]
CLOUD_2_bySeason_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


## _ Aggregation Representation limits?? ---------------------------------------
CLOUD_2_bySeason_monthly_mean[ GLB_att_N <=2, GLB_att := NA ]
CLEAR_2_bySeason_monthly_mean[ GLB_att_N <=2, GLB_att := NA ]
ALL_2_bySeason_monthly_mean[ GLB_att_N <=2, GLB_att := NA ]


hist(  ALL_2_bySeason_monthly_mean$GLB_att_N, breaks = 100)
hist(CLEAR_2_bySeason_monthly_mean$GLB_att_N, breaks = 100)
hist(CLOUD_2_bySeason_monthly_mean$GLB_att_N, breaks = 100)

table(  ALL_2_bySeason_monthly_mean$GLB_att_N)
table(CLEAR_2_bySeason_monthly_mean$GLB_att_N)
table(CLOUD_2_bySeason_monthly_mean$GLB_att_N)


## Flag seasons of year using quarters
ALL_2_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
ALL_2_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
ALL_2_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
ALL_2_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
CLEAR_2_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
CLEAR_2_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
CLEAR_2_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
CLEAR_2_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
CLOUD_2_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
CLOUD_2_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
CLOUD_2_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
CLOUD_2_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]

ALL_2_bySeason_monthly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
ALL_2_bySeason_monthly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
ALL_2_bySeason_monthly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
ALL_2_bySeason_monthly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
CLEAR_2_bySeason_monthly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
CLEAR_2_bySeason_monthly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
CLEAR_2_bySeason_monthly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
CLEAR_2_bySeason_monthly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
CLOUD_2_bySeason_monthly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
CLOUD_2_bySeason_monthly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
CLOUD_2_bySeason_monthly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
CLOUD_2_bySeason_monthly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]






## _ Seasonal by season SZA values daily ---------------------------------------

ALL_2_bySeason_daily_seas <-
    ALL_2_bySeason_daily_mean[, .(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                  HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                  GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                  # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                  DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                  HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                  GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                  # DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                                  GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                  HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                  DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                              by = .(SZA     = SZA,
                                     doy     = yday(Date),
                                     preNoon = preNoon,
                                     Season  = Season) ]

CLEAR_2_bySeason_daily_seas <-
    CLEAR_2_bySeason_daily_mean[, .(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                    HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                    GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                    # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                    DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                    HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                    GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                    # DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                                    GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                    HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                    DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                                by = .(SZA     = SZA,
                                       doy     = yday(Date),
                                       preNoon = preNoon,
                                       Season  = Season) ]

CLOUD_2_bySeason_daily_seas <-
    CLOUD_2_bySeason_daily_mean[, .(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                    HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                    GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                    # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                    DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                    HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                    GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                    # DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                                    GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                    HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                    DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                                by = .(SZA     = SZA,
                                       doy     = yday(Date),
                                       preNoon = preNoon,
                                       Season  = Season) ]


ALL_2_bySeason_daily_DESEAS <- merge(  ALL_2_bySeason_daily_mean,   ALL_2_bySeason_daily_seas, by = c("SZA", "doy", "preNoon", "Season"), all = T)
CLEAR_2_bySeason_daily_DESEAS <- merge(CLEAR_2_bySeason_daily_mean, CLEAR_2_bySeason_daily_seas, by = c("SZA", "doy", "preNoon", "Season"), all = T)
CLOUD_2_bySeason_daily_DESEAS <- merge(CLOUD_2_bySeason_daily_mean, CLOUD_2_bySeason_daily_seas, by = c("SZA", "doy", "preNoon", "Season"), all = T)

## _ Relative anomaly by season ------------------------------------------------
ALL_2_bySeason_daily_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
ALL_2_bySeason_daily_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
ALL_2_bySeason_daily_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
# ALL_2_bySeason_daily_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLEAR_2_bySeason_daily_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLEAR_2_bySeason_daily_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLEAR_2_bySeason_daily_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
# CLEAR_2_bySeason_daily_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLOUD_2_bySeason_daily_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLOUD_2_bySeason_daily_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLOUD_2_bySeason_daily_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
# CLOUD_2_bySeason_daily_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]

## Create year from quarter!
warning("Years in by Season are shifted by a month to match seasons")
ALL_2_bySeason_daily_DESEAS[, Year := year(Yqrt)]
CLEAR_2_bySeason_daily_DESEAS[, Year := year(Yqrt)]
CLOUD_2_bySeason_daily_DESEAS[, Year := year(Yqrt)]



## _ Seasonal by season SZA values monthly  ------------------------------------
ALL_2_bySeason_monthly_seas <-
    ALL_2_bySeason_monthly_mean[,
                                .(
                                    DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                    HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                    GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                    # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                    DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                    HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                    GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                    # DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                                    GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                    HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                    DIR_att_N_seas     = sum(!is.na(DIR_att))
                                ),
                                by = .(
                                    SZA     = SZA,
                                    Month,
                                    preNoon = preNoon,
                                    Season  = Season
                                ) ]

CLEAR_2_bySeason_monthly_seas <-
    CLEAR_2_bySeason_monthly_mean[,
                                  .(
                                      DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                      HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                      GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                      # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                      DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                      HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                      GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                      # DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                                      GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                      HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                      DIR_att_N_seas     = sum(!is.na(DIR_att))
                                  ),
                                  by = .(
                                      SZA     = SZA,
                                      Month,
                                      preNoon = preNoon,
                                      Season  = Season
                                  ) ]

CLOUD_2_bySeason_monthly_seas <-
    CLOUD_2_bySeason_monthly_mean[,
                                  .(
                                      DIR_att_seas       = mean(DIR_att,    na.rm = T),
                                      HOR_att_seas       = mean(HOR_att,    na.rm = T),
                                      GLB_att_seas       = mean(GLB_att,    na.rm = T),
                                      # DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                                      DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                                      HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                                      GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                                      # DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                                      GLB_att_N_seas     = sum(!is.na(GLB_att)),
                                      HOR_att_N_seas     = sum(!is.na(HOR_att)),
                                      DIR_att_N_seas     = sum(!is.na(DIR_att))
                                  ),
                                  by = .(
                                      SZA     = SZA,
                                      Month,
                                      preNoon = preNoon,
                                      Season  = Season
                                  ) ]


## _ Monthly de-seasonal anomaly -----------------------------------------------

ALL_2_bySeason_monthly_DESEAS <- merge(  ALL_2_bySeason_monthly_mean,   ALL_2_bySeason_monthly_seas, by = c("Month", "SZA", "preNoon", "Season"), all = T)
CLEAR_2_bySeason_monthly_DESEAS <- merge(CLEAR_2_bySeason_monthly_mean, CLEAR_2_bySeason_monthly_seas, by = c("Month", "SZA", "preNoon", "Season"), all = T)
CLOUD_2_bySeason_monthly_DESEAS <- merge(CLOUD_2_bySeason_monthly_mean, CLOUD_2_bySeason_monthly_seas, by = c("Month", "SZA", "preNoon", "Season"), all = T)


## _ Monthly relative anomaly --------------------------------------------------

### Using the % departure from seasonal values

ALL_2_bySeason_monthly_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
ALL_2_bySeason_monthly_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
ALL_2_bySeason_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
# ALL_2_bySeason_monthly_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLEAR_2_bySeason_monthly_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLEAR_2_bySeason_monthly_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLEAR_2_bySeason_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
# CLEAR_2_bySeason_monthly_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLOUD_2_bySeason_monthly_DESEAS[, DIR_att_des   := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLOUD_2_bySeason_monthly_DESEAS[, HOR_att_des   := 100 * (HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLOUD_2_bySeason_monthly_DESEAS[, GLB_att_des   := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
# CLOUD_2_bySeason_monthly_DESEAS[, DIR_transp_des:= 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]

