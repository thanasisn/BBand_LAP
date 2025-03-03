
04


## _ Monthly means by SZA prenoon month  ---------------------------------------

## Will create values for am, pm, and daily

ALL_3_monthly_meanA <-
    DATA_all[,.(DIR_att       = mean(DIR_att,    na.rm = T),
                GLB_att       = mean(GLB_att,    na.rm = T),
                HOR_att       = mean(HOR_att,    na.rm = T),
                DIR_transp    = mean(DIR_transp, na.rm = T),
                DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                DIR_transp_sd = sd(DIR_transp, na.rm = T),
                HOR_att_N     = sum(!is.na(HOR_att)),
                GLB_att_N     = sum(!is.na(GLB_att)),
                DIR_att_N     = sum(!is.na(DIR_att))  ),
             by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                    Year    = year(Date),
                    Month   = month(Date),
                    preNoon = preNoon)]

ALL_3_monthly_meanB <-
    DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                 GLB_att       = mean(GLB_att,    na.rm = T),
                 HOR_att       = mean(HOR_att,    na.rm = T),
                 DIR_transp    = mean(DIR_transp, na.rm = T),
                 DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                 HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                 GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                 DIR_transp_sd = sd(DIR_transp, na.rm = T),
                 HOR_att_N     = sum(!is.na(HOR_att)),
                 GLB_att_N     = sum(!is.na(GLB_att)),
                 DIR_att_N     = sum(!is.na(DIR_att)),
                 preNoon       = "am+pm"),
             by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                    Year    = year(Date),
                    Month   = month(Date))]

ALL_3_monthly_mean <- data.table(rbind( data.frame(ALL_3_monthly_meanB),
                                        data.frame(ALL_3_monthly_meanA) ))
rm(ALL_3_monthly_meanA, ALL_3_monthly_meanB)
ALL_3_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


CLEAR_3_monthly_meanA <-
    DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                   HOR_att       = mean(HOR_att,    na.rm = T),
                   GLB_att       = mean(GLB_att,    na.rm = T),
                   DIR_transp    = mean(DIR_transp, na.rm = T),
                   DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                   HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                   GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                   DIR_transp_sd = sd(DIR_transp, na.rm = T),
                   GLB_att_N     = sum(!is.na(GLB_att)),
                   HOR_att_N     = sum(!is.na(HOR_att)),
                   DIR_att_N     = sum(!is.na(DIR_att))),
               by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                      Year    = year(Date),
                      Month   = month(Date),
                      preNoon = preNoon)]

CLEAR_3_monthly_meanB <-
    DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                   HOR_att       = mean(HOR_att,    na.rm = T),
                   GLB_att       = mean(GLB_att,    na.rm = T),
                   DIR_transp    = mean(DIR_transp, na.rm = T),
                   DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                   HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                   GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                   DIR_transp_sd = sd(DIR_transp, na.rm = T),
                   GLB_att_N     = sum(!is.na(GLB_att)),
                   HOR_att_N     = sum(!is.na(HOR_att)),
                   DIR_att_N     = sum(!is.na(DIR_att)),
                   preNoon       = "am+pm"),
               by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                      Year    = year(Date),
                      Month   = month(Date))]

CLEAR_3_monthly_mean <- data.table(rbind(data.frame(CLEAR_3_monthly_meanB),
                                         data.frame(CLEAR_3_monthly_meanA) ))
rm(CLEAR_3_monthly_meanA, CLEAR_3_monthly_meanB)
CLEAR_3_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


CLOUD_3_monthly_meanA <-
    DATA_Cloud[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                   HOR_att       = mean(HOR_att,    na.rm = T),
                   GLB_att       = mean(GLB_att,    na.rm = T),
                   DIR_transp    = mean(DIR_transp, na.rm = T),
                   DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                   HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                   GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                   DIR_transp_sd = sd(DIR_transp, na.rm = T),
                   GLB_att_N     = sum(!is.na(GLB_att)),
                   HOR_att_N     = sum(!is.na(HOR_att)),
                   DIR_att_N     = sum(!is.na(DIR_att))),
               by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                      Year    = year(Date),
                      Month   = month(Date),
                      preNoon = preNoon)]

CLOUD_3_monthly_meanB <-
    DATA_Cloud[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                   HOR_att       = mean(HOR_att,    na.rm = T),
                   GLB_att       = mean(GLB_att,    na.rm = T),
                   DIR_transp    = mean(DIR_transp, na.rm = T),
                   DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                   HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                   GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                   DIR_transp_sd = sd(DIR_transp, na.rm = T),
                   GLB_att_N     = sum(!is.na(GLB_att)),
                   HOR_att_N     = sum(!is.na(HOR_att)),
                   DIR_att_N     = sum(!is.na(DIR_att)),
                   preNoon       = "am+pm"),
               by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                      Year    = year(Date),
                      Month   = month(Date))]

CLOUD_3_monthly_mean <- data.table(rbind(data.frame(CLOUD_3_monthly_meanB),
                                         data.frame(CLOUD_3_monthly_meanA) ))
rm(CLOUD_3_monthly_meanA, CLOUD_3_monthly_meanB)
CLOUD_3_monthly_mean[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]


## _ Exclude means with less than Monthly_aggegation_N_lim data points ---------
ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att       := NA]
ALL_3_monthly_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att       := NA]
ALL_3_monthly_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att       := NA]
ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp    := NA]
ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_sd    := NA]
ALL_3_monthly_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_sd    := NA]
ALL_3_monthly_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_sd    := NA]
ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_sd := NA]
ALL_3_monthly_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_EM    := NA]
ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_EM    := NA]
ALL_3_monthly_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_EM    := NA]
ALL_3_monthly_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_EM := NA]

CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att       := NA]
CLEAR_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att       := NA]
CLEAR_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att       := NA]
CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp    := NA]
CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_sd    := NA]
CLEAR_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_sd    := NA]
CLEAR_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_sd    := NA]
CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_sd := NA]
CLEAR_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_EM    := NA]
CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_EM    := NA]
CLEAR_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_EM    := NA]
CLEAR_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_EM := NA]

CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att       := NA]
CLOUD_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att       := NA]
CLOUD_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att       := NA]
CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp    := NA]
CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_sd    := NA]
CLOUD_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_sd    := NA]
CLOUD_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_sd    := NA]
CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_sd := NA]
CLOUD_3_monthly_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_EM    := NA]
CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_EM    := NA]
CLOUD_3_monthly_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_EM    := NA]
CLOUD_3_monthly_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_EM := NA]


## _  Monthly seasonal values --------------------------------------------------
ALL_3_monthly_seas <-
    ALL_3_monthly_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                           GLB_att_seas    = mean(GLB_att,    na.rm = T),
                           HOR_att_seas    = mean(HOR_att,    na.rm = T),
                           DIR_transp_seas = mean(DIR_transp, na.rm = T),
                           DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                           HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                           GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                           GLB_att_N_seas  = sum(!is.na(GLB_att)),
                           HOR_att_N_seas  = sum(!is.na(HOR_att)),
                           DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                       by = .( Month, SZA, preNoon ) ]

CLEAR_3_monthly_seas <-
    CLEAR_3_monthly_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                             GLB_att_seas    = mean(GLB_att,    na.rm = T),
                             HOR_att_seas    = mean(HOR_att,    na.rm = T),
                             DIR_transp_seas = mean(DIR_transp, na.rm = T),
                             DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                             HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                             GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                             GLB_att_N_seas  = sum(!is.na(GLB_att)),
                             HOR_att_N_seas  = sum(!is.na(HOR_att)),
                             DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                         by = .( Month, SZA, preNoon ) ]

CLOUD_3_monthly_seas <-
    CLOUD_3_monthly_mean[, .(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                             GLB_att_seas    = mean(GLB_att,    na.rm = T),
                             HOR_att_seas    = mean(HOR_att,    na.rm = T),
                             DIR_transp_seas = mean(DIR_transp, na.rm = T),
                             DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                             HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                             GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                             GLB_att_N_seas  = sum(!is.na(GLB_att)),
                             HOR_att_N_seas  = sum(!is.na(HOR_att)),
                             DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                         by = .( Month, SZA, preNoon ) ]





## _ Seasonal anomaly by SZA and period of day ---------------------------------

ALL_3_monthly_DESEAS <- merge(  ALL_3_monthly_mean,   ALL_3_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
CLEAR_3_monthly_DESEAS <- merge(CLEAR_3_monthly_mean, CLEAR_3_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)
CLOUD_3_monthly_DESEAS <- merge(CLOUD_3_monthly_mean, CLOUD_3_monthly_seas, by = c("Month", "SZA", "preNoon"), all = T)

## _ forget data
rm(  ALL_3_monthly_mean,   ALL_3_monthly_seas,
     CLEAR_3_monthly_mean, CLEAR_3_monthly_seas,
     CLOUD_3_monthly_mean, CLOUD_3_monthly_seas)

## Using the % departure from seasonal values

ALL_3_monthly_DESEAS[, DIR_att_des    := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
ALL_3_monthly_DESEAS[, GLB_att_des    := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
ALL_3_monthly_DESEAS[, DIR_transp_des := 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLEAR_3_monthly_DESEAS[, DIR_att_des    := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLEAR_3_monthly_DESEAS[, GLB_att_des    := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLEAR_3_monthly_DESEAS[, DIR_transp_des := 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLOUD_3_monthly_DESEAS[, DIR_att_des    := 100 * (DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLOUD_3_monthly_DESEAS[, GLB_att_des    := 100 * (GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLOUD_3_monthly_DESEAS[, DIR_transp_des := 100 * (DIR_transp - DIR_transp_seas) / DIR_transp_seas]

## create a nice data
ALL_3_monthly_DESEAS[, Date := as.Date(paste(Year, Month, 1), format = "%Y %m %d")]
CLEAR_3_monthly_DESEAS[, Date := as.Date(paste(Year, Month, 1), format = "%Y %m %d")]
CLOUD_3_monthly_DESEAS[, Date := as.Date(paste(Year, Month, 1), format = "%Y %m %d")]

## change rest of flags names
ALL_3_monthly_DESEAS[preNoon == TRUE,  preNoon := "am"]
ALL_3_monthly_DESEAS[preNoon == FALSE, preNoon := "pm"]
CLEAR_3_monthly_DESEAS[preNoon == TRUE,  preNoon := "am"]
CLEAR_3_monthly_DESEAS[preNoon == FALSE, preNoon := "pm"]
CLOUD_3_monthly_DESEAS[preNoon == TRUE,  preNoon := "am"]
CLOUD_3_monthly_DESEAS[preNoon == FALSE, preNoon := "pm"]



