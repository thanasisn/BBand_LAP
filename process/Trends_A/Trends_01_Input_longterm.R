
01

##  ERA5 cloud data  -----------------------------------------------------------
## create ERA5 cloud subset variables
cf_lim <- 0.1
DATA_all[near_tcc == 0,      near_tcc_zero   := TRUE    ]
DATA_all[near_tcc >  0,      near_tcc_NOzero := near_tcc]
DATA_all[near_tcc <  cf_lim, near_tcc_clear  := near_tcc]
DATA_all[near_tcc >  cf_lim, near_tcc_cloud  := near_tcc]

ALL_tcc_yearly_mean <-
    DATA_all[,.(
        near_tcc_att        = mean(near_tcc,        na.rm = T),
        bilin_tcc_att       = mean(bilin_tcc,       na.rm = T),
        near_tcc_zero_N     = sum(near_tcc_zero,    na.rm = T),
        near_tcc_TN         = sum(!is.na(near_tcc)),
        near_tcc_NOzero_att = mean(near_tcc_NOzero, na.rm = T),
        near_tcc_clear_att  = mean(near_tcc_clear,  na.rm = T),
        near_tcc_cloud_att  = mean(near_tcc_cloud,  na.rm = T)
    ),
    by = .( Year = year(Day) ) ]
ALL_tcc_yearly_mean[, near_tcc_zero_rel := near_tcc_zero_N / near_tcc_TN ]

## remove first and last non complete years
ALL_tcc_yearly_mean <- ALL_tcc_yearly_mean[!Year %in% c(1993, 2023) ]

## plot TCC trends  ------------------------------------------------------------
vars   <- grep("Year", names(ALL_tcc_yearly_mean), invert = T, value = T)
dbs    <- c("ALL_tcc_yearly_mean")
gather <- data.frame()
for (DBn in dbs) {
    DB <- get(DBn)
    for (avar in vars) {
        dataset <- DB
        ## linear model by day step
        lm1 <- lm(get(avar) ~ Year, data = dataset)
        ## correlation test
        cor1 <- cor.test(x = dataset[[avar]], y = as.numeric(dataset$Year), method = 'pearson')
        dt <- data.frame(Year = year(as.POSIXct(c("1993-01-01 00:00","2023-01-01 00:00"))))
        slopePyear <- diff(predict(lm1, dt)) / diff((dt$Year))
        ## capture lm for table
        gather <- rbind(gather,
                        data.frame(
                          linear_fit_stats(lm1, confidence_interval = Daily_confidence_limit),
                          cor_test_stats(cor1),
                          slopePyear = slopePyear,
                          DATA       = DBn,
                          var        = avar,
                          Mean       = mean(dataset[[avar]], na.rm = TRUE),
                          N          = sum(!is.na(dataset[[avar]]))
                        ))
        ## plot data
        plot(dataset$Year, dataset[[avar]],
             pch  = 19,
             col  = "#1a9850",
             cex      = 1,
             cex.main = 0.8,
             yaxt     = "n",
             xlab     = "",
             ylab     = bquote("?")
        )
        # y axis
        axis(2, pretty(dataset[[avar]]), las = 2 )
        # x axis
        axis(1,
             at = seq(1993, max(dataset$Year), by = 1),
             # format = "%Y",
             labels = NA,
             tcl = -0.25)
        ## plot fit line
        abline(lm1, lwd = 2)
        title(paste("ERA5  ", translate(avar)))
        ## display trend on graph
        fit <- lm1[[1]]
        legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
               paste("Trend: ",
                     if (fit[2] > 0) "+" else "-",
                     signif(abs(fit[2]) , 3),
                     "?/y")
        )
    }
}

## _ Daily de-seasonal anomaly -------------------------------------------------

## add TSI data process
ALL_1_daily_mean    [, tsi1au_att_des := 100*(tsi1au_att - mean(tsi1au_att)) / mean(tsi1au_att)]
ALL_1_daily_mean <-
  merge(ALL_1_daily_DESEAS,
        ALL_1_daily_mean[, .(Date, tsi1au_att)], by = "Date", all = T )

## just for completeness or to see if there is any selection bias
CLEAR_1_daily_DESEAS[, tsi1au_att_des := 100*(tsi1au_att - mean(tsi1au_att)) / mean(tsi1au_att)]
CLOUD_1_daily_DESEAS[, tsi1au_att_des := 100*(tsi1au_att - mean(tsi1au_att)) / mean(tsi1au_att)]
CLEAR_1_daily_mean <-
  merge(CLEAR_1_daily_DESEAS,
        CLEAR_1_daily_mean[, .(Date, tsi1au_att)], by = "Date", all = T )

CLOUD_1_daily_mean <-
  merge(CLOUD_1_daily_DESEAS,
        CLOUD_1_daily_mean[, .(Date, tsi1au_att)], by = "Date", all = T )



## _ Seasonal monthly daily values ---------------------------------------------




## Season of year daily aggregation --------------------------------------------

## Quarter of year with one month shift to include December in the next years winter
ALL_1_daily_mean[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
CLEAR_1_daily_mean[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
CLOUD_1_daily_mean[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]

## Flag seasons using quarters
ALL_1_daily_mean[season_Yqrt %% 1 == 0   , Season := "Winter"]
ALL_1_daily_mean[season_Yqrt %% 1 == 0.25, Season := "Spring"]
ALL_1_daily_mean[season_Yqrt %% 1 == 0.50, Season := "Summer"]
ALL_1_daily_mean[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
CLEAR_1_daily_mean[season_Yqrt %% 1 == 0   , Season := "Winter"]
CLEAR_1_daily_mean[season_Yqrt %% 1 == 0.25, Season := "Spring"]
CLEAR_1_daily_mean[season_Yqrt %% 1 == 0.50, Season := "Summer"]
CLEAR_1_daily_mean[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
CLOUD_1_daily_mean[season_Yqrt %% 1 == 0   , Season := "Winter"]
CLOUD_1_daily_mean[season_Yqrt %% 1 == 0.25, Season := "Spring"]
CLOUD_1_daily_mean[season_Yqrt %% 1 == 0.50, Season := "Summer"]
CLOUD_1_daily_mean[season_Yqrt %% 1 == 0.75, Season := "Autumn"]


## _ Create variables by season from daily means -------------------------------
ALL_1_bySeason_daily_mean <-
  ALL_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                      GLB_att    = mean(GLB_att,    na.rm = T),
                      HOR_att    = mean(HOR_att,    na.rm = T),
                      DIR_transp = mean(DIR_transp, na.rm = T),
                      DIR_att_sd = sd(  DIR_att,    na.rm = T),
                      HOR_att_sd = sd(  HOR_att,    na.rm = T),
                      GLB_att_sd = sd(  GLB_att,    na.rm = T),
                      GLB_att_N  = sum(!is.na(GLB_att)),
                      HOR_att_N  = sum(!is.na(HOR_att)),
                      DIR_att_N  = sum(!is.na(DIR_att)),
                      minDate    = min(Date),
                      maxDate    = max(Date),
                      medDate    = median(Date)    ),
                   by = .( Yqrt = season_Yqrt) ]

CLEAR_1_bySeason_daily_mean <-
  CLEAR_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                        GLB_att    = mean(GLB_att,    na.rm = T),
                        HOR_att    = mean(HOR_att,    na.rm = T),
                        DIR_transp = mean(DIR_transp, na.rm = T),
                        DIR_att_sd = sd(  DIR_att,    na.rm = T),
                        HOR_att_sd = sd(  HOR_att,    na.rm = T),
                        GLB_att_sd = sd(  GLB_att,    na.rm = T),
                        GLB_att_N  = sum(!is.na(GLB_att)),
                        HOR_att_N  = sum(!is.na(HOR_att)),
                        DIR_att_N  = sum(!is.na(DIR_att)),
                        minDate    = min(Date),
                        maxDate    = max(Date),
                        medDate    = median(Date)    ),
                     by = .( Yqrt = season_Yqrt) ]

CLOUD_1_bySeason_daily_mean <-
  CLOUD_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                        GLB_att    = mean(GLB_att,    na.rm = T),
                        HOR_att    = mean(HOR_att,    na.rm = T),
                        DIR_transp = mean(DIR_transp, na.rm = T),
                        DIR_att_sd = sd(  DIR_att,    na.rm = T),
                        HOR_att_sd = sd(  HOR_att,    na.rm = T),
                        GLB_att_sd = sd(  GLB_att,    na.rm = T),
                        GLB_att_N  = sum(!is.na(GLB_att)),
                        HOR_att_N  = sum(!is.na(HOR_att)),
                        DIR_att_N  = sum(!is.na(DIR_att)),
                        minDate    = min(Date),
                        maxDate    = max(Date),
                        medDate    = median(Date)    ),
                     by = .( Yqrt = season_Yqrt) ]


## Flag seasons using quarters
ALL_1_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
ALL_1_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
ALL_1_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
ALL_1_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
CLEAR_1_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
CLEAR_1_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
CLEAR_1_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
CLEAR_1_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
CLOUD_1_bySeason_daily_mean[Yqrt %% 1 == 0   , Season := "Winter"]
CLOUD_1_bySeason_daily_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
CLOUD_1_bySeason_daily_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
CLOUD_1_bySeason_daily_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]






## _ Seasonal by season daily values -------------------------------------------

ALL_1_bySeason_daily_seas <-
  ALL_1_daily_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                      GLB_att_seas    = mean(GLB_att,    na.rm = T),
                      HOR_att_seas    = mean(HOR_att,    na.rm = T),
                      DIR_transp_seas = mean(DIR_transp, na.rm = T),
                      DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                      HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                      GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                      GLB_att_N_seas  = sum(!is.na(GLB_att)),
                      HOR_att_N_seas  = sum(!is.na(HOR_att)),
                      DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                   by = .(Season)]

CLEAR_1_bySeason_daily_seas <-
  CLEAR_1_daily_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                        GLB_att_seas    = mean(GLB_att,    na.rm = T),
                        HOR_att_seas    = mean(HOR_att,    na.rm = T),
                        DIR_transp_seas = mean(DIR_transp, na.rm = T),
                        DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                        HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                        GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                        GLB_att_N_seas  = sum(!is.na(GLB_att)),
                        HOR_att_N_seas  = sum(!is.na(HOR_att)),
                        DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                     by = .(Season)]

CLOUD_1_bySeason_daily_seas <-
  CLOUD_1_daily_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                        GLB_att_seas    = mean(GLB_att,    na.rm = T),
                        HOR_att_seas    = mean(HOR_att,    na.rm = T),
                        DIR_transp_seas = mean(DIR_transp, na.rm = T),
                        DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                        HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                        GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                        GLB_att_N_seas  = sum(!is.na(GLB_att)),
                        HOR_att_N_seas  = sum(!is.na(HOR_att)),
                        DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                     by = .(Season)]



## _ De-seasonal by season daily mean  -----------------------------------------

ALL_1_D_bySeason_DESEAS <- merge(  ALL_1_bySeason_daily_mean,   ALL_1_bySeason_daily_seas, by = "Season", all = T)
CLEAR_1_D_bySeason_DESEAS <- merge(CLEAR_1_bySeason_daily_mean, CLEAR_1_bySeason_daily_seas, by = "Season", all = T)
CLOUD_1_D_bySeason_DESEAS <- merge(CLOUD_1_bySeason_daily_mean, CLOUD_1_bySeason_daily_seas, by = "Season", all = T)


rm(  ALL_1_bySeason_daily_mean,   ALL_1_bySeason_daily_seas,
     CLEAR_1_bySeason_daily_mean, CLEAR_1_bySeason_daily_seas,
     CLOUD_1_bySeason_daily_mean, CLOUD_1_bySeason_daily_seas)
dummy <- gc()

## calculate anomaly
ALL_1_D_bySeason_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
ALL_1_D_bySeason_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
ALL_1_D_bySeason_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
ALL_1_D_bySeason_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLEAR_1_D_bySeason_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLEAR_1_D_bySeason_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLEAR_1_D_bySeason_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLEAR_1_D_bySeason_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLOUD_1_D_bySeason_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLOUD_1_D_bySeason_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLOUD_1_D_bySeason_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLOUD_1_D_bySeason_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]

## Create year from quarter!
warning("Years in by Season are shifted by a month to match seasons")
ALL_1_D_bySeason_DESEAS[, Year := year(Yqrt)]
CLEAR_1_D_bySeason_DESEAS[, Year := year(Yqrt)]
CLOUD_1_D_bySeason_DESEAS[, Year := year(Yqrt)]




## Season of year monthly aggregation --------------------------------------------

## Quarter of year with one month shift to include December in the next years winter
ALL_1_D_monthly_DESEAS[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
CLEAR_1_D_monthly_DESEAS[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]
CLOUD_1_D_monthly_DESEAS[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Date), month(Date), sep = "-")) + 1/12)]

## Flag seasons using quarters
ALL_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0   , Season := "Winter"]
ALL_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0.25, Season := "Spring"]
ALL_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0.50, Season := "Summer"]
ALL_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
CLEAR_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0   , Season := "Winter"]
CLEAR_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0.25, Season := "Spring"]
CLEAR_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0.50, Season := "Summer"]
CLEAR_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0.75, Season := "Autumn"]
CLOUD_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0   , Season := "Winter"]
CLOUD_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0.25, Season := "Spring"]
CLOUD_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0.50, Season := "Summer"]
CLOUD_1_D_monthly_DESEAS[season_Yqrt %% 1 == 0.75, Season := "Autumn"]


## _ Create variables by season from daily means -------------------------------
ALL_1_bySeason_monthly_mean <-
  ALL_1_D_monthly_DESEAS[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                            GLB_att    = mean(GLB_att,    na.rm = T),
                            HOR_att    = mean(HOR_att,    na.rm = T),
                            DIR_transp = mean(DIR_transp, na.rm = T),
                            DIR_att_sd = sd(  DIR_att,    na.rm = T),
                            HOR_att_sd = sd(  HOR_att,    na.rm = T),
                            GLB_att_sd = sd(  GLB_att,    na.rm = T),
                            GLB_att_N  = sum(!is.na(GLB_att)),
                            HOR_att_N  = sum(!is.na(HOR_att)),
                            DIR_att_N  = sum(!is.na(DIR_att)),
                            minDate    = min(Date),
                            maxDate    = max(Date),
                            medDate    = median(Date)    ),
                         by = .( Yqrt = season_Yqrt) ]

CLEAR_1_bySeason_monthly_mean <-
  CLEAR_1_D_monthly_DESEAS[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                              GLB_att    = mean(GLB_att,    na.rm = T),
                              HOR_att    = mean(HOR_att,    na.rm = T),
                              DIR_transp = mean(DIR_transp, na.rm = T),
                              DIR_att_sd = sd(  DIR_att,    na.rm = T),
                              HOR_att_sd = sd(  HOR_att,    na.rm = T),
                              GLB_att_sd = sd(  GLB_att,    na.rm = T),
                              GLB_att_N  = sum(!is.na(GLB_att)),
                              HOR_att_N  = sum(!is.na(HOR_att)),
                              DIR_att_N  = sum(!is.na(DIR_att)),
                              minDate    = min(Date),
                              maxDate    = max(Date),
                              medDate    = median(Date)    ),
                           by = .( Yqrt = season_Yqrt) ]

CLOUD_1_bySeason_monthly_mean <-
    CLOUD_1_D_monthly_DESEAS[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                                GLB_att    = mean(GLB_att,    na.rm = T),
                                HOR_att    = mean(HOR_att,    na.rm = T),
                                DIR_transp = mean(DIR_transp, na.rm = T),
                                DIR_att_sd = sd(  DIR_att,    na.rm = T),
                                HOR_att_sd = sd(  HOR_att,    na.rm = T),
                                GLB_att_sd = sd(  GLB_att,    na.rm = T),
                                GLB_att_N  = sum(!is.na(GLB_att)),
                                HOR_att_N  = sum(!is.na(HOR_att)),
                                DIR_att_N  = sum(!is.na(DIR_att)),
                                minDate    = min(Date),
                                maxDate    = max(Date),
                                medDate    = median(Date)    ),
                             by = .( Yqrt = season_Yqrt) ]

# stop("DD")
## Flag seasons using quarters
ALL_1_bySeason_monthly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
ALL_1_bySeason_monthly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
ALL_1_bySeason_monthly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
ALL_1_bySeason_monthly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
CLEAR_1_bySeason_monthly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
CLEAR_1_bySeason_monthly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
CLEAR_1_bySeason_monthly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
CLEAR_1_bySeason_monthly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
CLOUD_1_bySeason_monthly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
CLOUD_1_bySeason_monthly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
CLOUD_1_bySeason_monthly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
CLOUD_1_bySeason_monthly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]






## _ Seasonal by season daily values -------------------------------------------

ALL_1_bySeason_monthly_seas <-
    ALL_1_bySeason_monthly_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                                   GLB_att_seas    = mean(GLB_att,    na.rm = T),
                                   HOR_att_seas    = mean(HOR_att,    na.rm = T),
                                   DIR_transp_seas = mean(DIR_transp, na.rm = T),
                                   DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                                   HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                                   GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                                   GLB_att_N_seas  = sum(!is.na(GLB_att)),
                                   HOR_att_N_seas  = sum(!is.na(HOR_att)),
                                   DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                                by = .(Season)]

CLEAR_1_bySeason_monthly_seas <-
    CLEAR_1_bySeason_monthly_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                                     GLB_att_seas    = mean(GLB_att,    na.rm = T),
                                     HOR_att_seas    = mean(HOR_att,    na.rm = T),
                                     DIR_transp_seas = mean(DIR_transp, na.rm = T),
                                     DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                                     HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                                     GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                                     GLB_att_N_seas  = sum(!is.na(GLB_att)),
                                     HOR_att_N_seas  = sum(!is.na(HOR_att)),
                                     DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                                  by = .(Season)]

CLOUD_1_bySeason_monthly_seas <-
    CLOUD_1_bySeason_monthly_mean[,.(DIR_att_seas    = mean(DIR_att,    na.rm = T),
                                     GLB_att_seas    = mean(GLB_att,    na.rm = T),
                                     HOR_att_seas    = mean(HOR_att,    na.rm = T),
                                     DIR_transp_seas = mean(DIR_transp, na.rm = T),
                                     DIR_att_sd_seas = sd(  DIR_att,    na.rm = T),
                                     HOR_att_sd_seas = sd(  HOR_att,    na.rm = T),
                                     GLB_att_sd_seas = sd(  GLB_att,    na.rm = T),
                                     GLB_att_N_seas  = sum(!is.na(GLB_att)),
                                     HOR_att_N_seas  = sum(!is.na(HOR_att)),
                                     DIR_att_N_seas  = sum(!is.na(DIR_att))  ),
                                  by = .(Season)]



## _ De-seasonal by season daily mean  -----------------------------------------

ALL_1_M_bySeason_DESEAS <- merge(  ALL_1_bySeason_monthly_mean,   ALL_1_bySeason_monthly_seas, by = "Season", all = T)
CLEAR_1_M_bySeason_DESEAS <- merge(CLEAR_1_bySeason_monthly_mean, CLEAR_1_bySeason_monthly_seas, by = "Season", all = T)
CLOUD_1_M_bySeason_DESEAS <- merge(CLOUD_1_bySeason_monthly_mean, CLOUD_1_bySeason_monthly_seas, by = "Season", all = T)


rm(  ALL_1_bySeason_monthly_mean,   ALL_1_bySeason_monthly_seas,
     CLEAR_1_bySeason_monthly_mean, CLEAR_1_bySeason_monthly_seas,
     CLOUD_1_bySeason_monthly_mean, CLOUD_1_bySeason_monthly_seas)
dummy <- gc()

## calculate anomaly
ALL_1_M_bySeason_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
ALL_1_M_bySeason_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
ALL_1_M_bySeason_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
ALL_1_M_bySeason_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLEAR_1_M_bySeason_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLEAR_1_M_bySeason_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLEAR_1_M_bySeason_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLEAR_1_M_bySeason_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLOUD_1_M_bySeason_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLOUD_1_M_bySeason_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLOUD_1_M_bySeason_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLOUD_1_M_bySeason_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]

## Create year from quarter!
warning("Years in by Season are shifted by a month to match seasons")
ALL_1_M_bySeason_DESEAS[, Year := year(Yqrt)]
CLEAR_1_M_bySeason_DESEAS[, Year := year(Yqrt)]
CLOUD_1_M_bySeason_DESEAS[, Year := year(Yqrt)]





