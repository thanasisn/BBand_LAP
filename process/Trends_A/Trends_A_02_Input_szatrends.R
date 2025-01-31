

## Data input for this paper

require(data.table)
require(zoo)
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")
source("./DHI_GHI_0_variables.R")
Script.Name <- "DHI_GHI_02_Input_szatrends.R"

if (!interactive()) {
    pdf( file = paste0("./runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("./runtime/",  basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
    filelock::lock(paste0("./runtime/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}

##  Prepare raw data if needed  ------------------------------------------------
## check previous steps
if (
    file.exists(raw_input_data) == FALSE |
    file.mtime(raw_input_data) < file.mtime("./DHI_GHI_0_variables.R") |
    file.mtime(raw_input_data) < file.mtime("./DHI_GHI_00_raw_data.R")
) {
    source("./DHI_GHI_00_raw_data.R")
    dummy <- gc()
}

## check current steps
if (
    file.exists(I2_szatrend) == FALSE |
    file.mtime(I2_szatrend) < file.mtime("./DHI_GHI_02_Input_szatrends.R")
) {
    cat(paste("\n Have to create SZA trends proccessed data\n\n"))
} else {
    cat(paste("\n SZA trends proccessed data are ready\n\n"))
    cond = structure(list(message = "SZA trends proccessed data are ready"),
                     class = c("exit", "condition"))
    signalCondition(cond)
    stop("Normal to exit here ")
}

##  Load raw data  -------------------------------------------------------------
DATA_all   <- readRDS(raw_input_data)
DATA_Clear <- DATA_all[TYPE == "Clear"]
DATA_Cloud <- DATA_all[TYPE == "Cloud"]

DATA_all  [, TYPE := NULL]
DATA_Clear[, TYPE := NULL]
DATA_Cloud[, TYPE := NULL]

#
# warning("REMOVING CLOUD ENHANCEMENT")
# DATA_Cloud <- DATA_Cloud[wattGLB < cosde(SZA) * TSIextEARTH_comb * 0.8 + 30 ]



# ......................................................................... ----
####  2. Long term by SZA  -----------------------------------------------------


## SZA test --------------------------------------------------------------------
# test_all <- DATA_all[, .(GLB_att       = mean(GLB_att,    na.rm = T),
#                          GLB_att_sd    = sd(  GLB_att,    na.rm = T),
#                          GLB_att_N     = sum(!is.na(GLB_att))),
#                      by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
#                             Date    = as.Date(Date),
#                             preNoon = preNoon ) ]
#
# test_all_year <- DATA_all[, .(GLB_att       = mean(GLB_att,    na.rm = T),
#                               GLB_att_sd    = sd(  GLB_att,    na.rm = T),
#                               GLB_att_N     = sum(!is.na(GLB_att))),
#                           by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
#                                  Year    = year(Date),
#                                  preNoon = preNoon ) ]
#
# test_all_monthly <- DATA_all[, .(GLB_att       = mean(GLB_att,    na.rm = T),
#                                  GLB_att_sd    = sd(  GLB_att,    na.rm = T),
#                                  GLB_att_N     = sum(!is.na(GLB_att))),
#                              by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
#                                     Year    = year(Date),
#                                     Month   = month(Date),
#                                     preNoon = preNoon ) ]
# test_all_monthly[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
#
# gather1 <- data.table()
# for (asza in unique(test_all_year$SZA)) {
#     for (pday in unique(test_all_year$preNoon)) {
#         subdata <- test_all_year[SZA == asza & preNoon == pday]
#         ll1     <- coefficients(lm(as.numeric(Year) ~ GLB_att, data = subdata))
#         gather1  <- rbind(gather1,
#                          data.frame(t(ll1),
#                                     SZA = asza,
#                                     preNoon = pday)
#         )
#     }
# }
# plot(gather1$SZA, gather1$GLB_att)
#
#
# gather2 <- data.table()
# for (asza in unique(test_all$SZA)) {
#     for (pday in unique(test_all$preNoon)) {
#         subdata <- test_all[SZA == asza & preNoon == pday]
#         ll1     <- coefficients(lm(as.numeric(Date) ~ GLB_att, data = subdata))
#         gather2  <- rbind(gather2,
#                          data.frame(t(ll1),
#                                     SZA = asza,
#                                     preNoon = pday)
#         )
#     }
# }
# plot(gather2$SZA, gather2$GLB_att)
#
#
# gather3 <- data.table()
# for (asza in unique(test_all$SZA)) {
#     for (pday in unique(test_all_monthly$preNoon)) {
#         subdata <- test_all_monthly[SZA == asza & preNoon == pday]
#         ll1     <- coefficients(lm(as.numeric(Date) ~ GLB_att, data = subdata))
#         gather3  <- rbind(gather3,
#                           data.frame(t(ll1),
#                                      SZA = asza,
#                                      preNoon = pday)
#         )
#     }
# }
# plot(gather3$SZA, gather3$GLB_att)


## SZA trends new approach -----------------------------------------------------

# ## create bin id
# DATA_all[,   SZAbin := (SZA - SZA_BIN / 2 ) %/% SZA_BIN ]
# DATA_Clear[, SZAbin := (SZA - SZA_BIN / 2 ) %/% SZA_BIN ]
# DATA_Cloud[, SZAbin := (SZA - SZA_BIN / 2 ) %/% SZA_BIN ]
#
#
# datadbs    <- c("DATA_all", "DATA_Clear", "DATA_Cloud")
# datadbs    <- c("DATA_all")
#
# szabins    <- unique(DATA_all$SZAbin)
# dayparts   <- unique(DATA_all$preNoon)
# gridsearch <- expand.grid(dbs     = datadbs,
#                           SZA     = szabins,
#                           preNoon = dayparts,
#                           stringsAsFactors = F)
# source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
#
# SZA_slope     <- data.table()
# SZA_slope_inv <- data.table()
#
# for (ii in 1:nrow(gridsearch)) {
#     dd      <- gridsearch[ii,]
#     DB      <- get(dd$dbs)
#     subdata <- DB[SZAbin == dd$SZA & preNoon == dd$preNoon]
#
#     DDaily <- subdata[, .(GLB_att   = mean(GLB_att, na.rm = T),
#                           GLB_att_N = sum(!is.na(GLB_att)) ),
#                       by = .(Date = as.Date(Date))]
#
#     DMonthly <- subdata[, .(GLB_att   = mean(GLB_att, na.rm = T),
#                             GLB_att_N = sum(!is.na(GLB_att)) ),
#                         by = .(Year  = year(Date),
#                                Month = month(Date))]
#     DMonthly[, Date := as.Date(paste(Year, Month, 1), "%Y %m %d") ]
#
#     DYearly <- subdata[, .(GLB_att   = mean(GLB_att, na.rm = T),
#                            GLB_att_N = sum(!is.na(GLB_att)) ),
#                        by = .(Year = year(Date))]
#
#     # lmD <- linear_fit_stats(lm(as.numeric(Date) ~ GLB_att, data = DDaily))
#     # lmM <- linear_fit_stats(lm(as.numeric(Date) ~ GLB_att, data = DMonthly))
#     # lmY <- linear_fit_stats(lm(as.numeric(Year) ~ GLB_att, data = DYearly))
#
#     lmD_inv <- linear_fit_stats(lm(GLB_att ~ as.numeric(Date), data = DDaily))
#     lmM_inv <- linear_fit_stats(lm(GLB_att ~ as.numeric(Date), data = DMonthly))
#     lmY_inv <- linear_fit_stats(lm(GLB_att ~ as.numeric(Year), data = DYearly))
#
#     ##plots
#     plot(DDaily$Date, DDaily$GLB_att)
#     title(paste("Daily", dd$dbs, dd$SZA))
#     # abline(lm(as.numeric(Date) ~ GLB_att, data = DDaily),   col = "red"  )
#     abline(lm(GLB_att ~ as.numeric(Date), data = DDaily),   col = "green")
#     abline(a = lmD_inv$intercept,
#            b = lmD_inv$slope, col = "blue", lty = 3)
#
#     plot(DMonthly$Date, DMonthly$GLB_att)
#     title(paste("Monthly", dd$dbs, dd$SZA))
#     # abline(lm(as.numeric(Date) ~ GLB_att, data = DMonthly), col = "red"  )
#     abline(lm(GLB_att ~ as.numeric(Date), data = DMonthly), col = "green")
#     abline(a = lmM_inv$intercept,
#            b = lmM_inv$slope, col = "blue", lty = 3)
#     #
#     #
#     plot(DYearly$Year, DYearly$GLB_att)
#     title(paste("Yearly", dd$dbs, dd$SZA))
#     # abline(lm(as.numeric(Year) ~ GLB_att, data = DYearly),  col = "red"  )
#     abline(lm(GLB_att ~ as.numeric(Year), data = DYearly),  col = "green")
#     abline(a = lmY_inv$intercept,
#            b = lmY_inv$slope, col = "blue", lty = 3)
#
#
#
#     # SZA_slope <-
#     #     rbind(SZA_slope,
#     #           data.frame(lmD,
#     #                      lm_N    = sum(!is.na(DDaily$GLB_att)),
#     #                      SZA     = dd$SZA,
#     #                      preNoon = dd$preNoon,
#     #                      DATA    = dd$dbs,
#     #                      aggr    = "Daily"),
#     #           data.frame(lmM,
#     #                      lm_N    = sum(!is.na(DMonthly$GLB_att)),
#     #                      SZA     = dd$SZA,
#     #                      preNoon = dd$preNoon,
#     #                      DATA    = dd$dbs,
#     #                      aggr    = "Monthly"),
#     #           data.frame(lmY,
#     #                      lm_N    = sum(!is.na(DYearly$GLB_att)),
#     #                      SZA     = dd$SZA,
#     #                      preNoon = dd$preNoon,
#     #                      DATA    = dd$dbs,
#     #                      aggr    = "Yearly"))
#
#     SZA_slope_inv <-
#         rbind(SZA_slope_inv,
#               data.frame(lmD_inv,
#                          lm_N    = sum(!is.na(DDaily$GLB_att)),
#                          SZA     = dd$SZA,
#                          preNoon = dd$preNoon,
#                          DATA    = dd$dbs,
#                          aggr    = "Daily"),
#               data.frame(lmM_inv,
#                          lm_N    = sum(!is.na(DMonthly$GLB_att)),
#                          SZA     = dd$SZA,
#                          preNoon = dd$preNoon,
#                          DATA    = dd$dbs,
#                          aggr    = "Monthly"),
#               data.frame(lmY_inv,
#                          lm_N    = sum(!is.na(DYearly$GLB_att)),
#                          SZA     = dd$SZA,
#                          preNoon = dd$preNoon,
#                          DATA    = dd$dbs,
#                          aggr    = "Yearly"))
#
# }

#_ One way ---------------------------------------------------------------------

## This should be wrong!!!

# SZA_slope[aggr == "Yearly",
#           slopePC := slope  ]
#
# SZA_slope[aggr %in% c("Daily"),
#           slopePC := 100 * slope / Days_of_year ]
#
# SZA_slope[aggr %in% c("Monthly"),
#           slopePC := (100 * slope / Days_of_year) / 12 ]
#
# # SZA_slope[ SZA > 70, slopePC := NA]
# # SZA_slope[ slope.p > 0.1, slopePC := NA]
#
# for (aDATA in unique( SZA_slope$DATA )) {
#     for (aAggr in unique( SZA_slope$aggr )) {
#         pp <- SZA_slope[DATA == aDATA & aggr == aAggr]
#
#         plot(pp$SZA, pp$slopePC )
#         title(paste(aDATA, aAggr))
#
#     }
# }



# for (aDATA in unique( SZA_slope$DATA )) {
#     for (aAggr in  c("Daily", "Monthly")) {
#         pp <- SZA_slope[DATA == aDATA & aggr == aAggr]
#         plot(pp$SZA, pp$slopePC )
#         title(paste(aDATA, aAggr))
#
#     }
# }
#
# for (aDATA in unique( SZA_slope$DATA )) {
#     for (aAggr in c("Daily", "Monthly")) {
#         pp <- SZA_slope[DATA == aDATA & aggr == aAggr]
#
#         plot(pp$SZA, 100 * pp$slope / Days_of_year )
#         title(paste(aDATA, aAggr))
#
#     }
# }
#
# for (aDATA in unique( SZA_slope$DATA )) {
#     for (aAggr in c("Yearly")) {
#         pp <- SZA_slope[DATA == aDATA & aggr == aAggr]
#
#         plot(pp$SZA, 100 * pp$slope )
#         title(paste(aDATA, aAggr))
#
#     }
# }

#_ Inverted way ----------------------------------------------------------------

# for (aDATA in unique( SZA_slope_inv$DATA )) {
#     for (aAggr in unique( SZA_slope_inv$aggr )) {
#         pp <- SZA_slope_inv[DATA == aDATA & aggr == aAggr]
#
#         plot(pp$SZA, pp$slope)
#         title(paste(aDATA, aAggr))
#
#     }
# }


# save.image(file="temp.Rdata")
# stop()
# load(file="temp.Rdata")


##  Daily SZA means ------------------------------------------------------------

## _ daily means  --------------------------------------------------------------
ALL_2_daily_mean <-
    DATA_all[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                 HOR_att       = mean(HOR_att,    na.rm = T),
                 GLB_att       = mean(GLB_att,    na.rm = T),
                 DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                 HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                 GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                 doy           = yday(Date),
                 GLB_att_N     = sum(!is.na(GLB_att)),
                 HOR_att_N     = sum(!is.na(HOR_att)),
                 DIR_att_N     = sum(!is.na(DIR_att))  ),
             by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                    Date    = Day,
                    preNoon = preNoon ) ]

CLEAR_2_daily_mean <-
    DATA_Clear[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                   HOR_att       = mean(HOR_att,    na.rm = T),
                   GLB_att       = mean(GLB_att,    na.rm = T),
                   DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                   HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                   GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                   doy           = yday(Date),
                   GLB_att_N     = sum(!is.na(GLB_att)),
                   HOR_att_N     = sum(!is.na(HOR_att)),
                   DIR_att_N     = sum(!is.na(DIR_att))  ),
               by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                      Date    = Day,
                      preNoon = preNoon ) ]

CLOUD_2_daily_mean <-
    DATA_Cloud[, .(DIR_att       = mean(DIR_att,    na.rm = T),
                   HOR_att       = mean(HOR_att,    na.rm = T),
                   GLB_att       = mean(GLB_att,    na.rm = T),
                   DIR_att_sd    = sd(  DIR_att,    na.rm = T),
                   HOR_att_sd    = sd(  HOR_att,    na.rm = T),
                   GLB_att_sd    = sd(  GLB_att,    na.rm = T),
                   doy           = yday(Date),
                   GLB_att_N     = sum(!is.na(GLB_att)),
                   HOR_att_N     = sum(!is.na(HOR_att)),
                   DIR_att_N     = sum(!is.na(DIR_att))  ),
               by = .(SZA     = (SZA - SZA_BIN / 2 ) %/% SZA_BIN,
                      Date    = Day,
                      preNoon = preNoon ) ]


## _ Aggregation limit with SZA_aggregation_N_lim data points ------------------
ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
ALL_2_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
ALL_2_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
ALL_2_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
ALL_2_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
ALL_2_daily_mean[   DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
ALL_2_daily_mean[   HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
ALL_2_daily_mean[   GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]

CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
CLEAR_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
CLEAR_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
CLEAR_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
CLEAR_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
CLEAR_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
CLEAR_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
CLEAR_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]

CLOUD_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att       := NA ]
CLOUD_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att       := NA ]
CLOUD_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att       := NA ]
CLOUD_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_sd    := NA ]
CLOUD_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_sd    := NA ]
CLOUD_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_sd    := NA ]
CLOUD_2_daily_mean[ DIR_att_N <= SZA_aggregation_N_lim, DIR_att_EM    := NA ]
CLOUD_2_daily_mean[ HOR_att_N <= SZA_aggregation_N_lim, HOR_att_EM    := NA ]
CLOUD_2_daily_mean[ GLB_att_N <= SZA_aggregation_N_lim, GLB_att_EM    := NA ]


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


## _ Margin of error for confidence interval  ----------------------------------
conf_param  <- 1 - (1 - Daily_confidence_limit) / 2
suppressWarnings({
    ALL_2_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)* DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
    ALL_2_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)* HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
    ALL_2_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)* GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
    CLEAR_2_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)* DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
    CLEAR_2_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)* HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
    CLEAR_2_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)* GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
    CLOUD_2_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas -1)* DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
    CLOUD_2_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas -1)* HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
    CLOUD_2_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas -1)* GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
    # CLOUD_2_daily_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas -1)* DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
})


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

rm(   ALL_2_daily_mean,
      CLEAR_2_daily_mean,
      CLOUD_2_daily_mean )
gc()



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

## forget some daily data (want monthly_mean for yearly)
rm(  ALL_2_monthly_seas,
     CLEAR_2_monthly_seas,
     CLOUD_2_monthly_seas)
gc()

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



## forget some daily data (want monthly_mean for yearly)
rm(  ALL_2_monthly_mean,
     CLEAR_2_monthly_mean,
     CLOUD_2_monthly_mean)
gc()





## _ Margin of error calculation for confidence interval -----------------------
# conf_param  <- 1 - ( 1 - SZA_confidence_limit ) / 2
# suppressWarnings({
#     ALL_2_daily_mean[,   DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
#     ALL_2_daily_mean[,   HOR_att_EM   := qt(conf_param,df=HOR_att_N -1) * HOR_att_sd    / sqrt(HOR_att_N)]
#     ALL_2_daily_mean[,   GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
#     # ALL_2_daily_mean[,   DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
#     CLEAR_2_daily_mean[, DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
#     CLEAR_2_daily_mean[, HOR_att_EM   := qt(conf_param,df=HOR_att_N -1) * HOR_att_sd    / sqrt(HOR_att_N)]
#     CLEAR_2_daily_mean[, GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
#     # CLEAR_2_daily_mean[, DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
#     CLOUD_2_daily_mean[, DIR_att_EM   := qt(conf_param,df=DIR_att_N -1) * DIR_att_sd    / sqrt(DIR_att_N)]
#     CLOUD_2_daily_mean[, HOR_att_EM   := qt(conf_param,df=HOR_att_N -1) * HOR_att_sd    / sqrt(HOR_att_N)]
#     CLOUD_2_daily_mean[, GLB_att_EM   := qt(conf_param,df=GLB_att_N -1) * GLB_att_sd    / sqrt(GLB_att_N)]
#     # CLOUD_2_daily_mean[, DIR_transp_EM:= qt(conf_param,df=DIR_att_N -1) * DIR_transp_sd / sqrt(DIR_att_N)]
# })



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

#   ALL_2_bySeason_yearly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
#   ALL_2_bySeason_yearly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
#   ALL_2_bySeason_yearly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
#   ALL_2_bySeason_yearly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
# CLEAR_2_bySeason_yearly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
# CLEAR_2_bySeason_yearly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
# CLEAR_2_bySeason_yearly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
# CLEAR_2_bySeason_yearly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]
# CLOUD_2_bySeason_yearly_mean[Yqrt %% 1 == 0   , Season := "Winter"]
# CLOUD_2_bySeason_yearly_mean[Yqrt %% 1 == 0.25, Season := "Spring"]
# CLOUD_2_bySeason_yearly_mean[Yqrt %% 1 == 0.50, Season := "Summer"]
# CLOUD_2_bySeason_yearly_mean[Yqrt %% 1 == 0.75, Season := "Autumn"]





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

setorder(  ALL_2_bySeason_daily_DESEAS, Yqrt)
setorder(CLEAR_2_bySeason_daily_DESEAS, Yqrt)
setorder(CLOUD_2_bySeason_daily_DESEAS, Yqrt)

rm(  ALL_2_bySeason_daily_seas,
     CLEAR_2_bySeason_daily_seas,
     CLOUD_2_bySeason_daily_seas)
gc()



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

setorder(  ALL_2_bySeason_monthly_DESEAS, Date)
setorder(CLEAR_2_bySeason_monthly_DESEAS, Date)
setorder(CLOUD_2_bySeason_monthly_DESEAS, Date)

## forget monthly data
rm(  ALL_2_bySeason_monthly_mean,   ALL_2_bySeason_monthly_seas,
     CLEAR_2_bySeason_monthly_mean, CLEAR_2_bySeason_monthly_seas,
     CLOUD_2_bySeason_monthly_mean, CLOUD_2_bySeason_monthly_seas)


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


# ......................................................................... ----
##  Save data ------------------------------------------------------------------
save(file = I2_szatrend,
     list = ls(pattern = "^ALL_2_|^CLEAR_2_|^CLOUD_2_"),
     compress = "xz")
cat(paste("\n SZA trends proccessed data saved", I2_szatrend, "\n\n"))
