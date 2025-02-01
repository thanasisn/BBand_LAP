

## Data input for this paper

## to force a rebuild of the dataset remove stored
# file.remove(common_data)

require(data.table)
require(zoo)
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")
source("./DHI_GHI_0_variables.R")
Script.Name <- "DHI_GHI_03_Input_consistency.R"

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
    file.exists(I3_trendsconsist) == FALSE |
    file.mtime(I3_trendsconsist) < file.mtime("./DHI_GHI_03_Input_consistency.R")
) {
    cat(paste("\n Have to create trends consistency data\n\n"))
} else {
    cat(paste("\n trends consistency data are ready\n\n"))
    cond = structure(list(message = "trends consistency data are ready"),
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


# warning("REMOVING CLOUD ENHANCEMENT")
# DATA_Cloud <- DATA_Cloud[wattGLB < cosde(SZA) * TSIextEARTH_comb * 0.8 + 30 ]




# ......................................................................... ----
####  3. Consistency of trends  ################################################

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

## _ Margin of error calculation  ----------------------------------------------
conf_param  <- 1 - ( 1 - Monthly_confidence_limit ) / 2
suppressWarnings({
    ALL_3_monthly_mean[,  DIR_att_EM   :=qt(conf_param,df=DIR_att_N -1)* DIR_att_sd   /sqrt(DIR_att_N)]
    ALL_3_monthly_mean[,  HOR_att_EM   :=qt(conf_param,df=HOR_att_N -1)* HOR_att_sd   /sqrt(HOR_att_N)]
    ALL_3_monthly_mean[,  GLB_att_EM   :=qt(conf_param,df=GLB_att_N -1)* GLB_att_sd   /sqrt(GLB_att_N)]
    ALL_3_monthly_mean[,  DIR_transp_EM:=qt(conf_param,df=DIR_att_N -1)* DIR_transp_sd/sqrt(DIR_att_N)]
    CLEAR_3_monthly_mean[,DIR_att_EM   :=qt(conf_param,df=DIR_att_N -1)* DIR_att_sd   /sqrt(DIR_att_N)]
    CLEAR_3_monthly_mean[,HOR_att_EM   :=qt(conf_param,df=HOR_att_N -1)* HOR_att_sd   /sqrt(HOR_att_N)]
    CLEAR_3_monthly_mean[,GLB_att_EM   :=qt(conf_param,df=GLB_att_N -1)* GLB_att_sd   /sqrt(GLB_att_N)]
    CLEAR_3_monthly_mean[,DIR_transp_EM:=qt(conf_param,df=DIR_att_N -1)* DIR_transp_sd/sqrt(DIR_att_N)]
    CLOUD_3_monthly_mean[,DIR_att_EM   :=qt(conf_param,df=DIR_att_N -1)* DIR_att_sd   /sqrt(DIR_att_N)]
    CLOUD_3_monthly_mean[,HOR_att_EM   :=qt(conf_param,df=HOR_att_N -1)* HOR_att_sd   /sqrt(HOR_att_N)]
    CLOUD_3_monthly_mean[,GLB_att_EM   :=qt(conf_param,df=GLB_att_N -1)* GLB_att_sd   /sqrt(GLB_att_N)]
    CLOUD_3_monthly_mean[,DIR_transp_EM:=qt(conf_param,df=DIR_att_N -1)* DIR_transp_sd/sqrt(DIR_att_N)]
})

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



## forget original data
rm(DATA_all)
rm(DATA_Clear)
rm(DATA_Cloud)





# #### run on all quarter of the hour
# ayear$quarter <- ((as.numeric( ayear$Date ) %/% (3600/4) ) )
#
# selectqua  <- list(ayear$quarter)
#
# qDates     <- aggregate(ayear$Date,       by = selectqua, FUN = min)
#
# qGlobal    <- aggregate(ayear$wattGLB,    by = selectqua, FUN = mean, na.rm = TRUE )
# qGlobalCNT <- aggregate(ayear$wattGLB,    by = selectqua, FUN = function(x) sum(!is.na(x)) )
# qGlobalSTD <- aggregate(ayear$wattGLB,    by = selectqua, FUN = sd,   na.rm = TRUE )
#
# qElevaMEAN <- aggregate(ayear$Eleva,      by = selectqua, FUN = mean, na.rm = TRUE )
#
# qGLstd     <- aggregate(ayear$wattGLB_SD, by = selectqua, FUN = mean, na.rm = TRUE )
# qGLstdCNT  <- aggregate(ayear$wattGLB_SD, by = selectqua, FUN = function(x) sum(!is.na(x)) )
# qGLstdSTD  <- aggregate(ayear$wattGLB_SD, by = selectqua, FUN = sd,   na.rm = TRUE )
#
# #### output of quarterly data
# ayearquarter <- data.frame( Dates      = qDates$x,
#                             qGlobal    = qGlobal$x,
#                             qGlobalCNT = qGlobalCNT$x,
#                             qGlobalSTD = qGlobalSTD$x,
#                             qElevaMEAN = qElevaMEAN$x,
#                             qGLstd     = qGLstd$x,
#                             qGLstdCNT  = qGLstdCNT$x,
#                             qGLstdSTD  = qGLstdSTD$x)
#
# #### run on 4 quarters of every hour
# ayearquarter$hourly <- as.numeric( ayearquarter$Dates ) %/% 3600
# hposic              <- as.POSIXct( ayearquarter$hourly * 3600, origin = "1970-01-01" )
#
# selecthour <- list(ayearquarter$hourly)
#
# hDates     <- aggregate( ayearquarter$Dates,   by = selecthour, FUN = min )
#
# hGlobal    <- aggregate( ayearquarter$qGlobal, by = selecthour, FUN = mean, na.rm = FALSE )  ## na.rm must be FALSE!
# hGlobalCNT <- aggregate( ayearquarter$qGlobal, by = selecthour, FUN = function(x) sum(!is.na(x)))


# ......................................................................... ----
##  Save data ------------------------------------------------------------------
save(file = I3_trendsconsist,
     list = ls(pattern = "^ALL_3_|^CLEAR_3_|^CLOUD_3_"),
     compress = "xz")
cat(paste("\n trends consistency data saved", I3_trendsconsist, "\n\n"))
