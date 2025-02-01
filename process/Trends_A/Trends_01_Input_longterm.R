#+ echo=T, include=T

require(data.table, quietly = TRUE, warn.conflicts = FALSE)
require(zoo,        quietly = TRUE, warn.conflicts = FALSE)
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
source("~/CODE/FUNCTIONS/R/cor_test_stats.R")
source("./DHI_GHI_0_variables.R")
source("./var_translation.R")

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
    file.exists(I1_longterm) == FALSE |
    file.mtime(I1_longterm) < file.mtime("./DHI_GHI_01_Input_longterm.R")
) {
    cat(paste("\n Have to create Long term proccessed data\n\n"))
} else {
    cat(paste("\n Long term proccessed data are ready\n\n"))
    cond = structure(list(message = "Long term proccessed data are already done"),
                     class = c("exit", "condition"))
    signalCondition(cond)
    stop("Normal to exit here")
}

#+ echo=T, include=T
##  Load raw data  -------------------------------------------------------------
DATA_all   <- readRDS(raw_input_data)

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








##  Daily means  ---------------------------------------------------------------

ALL_1_daily_mean <-
    DATA_all[,.(
        DIR_att       = mean(DIR_att,      na.rm = T),
        GLB_att       = mean(GLB_att,      na.rm = T),
        HOR_att       = mean(HOR_att,      na.rm = T),
        near_tcc_att  = mean(near_tcc,     na.rm = T),
        bilin_tcc_att = mean(bilin_tcc,    na.rm = T),
        near_tcc_zero_att   = mean(near_tcc_zero,   na.rm = T),
        near_tcc_NOzero_att = mean(near_tcc_NOzero, na.rm = T),
        near_tcc_clear_att  = mean(near_tcc_clear,  na.rm = T),
        near_tcc_cloud_att  = mean(near_tcc_cloud,  na.rm = T),
        wattGLB       = mean(wattGLB,      na.rm = T),
        wattGLB_sd    = sd(  wattGLB,      na.rm = T),
        wattGLB_N     = sum(!is.na(wattGLB)),
        DIR_transp    = mean(DIR_transp,   na.rm = T),
        tsi1au_att    = mean(tsi_1au_comb, na.rm = T),
        DIR_att_sd    = sd(  DIR_att,      na.rm = T),
        GLB_att_sd    = sd(  GLB_att,      na.rm = T),
        HOR_att_sd    = sd(  HOR_att,      na.rm = T),
        DIR_transp_sd = sd(  DIR_transp,   na.rm = T),
        tsi1au_att_sd = sd(  tsi_1au_comb, na.rm = T),
        DayLength     = max(DayLength),
        doy           = yday(Date),
        GLB_att_N     = sum(!is.na(GLB_att)),
        HOR_att_N     = sum(!is.na(HOR_att)),
        DIR_att_N     = sum(!is.na(DIR_att))
    ),
    by = .( Date = Day ) ]


CLEAR_1_daily_mean <-
    DATA_Clear[,.(
        DIR_att       = mean(DIR_att,      na.rm = T),
        GLB_att       = mean(GLB_att,      na.rm = T),
        HOR_att       = mean(HOR_att,      na.rm = T),
        wattGLB       = mean(wattGLB,      na.rm = T),
        wattGLB_sd    = sd(  wattGLB,      na.rm = T),
        wattGLB_N     = sum(!is.na(wattGLB)),
        DIR_transp    = mean(DIR_transp,   na.rm = T),
        tsi1au_att    = mean(tsi_1au_comb, na.rm = T),
        DIR_att_sd    = sd(  DIR_att,      na.rm = T),
        GLB_att_sd    = sd(  GLB_att,      na.rm = T),
        HOR_att_sd    = sd(  HOR_att,      na.rm = T),
        DIR_transp_sd = sd(  DIR_transp,   na.rm = T),
        tsi1au_att_sd = sd(  tsi_1au_comb, na.rm = T),
        DayLength     = max(DayLength),
        doy           = yday(Date),
        GLB_att_N     = sum(!is.na(GLB_att)),
        HOR_att_N     = sum(!is.na(HOR_att)),
        DIR_att_N     = sum(!is.na(DIR_att))
    ),
    by = .( Date = Day ) ]

CLOUD_1_daily_mean <-
    DATA_Cloud[,.(
        DIR_att       = mean(DIR_att,      na.rm = T),
        GLB_att       = mean(GLB_att,      na.rm = T),
        HOR_att       = mean(HOR_att,      na.rm = T),
        wattGLB       = mean(wattGLB,      na.rm = T),
        wattGLB_sd    = sd(  wattGLB,      na.rm = T),
        wattGLB_N     = sum(!is.na(wattGLB)),
        DIR_transp    = mean(DIR_transp,   na.rm = T),
        tsi1au_att    = mean(tsi_1au_comb, na.rm = T),
        DIR_att_sd    = sd(  DIR_att,      na.rm = T),
        GLB_att_sd    = sd(  GLB_att,      na.rm = T),
        HOR_att_sd    = sd(  HOR_att,      na.rm = T),
        DIR_transp_sd = sd(  DIR_transp,   na.rm = T),
        tsi1au_att_sd = sd(  tsi_1au_comb, na.rm = T),
        DayLength     = max(DayLength),
        doy           = yday(Date),
        GLB_att_N     = sum(!is.na(GLB_att)),
        HOR_att_N     = sum(!is.na(HOR_att)),
        DIR_att_N     = sum(!is.na(DIR_att))
    ),
    by = .( Date = Day ) ]






## _ Exclude days with few data for Clear and cloud  ---------------------------
hist(CLEAR_1_daily_mean[, GLB_att_N / DayLength], breaks = 100)
abline(v = Clear_daily_ratio_lim, col = "red")

hist(CLOUD_1_daily_mean[, GLB_att_N / DayLength], breaks = 100)
abline(v = Cloud_daily_ratio_lim, col = "red")


# ## proper way to apply filter
# CLEAR_1_daily_mean[GLB_att_N / DayLength < Clear_daily_ratio_lim, GLB_att    := NA]
# CLEAR_1_daily_mean[GLB_att_N / DayLength < Clear_daily_ratio_lim, GLB_att_N  := NA]
# CLEAR_1_daily_mean[GLB_att_N / DayLength < Clear_daily_ratio_lim, GLB_att_sd := NA]
# CLOUD_1_daily_mean[GLB_att_N / DayLength < Cloud_daily_ratio_lim, GLB_att    := NA]
# CLOUD_1_daily_mean[GLB_att_N / DayLength < Cloud_daily_ratio_lim, GLB_att_N  := NA]
# CLOUD_1_daily_mean[GLB_att_N / DayLength < Cloud_daily_ratio_lim, GLB_att_sd := NA]

## HACK !!!!
warning("This breaks other variables for Clear and Cloud!!")
CLEAR_1_daily_mean <- CLEAR_1_daily_mean[!is.na(GLB_att) & GLB_att_N / DayLength > Clear_daily_ratio_lim, ]
CLOUD_1_daily_mean <- CLOUD_1_daily_mean[!is.na(GLB_att) & GLB_att_N / DayLength > Cloud_daily_ratio_lim, ]
## HACK !!!!




## _ Daily seasonal values from daily ------------------------------------------
ALL_1_daily_seas <-
    ALL_1_daily_mean[,.(
        DIR_att_seas             = mean(DIR_att,    na.rm = T),
        GLB_att_seas             = mean(GLB_att,    na.rm = T),
        HOR_att_seas             = mean(HOR_att,    na.rm = T),
        near_tcc_att_seas        = mean(near_tcc_att,   na.rm = T),
        bilin_tcc_att_seas       = mean(bilin_tcc_att,  na.rm = T),
        near_tcc_zero_att_seas   = mean(near_tcc_zero_att,   na.rm = T),
        near_tcc_NOzero_att_seas = mean(near_tcc_NOzero_att, na.rm = T),
        near_tcc_clear_att_seas  = mean(near_tcc_clear_att,  na.rm = T),
        near_tcc_cloud_att_seas  = mean(near_tcc_cloud_att,  na.rm = T),
        wattGLB_seas             = mean(wattGLB,    na.rm = T),
        wattGLB_sd_seas          = sd(  wattGLB,    na.rm = T),
        wattGLB_N_seas           = sum(!is.na(wattGLB)),
        DIR_transp_seas          = mean(DIR_transp, na.rm = T),
        DIR_att_sd_seas          = sd(  DIR_att,    na.rm = T),
        HOR_att_sd_seas          = sd(  HOR_att,    na.rm = T),
        GLB_att_sd_seas          = sd(  GLB_att,    na.rm = T),
        DIR_transp_sd_seas       = sd(  DIR_transp, na.rm = T),
        DIR_att_N_obs            = sum( DIR_att_N,  na.rm = T),
        GLB_att_N_obs            = sum( GLB_att_N,  na.rm = T),
        HOR_att_N_obs            = sum( HOR_att_N,  na.rm = T),
        GLB_att_N_seas           = sum(!is.na(GLB_att)),
        HOR_att_N_seas           = sum(!is.na(HOR_att)),
        DIR_att_N_seas           = sum(!is.na(DIR_att))
    ),
    by = .( doy ) ]

CLEAR_1_daily_seas <-
    CLEAR_1_daily_mean[,.(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                          GLB_att_seas       = mean(GLB_att,    na.rm = T),
                          HOR_att_seas       = mean(HOR_att,    na.rm = T),
                          wattGLB_seas       = mean(wattGLB,      na.rm = T),
                          wattGLB_sd_seas    = sd(  wattGLB,      na.rm = T),
                          wattGLB_N_seas     = sum(!is.na(wattGLB)),
                          DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                          DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                          HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                          GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                          DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                          DIR_att_N_obs      = sum( DIR_att_N,  na.rm = T),
                          GLB_att_N_obs      = sum( GLB_att_N,  na.rm = T),
                          HOR_att_N_obs      = sum( HOR_att_N,  na.rm = T),
                          GLB_att_N_seas     = sum(!is.na(GLB_att)),
                          HOR_att_N_seas     = sum(!is.na(HOR_att)),
                          DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                       by = .( doy ) ]

CLOUD_1_daily_seas <-
    CLOUD_1_daily_mean[,.(DIR_att_seas       = mean(DIR_att,    na.rm = T),
                          GLB_att_seas       = mean(GLB_att,    na.rm = T),
                          HOR_att_seas       = mean(HOR_att,    na.rm = T),
                          wattGLB_seas       = mean(wattGLB,      na.rm = T),
                          wattGLB_sd_seas    = sd(  wattGLB,      na.rm = T),
                          wattGLB_N_seas     = sum(!is.na(wattGLB)),
                          DIR_transp_seas    = mean(DIR_transp, na.rm = T),
                          DIR_att_sd_seas    = sd(  DIR_att,    na.rm = T),
                          HOR_att_sd_seas    = sd(  HOR_att,    na.rm = T),
                          GLB_att_sd_seas    = sd(  GLB_att,    na.rm = T),
                          DIR_transp_sd_seas = sd(  DIR_transp, na.rm = T),
                          DIR_att_N_obs      = sum( DIR_att_N,  na.rm = T),
                          GLB_att_N_obs      = sum( GLB_att_N,  na.rm = T),
                          HOR_att_N_obs      = sum( HOR_att_N,  na.rm = T),
                          GLB_att_N_seas     = sum(!is.na(GLB_att)),
                          HOR_att_N_seas     = sum(!is.na(HOR_att)),
                          DIR_att_N_seas     = sum(!is.na(DIR_att))  ),
                       by = .( doy ) ]



plot(ALL_1_daily_seas[, GLB_att_seas, doy],pch= 1, cex = 1,
     main = "ALL_1_daily_seas[, GLB_att_seas, doy]")
cat(" \n \n")


plot(CLEAR_1_daily_seas[, GLB_att_seas, doy],pch= 1, cex = 1,
     main = "CLEAR_1_daily_seas[, GLB_att_seas, doy]")
cat(" \n \n")

plot(CLOUD_1_daily_seas[, GLB_att_seas, doy],pch= 1, cex = 1,
     main = "CLOUD_1_daily_seas[, GLB_att_seas, doy]")
cat(" \n \n")


## _ Margin of error for confidence interval on seasonal data ------------------
conf_param  <- 1 - ( 1 - Daily_confidence_limit ) / 2
suppressWarnings({
    ALL_1_daily_seas[,  DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas-1)*DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
    ALL_1_daily_seas[,  HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas-1)*HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
    ALL_1_daily_seas[,  GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas-1)*GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
    ALL_1_daily_seas[,  DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas-1)*DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
    CLEAR_1_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas-1)*DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
    CLEAR_1_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas-1)*HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
    CLEAR_1_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas-1)*GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
    CLEAR_1_daily_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas-1)*DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
    CLOUD_1_daily_seas[,DIR_att_EM_seas   :=qt(conf_param,df=DIR_att_N_seas-1)*DIR_att_sd_seas   /sqrt(DIR_att_N_seas)]
    CLOUD_1_daily_seas[,HOR_att_EM_seas   :=qt(conf_param,df=HOR_att_N_seas-1)*HOR_att_sd_seas   /sqrt(HOR_att_N_seas)]
    CLOUD_1_daily_seas[,GLB_att_EM_seas   :=qt(conf_param,df=GLB_att_N_seas-1)*GLB_att_sd_seas   /sqrt(GLB_att_N_seas)]
    CLOUD_1_daily_seas[,DIR_transp_EM_seas:=qt(conf_param,df=DIR_att_N_seas-1)*DIR_transp_sd_seas/sqrt(DIR_att_N_seas)]
})


## _ Daily de-seasonal anomaly -------------------------------------------------

ALL_1_daily_DESEAS <- merge(  ALL_1_daily_mean,   ALL_1_daily_seas, by = "doy", all = T)
CLEAR_1_daily_DESEAS <- merge(CLEAR_1_daily_mean, CLEAR_1_daily_seas, by = "doy", all = T)
CLOUD_1_daily_DESEAS <- merge(CLOUD_1_daily_mean, CLOUD_1_daily_seas, by = "doy", all = T)

setorder(  ALL_1_daily_DESEAS, Date)
setorder(CLEAR_1_daily_DESEAS, Date)
setorder(CLOUD_1_daily_DESEAS, Date)

## Using the % departure from seasonal values

ALL_1_daily_DESEAS[, DIR_att_des         := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas   ]
ALL_1_daily_DESEAS[, HOR_att_des         := 100*( HOR_att    - HOR_att_seas    ) / HOR_att_seas   ]
ALL_1_daily_DESEAS[, GLB_att_des         := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas   ]
ALL_1_daily_DESEAS[, DIR_transp_des      := 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas]
ALL_1_daily_DESEAS[, wattGLB_des         := wattGLB             - wattGLB_seas                    ]
ALL_1_daily_DESEAS[, near_tcc_att_des    := near_tcc_att        - near_tcc_att_seas               ]
ALL_1_daily_DESEAS[, bilin_tcc_att_des   := bilin_tcc_att       - bilin_tcc_att_seas              ]
ALL_1_daily_DESEAS[, near_tcc_zero_des   := near_tcc_zero_att   - near_tcc_zero_att_seas          ]
ALL_1_daily_DESEAS[, near_tcc_NOzero_des := near_tcc_NOzero_att - near_tcc_NOzero_att_seas        ]
ALL_1_daily_DESEAS[, near_tcc_clear_des  := near_tcc_clear_att  - near_tcc_clear_att_seas         ]
ALL_1_daily_DESEAS[, near_tcc_cloud_des  := near_tcc_cloud_att  - near_tcc_cloud_att_seas         ]
CLEAR_1_daily_DESEAS[, DIR_att_des       := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas   ]
CLEAR_1_daily_DESEAS[, HOR_att_des       := 100*( HOR_att    - HOR_att_seas    ) / HOR_att_seas   ]
CLEAR_1_daily_DESEAS[, GLB_att_des       := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas   ]
CLEAR_1_daily_DESEAS[, DIR_transp_des    := 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas]
CLEAR_1_daily_DESEAS[, wattGLB_des       := wattGLB - wattGLB_seas]
CLOUD_1_daily_DESEAS[, DIR_att_des       := 100*( DIR_att    - DIR_att_seas    ) / DIR_att_seas   ]
CLOUD_1_daily_DESEAS[, HOR_att_des       := 100*( HOR_att    - HOR_att_seas    ) / HOR_att_seas   ]
CLOUD_1_daily_DESEAS[, GLB_att_des       := 100*( GLB_att    - GLB_att_seas    ) / GLB_att_seas   ]
CLOUD_1_daily_DESEAS[, DIR_transp_des    := 100*( DIR_transp - DIR_transp_seas ) / DIR_transp_seas]
CLOUD_1_daily_DESEAS[, wattGLB_des       := wattGLB - wattGLB_seas]

## add TSI data process
## data departure from mean value
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



## Monthly means from daily means ----------------------------------------------

ALL_1_monthly_daily_mean <-
    ALL_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                        GLB_att    = mean(GLB_att,    na.rm = T),
                        HOR_att    = mean(HOR_att,    na.rm = T),
                        DIR_transp = mean(DIR_transp, na.rm = T),
                        DIR_att_sd = sd(  DIR_att,    na.rm = T),
                        HOR_att_sd = sd(  HOR_att,    na.rm = T),
                        GLB_att_sd = sd(  GLB_att,    na.rm = T),
                        GLB_att_N  = sum(!is.na(GLB_att)),
                        HOR_att_N  = sum(!is.na(HOR_att)),
                        DIR_att_N  = sum(!is.na(DIR_att))  ),
                     by = .(
                         Year = year(Date),
                         Month = month(Date)
                     ) ]

CLEAR_1_monthly_daily_mean <-
    CLEAR_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                          GLB_att    = mean(GLB_att,    na.rm = T),
                          HOR_att    = mean(HOR_att,    na.rm = T),
                          DIR_transp = mean(DIR_transp, na.rm = T),
                          DIR_att_sd = sd(  DIR_att,    na.rm = T),
                          HOR_att_sd = sd(  HOR_att,    na.rm = T),
                          GLB_att_sd = sd(  GLB_att,    na.rm = T),
                          GLB_att_N  = sum(!is.na(GLB_att)),
                          HOR_att_N  = sum(!is.na(HOR_att)),
                          DIR_att_N  = sum(!is.na(DIR_att))  ),
                       by = .(
                           Year  = year(Date),
                           Month = month(Date)
                       ) ]

CLOUD_1_monthly_daily_mean <-
    CLOUD_1_daily_mean[,.(DIR_att    = mean(DIR_att,    na.rm = T),
                          GLB_att    = mean(GLB_att,    na.rm = T),
                          HOR_att    = mean(HOR_att,    na.rm = T),
                          DIR_transp = mean(DIR_transp, na.rm = T),
                          DIR_att_sd = sd(  DIR_att,    na.rm = T),
                          HOR_att_sd = sd(  HOR_att,    na.rm = T),
                          GLB_att_sd = sd(  GLB_att,    na.rm = T),
                          GLB_att_N  = sum(!is.na(GLB_att)),
                          HOR_att_N  = sum(!is.na(HOR_att)),
                          DIR_att_N  = sum(!is.na(DIR_att))  ),
                       by = .(
                           Year  = year(Date),
                           Month = month(Date)
                       ) ]


hist(ALL_1_monthly_daily_mean$GLB_att, breaks = 100)
cat(" \n \n")

hist(CLEAR_1_monthly_daily_mean$GLB_att, breaks = 100)
cat(" \n \n")

hist(CLOUD_1_monthly_daily_mean$GLB_att, breaks = 100)
cat(" \n \n")



## _ Exclude means with less than Monthly_aggegation_N_lim data points ---------
ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att       := NA]
ALL_1_monthly_daily_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att       := NA]
ALL_1_monthly_daily_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att       := NA]
ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp    := NA]
ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_sd    := NA]
ALL_1_monthly_daily_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_sd    := NA]
ALL_1_monthly_daily_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_sd    := NA]
ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_sd := NA]
ALL_1_monthly_daily_mean[   HOR_att_N <= Monthly_aggegation_N_lim, HOR_att_EM    := NA]
ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_att_EM    := NA]
ALL_1_monthly_daily_mean[   GLB_att_N <= Monthly_aggegation_N_lim, GLB_att_EM    := NA]
ALL_1_monthly_daily_mean[   DIR_att_N <= Monthly_aggegation_N_lim, DIR_transp_EM := NA]

# CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att       := NA]
# CLEAR_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att       := NA]
# CLEAR_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att       := NA]
# CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp    := NA]
# CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_sd    := NA]
# CLEAR_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_sd    := NA]
# CLEAR_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_sd    := NA]
# CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_sd := NA]
# CLEAR_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_EM    := NA]
# CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_EM    := NA]
# CLEAR_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_EM    := NA]
# CLEAR_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_EM := NA]

# CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att       := NA]
# CLOUD_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att       := NA]
# CLOUD_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att       := NA]
# CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp    := NA]
# CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_sd    := NA]
# CLOUD_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_sd    := NA]
# CLOUD_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_sd    := NA]
# CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_sd := NA]
# CLOUD_1_monthly_daily_mean[ HOR_att_N <= Monthly_aggegation_N_lim/2, HOR_att_EM    := NA]
# CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_att_EM    := NA]
# CLOUD_1_monthly_daily_mean[ GLB_att_N <= Monthly_aggegation_N_lim/2, GLB_att_EM    := NA]
# CLOUD_1_monthly_daily_mean[ DIR_att_N <= Monthly_aggegation_N_lim/2, DIR_transp_EM := NA]



## _ Seasonal monthly daily values ---------------------------------------------

ALL_1_monthly_daily_seas <-
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
                     by = .( Month = month(Date) ) ]

CLEAR_1_monthly_daily_seas <-
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
                       by = .( Month = month(Date) ) ]

CLOUD_1_monthly_daily_seas <-
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
                       by = .( Month = month(Date) ) ]



plot(ALL_1_monthly_daily_seas[, GLB_att_seas, Month],pch= 1, cex = 1,
     main = "ALL_1_monthly_daily_seas[, GLB_att_seas, Month]")
cat(" \n \n")

plot(CLEAR_1_monthly_daily_seas[, GLB_att_seas, Month],pch= 1, cex = 1,
     main = "CLEAR_1_monthly_daily_seas[, GLB_att_seas, Month]")
cat(" \n \n")

plot(CLOUD_1_monthly_daily_seas[, GLB_att_seas, Month],pch= 1, cex = 1,
     main = "CLOUD_1_monthly_daily_seas[, GLB_att_seas, Month]")
cat(" \n \n")



## _ Monthly daily de-seasonal anomaly -----------------------------------------

ALL_1_D_monthly_DESEAS <- merge(  ALL_1_monthly_daily_mean,   ALL_1_monthly_daily_seas, by = "Month", all = T)
CLEAR_1_D_monthly_DESEAS <- merge(CLEAR_1_monthly_daily_mean, CLEAR_1_monthly_daily_seas, by = "Month", all = T)
CLOUD_1_D_monthly_DESEAS <- merge(CLOUD_1_monthly_daily_mean, CLOUD_1_monthly_daily_seas, by = "Month", all = T)


## forget data
rm(  ALL_1_monthly_daily_mean,   ALL_1_monthly_daily_seas,
     CLEAR_1_monthly_daily_mean, CLEAR_1_monthly_daily_seas,
     CLOUD_1_monthly_daily_mean, CLOUD_1_monthly_daily_seas)
dummy <- gc()


## create date
ALL_1_D_monthly_DESEAS[, Date := as.Date(paste(Year, Month, "1"), format = "%Y %m %d")]
CLEAR_1_D_monthly_DESEAS[, Date := as.Date(paste(Year, Month, "1"), format = "%Y %m %d")]
CLOUD_1_D_monthly_DESEAS[, Date := as.Date(paste(Year, Month, "1"), format = "%Y %m %d")]

setorder(  ALL_1_D_monthly_DESEAS, Date)
setorder(CLEAR_1_D_monthly_DESEAS, Date)
setorder(CLOUD_1_D_monthly_DESEAS, Date)


## Using the % departure from seasonal values
ALL_1_D_monthly_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
ALL_1_D_monthly_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
ALL_1_D_monthly_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
ALL_1_D_monthly_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLEAR_1_D_monthly_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLEAR_1_D_monthly_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLEAR_1_D_monthly_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLEAR_1_D_monthly_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]
CLOUD_1_D_monthly_DESEAS[, DIR_att_des   := 100*(DIR_att    - DIR_att_seas   ) / DIR_att_seas   ]
CLOUD_1_D_monthly_DESEAS[, HOR_att_des   := 100*(HOR_att    - HOR_att_seas   ) / HOR_att_seas   ]
CLOUD_1_D_monthly_DESEAS[, GLB_att_des   := 100*(GLB_att    - GLB_att_seas   ) / GLB_att_seas   ]
CLOUD_1_D_monthly_DESEAS[, DIR_transp_des:= 100*(DIR_transp - DIR_transp_seas) / DIR_transp_seas]





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


setorder(  ALL_1_D_bySeason_DESEAS, Yqrt)
setorder(CLEAR_1_D_bySeason_DESEAS, Yqrt)
setorder(CLOUD_1_D_bySeason_DESEAS, Yqrt)


rm(ALL_1_daily_mean,   ALL_1_daily_seas,
   CLEAR_1_daily_mean, CLEAR_1_daily_seas,
   CLOUD_1_daily_mean, CLOUD_1_daily_seas)
dummy <- gc()





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






## TODO
#### WRDC ? ####

# ## add all minutes of the days
# xlim      <- range(DATA_all$Date)
# alldates  <- data.frame(Date = seq(xlim[1], xlim[2], by = "min"))

# DATA_ALLQ <- merge( DATA_all, alldates, all = T )
# Q_all <- DATA_ALLQ[,.(DIR_att       = mean(DIR_att,     na.rm = T),
#                       GLB_att       = mean(GLB_att,     na.rm = T),
#                       HOR_att       = mean(HOR_att,     na.rm = T),
#                       DIR_transp    = mean(DIR_transp,  na.rm = T),
#                       tsi1au_att    = mean(tsi_1au_comb,na.rm = T),
#                       DIR_att_sd    = sd(  DIR_att,     na.rm = T),
#                       GLB_att_sd    = sd(  GLB_att,     na.rm = T),
#                       HOR_att_sd    = sd(  HOR_att,     na.rm = T),
#                       DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
#                       tsi1au_att_sd = sd(  tsi_1au_comb,na.rm = T),
#                       GLB_att_N     = sum(!is.na(GLB_att)),
#                       HOR_att_N     = sum(!is.na(HOR_att)),
#                       DIR_att_N     = sum(!is.na(DIR_att)),
#                       Elevat        = mean(Elevat,  na.rm = TRUE),
#                       Azimuth       = mean(Azimuth, na.rm = TRUE),
#                       SZA           = mean(SZA,     na.rm = TRUE)),
#                    by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600/4)*(3600/4),
#                                             origin = "1970-01-01" ))
# ]
# rm(DATA_ALLQ)


# DATA_CLEARQ <- merge( DATA_Clear, alldates, all = T )
# Q_clear <- DATA_CLEARQ[,.(DIR_att       = mean(DIR_att,     na.rm = T),
#                           GLB_att       = mean(GLB_att,     na.rm = T),
#                           HOR_att       = mean(HOR_att,     na.rm = T),
#                           DIR_transp    = mean(DIR_transp,  na.rm = T),
#                           tsi1au_att    = mean(tsi_1au_comb,na.rm = T),
#                           DIR_att_sd    = sd(  DIR_att,     na.rm = T),
#                           GLB_att_sd    = sd(  GLB_att,     na.rm = T),
#                           HOR_att_sd    = sd(  HOR_att,     na.rm = T),
#                           DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
#                           tsi1au_att_sd = sd(  tsi_1au_comb,na.rm = T),
#                           GLB_att_N     = sum(!is.na(GLB_att)),
#                           HOR_att_N     = sum(!is.na(HOR_att)),
#                           DIR_att_N     = sum(!is.na(DIR_att)),
#                           Elevat        = mean(Elevat,  na.rm = TRUE),
#                           Azimuth       = mean(Azimuth, na.rm = TRUE),
#                           SZA           = mean(SZA,     na.rm = TRUE)),
#                        by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600/4)*(3600/4),
#                                                 origin = "1970-01-01" ))
# ]
# rm(DATA_CLEARQ)


# DATA_CLOUDQ <- merge( DATA_Cloud, alldates, all = T )
# Q_cloud <- DATA_CLOUDQ[,.(DIR_att       = mean(DIR_att,     na.rm = T),
#                           GLB_att       = mean(GLB_att,     na.rm = T),
#                           HOR_att       = mean(HOR_att,     na.rm = T),
#                           DIR_transp    = mean(DIR_transp,  na.rm = T),
#                           tsi1au_att    = mean(tsi_1au_comb,na.rm = T),
#                           DIR_att_sd    = sd(  DIR_att,     na.rm = T),
#                           GLB_att_sd    = sd(  GLB_att,     na.rm = T),
#                           HOR_att_sd    = sd(  HOR_att,     na.rm = T),
#                           DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
#                           tsi1au_att_sd = sd(  tsi_1au_comb,na.rm = T),
#                           GLB_att_N     = sum(!is.na(GLB_att)),
#                           HOR_att_N     = sum(!is.na(HOR_att)),
#                           DIR_att_N     = sum(!is.na(DIR_att)),
#                           Elevat        = mean(Elevat,  na.rm = TRUE),
#                           Azimuth       = mean(Azimuth, na.rm = TRUE),
#                           SZA           = mean(SZA,     na.rm = TRUE)),
#                        by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600/4)*(3600/4),
#                                                 origin = "1970-01-01" ))
# ]
# rm(DATA_CLOUDQ)



# QH_all <- Q_all[,.(DIR_att       = mean(DIR_att,     na.rm = T),
#                    GLB_att       = mean(GLB_att,     na.rm = T),
#                    HOR_att       = mean(HOR_att,     na.rm = T),
#                    DIR_transp    = mean(DIR_transp,  na.rm = T),
#                    tsi1au_att    = mean(tsi1au_att,  na.rm = T),
#                    DIR_att_sd    = sd(  DIR_att,     na.rm = T),
#                    GLB_att_sd    = sd(  GLB_att,     na.rm = T),
#                    HOR_att_sd    = sd(  HOR_att,     na.rm = T),
#                    DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
#                    tsi1au_att_sd = sd(  tsi1au_att,  na.rm = T),
#                    GLB_att_N     = sum(!is.na(GLB_att)),
#                    HOR_att_N     = sum(!is.na(HOR_att)),
#                    DIR_att_N     = sum(!is.na(DIR_att)),
#                    Elevat        = mean(Elevat,  na.rm = TRUE),
#                    Azimuth       = mean(Azimuth, na.rm = TRUE),
#                    SZA           = mean(SZA,     na.rm = TRUE)),
#                 by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600)*(3600),
#                                          origin = "1970-01-01" ))]

# QH_clear <- Q_clear[,.(DIR_att       = mean(DIR_att,     na.rm = T),
#                        GLB_att       = mean(GLB_att,     na.rm = T),
#                        HOR_att       = mean(HOR_att,     na.rm = T),
#                        DIR_transp    = mean(DIR_transp,  na.rm = T),
#                        tsi1au_att    = mean(tsi1au_att,  na.rm = T),
#                        DIR_att_sd    = sd(  DIR_att,     na.rm = T),
#                        GLB_att_sd    = sd(  GLB_att,     na.rm = T),
#                        HOR_att_sd    = sd(  HOR_att,     na.rm = T),
#                        DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
#                        tsi1au_att_sd = sd(  tsi1au_att,  na.rm = T),
#                        GLB_att_N     = sum(!is.na(GLB_att)),
#                        HOR_att_N     = sum(!is.na(HOR_att)),
#                        DIR_att_N     = sum(!is.na(DIR_att)),
#                        Elevat        = mean(Elevat,  na.rm = TRUE),
#                        Azimuth       = mean(Azimuth, na.rm = TRUE),
#                        SZA           = mean(SZA,     na.rm = TRUE)),
#                     by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600)*(3600),
#                                              origin = "1970-01-01" ))]

# QH_cloud <- Q_cloud[,.(DIR_att       = mean(DIR_att,     na.rm = T),
#                        GLB_att       = mean(GLB_att,     na.rm = T),
#                        HOR_att       = mean(HOR_att,     na.rm = T),
#                        DIR_transp    = mean(DIR_transp,  na.rm = T),
#                        tsi1au_att    = mean(tsi1au_att,  na.rm = T),
#                        DIR_att_sd    = sd(  DIR_att,     na.rm = T),
#                        GLB_att_sd    = sd(  GLB_att,     na.rm = T),
#                        HOR_att_sd    = sd(  HOR_att,     na.rm = T),
#                        DIR_transp_sd = sd(  DIR_transp,  na.rm = T),
#                        tsi1au_att_sd = sd(  tsi1au_att,  na.rm = T),
#                        GLB_att_N     = sum(!is.na(GLB_att)),
#                        HOR_att_N     = sum(!is.na(HOR_att)),
#                        DIR_att_N     = sum(!is.na(DIR_att)),
#                        Elevat        = mean(Elevat,  na.rm = TRUE),
#                        Azimuth       = mean(Azimuth, na.rm = TRUE),
#                        SZA           = mean(SZA,     na.rm = TRUE)),
#                     by = .(Date = as.POSIXct(as.numeric(Date)%/%(3600)*(3600),
#                                              origin = "1970-01-01" ))]


# hist(QH_all[ Elevat < 20, Elevat ] )

# ## keep only whole hours
# QH_all <- QH_all[ !is.na(SZA) ]
# QH_all[ GLB_att_N < 4 , GLB_att := NA ]
# QH_all[ DIR_att_N < 4 , DIR_att := NA ]
# QH_all[ HOR_att_N < 4 , HOR_att := NA ]

# QH_clear <- QH_clear[ !is.na(SZA) ]
# QH_clear[ GLB_att_N < 4 , GLB_att := NA ]
# QH_clear[ DIR_att_N < 4 , DIR_att := NA ]
# QH_clear[ HOR_att_N < 4 , HOR_att := NA ]

# QH_cloud <- QH_cloud[ !is.na(SZA) ]
# QH_cloud[ GLB_att_N < 4 , GLB_att := NA ]
# QH_cloud[ DIR_att_N < 4 , DIR_att := NA ]
# QH_cloud[ HOR_att_N < 4 , HOR_att := NA ]


# QHD_all <- QH_all[,.(DIR_att       = mean(DIR_att   ),
#                      GLB_att       = mean(GLB_att   ),
#                      HOR_att       = mean(HOR_att   ),
#                      DIR_transp    = mean(DIR_transp)),
#                   by = .(Date = as.Date(Date))]

# QHD_clear <- QH_clear[,.(DIR_att       = mean(DIR_att   ),
#                          GLB_att       = mean(GLB_att   ),
#                          HOR_att       = mean(HOR_att   ),
#                          DIR_transp    = mean(DIR_transp)),
#                       by = .(Date = as.Date(Date))]

# QHD_cloud <- QH_cloud[,.(DIR_att       = mean(DIR_att   ),
#                          GLB_att       = mean(GLB_att   ),
#                          HOR_att       = mean(HOR_att   ),
#                          DIR_transp    = mean(DIR_transp)),
#                       by = .(Date = as.Date(Date))]

# ## Too few days remain
# QHD_all[   !is.na(GLB_att), .N ]
# QHD_clear[ !is.na(GLB_att), .N ]
# QHD_cloud[ !is.na(GLB_att), .N ]



#' **END**
#+ include=T, echo=F
