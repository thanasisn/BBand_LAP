
02

#+ echo=F, include=T
library(fANCOVA,    quietly = TRUE, warn.conflicts = FALSE)
library(xts,        quietly = TRUE, warn.conflicts = FALSE)
library(zoo,        quietly = TRUE, warn.conflicts = FALSE)
# library(forecast,   quietly = TRUE, warn.conflicts = FALSE)
library(astsa,      quietly = TRUE, warn.conflicts = FALSE)

## __ Load external functions --------------------------------------------------
source("~/CODE/FUNCTIONS/R/sumNA.R")
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
source("~/CODE/FUNCTIONS/R/cor_test_stats.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")

source("./DHI_GHI_0_variables.R")
source("./var_translation.R")


## choose loess criterion for span
LOESS_CRITERIO <-  c("aicc", "gcv")[1]

ALL_1_daily_DESEAS[           GLB_att_des == 0]
CLOUD_1_daily_DESEAS[         GLB_att_des == 0]
# test <- CLEAR_1_daily_DESEAS[ GLB_att_des == 0]
# test <- rm.cols.dups.DT(test)
CLEAR_1_daily_DESEAS[         GLB_att_des == 0, GLB_att_des := NA]



#'
#' ## 1. Long term anomaly trends
#'
#' ### Data info
#'
#' Time data span `r range(ALL_1_daily_DESEAS$Date)`
#'
#' Where is a **running mean the window is `r running_mean_window_days` days** or
#' `r running_mean_window_days / Days_of_year` years.
#'
#' ### Process
#'
#' - Daily means are used to produce monthly means.
#' - Some quality constrains have been applied
#' - In each case the seasonal data have been created with the same process as
#' the data that are de-seasonalised.
#'
#'
#+ echo=F, include=F

col_wattGLB_NA              <- "green"

## ____ Scatter plots with SZA all data ----------------------------------------
data_list <- c(  "ALL_1_D_monthly_DESEAS",
               "CLEAR_1_D_monthly_DESEAS",
               "CLOUD_1_D_monthly_DESEAS",
                     "ALL_1_daily_DESEAS",
                   "CLEAR_1_daily_DESEAS",
                   "CLEAR_1_daily_DESEAS")


## __ Plot data trends  --------------------------------------------------------
#'
#' \newpage
#' \FloatBarrier
#'
#' ### Trends from daily means
#'
#+ LongtermTrends, echo=F, include=T, results="asis"
# vars <- c("HOR_att","DIR_transp", "DIR_att", "GLB_att", "tsi1au_att")
vars <- c("DIR_att_des", "GLB_att_des", "tsi1au_att", "near_tcc_att", "near_tcc_att_des")

# test
vars <- c( "GLB_att_des")

dbs         <- c("ALL_1_daily_DESEAS",                        "CLEAR_1_daily_DESEAS",
                 "CLOUD_1_daily_DESEAS")
## gather trends
gather <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    cat("\n\\newpage\n")
    cat("\n#### Trends on", translate(DBn), "data\n\n" )

        for (avar in vars) {
            dataset <- DB

            if (all(is.na(dataset[[avar]]))) next()

            ## linear model by day step
            lm1 <- lm(dataset[[avar]] ~ dataset$Date)
            d   <- summary(lm1)$coefficients
            cat("lm:      ", lm1$coefficients[2] * Days_of_year, "+/-", d[2,2] * Days_of_year,"\n\n")
            ## correlation test
            cor1 <- cor.test(x = dataset[[avar]], y = as.numeric(dataset$Date), method = 'pearson')


            ## create times series
            dd <- read.zoo(dataset, index.column = "Date")
            dd <- as.ts(dd)


            ## _ Arima tests  --------------------------------------------------

            ## _ auto reggression arima Tourpali -------------------------------
            ## create a time variable (with lag of 1 day ?)
            dataset[, ts := (year(Date) - min(year(Date))) + ( doy - 1 ) / Hmisc::yearDays(Date) ]
            tmodelo <- arima(x = dataset[[avar]], order = c(1,0,0), xreg = dataset$ts, method = "ML")

            ## trend per year with auto correlation
            Tres <- data.frame(t(lmtest::coeftest(tmodelo)[3,]))
            Tint <- data.frame(t(lmtest::coeftest(tmodelo)[2,]))
            names(Tres) <- paste0("Tmod_", names(Tres))

            cat("Tourpali:", paste(round(Tres, 4)), "\n\n")


            lag   <- 1
            dd    <- acf(dataset[[avar]], na.action = na.pass, plot = FALSE)
            N_eff <- sum(!is.na(dataset[[avar]])) * (1 - dd[lag][[1]]) / (1 + dd[lag][[1]])
            se_sq <- sum((lm1$residuals)^2, na.rm = T) / (N_eff - 2)
            sa_sq <- se_sq / sum((dataset[[avar]] - mean(dataset[[avar]], na.rm = T))^2, na.rm = T)
            t_eff     <- lm1$coefficients[[2]] / sa_sq
            #find two-tailed t critical values
            t_eff_cri <- qt(p = .05/2, df = N_eff, lower.tail = FALSE)

            conf      <- confint(lm1)
            conf_2.5  <- conf[2,1]
            conf_97.5 <- conf[2,2]

            ## get daily climatology
            dclima <- dataset[, max(get(gsub("_des", "_seas", avar))), by = doy]

            ## capture lm for table
            gather <- rbind(gather,
                            data.frame(
                                linear_fit_stats(lm1, confidence_interval = Daily_confidence_limit),
                                cor_test_stats(cor1),
                                DATA       = DBn,
                                var        = avar,
                                N          = sum(!is.na(dataset[[avar]])),
                                N_eff      = N_eff,
                                t_eff      = t_eff,
                                t_eff_cri  = t_eff_cri,
                                conf_2.5   = conf_2.5,
                                conf_97.5  = conf_97.5,
                                mean_clima = mean(dclima$V1, na.rm = T),
                                Tres
                            )
            )


            ## plot data
            plot(dataset$Date, dataset[[avar]],
                 pch      = ".",
                 col      = acol,
                 cex      = 2,
                 # main     = paste(translate(DBn), translate(avar)),
                 cex.main = 0.8,
                 yaxt     = "n",
                 xlab     = "",
                 ylab     = bquote("Anomalies [%]")
            )

            ## plot fit line lm
            abline(lm1, lwd = 2)


            if (DRAFT == TRUE) {
                ## Running mean
                first <- head(which(!is.na(dataset[[avar]])),1)
                last  <- tail(which(!is.na(dataset[[avar]])),1)

                rm <- frollmean(dataset[[avar]][first:last],
                                round(running_mean_window_days),
                                na.rm = TRUE,
                                algo  = "exact",
                                align = "center")

                # points(dataset$Date, rm, col = "red", cex = 0.5)
                lines(dataset$Date[first:last], rm, col = "red", lwd = 1.5)

                ## LOESS curve
                vec <- !is.na(dataset[[avar]])
                FTSE.lo3 <- loess.as(dataset$Date[vec], dataset[[avar]][vec],
                                     degree = 1,
                                     criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
                FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$Date)
                lines(dataset$Date, FTSE.lo.predict3, col = "cyan", lwd = 2.5)
            }


            ## display trend on graph
            fit <- lm1[[1]]
            legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
                   paste("Trend: ",
                         if (fit[2] > 0) "+" else "-",
                         signif(abs(fit[2]) * Days_of_year, 2),
                         "±", signif(2 * Tres[2], 2) ,"%/y" )
            )
    }
}
#+ echo=F, include=F





#+ LongtermTrendsRuMe, echo=F, include=T, results="asis"
# vars <- c("HOR_att","DIR_transp", "DIR_att", "GLB_att", "tsi1au_att")
vars <- c("GLB_att_des", "DIR_att_des", "tsi1au_att", "wattGLB", "near_tcc_att", "near_tcc_att_des")

dbs         <- c(  "ALL_1_daily_DESEAS",
                 "CLEAR_1_daily_DESEAS",
                 "CLOUD_1_daily_DESEAS")
##  Gather daily trends  -------------------------------------------------------
gather <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    cat("\n\\newpage\n")
    cat("\n#### Trends on", translate(DBn), "data with running mean\n\n" )

        for (avar in vars) {
            dataset <- DB

            if (all(is.na(dataset[[avar]]))) next()

            ## linear model by day step
            lm1  <- lm(dataset[[avar]] ~ dataset$Date)
            ## correlation test
            cor1 <- cor.test(x = dataset[[avar]], y = as.numeric(dataset$Date), method = 'pearson')


            ## _ auto reggression arima Tourpali -------------------------------
            ## create a time variable (with lag of 1 day ?)
            dataset[, ts := (year(Date) - min(year(Date))) + ( doy - 1 ) / Hmisc::yearDays(Date) ]
            tmodelo <- arima(x = dataset[[avar]], order = c(1,0,0), xreg = dataset$ts, method = "ML")

            ## trend per year with auto correlation
            Tres <- data.frame(t(lmtest::coeftest(tmodelo)[3,]))
            names(Tres) <- paste0("Tmod_", names(Tres))

            lag   <- 1
            dd    <- acf(dataset[[avar]], na.action = na.pass, plot = FALSE)
            N_eff <- sum(!is.na(dataset[[avar]])) * (1 - dd[lag][[1]]) / (1 + dd[lag][[1]])
            se_sq <- sum((lm1$residuals)^2, na.rm = T) / (N_eff - 2)
            sa_sq <- se_sq / sum((dataset[[avar]] - mean(dataset[[avar]], na.rm = T))^2, na.rm = T)
            t_eff     <- lm1$coefficients[[2]] / sa_sq
            #find two-tailed t critical values
            t_eff_cri <- qt(p = .05/2, df = N_eff, lower.tail = FALSE)

            conf      <- confint(lm1)
            conf_2.5  <- conf[2,1]
            conf_97.5 <- conf[2,2]

            ## get daily climatology
            dclima <- dataset[, max(get(gsub("_des", "_seas", avar))), by = doy]

            ## capture stats for table
            gather <- rbind(gather,
                            data.frame(
                                linear_fit_stats(lm1, confidence_interval = Daily_confidence_limit),
                                cor_test_stats(cor1),
                                DATA      = DBn,
                                var       = avar,
                                N         = sum(!is.na(dataset[[avar]])),
                                Obs       = sum(dataset[[sub("_des","_N", avar)]], na.rm = TRUE),
                                N_eff     = N_eff,
                                t_eff     = t_eff,
                                t_eff_cri = t_eff_cri,
                                conf_2.5  = conf_2.5,
                                conf_97.5 = conf_97.5,
                                mean_clima = mean(dclima$V1, na.rm = T),
                                Tres
                            )
            )

            par("mar" = c(3, 4, 2, 1))

            if (grepl("wattGLB", avar)) {
                acol <- "green"
            } else if (grepl("near_tcc", avar)) {
                acol <- "cyan"
            }
            else {
                acol <- get(paste0(c("col", unlist(strsplit(avar, split = "_"))[1:2]),
                                   collapse = "_"))
            }


            ## plot data
            plot(dataset$Date, dataset[[avar]],
                 pch      = ".",
                 col      = acol,
                 cex      = 2,
                 main     = paste(translate(DBn), translate(avar)),
                 cex.main = 0.8,
                 yaxt     = "n",
                 xlab     = "",
                 ylab     = bquote("Anomalies [%]")
            )
            axis(2, pretty(dataset[[avar]]), las = 2 )

            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )

            ## plot fit line
            abline(lm1, lwd = 2)

            ## Running mean
            first <- head(which(!is.na(dataset[[avar]])),1)
            last  <- tail(which(!is.na(dataset[[avar]])),1)

            rm <- frollmean(dataset[[avar]][first:last],
                            round(running_mean_window_days),
                            na.rm = TRUE,
                            algo  = "exact",
                            align = "center")

            # points(dataset$Date, rm, col = "red", cex = 0.5)
            lines(dataset$Date[first:last], rm, col = "red", lwd = 1.5)

            ## LOESS curve
            vec <- !is.na(dataset[[avar]])
            FTSE.lo3 <- loess.as(dataset$Date[vec], dataset[[avar]][vec],
                                 degree = 1,
                                 criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
            FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$Date)
            lines(dataset$Date, FTSE.lo.predict3, col = "cyan", lwd = 2.5)


            ## decorations
            fit <- lm1[[1]]

            legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
                   paste("Trend: ",
                         if (fit[2] > 0) "+" else "-",
                         signif(abs(fit[2]) * Days_of_year, 3),
                         "%/y")
            )

    }
}
#+ echo=F, include=F


#+ LongtermSeasonal, echo=F, include=T, results="asis"
# vars <- c("HOR_att","DIR_transp", "DIR_att", "GLB_att", "tsi1au_att")
vars <- c("DIR_att_seas", "GLB_att_seas", "wattGLB_seas")

dbs         <- c(  "ALL_1_daily_DESEAS",
                   "CLEAR_1_daily_DESEAS",
                   "CLOUD_1_daily_DESEAS")
rmw_days <- 15

for (DBn in dbs) {
    DB <- get(DBn)
    cat("\n\\newpage\n")
    cat("\n#### Climatology on", translate(DBn), "data with", rmw_days, "d running mean\n\n" )

    for (avar in vars) {
        dataset <- DB
        dataset <- unique(dataset[, .( doy, get(avar))])
        setorder(dataset, doy)

        if (all(is.na(dataset$V2))) next()

        par("mar" = c(3, 4, 2, 1))

        if (grepl("wattGLB", yvar)) {
            acol <- "green"
        } else {
            acol <- get(paste0(c("col", unlist(strsplit(yvar, split = "_"))[1:2]),
                              collapse = "_"))
        }

        ## plot data
        plot(dataset$doy, dataset$V2,
             pch      = ".",
             col      = acol,
             cex      = 2,
             main     = paste("Climatology", translate(DBn), translate(avar)),
             cex.main = 0.8,
             yaxt     = "n",
             xlab     = "",
             ylab     = bquote("SDR")
        )
        axis(2, pretty(dataset$V2), las = 2 )


        ## Running mean
        rm <- frollmean(dataset$V2,
                        15,
                        na.rm = TRUE,
                        algo  = "exact",
                        align = "center")

        # points(dataset$Date, rm, col = "red", cex = 0.5)
        lines(dataset$doy, rm, col = "red", lwd = 1.5)

        ## LOESS curve
        vec <- !is.na(dataset$V2)
        FTSE.lo3 <- loess.as(dataset$doy[vec], dataset$V2[vec],
                             degree = 1,
                             criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
        FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$doy)
        lines(dataset$doy, FTSE.lo.predict3, col = "cyan", lwd = 2.5)

    }
}
#+ echo=F, include=F




## __ Daily trends table  ------------------------------------------------------
#'
#' \newpage
#'
#' #### Table of total trends.
#'
#+ echo=F, include=T

# wecare      <- grep("intercept", names(gather), value = T, invert = T)
wecare      <- names(gather)
gather      <- data.table(gather)
gather$DATA <- sub("_.*", "", gather$DATA)

pprint      <- gather[ , ..wecare]

pprint[, slope.stat_sig := 100 * (1 - slope.p)]
pprint[, slope.t        := NULL]
pprint$cor.data_name <- NULL



saveRDS(pprint,
        "./figures/tbl_longterm_trends.Rds")

## convert slope / year
pprint[, slope              := slope              * Days_of_year]
pprint[, slope.sd           := slope.sd           * Days_of_year]
pprint[, slope.ConfInt_0.95 := slope.ConfInt_0.95 * Days_of_year]
pprint[, slope.ConfInt_0.99 := slope.ConfInt_0.99 * Days_of_year]


wecare <- grep("intercept", names(pprint), value = T, invert = T)
pprint <- pprint[ , ..wecare]
pprint[, DATA               := translate(DATA)                  ]
pprint[, var                := translate(var)                   ]


pprint[, ChangeWpY := mean_clima * slope / 100 ]



pprint$cor.data_name   <- NULL
pprint$cor.null_value  <- NULL
pprint$cor.method      <- NULL
pprint$cor.alternative <- NULL
pprint$cor.p           <- NULL

#+ echo=F, include=T
#' \scriptsize
#+ echo=F, include=T
pander(pprint,
       cap = "Slope is in %/year")
#'
#' \normalsize
#+ echo=F, include=T






##  SEASONAL TRENDS  ###########################################################

#' \newpage
#' \FloatBarrier
#'
#' ### Trends for each season of the year
#'
#' We calculated Seasonal means from the daily means
#'
#+ echo=F, include=F

## __ Plot trends for each season  ---------------------------------------------

## ____ by sky conditions  -----------------------------------------------------

#+ SeasonalTrends, echo=F, include=T, results="asis, fig.asp = 0.5"
# vars        <- c("DIR_att", "GLB_att")
vars        <- c("GLB_att_des")

## Monthly aggregation
dbs         <- c(  "ALL_1_D_bySeason_DESEAS",
                 "CLEAR_1_D_bySeason_DESEAS",
                 "CLOUD_1_D_bySeason_DESEAS")

Seasons     <- c("Winter", "Spring", "Summer", "Autumn")
for (DBn in dbs) {
    DB <- get(DBn)

    if (!FIGURESGRID) {
        cat("\n\\newpage\n")
        cat("\n#### ", translate(DBn), "\n\n")
    }

    for (avar in vars) {
        ## plot in a grid
        if (FIGURESGRID) {
            par(mfrow = c(2, 2))
            cat("\n\\newpage\n")
            cat("\n#### ", translate(DBn), translate(avar) , "\n\n")
        }

        ylim <- range(DB[[avar]], na.rm = TRUE )

        for (ase in Seasons) {

            dataset <- DB[ Season == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ## linear model counting years
            lm2 <- lm( dataset[[avar]] ~ dataset$Year )

            ## plot
            par("mar" = c(2, 3.6, 2, 0.5))

            plot(dataset$Year, dataset[[avar]],
                 # ylim = ylim,
                 pch      = 19,
                 col      = get(paste0(c("col",
                                         unlist(strsplit(avar, split = "_" ))[1:2]),
                                       collapse = "_")),
                 cex      = .5,
                 main     = paste(ase, translate(DBn), translate(avar)),
                 ylab     = "",
                 yaxt     = "n",
                 xlab     = "",
                 cex.main = 0.9,
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 mgp  = c(2, 0.5, 0)
            )
            axis(2, pretty(dataset[[avar]]), las = 2)
            mtext(text = bquote("Anomaly [%]"),
                  cex  = 0.8,
                  side = 2,
                  line = 2.6)
            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]"))

            abline(lm2)

            if (DRAFT) {
                ## Running mean years * months in data set
                first <- head(which(!is.na(dataset[[avar]])),1)
                last  <- tail(which(!is.na(dataset[[avar]])),1)

                rm <- frollmean(dataset[[avar]][first:last],
                                running_mean_window_years,
                                na.rm = TRUE,
                                algo  = "exact",
                                align = "center")

                # points(dataset$Date, rm, col = "red", cex = 0.5)
                lines(dataset$Year[first:last], rm, col = "red", cex = 0.5)

                ## LOESS curve
                vec <- !is.na(dataset[[avar]])
                FTSE.lo3 <- loess.as(dataset$Year[vec], dataset[[avar]][vec],
                                     degree = 1,
                                     criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
                FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$Year)
                lines(dataset$Year, FTSE.lo.predict3, col = "cyan", lwd = 2.5)
            }

            ## decorations
            fit <- lm2[[1]]

            legend("bottom", lty = 1, bty = "n", lwd = 2, cex = 1,
                   paste("Trend: ",
                         if (fit[2] > 0) '+' else '-',
                         signif(abs(fit[2]), 3),
                         "% / year")
            )

        }
        par(mfrow = c(1, 1)) ## just reset layout

        ## __ Extreme values table ---------------------------------------------
        wca <- c("Year", "Season", grep(paste0("^", strsplit(avar, "_")[[1]][1]), names(DB), value = T ))

        cat("\n \n \\footnotesize \n ")
        cat(
            pander(
                DB[order(abs(DB[[avar]]), decreasing = T )[1:5] , ..wca ],
                cap = "Extreme anomaly values"
            )
        )
        cat("\n \n \\normalsize \n ")
    }
}
#+ echo=F, include=F


# ____ by season  --------------------------------------------------------------

#+ SeasonalTrendsTogether, echo=F, include=T
# vars        <- c("DIR_att", "GLB_att")
vars        <- c("GLB_att_des")

## Monthly aggregation
dbs         <- c(  "ALL_1_D_bySeason_DESEAS",
                 "CLEAR_1_D_bySeason_DESEAS",
                 "CLOUD_1_D_bySeason_DESEAS")

Seasons     <- c("Winter", "Spring", "Summer", "Autumn")

## variable
for (avar in vars) {

    par(mfcol = c(length(Seasons), length(dbs)))
    cplots <- 0

    ## get global ylim
    gylim <- c()
    for (DBn in dbs) {
        DB   <- get(DBn)
        gylim <- c(gylim, range(DB[[avar]], na.rm = TRUE))
    }
    ylim <- range(gylim, na.rm = TRUE)


    ## sky conditions
    for (DBn in dbs) {
        DB <- get(DBn)

        for (ase in Seasons) {
            cplots <- cplots + 1

            dataset <- DB[ Season == ase, ]

            # if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ## linear model counting years
            lm2 <- lm(dataset[[avar]] ~ dataset$Year)

            ## plot
            par("mar" = c(2, 3.6, 2, 0))

            plot(dataset$Year, dataset[[avar]],
                 ylim     = ylim,
                 pch      = 19,
                 col      = get(paste0(c("col",
                                         unlist(strsplit(avar, split = "_" ))[1:2]),
                                       collapse = "_")),
                 cex      = .5,
                 main     = paste(ase, translate(DBn), translate(avar)),
                 ylab     = "",
                 yaxt     = "n",
                 xlab     = "",
                 cex.main = 0.9,
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 mgp  = c(2, 0.5, 0)
            )
            # axis(2, pretty(dataset[[avar]]), las = 2)
            axis(2, pretty(ylim), las = 2)
            if (cplots <= 4) {
                mtext(text = bquote("Anomaly [%]"),
                      cex  = 0.8,
                      side = 2,
                      line = 2.6)
            }
            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]"))

            abline(lm2)

            if (DRAFT) {
                ## Running mean years * months in data set
                first <- head(which(!is.na(dataset[[avar]])),1)
                last  <- tail(which(!is.na(dataset[[avar]])),1)

                rm <- frollmean(dataset[[avar]][first:last],
                                running_mean_window_years,
                                na.rm = TRUE,
                                algo  = "exact",
                                align = "center")

                # points(dataset$Date, rm, col = "red", cex = 0.5)
                lines(dataset$Year[first:last], rm, col = "red", cex = 0.5)

                ## LOESS curve
                vec <- !is.na(dataset[[avar]])
                FTSE.lo3 <- loess.as(dataset$Year[vec], dataset[[avar]][vec],
                                     degree = 1,
                                     criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
                FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$Year)
                lines(dataset$Year, FTSE.lo.predict3, col = "cyan", lwd = 2.5)
            }

            ## decorations
            fit <- lm2[[1]]

            legend("bottom", lty = 1, bty = "n", lwd = 2, cex = 0.8,
                   paste("Trend: ",
                         if (fit[2] > 0) '+' else '-',
                         signif(abs(fit[2]), 2),
                         "% / year")
            )
        }
    }
    par(mfrow = c(1, 1)) ## just reset layout
}
#+ echo=F, include=F






## ____ by season in a nice grid -----------------------------------------------

#+ SeasonalTrendsTogether2, echo=F, include=T
{
    vars        <- c("GLB_att_des")
    avar        <- vars[1]
    dbs         <- c(  "ALL_1_D_bySeason_DESEAS",
                       "CLEAR_1_D_bySeason_DESEAS",
                       "CLOUD_1_D_bySeason_DESEAS")
    Seasons     <- c("Winter", "Spring", "Summer", "Autumn")

    ## the order must be predefined to match
    expanded <- expand.grid(Dataset = dbs, Seasons = Seasons, stringsAsFactors = FALSE)

    nf <- layout(
        matrix(1:30, ncol = 5, byrow = TRUE),
        widths  = c(0.25,   1,1,1, 0.05),
        heights = c(0.1,  1,1,1,1, 0.5)
    )
    layout.show(nf)

    # 1
    par("mar"=c(0,0,0,0))
    plot.new()
    # 2
    plot.new()
    text(x = 0.5, y = 0.3,
         adj  = c(0.5, 0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.6, y = 0.4,
         adj  = c(0.5,0.5),
         "Clear sky cond.",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.6, y = 0.4,
         adj  = c(0.5,0.5),
         "Cloudy sky cond.", cex = 0.9, font = 2)

    # 5
    plot.new()



    for (i  in 6:25) {

        if (i == 6) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }
        if (i == 10) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }

        if (i == 11) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }
        if (i == 15) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }


        if (i == 16) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }
        if (i == 20) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }


        if (i == 21) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }
        if (i == 25) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }


        # ## fill right column
        # if (i %in% c(10, 15, 20, 25)) {
        #     plot.new()
        # }

        ## actual plots
        if (! i %in% c(6,11,16,21,10,15,20,25)) {

            kk       <- expanded[1,]
            expanded <- expanded[-1, ]

            DB      <- get(kk$Dataset)
            dataset <- DB[ Season == kk$Seasons, ]

            ## linear model counting years
            lm2 <- lm(dataset[[avar]] ~ dataset$Year)

            ## plot
            par("mar" = c(1, 2.5, 1, 0))

            plot(dataset$Year, dataset[[avar]],
                 ylim     = ylim,
                 pch      = 19,
                 col      = get(paste0(c("col",
                                         unlist(strsplit(avar, split = "_" ))[1:2]),
                                       collapse = "_")),
                 cex      = .5,
                 ylab     = "",
                 yaxt     = "n",
                 xlab     = "",
                 cex.main = 0.9,
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 mgp  = c(2, 0.5, 0)
            )
            # axis(2, pretty(dataset[[avar]]), las = 2)

            axis(2, pretty(ylim), las = 2, cex.axis = 0.8)

            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]"))

            abline(lm2)


            ## decorations
            fit <- lm2[[1]]

            legend("top", lty = 1, bty = "n", lwd = 1, cex = 0.8,
                   paste("Trend: ",
                         if (fit[2] > 0) '+' else '-',
                         signif(abs(fit[2]), 2),
                         "% / year")
            )

            if (i %in% c(7,12,17,22)) {
                mtext(text = bquote("Anomaly [%]"),
                      cex  = 0.6,
                      side = 2,
                      line = 2.3)
            }

            par("mar" = c(0,0,0,0))
        }

    }

    # ## fill bottom empty row
    # for (i in 1:5) {
    #     plot.new()
    # }
    #

    # 1
    par("mar" = c(0,0,0,0))
    plot.new()

    # 2
    plot.new()
    text(x = 0.6, y = 0.3,
         adj  = c(0.6,0.5),
         "All sky cond.",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.6, y = 0.3,
         adj  = c(0.5, 0.5),
         "Clear sky cond.",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.6, y = 0.3,
         adj  = c(0.5, 0.5),
         "Cloudy sky cond.", cex = 0.9, font = 2)

    # 5
    plot.new()

    par(mfrow = c(1,1))
}
#+ echo=F, include=F



## ____ tight grid by season from Daily ----------------------------------------

#'
#' ### Grid of trends for each season of year from daily
#'
#+ echo=F, include=F

#+ SeasonalTrendsTogether3, echo=F, include=T
{
    vars        <- c("GLB_att_des")
    avar        <- vars[1]

    dbs         <- c("ALL_1_D_bySeason_DESEAS",
                     "CLEAR_1_D_bySeason_DESEAS",
                     "CLOUD_1_D_bySeason_DESEAS")
    Seasons     <- c("Winter", "Spring", "Summer", "Autumn")
    lec         <- 0

    ## the order must be predefined to match
    expanded <- expand.grid(Dataset = dbs, Seasons = Seasons, stringsAsFactors = FALSE)

    nf <- layout(
        matrix(1:30, ncol = 5, byrow = TRUE),
        widths  = c(0.25,   1,1,1, 0.05),
        heights = c(0.1, 1,1,1,1, 0.3)
    )
    ## an empty layout
    layout.show(nf)

    # 1
    par("mar"=c(0,0,0,0))
    plot.new()
    # 2
    plot.new()
    text(x = 0.5, y = 0.3,
         adj  = c(0.5, 0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.3,
         adj  = c(0.5, 0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.3,
         adj  = c(0.5, 0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    for (i  in 6:25) {

        if (i == 6) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }
        if (i == 10) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }

        if (i == 11) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }
        if (i == 15) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }


        if (i == 16) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }
        if (i == 20) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }


        if (i == 21) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }
        if (i == 25) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }


        ## actual plots
        if (! i %in% c(6,11,16,21,10,15,20,25)) {
            lec      <- lec + 1
            kk       <- expanded[1,]
            expanded <- expanded[-1, ]

            DB      <- get(kk$Dataset)
            dataset <- DB[ Season == kk$Seasons, ]

            ## linear model counting years
            lm2 <- lm(dataset[[avar]] ~ dataset$Year)

            ## plot
            par("mar" = c(0, 0, 0.5, 0.5))

            plot(dataset$Year, dataset[[avar]],
                 ylim     = ylim,
                 pch      = 19,
                 col      = get(paste0(c("col",
                                         unlist(strsplit(avar, split = "_" ))[1:2]),
                                       collapse = "_")),
                 cex      = .6,
                 ylab     = "",
                 xlab     = "",
                 xaxt     = "n",
                 yaxt     = "n",
                 cex.main = 0.9,
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 mgp  = c(2, 0.5, 0)
            )

            ## y axis
            if (i %in% c(7,12,17,22)){
                axis(2, pretty(ylim), las = 2,  cex.axis = 0.8)
                ## minor ticks
                axis(2, seq(-100, 100, by = 5), cex.axis = 0.8, labels = NA, tcl = -0.25)
            } else {
                # axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck = -0.03)
                ## minor ticks
                axis(2, seq(-100, 100, by = 5), labels = NA, tcl = -0.25/2)
            }

            ## x axis
            if (i %in% c(22, 23, 24)) {
                ## bottom row axis
                axis(1, pretty(dataset$Year), las = 1, cex.axis = 0.8, line =  0,   labels = NA)
                axis(1, pretty(dataset$Year), las = 1, cex.axis = 0.8, line = -0.5, tck = 0, lwd = 0)
                ## minor ticks
                axis(1, at = seq(1993, year(Sys.time()), by = 1), labels = NA,
                        tcl = -0.25)
            } else {
                ## major ticks
                # axis(1, pretty(dataset$Year), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(1, pretty(dataset$Year),                cex.axis = 0.8, labels = NA, tck = -0.04)
                ## minor ticks
                axis(1, seq(1993, year(Sys.time()), by = 1), cex.axis = 0.8, labels = NA, tcl = -0.3/2)
                # axis(1, at = seq(1993, year(Sys.time()), by = 1), labels = NA,
                #      tcl = +0.25/2)
            }

            abline(lm2)


            ## decorations
            fit <- lm2[[1]]

            legend("top", lty = 1, bty = "n", lwd = 1, cex = 0.8,
                   inset = 0.1,
                   paste("Trend: ",
                         if (fit[2] > 0) '+' else '-',
                         signif(abs(fit[2]), 2),
                         "%/y")
            )


            ## tag plots with letters
            legend("bottomright", 0, paste0("(",letters[lec],")"),
                   cex   = 1.1,
                   bty   = "n",
                   xjust = 0.5,      # 0.5 means center adjusted
                   yjust = 0.5,      # 0.5 means center adjusted
                   x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                   y.intersp =  0.2, # adjust character interspacing to effect box height
                   adj = c(0, 0.5))  # adjust string position (default values used here)



            if (i %in% c(7,12,17,22)) {
                mtext(text = bquote("Anomalies [%]"),
                      cex  = 0.6,
                      side = 2,
                      line = 2.3)
            }

            par("mar" = c(0,0,0,0))
        }

    }

    # 1
    par("mar" = c(0,0,0,0))
    plot.new()

    # 2
    plot.new()
    text(x = 0.5, y = 0.15,
         adj  = c(0.5, 0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.15,
         adj  = c(0.5, 0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.15,
         adj  = c(0.5, 0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    par(mfrow = c(1,1))
}
#+ echo=F, include=F











## __ Calculate trends for each season  ----------------------------------------
vars        <- c("DIR_att_des", "GLB_att_des")

## vars are  set above
gather_seas <- data.frame()
for (DBn in dbs) {
    DB <- get(DBn)

    for (ase in Seasons) {
        for (avar in vars) {
            dataset <- DB[ Season == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ## linear model counting years
            lm2 <- lm(dataset[[avar]] ~ dataset$Year)
            ## correlation test
            cor1 <- cor.test(x = dataset[[avar]], y = as.numeric(dataset$Year), method = 'pearson')

            lag   <- 1
            dd    <- acf(dataset[[avar]], na.action = na.pass, plot = FALSE)
            N_eff <- sum(!is.na(dataset[[avar]])) * (1 - dd[lag][[1]]) / (1 + dd[lag][[1]])
            se_sq <- sum((lm1$residuals)^2, na.rm = T) / (N_eff - 2)
            sa_sq <- se_sq / sum((dataset[[avar]] - mean(dataset[[avar]], na.rm = T))^2, na.rm = T)
            t_eff     <- lm1$coefficients[[2]] / sa_sq
            #find two-tailed t critical values
            t_eff_cri <- qt(p = .05/2, df = N_eff, lower.tail = FALSE)

            conf      <- confint(lm1)
            conf_2.5  <- conf[2,1]
            conf_97.5 <- conf[2,2]

            ## gather stats
            gather_seas <- rbind(gather_seas,
                                 data.frame(
                                     linear_fit_stats(lm2,
                                                      confidence_interval = Daily_confidence_limit),
                                     cor_test_stats(cor1),
                                     DATA      = DBn,
                                     Season    = ase,
                                     var       = avar,
                                     N         = sum(!is.na(dataset[[avar]])),
                                     N_eff     = N_eff,
                                     t_eff     = t_eff,
                                     t_eff_cri = t_eff_cri,
                                     conf_2.5  = conf_2.5,
                                     conf_97.5 = conf_97.5
                                 ))
        }
    }
}

## __ Display data table  ------------------------------------------------------
#'
#' \newpage
#' \FloatBarrier
#'
#' #### Table of trends by season.
#'
#+ echo=F, include=T

wecare           <- grep("intercept", names(gather_seas), value = T, invert = T)
gather_seas      <- data.table(gather_seas)
gather_seas$DATA <- sub("_.*", "", gather_seas$DATA)


pprint           <- gather_seas[ , ..wecare]

pprint$cor.data_name        <- NULL
pprint[, slope.stat_sig     := 100 * (1 - slope.p)]
pprint[, slope.t            := NULL               ]
# pprint[, Rsqrd              := NULL               ]
# pprint[, RsqrdAdj           := NULL               ]
pprint[, N                  := NULL               ]
pprint[, slope.ConfInt_0.99 := NULL               ]
pprint[, slope.ConfInt_0.95 := NULL               ]
pprint[, DATA               := translate(DATA)    ]
pprint[, var                := translate(var)     ]

pprint$cor.data_name   <- NULL
pprint$cor.null_value  <- NULL
pprint$cor.method      <- NULL
pprint$cor.alternative <- NULL
pprint$cor.p           <- NULL

setorder(pprint, DATA, var)

#+ echo=F, include=T
pander(pprint,
       cap = "Slope is in %/year")
#+ echo=F, include=F











## ____ tight grid by season from Monthly ----------------------------------------

#'
#' ### Grid of trends for each season of year from Monthly
#'
#+ echo=F, include=F

#+ SeasonalMTrendsTogether3, echo=F, include=T
{
    vars        <- c("GLB_att_des")
    avar        <- vars[1]

    dbs         <- c("ALL_1_M_bySeason_DESEAS",
                     "CLEAR_1_M_bySeason_DESEAS",
                     "CLOUD_1_M_bySeason_DESEAS")
    Seasons     <- c("Winter", "Spring", "Summer", "Autumn")
    lec         <- 0

    ## the order must be predefined to match
    expanded <- expand.grid(Dataset = dbs, Seasons = Seasons, stringsAsFactors = FALSE)

    nf <- layout(
        matrix(1:30, ncol = 5, byrow = TRUE),
        widths  = c(0.25,   1,1,1, 0.05),
        heights = c(0.1, 1,1,1,1, 0.3)
    )
    ## an empty layout
    layout.show(nf)

    # 1
    par("mar"=c(0,0,0,0))
    plot.new()
    # 2
    plot.new()
    text(x = 0.5, y = 0.3,
         adj  = c(0.5, 0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.3,
         adj  = c(0.5, 0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.3,
         adj  = c(0.5, 0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    for (i  in 6:25) {

        if (i == 6) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }
        if (i == 10) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Winter", cex = 0.9, font = 2)
        }

        if (i == 11) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }
        if (i == 15) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }


        if (i == 16) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }
        if (i == 20) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }


        if (i == 21) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }
        if (i == 25) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }


        ## actual plots
        if (! i %in% c(6,11,16,21,10,15,20,25)) {
            lec      <- lec + 1
            kk       <- expanded[1,]
            expanded <- expanded[-1, ]

            DB      <- get(kk$Dataset)
            dataset <- DB[ Season == kk$Seasons, ]

            ## linear model counting years
            lm2 <- lm(dataset[[avar]] ~ dataset$Year)

            ## plot
            par("mar" = c(0, 0, 0.5, 0.5))

            plot(dataset$Year, dataset[[avar]],
                 ylim     = ylim,
                 pch      = 19,
                 col      = get(paste0(c("col",
                                         unlist(strsplit(avar, split = "_" ))[1:2]),
                                       collapse = "_")),
                 cex      = .6,
                 ylab     = "",
                 xlab     = "",
                 xaxt     = "n",
                 yaxt     = "n",
                 cex.main = 0.9,
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 mgp  = c(2, 0.5, 0)
            )

            ## y axis
            if (i %in% c(7,12,17,22)){
                axis(2, pretty(ylim), las = 2,  cex.axis = 0.8)
                ## minor ticks
                axis(2, seq(-100, 100, by = 5), cex.axis = 0.8, labels = NA, tcl = -0.25)
            } else {
                # axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck = -0.03)
                ## minor ticks
                axis(2, seq(-100, 100, by = 5), labels = NA, tcl = -0.25/2)
            }

            ## x axis
            if (i %in% c(22, 23, 24)) {
                ## bottom row axis
                axis(1, pretty(dataset$Year), las = 1, cex.axis = 0.8, line =  0,   labels = NA)
                axis(1, pretty(dataset$Year), las = 1, cex.axis = 0.8, line = -0.5, tck = 0, lwd = 0)
                ## minor ticks
                axis(1, at = seq(1993, year(Sys.time()), by = 1), labels = NA,
                     tcl = -0.25)
            } else {
                ## major ticks
                # axis(1, pretty(dataset$Year), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(1, pretty(dataset$Year),                cex.axis = 0.8, labels = NA, tck = -0.04)
                ## minor ticks
                axis(1, seq(1993, year(Sys.time()), by = 1), cex.axis = 0.8, labels = NA, tcl = -0.3/2)
                # axis(1, at = seq(1993, year(Sys.time()), by = 1), labels = NA,
                #      tcl = +0.25/2)
            }

            abline(lm2)

            ## decorations
            fit <- lm2[[1]]
            pm <- lmtest::coeftest(lm2)[2,2]

            legend("bottom", lty = 1, bty = "n", lwd = 1, cex = 0.8,
                   inset = 0.03, # o.1
                   paste("Trend: ",
                         if (fit[2] > 0) '+' else '-',
                         signif(abs(fit[2]), 2),
                         "±", signif(2 * pm, 2), "%/y")
            )


            ## tag plots with letters
            legend("bottomright", 0, paste0("(",letters[lec],")"),
                   cex   = 1.1,
                   bty   = "n",
                   xjust = 0.5,      # 0.5 means center adjusted
                   yjust = 0.5,      # 0.5 means center adjusted
                   x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                   y.intersp =  0.2, # adjust character interspacing to effect box height
                   adj = c(0, 0.5))  # adjust string position (default values used here)



            if (i %in% c(7,12,17,22)) {
                mtext(text = bquote("Anomalies [%]"),
                      cex  = 0.6,
                      side = 2,
                      line = 2.3)
            }

            par("mar" = c(0,0,0,0))
        }

    }

    # 1
    par("mar" = c(0,0,0,0))
    plot.new()

    # 2
    plot.new()
    text(x = 0.5, y = 0.15,
         adj  = c(0.5, 0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.15,
         adj  = c(0.5, 0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.15,
         adj  = c(0.5, 0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    par(mfrow = c(1,1))
}
#+ echo=F, include=F





## __ Calculate trends for each season  ----------------------------------------
vars        <- c("GLB_att_des")

## vars are  set above
gather_seas <- data.frame()
for (DBn in dbs) {
    DB <- get(DBn)

    for (ase in Seasons) {
        for (avar in vars) {
            dataset <- DB[ Season == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ## linear model counting years
            lm2 <- lm(dataset[[avar]] ~ dataset$Year)
            ## correlation test
            cor1 <- cor.test(x = dataset[[avar]], y = as.numeric(dataset$Year), method = 'pearson')

            lag   <- 1
            dd    <- acf(dataset[[avar]], na.action = na.pass, plot = FALSE)
            N_eff <- sum(!is.na(dataset[[avar]])) * (1 - dd[lag][[1]]) / (1 + dd[lag][[1]])
            se_sq <- sum((lm1$residuals)^2, na.rm = T) / (N_eff - 2)
            sa_sq <- se_sq / sum((dataset[[avar]] - mean(dataset[[avar]], na.rm = T))^2, na.rm = T)
            t_eff     <- lm1$coefficients[[2]] / sa_sq
            #find two-tailed t critical values
            t_eff_cri <- qt(p = .05/2, df = N_eff, lower.tail = FALSE)

            conf      <- confint(lm1)
            conf_2.5  <- conf[2,1]
            conf_97.5 <- conf[2,2]

            ## trend per year with st error
            Tres <- data.frame(t(lmtest::coeftest(lm2)[2,]))
            Tint <- data.frame(t(lmtest::coeftest(lm2)[1,]))
            names(Tres) <- paste0("Tlm_", names(Tres))

            d   <- summary(lm2)$coefficients
            cat("lm:      ", lm2$coefficients[2] , "+/-", d[2,2] ,"\n\n")
            # lmtest::coeftest(lm2)

            ## gather stats
            gather_seas <- rbind(gather_seas,
                                 data.frame(
                                     linear_fit_stats(lm2,
                                                      confidence_interval = Daily_confidence_limit),
                                     cor_test_stats(cor1),
                                     DATA      = DBn,
                                     Season    = ase,
                                     var       = avar,
                                     N         = sum(!is.na(dataset[[avar]])),
                                     N_eff     = N_eff,
                                     t_eff     = t_eff,
                                     t_eff_cri = t_eff_cri,
                                     conf_2.5  = conf_2.5,
                                     conf_97.5 = conf_97.5,
                                     Tres
                                 ))
        }
    }
}

## __ Display data table  ------------------------------------------------------
#'
#' \newpage
#' \FloatBarrier
#'
#' #### Table of trends by season.
#'
#+ echo=F, include=T

wecare           <- grep("intercept", names(gather_seas), value = T, invert = T)
gather_seas      <- data.table(gather_seas)
gather_seas$DATA <- sub("_.*", "", gather_seas$DATA)


pprint           <- gather_seas[ , ..wecare]

pprint$cor.data_name        <- NULL
pprint[, slope.stat_sig     := 100 * (1 - slope.p)]
pprint[, slope.t            := NULL               ]
# pprint[, Rsqrd              := NULL               ]
# pprint[, RsqrdAdj           := NULL               ]
pprint[, N                  := NULL               ]
pprint[, slope.ConfInt_0.99 := NULL               ]
pprint[, slope.ConfInt_0.95 := NULL               ]
pprint[, DATA               := translate(DATA)    ]
pprint[, var                := translate(var)     ]

pprint$cor.data_name   <- NULL
pprint$cor.null_value  <- NULL
pprint$cor.method      <- NULL
pprint$cor.alternative <- NULL
pprint$cor.p           <- NULL

setorder(pprint, DATA, var)

#+ echo=F, include=T
pander(pprint,
       cap = "Slope is in %/year")
#+ echo=F, include=F




##  MONTHLY TRENDS  ############################################################

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Monthly trends
#'
#' We calculated monthly means from the daily means and the we produce the
#' seasonal data and the departure from the seasonal value in %.
#'
#+ echo=F, include=F


## __ Plot each month ----------------------------------------------------------

#+ TrendByMonth, echo=F, include=T, results="asis", fig.width=7, fig.height=11, out.height="97%"
# vars   <- c("DIR_att", "GLB_att")
vars   <- c("GLB_att_des")

## Monthly aggregation
dbs <- c(  "ALL_1_D_monthly_DESEAS",
         "CLEAR_1_D_monthly_DESEAS",
         "CLOUD_1_D_monthly_DESEAS")

for (DBn in dbs) {
    DB <- get(DBn)
    ## set seasons in each data base
    DB[, Month := month(Date) ]
    ## sanity check
    stopifnot(!any(is.na(DB$Month)))

    if (!FIGURESGRID) {
        cat("\n\\newpage\n")
        cat("\n#### ", translate(DBn), "\n\n")
    }

    for (avar in vars) {

        ## plot in a grid
        if (FIGURESGRID) {
            par(mfrow = c(6, 2))
            cat("\n\\newpage\n")
            cat("\n#### ", translate(DBn), translate(avar) , "\n\n")
        }

        ## common range
        ylim <- range(DB[[avar]],na.rm = TRUE)

        for (ase in 1:12) {

            dataset <- DB[Month == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ## linear model counting years
            lm2 <- lm(dataset[[avar]] ~ dataset$Year)

            ## plot
            par("mar" = c(2, 3.4, 2, 0.5))

            plot(dataset$Year, dataset[[avar]],
                 # ylim = ylim,
                 pch  = 19,
                 col  = get(paste0(c("col",
                                     unlist(strsplit(avar, split = "_" ))[1:2]),
                                   collapse = "_")),
                 cex      = 0.5,
                 main     = paste(month.name[ase], translate(DBn), translate(avar)),
                 xlab     = "",
                 ylab     = bquote("Anomaly [%]"),
                 cex.main = 0.9,
                 cex.lab  = 0.8,
                 cex.axis = 0.8,
                 mgp      = c(2, 0.5, 0)
            )
            # ylab = bquote("Deseas." ~ .(translate(avar)) ~ "[" ~ Watt/m^2 ~ "]" ) )

            abline(lm2)


            if (DRAFT) {
                ## plot running mean
                first <- head(which(!is.na(dataset[[avar]])),1)
                last  <- tail(which(!is.na(dataset[[avar]])),1)

                rm <- frollmean(dataset[[avar]][first:last],
                                running_mean_window_years,
                                na.rm = TRUE,
                                algo  = "exact",
                                align = "center")
                lines(dataset$Year[first:last], rm, col = "red")


                ## LOESS curve
                vec <- !is.na(dataset[[avar]])
                FTSE.lo3 <- loess.as(dataset$Year[vec], dataset[[avar]][vec],
                                     degree = 1,
                                     criterion = LOESS_CRITERIO, user.span = NULL, plot = F)
                FTSE.lo.predict3 <- predict(FTSE.lo3, dataset$Year)
                lines(dataset$Year, FTSE.lo.predict3, col = "cyan", lwd = 2.5)
            }


            ## decorations
            fit <- lm2[[1]]

            legend("bottom", lty = 1, bty = "n", lwd = 2, cex = 1,
                   paste("Trend: ",
                         if (fit[2] > 0) "+" else "-",
                         signif(abs(fit[2]), 3),
                         "% per year")
            )

        }
        par(mfrow = c(1, 1)) ## just reset layout


        ## __ Extreme values table ---------------------------------------------
        wca <- c("Year", "Month", grep(paste0("^", strsplit(avar, "_")[[1]][1]), names(DB), value = T ))

        cat("\n \n \\scriptsize \n ")
        cat(
            pander(
                DB[ order(abs(DB[[avar]]), decreasing = T )[1:10] , ..wca ],
                cap = "Extreme anomaly values"
            )
        )
        cat("\n \n \\normalsize \n ")

    }
}
#+ echo=F, include=F


## __ Calculate trend by each month  -------------------------------------------
vars        <- c("DIR_att_des", "GLB_att_des")
gather_seas <- data.frame()
for (DBn in dbs) {
    DB <- get(DBn)
    ## set seasons in each data base
    DB[, Month := month(Date) ]
    ## sanity check
    stopifnot( !any(is.na(DB$Month)) )
    for (ase in 1:12) {
        for (avar in vars) {
            dataset <- DB[ Month == ase, ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ## linear model counting years
            lm2 <- lm(dataset[[avar]] ~ dataset$Year)
            ## correlation test

            # if (sum(!is.na(dataset[[avar]])) > 2) {
            #     cor1    <- cor.test(x = dataset[[avar]], y = as.numeric(dataset$Date), method = 'pearson')
            #     tempcor <- cor_test_stats(cor1)
            # } else {
            #     tempcor <- NA
            # }

            ## gather stats
            temp <- data.frame(
                linear_fit_stats(lm2,
                                 confidence_interval = Daily_confidence_limit),
                # tempcor,
                DATA      = DBn,
                Month     = ase,
                var       = avar,
                N         = sum(!is.na(dataset[[avar]]))
            )
            gather_seas <- rbind(gather_seas, temp, fill = TRUE )

        }
    }
}



## __ Display table with trends and stats  -------------------------------------
#'
#' \FloatBarrier
#'
#' #### Table of trends by month.
#'
#+ echo=F, include=T

wecare        <- grep("intercept", names(gather_seas), value = T, invert = T)
gather_seas   <- data.table(gather_seas)
## some wired data where inserted
gather_seas   <- gather_seas[DATA != "TRUE",]
