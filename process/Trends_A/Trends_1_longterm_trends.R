
02

library(fANCOVA,    quietly = TRUE, warn.conflicts = FALSE)
library(astsa,      quietly = TRUE, warn.conflicts = FALSE)

source("~/CODE/FUNCTIONS/R/sumNA.R")
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
source("~/CODE/FUNCTIONS/R/cor_test_stats.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")

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

