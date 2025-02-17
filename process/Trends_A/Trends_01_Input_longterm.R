
01

## TODO include cloud data From era5

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

