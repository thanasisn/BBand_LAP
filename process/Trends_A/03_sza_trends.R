
03


source("~/CODE/FUNCTIONS/R/sumNA.R")
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")



## __ Flags --------------------------------------------------------------------

LOESS_CRITERIO <-  c("aicc", "gcv")[1]


##  Daily SZA trends for all year  ---------------------------------------------

#'
#' \FloatBarrier
#'
#' ## Daily SZA trends
#'
#+ echo=F, include=F

test <- ALL_2_daily_DESEAS[preNoon == TRUE & SZA > 70 & SZA < 86 ]
test <- rm.cols.dups.DT(test)
test <- rm.cols.NA.DT(test)


for (asz in unique(test$SZA)) {
    if (all(is.na(test[SZA == asz, GLB_att_des]))) next()

    # plot(test[SZA == asz, GLB_att_des, Date],
    #      main = asz)
    hist(test[SZA == asz, GLB_att_des], breaks = 100,
         main = asz)
    abline(v = 170, col = "red")
}
test[GLB_att_des > 180, .N , by = Date]

for (asz in unique(test$SZA)) {
    # plot(test[SZA == asz, GLB_att_des, Date],
    #      main = asz)
    if (all(is.na(test[SZA == asz, GLB_att]))) next()
    hist(test[SZA == asz, GLB_att], breaks = 100,
         main = asz)
    abline(v = 300, col = "red")
}
test[GLB_att_des > 180, .N , by = Date]






test <- CLOUD_2_daily_DESEAS[preNoon == TRUE & SZA > 70 & SZA < 86 ]
test <- rm.cols.dups.DT(test)
test <- rm.cols.NA.DT(test)

for (asz in unique(test$SZA)) {
    if (all(is.na(test[SZA == asz, GLB_att]))) next()

    # plot(test[SZA == asz, GLB_att, Date],
    #      main = asz)
    # abline(lm(test[SZA == asz, Date, GLB_att]), col = "red")

    plot(test[SZA == asz, GLB_att_des, Date],
         main = asz)
    abline(lm(test[SZA == asz, Date, GLB_att_des]), col = "red")


    # hist(test[SZA == asz, GLB_att_des], breaks = 100,
    #      main = asz)
    # abline(v = 170, col = "red")
}
test[GLB_att_des > 170, .N , by = Date]

for (asz in unique(test$SZA)) {
    # plot(test[SZA == asz, GLB_att_des, Date],
    #      main = asz)
    if (all(is.na(test[SZA == asz, GLB_att]))) next()
    hist(test[SZA == asz, GLB_att], breaks = 100,
         main = asz)
    abline(v = 300, col = "red")
}
test[GLB_att_des > 170, .N , by = Date]



test[SZA == 78 & GLB_att_des > 200, .N , by = Date ]



test <- ALL_2_daily_DESEAS[ SZA > 75 & SZA < 86 ]
test <- rm.cols.dups.DT(test)
test <- rm.cols.NA.DT(test)


count <- test[, sum(GLB_att_N) , by = .(SZA, preNoon, Date )]

tc <- count[, (diff(V1)), by = .(SZA, Date) ]

hist(tc$SZA)
plot(tc[SZA==76 , V1, Date])
plot(tc[SZA==77 , V1, Date])
# plot(tc[SZA==78 , V1, Date])
# plot(tc[SZA==79 , V1, Date])

tc[SZA==78 & V1 < -2]



##  EXCLUDE PROBLEMATIC DATA  --------------------------------------------------

dbs  <- c("ALL_2_daily_DESEAS",
          "CLEAR_2_daily_DESEAS",
          "CLOUD_2_daily_DESEAS",
          ## monthly
          "ALL_2_monthly_DESEAS",
          "CLEAR_2_monthly_DESEAS",
          "CLOUD_2_monthly_DESEAS",
          ## seasonal daily
          "ALL_2_bySeason_daily_DESEAS",
          "CLEAR_2_bySeason_daily_DESEAS",
          "CLOUD_2_bySeason_daily_DESEAS")

for (DBn in dbs) {
    DB <- get(DBn)
    # DB[ !(SZA > 77 & preNoon == TRUE) ]
    assign(DBn, DB[ !(SZA > 76 & preNoon == TRUE) ])
}

dbs         <- c(  "ALL_2_bySeason_daily_DESEAS",
                   "CLEAR_2_bySeason_daily_DESEAS",
                   "CLOUD_2_bySeason_daily_DESEAS")



## __ Calculate trend SZA ~ Day ------------------------------------------------

vars <- c("GLB_att_des", "GLB_att")

dbs  <- c(  "ALL_2_daily_DESEAS",
          "CLEAR_2_daily_DESEAS",
          "CLOUD_2_daily_DESEAS")

gather <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    for (avar in vars) {
        for (anoon in unique( DB$preNoon)) {
            for (asza in unique( DB$SZA )) {

                dataset <- DB[ SZA == asza & preNoon == anoon ]

                if (sum(!is.na(dataset[[avar]])) <= 1) next()

                lm1 <- lm( dataset[[avar]] ~ dataset$Date )

                gather <- rbind(gather,
                                data.frame(
                                    linear_fit_stats(lm1),
                                    preNoon = anoon,
                                    SZA     = asza,
                                    DATA    = DBn,
                                    var     = avar,
                                    N       = sum(!is.na(dataset[[avar]]))
                                ))
            }
        }
    }
}
#+ echo=F, include=F
gather    <- data.table(gather)
szatrends <- data.table(gather)
setorder(szatrends, SZA)






##__ Covert to trend per year --------------------------------------------------
szatrends[, slope    := slope    * Days_of_year ]
szatrends[, slope.sd := slope.sd * Days_of_year ]

## For raw values
# szatrends[, slope    := 100 * slope    / Days_of_year ]
# szatrends[, slope.sd := 100 * slope.sd / Days_of_year ]


## set some plot option for data
# szatrends[var == "DIR_att",    col := col_DIR_att    ]
# szatrends[var == "GLB_att",    col := col_GLB_att    ]
# szatrends[var == "DIR_transp", col := col_DIR_transp ]
szatrends[preNoon == T, pch := pch_am ]
szatrends[preNoon == F, pch := pch_pm ]






szatrends[DATA == "ALL_2_daily_DESEAS" & preNoon == TRUE & slope > 0.6]




## __ Plot trend ~ SZA stats from daily ----------------------------------------

## stats vars to plot
wecare <- grep("^slope|^N",  names(szatrends), ignore.case = T, value = T)
wecare <- grep("^slope\\.t", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope\\.sd", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope.Conf", wecare, ignore.case = T, value = T, invert = T)


warning("this should be _des  !!")
vars <- c("GLB_att_des")
# vars <- c("GLB_att")


#+ SzaTrends, echo=F, include=T, results = "asis"
for (avar in vars) {
    ## ALL - CS
    lec <- 0
    for (type in unique(szatrends$DATA)) {
        lec <- lec + 1

        cat("\n\\newpage\n\n")
        cat("\n#### ", translate(type), translate(avar) , "from daily\n\n")

        ## plot in a grid
        if (FIGURESGRID) {
            par(mfrow = c(ceiling(length(wecare)/2), 2))
        }

        par("mar" = c(4, 5, 2, 2))

        ## statistic variable
        for (awe in wecare) {
            awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

            ## Replace variable name
            if (awename == "Slope") { awename <- "Trend [%/y]" }

            ## limit plot p-values
            p_lim     <- 0.05

            szatrends <- data.table(szatrends)

            ## select All/CS and DIR/GLB/trans
            subdata <- szatrends[DATA == type &
                                 var  == avar, ]

            ## set symbols for plotting
            subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
            subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
            subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
            subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]

            ## plot only under accepted p-value limit
            # subdata <- subdata[ slope.p < p_lim, ]

            xlim <- range(subdata$SZA,    na.rm = T)
            ylim <- range(subdata[[awe]], na.rm = T)

            pam  <- subdata[preNoon == TRUE ]
            ppm  <- subdata[preNoon == FALSE]

            ccex <- ccex_sbs
            par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

            if (DRAFT == TRUE) {
                par("mar" = c(4,   5,   2,   2))
            } else {
                par("mar" = c(4, 4.5, 0.5, 0.5))
            }

            ## empty plot
            plot(1, type = "n",
                 xlab = "",
                 ylab = "",
                 xlim = xlim,
                 ylim = ylim,
                 yaxt = "n")

            ## y axis
            axis(2, pretty(ylim), las = 2)

            ## x axis
            axis(1, at = seq(xlim[1], xlim[2]), labels = NA,
                      tcl = -0.25)
            title(xlab = bquote("Solar zenith angle (SZA)"),
                  line = 2.5)

            title(ylab = awename,
                  line = 3.3)

            ## zero line
            abline(h = 0, lty = 3)

            if (DRAFT == TRUE) {
                # title(paste(translate(avar), awename, "for", translate(type)), cex.main =  .8 * ccex)
                title(paste(translate(avar), "trend", "for", translate(type)), cex.main =  .8 * ccex)
            } else {
                # translate(type)
                legend("bottomright", 0, paste0("(", letters[lec], ")"),
                       bty   = "n",
                       # cex   = .8 * ccex,
                       cex   = 1.7,
                       xjust = 0.5,      # 0.5 means center adjusted
                       yjust = 0.5,      # 0.5 means center adjusted
                       x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                       y.intersp =  0.2, # adjust character interspacing to effect box height
                       adj = c(0, 0.5))  # adjust string position (default values used here)
                # text.font = 2)  # bold the text if you like (not used here)
                par("mar" = c(4,   5,   2,   2))
            }

            legend("topleft",
                   legend = c("Morning", "Evening"),
                   # col    = c(unique(pam$col), unique(ppm$col)),
                   col    = c( 2,  3),
                   pch    = c(16, 17), ncol = 2, bty = "n",
                   cex    = ccex)

            ## morning lines
            lines(pam$SZA, pam[[awe]],
                  col  = 2,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## morning points
            points(pam$SZA, pam[[awe]],
                   pch = pam$pch,
                   col = 2,
                   cex = 1)

            ## evening lines
            lines(ppm$SZA, ppm[[awe]],
                  col  = 3,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## evening points
            points(ppm$SZA, ppm[[awe]],
                   pch = ppm$pch,
                   col = 3,
                   cex = 1)

            ccex <- 1
            par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

            cat("\n\n")
        }
        par(mfrow = c(1, 1)) ## just reset layout
    }
}



# __ Plot daily climatology --------------------------------------------------
#+ SzaClimaDaily, echo=F, include=T, results = "asis", out.heigth="30%"
vars <- c("GLB_att_seas")

dbs  <- c(  "ALL_2_daily_DESEAS",
          "CLEAR_2_daily_DESEAS",
          "CLOUD_2_daily_DESEAS")

for (DBn in dbs) {
    DB <- get(DBn)

    cat("\n\\newpage\n\n")
    cat("\n#### Daily SZA Climatology", translate(type), translate(avar), "\n\n")

    for (avar in vars) {
        for (asza in sort(unique( DB$SZA ))) {

            par("mar" = c(4, 4, 2, 2))

            dataset <- DB[ SZA == asza ]
            ## get only one season
            dataset <- dataset[!duplicated(dataset[ , c("SZA", "doy", "preNoon")]), ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ylim <- range(0, DB[[avar]], na.rm = T)
            xlim <- range(DB$doy)

            pam  <- dataset[preNoon == TRUE ]
            ppm  <- dataset[preNoon == FALSE]

            ## empty plot
            plot(1, type = "n",
                 xlab = "",
                 ylab = avar,
                 xlim = xlim,
                 ylim = ylim)

            ## morning lines
            points(pam$doy, pam[[avar]],
                   col  = 2,
                   pch  = pam$pch,
                   cex  = 1)

            points(ppm$doy, ppm[[avar]],
                   col  = 3,
                   pch  = ppm$pch,
                   cex  = 1)

            title(main = paste("Seasonal ", DBn, asza, sub("_des", "_seas", avar)))

            legend("top",
                   legend = c("Morning", "Evening"),
                   col    = c( 2,  3),
                   pch    = c(16, 17), ncol = 2, bty = "n",
                   cex    = ccex)

        }
    }
}




##  Monthly SZA trends for all year  -------------------------------------------

#'
#' \FloatBarrier
#'
#' ## Monthly SZA trends
#'
#+ echo=F, include=F


## __ Calculate trend SZA ~ Month ----------------------------------------------

vars <- c("GLB_att_des")

dbs  <- c(  "ALL_2_monthly_DESEAS",
          "CLEAR_2_monthly_DESEAS",
          "CLOUD_2_monthly_DESEAS")

gather <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)
    for (avar in vars) {
        for (anoon in unique( DB$preNoon)) {
            for (asza in unique( DB$SZA )) {

                dataset <- DB[ SZA == asza & preNoon == anoon ]

                if (sum(!is.na(dataset[[avar]])) <= 1) next()

                lm1 <- lm( dataset[[avar]] ~ dataset$Date )

                gather <- rbind(gather,
                                data.frame(
                                    linear_fit_stats(lm1),
                                    preNoon = anoon,
                                    SZA     = asza,
                                    DATA    = DBn,
                                    var     = avar,
                                    N       = sum(!is.na(dataset[[avar]]))
                                ))
            }
        }
    }
}
#+ echo=F, include=F
szatrends_M <- data.table(gather)
setorder(szatrends_M, SZA)

##__ covert to trend per year --------------------------------------------------
szatrends_M[, slope    := slope    * Days_of_year ]
szatrends_M[, slope.sd := slope.sd * Days_of_year ]

## set some plot option for data
szatrends_M[preNoon == T, pch := pch_am ]
szatrends_M[preNoon == F, pch := pch_pm ]






## __ Plot trend ~ SZA stats from monthly --------------------------------------

## stats vars to plot
wecare <- grep("^slope|^N",  names(szatrends_M), ignore.case = T, value = T)
wecare <- grep("^slope\\.t", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope\\.sd", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope.Conf", wecare, ignore.case = T, value = T, invert = T)

vars <- c("GLB_att_des")

## TODO separate plots by direct global

#+ SzaTrendsMonthly, echo=F, include=T, results = "asis"
for (avar in vars) {
    ## ALL - CS
    for (type in unique(szatrends_M$DATA)) {

        cat("\n\\newpage\n\n")
        cat("\n#### ", translate(type), translate(avar), "from monthly\n\n")

        ## plot in a grid
        if (FIGURESGRID) {
            par(mfrow = c(ceiling(length(wecare)/2), 2))
        }

        par("mar" = c(4, 5, 2, 2))

        ## statistic variable
        for (awe in wecare) {
            awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

            ## Replace variable name
            if (awename == "Slope") { awename <- "Trend [%/y]" }

            ## limit plot p-values
            p_lim       <- 0.05

            szatrends_M <- data.table(szatrends_M)

            ## select All/CS and DIR/GLB/trans
            subdata <- szatrends_M[DATA == type &
                                   var  == avar, ]

            ## set symbols for plotting
            subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
            subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
            subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
            subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]


            ## plot only under accepted p-value limit
            # subdata <- subdata[ slope.p < p_lim, ]

            xlim <- range(subdata$SZA,    na.rm = T)
            ylim <- range(subdata[[awe]], na.rm = T)

            pam  <- subdata[preNoon == TRUE ]
            ppm  <- subdata[preNoon == FALSE]

            ccex <- ccex_sbs
            par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

            if (DRAFT == TRUE) {
                par("mar" = c(4,   5,   2,   2))
            } else {
                par("mar" = c(4, 4.5, 0.5, 0.5))
            }

            ## empty plot
            plot(1, type = "n",
                 xlab = "",
                 ylab = awename,
                 xlim = xlim,
                 ylim = ylim,
                 yaxt = "n")

            ## y axis
            axis(2, pretty(ylim), las = 2)

            ## x axis
            axis(1, at = seq(xlim[1], xlim[2]), labels = NA,
                 tcl = -0.25)
            title(xlab = bquote("Solar zenith angle (SZA)"),
                  line = 2.5)

            ## zero line
            abline(h = 0, lty = 3)

            if (DRAFT == TRUE) {
                # title(paste(translate(avar), awename, "for", translate(type)), cex.main =  .8 * ccex)
                title(paste(translate(avar), "trend", "for", translate(type)), cex.main =  .8 * ccex)
            } else {
                legend("bottomleft", 0, translate(type),
                       bty   = "n",
                       cex   = .8 * ccex,
                       xjust = 0.5,      # 0.5 means center adjusted
                       yjust = 0.5,      # 0.5 means center adjusted
                       x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                       y.intersp =  0.2, # adjust character interspacing to effect box height
                       adj = c(0, 0.5))  # adjust string position (default values used here)
                # text.font = 2)  # bold the text if you like (not used here)
                par("mar" = c(4,   5,   2,   2))
            }

            legend("topleft",
                   legend = c("Morning", "Evening"),
                   # col    = c(unique(pam$col), unique(ppm$col)),
                   col    = c( 2,  3),
                   pch    = c(16, 17), ncol = 2, bty = "n",
                   cex    = ccex)

            ## morning lines
            lines(pam$SZA, pam[[awe]],
                  col  = 2,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## morning points
            points(pam$SZA, pam[[awe]],
                   pch = pam$pch,
                   col = 2,
                   cex = 1)

            ## evening lines
            lines(ppm$SZA, ppm[[awe]],
                  col  = 3,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## evening points
            points(ppm$SZA, ppm[[awe]],
                   pch = ppm$pch,
                   col = 3,
                   cex = 1)

            ccex <- 1
            par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

            cat("\n\n")
        }
        par(mfrow = c(1, 1)) ## just reset layout
    }
}


# __ Plot Monthly climatology --------------------------------------------------
#+ SzaClimaMonthly, echo=F, include=T, results = "asis", out.heigth="30%"
vars <- c("GLB_att_seas")

dbs  <- c(  "ALL_2_monthly_DESEAS",
            "CLEAR_2_monthly_DESEAS",
            "CLOUD_2_monthly_DESEAS")

for (DBn in dbs) {
    DB <- get(DBn)

    cat("\n\\newpage\n\n")
    cat("\n#### Monthly SZA Climatology", translate(type), translate(avar), "\n\n")

    for (avar in vars) {
        for (asza in sort(unique( DB$SZA ))) {

            par("mar" = c(4, 4, 2, 2))

            dataset <- DB[ SZA == asza ]
            ## get only one season
            dataset <- dataset[!duplicated(dataset[ , c("SZA", "Month", "preNoon")]), ]

            if (sum(!is.na(dataset[[avar]])) <= 1) next()

            ylim <- range(0, DB[[avar]], na.rm = T)
            xlim <- range(1:12)

            pam  <- dataset[preNoon == TRUE ]
            ppm  <- dataset[preNoon == FALSE]

            ## empty plot
            plot(1, type = "n",
                 xlab = "",
                 ylab = avar,
                 xlim = xlim,
                 ylim = ylim)

            ## morning lines
            points(pam$Month, pam[[avar]],
                   col  = 2,
                   pch  = pam$pch,
                   cex  = 1)

            points(ppm$Month, ppm[[avar]],
                   col  = 3,
                   pch  = ppm$pch,
                   cex  = 1)

            title(main = paste("Seasonal ", DBn, asza, sub("_des", "_seas", avar)))

            legend("bottom",
                   legend = c("Morning", "Evening"),
                   col    = c( 2,  3),
                   pch    = c(16, 17), ncol = 2, bty = "n",
                   cex    = ccex)

        }
    }
}


## ........................................................................ ----




##  SZA trends for season of year from daily -----------------------------------

#'
#' ## Plot of SZA trends for each season of year from daily
#'
#+ echo=F, include=F


## __ Calculate trends SZA ~ Season from daily  --------------------------------

# vars        <- c("DIR_att_des", "GLB_att_des", "DIR_transp_des")
vars        <- c("GLB_att_des")

dbs         <- c(  "ALL_2_bySeason_daily_DESEAS",
                 "CLEAR_2_bySeason_daily_DESEAS",
                 "CLOUD_2_bySeason_daily_DESEAS")
seasons     <- c("Winter", "Spring", "Summer", "Autumn")
gather_seas <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)

    stopifnot( !any(is.na(DB$Season)) )

    for (ase in seasons) {
        for (avar in vars) {
            for (anoon in unique( DB$preNoon)) {
                for (asza in unique( DB$SZA )) {

                    dataset <- DB[ SZA == asza & preNoon == anoon & Season == ase ]

                    if (sum(!is.na(dataset[[avar]])) <= 1) next()

                    lm1 <- lm(dataset[[avar]] ~ dataset$Date)

                    gather_seas <- rbind(gather_seas,
                                    data.frame(
                                        linear_fit_stats(lm1),
                                        preNoon   = anoon,
                                        SZA       = asza,
                                        DATA      = DBn,
                                        var       = avar,
                                        Season    = ase,
                                        N         = sum(!is.na(dataset[[avar]]))
                                    ))
                }
            }
        }
    }
}
#+ echo=F, include=F
szatrends_seas <- data.table(gather_seas)
setorder(szatrends_seas, SZA)

hist(gather_seas$N[gather_seas$N > 50], breaks = 100)






##__ Covert to trend per year --------------------------------------------------
szatrends_seas[, slope    := slope    * Days_of_year]
szatrends_seas[, slope.sd := slope.sd * Days_of_year]

# szatrends_seas[, slope    := 100 * slope    / Days_of_year]
# szatrends_seas[, slope.sd := 100 * slope.sd / Days_of_year]


## define plot colors
szatrends_seas[ var == "DIR_att_des", col := col_DIR_att   ]
szatrends_seas[ var == "GLB_att_des", col := col_GLB_att   ]
szatrends_seas[ var == "DIR_transp",  col := col_DIR_transp]
szatrends_seas[ preNoon == T, pch := pch_am ]
szatrends_seas[ preNoon == F, pch := pch_pm ]





## __ Plot SZA ~ Season stats  -------------------------------------------------

## stats vars to plot
wecare <- grep( "^slope|^N", names(szatrends_seas), ignore.case = T, value = T)
wecare <- grep("^slope\\.t", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope\\.sd", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope.Conf", wecare, ignore.case = T, value = T, invert = T)

wecare <- "slope"

# FIGURESGRID <- TRUE

#+ SzaTrendsSeas, echo=F, include=T, results = "asis", out.heigth="30%"
## Winter - Summer ....
for (ase in seasons) {
    ## ALL - Clear sky
    for (type in unique(szatrends_seas$DATA)) {
        ## DIR - GLB - transp
        for (avar in unique(szatrends_seas$var)) {

            cat("\n\\newpage\n\n")
            cat(paste("###",ase, translate(type), translate(avar),"\n\n"))

            ## plot in a grid
            if (FIGURESGRID) {
                par(mfrow = c(ceiling(length(wecare)/2), 2))
            }

            ## statistic variable
            for (awe in wecare) {
                awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

                par("mar" = c(4,4,1,0))

                ## limit plot p-values
                p_lim     <- 0.05

                ## select All/CS  DIR/GLB/trans winter/summer
                subdata <- szatrends_seas[ DATA   == type &
                                           var    == avar &
                                           Season == ase    , ]

                ## set symbols for plotting
                subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
                subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
                subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
                subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]


                # xlim <- range( subdata$SZA,        na.rm = T )
                ## use same axis for all
                xlim <- range(szatrends_seas$SZA, na.rm = T)
                ylim <- range(subdata[[awe]],     na.rm = T)

                ## test always show zero on plots
                ylim <- range(0, subdata[[awe]], na.rm = T)


                pam  <- subdata[ preNoon == T ]
                ppm  <- subdata[ preNoon == F ]


                ccex <- ccex_sbs
                par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

                if (DRAFT == TRUE) {
                    par("mar" = c(4,   5,   2,   2))
                } else {
                    par("mar" = c(4.5, 4.5, 0.5, 0.5))
                }


                plot(1, type = "n",
                     xlab = "",
                     ylab = awename,
                     xlim = xlim,
                     ylim = ylim,
                     yaxt = "n")

                ## y axis
                axis(2, pretty(ylim), las = 2)

                ## x axis
                axis(1, at = seq(xlim[1], xlim[2]), labels = NA,
                     tcl = -0.25)
                title(xlab = bquote("Solar zenith angle (SZA)"),
                      line = 2.5)

                ## zero line
                abline(h = 0, lty = 3)

                ## test for some plots
                if (grepl("CLEAR", type, ignore.case = T)) typeP <- "Clear Sky"
                if (grepl("ALL",   type, ignore.case = T)) typeP <- "All Sky"

                title(paste(ase, awename, typeP, translate(avar)), cex.main = 0.8,)

                ## morning lines
                lines(pam$SZA, pam[[awe]],
                      col  = 2,
                      type = "c",
                      lwd  = ccex,
                      cex  = 0.8)
                ## morning points
                points(pam$SZA, pam[[awe]],
                       pch = pam$pch,
                       col = 2,
                       cex = 0.8)

                ## evening lines
                lines(ppm$SZA, ppm[[awe]],
                      col  = 3,
                      type = "c",
                      lwd  = ccex,
                      cex  = 0.8)
                ## evening points
                points(ppm$SZA, ppm[[awe]],
                       pch = ppm$pch,
                       col = 3,
                       cex = 0.8)

                # legend("top",
                #        legend = c("Morning", "Evening"),
                #        # col    = c(unique(pam$col), unique(ppm$col)),
                #        col    = c( 2,  3),
                #        pch    = c(16, 17), ncol = 2, bty = "n",
                #        cex    = ccex)

                ## reset fonts
                ccex <- 1
                par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

                legend("bottom",
                       legend = c("Morning",       "Evening"),
                       # col    = c(unique(pam$col), unique(ppm$col)),
                       col    = c(2, 3),
                       pch    = c(16, 17), ncol = 2, bty = "n")
            }
            par(mfrow = c(1, 1)) ## just reset layout
        }
    }
}



## __ by season daily in tight grid --------------------------------------------


#'
#' ### Grid of SZA trends for each season of year from daily
#'
#+ echo=F, include=F

#+ SzaTrendsSeasTogether, echo=F, include=T
{
    vars        <- c("GLB_att_des") ## original
    # vars        <- c("GLB_att")
    avar        <- vars[1]
    dbs         <- c(  "ALL_2_bySeason_daily_DESEAS",
                     "CLEAR_2_bySeason_daily_DESEAS",
                     "CLOUD_2_bySeason_daily_DESEAS")
    Seasons     <- c("Winter", "Spring", "Summer", "Autumn")
    lec         <- 0

    ## the order must be predefined to match
    expanded <- expand.grid(Dataset = dbs, Seasons = Seasons, stringsAsFactors = FALSE)

    nf <- layout(
        matrix(1:30, ncol = 5, byrow = TRUE),
        widths  = c(0.25,   1,1,1, 0.05),
        heights = c(0.1,  1,1,1,1, 0.5 )
    )
    layout.show(nf)

    # 1
    par("mar" = c(0, 0, 0, 0))
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
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }
        if (i == 15) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }


        if (i == 16) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }
        if (i == 20) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }


        if (i == 21) {
            plot.new()
            text(x = 0.05, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }
        if (i == 25) {
            plot.new()
            text(x = 0.25, y = 0.5,
                 adj  = c(0.5, 0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }


        ## actual plots
        if (! i %in% c(6,11,16,21,10,15,20,25)) {

            lec <- lec + 1
            par("mar" = c(0,0,0,0))

            kk       <- expanded[ 1, ]
            expanded <- expanded[-1, ]

            ## limit plot p-values
            p_lim    <- 0.05
            N_lim    <- 85


            ## select All/CS  DIR/GLB/trans winter/summer
            subdata <- szatrends_seas[ DATA   == kk$Dataset &
                                       var    == avar &
                                       Season == kk$Seasons, ]

            ## set symbols for plotting
            subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
            subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
            subdata[ N       <= N_lim & preNoon == TRUE,  pch :=  1 ]

            subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
            subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]
            subdata[ N       <= N_lim & preNoon == FALSE, pch :=  2 ]


            # xlim <- range( subdata$SZA,        na.rm = T )
            ## use same axis for all
            xlim <- range(szatrends_seas$SZA, na.rm = T)

            ## test always show zero on plots
            ylim <- range(0, szatrends_seas$slope, na.rm = T)
            # ylim <- c(-2.5, 4.5)

            # ylim <- range(szatrends_seas[slope.p < p_lim & SZA < 75 , slope] , na.rm = T)
            ylim <- range(szatrends_seas[slope.p < p_lim & N > N_lim , slope], na.rm = T)


            pam  <- subdata[ preNoon == T ]
            ppm  <- subdata[ preNoon == F ]

            ## empty plot
            par("mar" = c(0, 0, 0.5, 0.5))
            plot(1, type = "n",
                 cex      = .6,
                 cex.axis = 0.8,
                 cex.lab  = 0.8,
                 cex.main = 0.9,
                 mgp      = c(2, 0.5, 0),
                 xaxt     = "n",
                 xlab     = "",
                 xlim     = xlim,
                 yaxt     = "n",
                 ylab     = "",
                 ylim     = ylim,
            )

            ## y axis
            if (i %in% c(7,12,17,22)){
                axis(2, pretty(ylim), las = 2, cex.axis = 0.8,              tck = -0.06   )
                axis(2, seq(-3, 3, by = 0.1),  cex.axis = 0.8, labels = NA, tck = -0.03   )
            } else {
                # axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(2, pretty(ylim),          cex.axis = 0.8, labels = NA, tck = -0.03   )
                axis(2, seq(-3, 3, by = 0.1),  cex.axis = 0.8, labels = NA, tck = -0.03/2 )

            }

            ## x axis
            if (i %in% c(22, 23, 24)) {
                ## bottom row axis
                axis(1, seq(5, 90, 5), las = 1, cex.axis = 0.8, line =  0,   labels = NA)
                axis(1, seq(5, 90, 5), las = 1, cex.axis = 0.8, line = -0.5, tck = 0, lwd = 0)
                ## minor ticks
                axis(1, at = seq(5, 90, 1), labels = NA, tcl = -0.25)
            } else {
                ## major ticks
                # axis(1, seq(5, 90, 5), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(1, seq(5, 90, 5), cex.axis = 0.8, labels = NA, tck = -0.04)
                ## minor ticks
                axis(1, seq(5, 90, 1), cex.axis = 0.8, labels = NA, tcl = -0.3/2)
                # axis(1, at = seq(5, 90, 1), labels = NA,
                #      tcl = +0.25/2)
            }

            ## zero line
            abline(h = 0, lty = 3)

            ccex <- .8

            ## morning lines
            lines(pam$SZA, pam[[awe]],
                  col  = 2,
                  type = "c",
                  lwd  = 1,
                  cex  = ccex)
            ## morning points
            points(pam$SZA, pam[[awe]],
                   pch    = pam$pch,
                   lwd    = .5,
                   col    = 2,
                   cex    = ccex)

            ## evening lines
            lines(ppm$SZA, ppm[[awe]],
                  col  = 3,
                  type = "c",
                  lwd  = 1,
                  cex  = ccex)
            ## evening points
            points(ppm$SZA, ppm[[awe]],
                   pch    = ppm$pch,
                   lwd    = .5,
                   col    = 3,
                   cex    = ccex)

            ## tag plots with letters
            legend("bottomright", 0, paste0("(",letters[lec],")"),
                   cex   = 1.1,
                   bty   = "n",
                   xjust = 0.5,      # 0.5 means center adjusted
                   yjust = 0.5,      # 0.5 means center adjusted
                   x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                   y.intersp =  0.2, # adjust character interspacing to effect box height
                   adj = c(0, 0.5))  # adjust string position (default values used here)


            ## legend on any of the plots
            if (i %in% c(7)) {
                # legend("top",
                #        legend = c("Morning low stat. sig.", "Evening low stat. sig.",
                #                   "Morning",                "Evening"),
                #        col    = c(2, 3),
                #        pt.cex = 1,
                #        cex    = 0.8,
                #        pch    = c(1, 2, 16, 17),
                #        ncol   = 2,
                #        bty = "n")
                legend("topleft",
                       legend = c("Morning",                "Evening",
                                  "Morning low stat. sig.", "Evening low stat. sig."),
                       col    = c(2, 3),
                       pt.cex = 1,
                       cex    = 0.8,
                       pch    = c(16, 17, 1, 2),
                       ncol   = 1,
                       bty = "n")
            }

            ## x labels
            if (i %in% c(22, 23, 24)) {
                mtext(text = bquote("Solar zenith angle (SZA)"),
                      cex  = 0.6,
                      side = 1,
                      line = 1.3)
                }


            ## y labels
            if (i %in% c(7,12,17,22)) {
                mtext(text = bquote("Trend [%/y]"),
                      cex  = 0.6,
                      side = 2,
                      line = 2.3)
            }
            ## set all to zero margin
            par("mar" = c(0,0,0,0))
        }
    }

    # 1
    par("mar" = c(0,0,0,0))
    plot.new()

    # 2
    plot.new()
    text(x = 0.5, y = 0.23,
         adj  = c(0.6,0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.23,
         adj  = c(0.5, 0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.23,
         adj  = c(0.5, 0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    par(mfrow = c(1, 1))
}
#+ echo=F, include=F





##  SZA trends for season of year from monthly -----------------------------------

#'
#' ## Plot of SZA trends for each season of year from monthly
#'
#+ echo=F, include=F


## __ Calculate SZA ~ Season stats  --------------------------------------------

vars          <- c("GLB_att_des")

dbs           <- c(  "ALL_2_bySeason_monthly_DESEAS",
                 "CLEAR_2_bySeason_monthly_DESEAS",
                 "CLOUD_2_bySeason_monthly_DESEAS")
seasons       <- c("Winter", "Spring", "Summer", "Autumn")
gather_seas_M <- data.frame()

for (DBn in dbs) {
    DB <- get(DBn)

    stopifnot( !any(is.na(DB$Season)) )

    for (ase in seasons) {
        for (avar in vars) {
            for (anoon in unique(DB$preNoon)) {
                for (asza in unique( DB$SZA )) {

                    dataset <- DB[ SZA == asza & preNoon == anoon & Season == ase ]

                    if (sum(!is.na(dataset[[avar]])) <= 1)       next()
                    if (sum(dataset[[avar]], na.rm = TRUE) == 0) next()

                    lm1 <- lm(dataset[[avar]] ~ dataset$Date)

                    gather_seas_M <- rbind(gather_seas_M,
                                         data.frame(
                                             linear_fit_stats(lm1),
                                             preNoon   = anoon,
                                             SZA       = asza,
                                             DATA      = DBn,
                                             var       = avar,
                                             Season    = ase,
                                             N         = sum(!is.na(dataset[[avar]]))
                                         ))
                }
            }
        }
    }
}
#+ echo=F, include=F

szatrends_seas_M <- data.table(gather_seas_M)
setorder(szatrends_seas_M, SZA)


## covert to trend per year
szatrends_seas_M[, slope    := slope    * Days_of_year]
szatrends_seas_M[, slope.sd := slope.sd * Days_of_year]

# szatrends_seas[, slope    := 100 * slope    / Days_of_year]
# szatrends_seas[, slope.sd := 100 * slope.sd / Days_of_year]


## define plot colors
szatrends_seas_M[ var == "DIR_att_des", col := col_DIR_att   ]
szatrends_seas_M[ var == "GLB_att_des", col := col_GLB_att   ]
szatrends_seas_M[ var == "DIR_transp",  col := col_DIR_transp]
szatrends_seas_M[ preNoon == T, pch := pch_am ]
szatrends_seas_M[ preNoon == F, pch := pch_pm ]





## __ Plot SZA ~ Season stats  -------------------------------------------------

## stats vars to plot
wecare <- grep( "^slope|^N", names(szatrends_seas_M), ignore.case = T, value = T)
wecare <- grep("^slope\\.t", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope\\.sd", wecare, ignore.case = T, value = T, invert = T)
wecare <- grep("slope.Conf", wecare, ignore.case = T, value = T, invert = T)

wecare <- "slope"

# FIGURESGRID <- TRUE

#+ SzaTrendsSeasMonthly, echo=F, include=T, results = "asis"
## Winter - Summer ....
for (ase in seasons) {
    ## ALL - Clear sky
    for (type in unique(szatrends_seas_M$DATA)) {
        ## DIR - GLB - transp
        for (avar in unique(szatrends_seas_M$var)) {

            cat("\n\\newpage\n\n")
            cat(paste("###",ase, translate(type), translate(avar),"\n\n"))

            ## plot in a grid
            if (FIGURESGRID) {
                par(mfrow = c(ceiling(length(wecare)/2), 2))
            }


            ## statistic variable
            for (awe in wecare) {
                awename <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", sub("\\."," ", awe), perl = TRUE)

                par("mar" = c(4,4,1,0))

                ## limit plot p-values
                p_lim     <- 0.05

                ## select All/CS  DIR/GLB/trans winter/summer
                subdata <- szatrends_seas_M[ DATA   == type &
                                                 var    == avar &
                                                 Season == ase    , ]

                ## set symbols for plotting
                subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
                subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
                subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
                subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]


                # xlim <- range( subdata$SZA,        na.rm = T )
                ## use same axis for all
                xlim <- range(szatrends_seas$SZA, na.rm = T)
                ylim <- range(subdata[[awe]],     na.rm = T)

                ## test always show zero on plots
                ylim <- range(0, subdata[[awe]], na.rm = T)


                pam  <- subdata[ preNoon == T ]
                ppm  <- subdata[ preNoon == F ]


                ccex <- ccex_sbs
                par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

                if (DRAFT == TRUE) {
                    par("mar" = c(4,   5,   2,   2))
                } else {
                    par("mar" = c(4.5, 4.5, 0.5, 0.5))
                }


                plot(1, type = "n",
                     xlab = "",
                     ylab = awename,
                     xlim = xlim,
                     ylim = ylim,
                     yaxt = "n")

                ## y axis
                axis(2, pretty(ylim), las = 2)

                ## x axis
                axis(1, at = seq(xlim[1], xlim[2]), labels = NA,
                     tcl = -0.25)
                title(xlab = bquote("Solar zenith angle (SZA)"),
                      line = 2.5)

                ## zero line
                abline(h = 0, lty = 3)

                ## test for some plots
                if (grepl("CLEAR", type, ignore.case = T)) typeP <- "Clear Sky"
                if (grepl("ALL",   type, ignore.case = T)) typeP <- "All Sky"

                title(paste(ase, awename, typeP, translate(avar)), cex.main = 0.8,)

                ## morning lines
                lines(pam$SZA, pam[[awe]],
                      col  = 2,
                      type = "c",
                      lwd  = ccex,
                      cex  = 0.8)
                ## morning points
                points(pam$SZA, pam[[awe]],
                       pch = pam$pch,
                       col = 2,
                       cex = 0.8)

                ## evening lines
                lines(ppm$SZA, ppm[[awe]],
                      col  = 3,
                      type = "c",
                      lwd  = ccex,
                      cex  = 0.8)
                ## evening points
                points(ppm$SZA, ppm[[awe]],
                       pch = ppm$pch,
                       col = 3,
                       cex = 0.8)

                # legend("top",
                #        legend = c("Morning", "Evening"),
                #        # col    = c(unique(pam$col), unique(ppm$col)),
                #        col    = c( 2,  3),
                #        pch    = c(16, 17), ncol = 2, bty = "n",
                #        cex    = ccex)

                ## reset fonts
                ccex <- 1
                par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

                legend("bottom",
                       legend = c("Morning",       "Evening"),
                       # col    = c(unique(pam$col), unique(ppm$col)),
                       col    = c(2, 3),
                       pch    = c(16, 17), ncol = 2, bty = "n")
            }
            par(mfrow = c(1, 1)) ## just reset layout
        }
    }
}



## __ by season in a tight grid ----------------------------------------------
#+ SzaTrendsSeasTogetherMonthly, echo=F, include=T
{
    vars        <- c("GLB_att_des")
    avar        <- vars[1]
    dbs         <- c(  "ALL_2_bySeason_daily_DESEAS",
                       "CLEAR_2_bySeason_daily_DESEAS",
                       "CLOUD_2_bySeason_daily_DESEAS")
    Seasons     <- c("Winter", "Spring", "Summer", "Autumn")

    ## the order must be predefined to match
    expanded <- expand.grid(Dataset = dbs, Seasons = Seasons, stringsAsFactors = FALSE)

    nf <- layout(
        matrix(1:30, ncol = 5, byrow = TRUE),
        widths  = c(0.3,   1,1,1, 0.1),
        heights = c(0.2, 1,1,1,1, 0.5)
    )
    layout.show(nf)

    # 1
    par("mar"=c(0,0,0,0))
    plot.new()
    # 2
    plot.new()
    text(x = 0.5, y = 0.5,
         adj  = c(0.6,0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.5,
         adj  = c(0.5,0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.5,
         adj  = c(0.5,0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    for (i  in 6:25) {

        if (i == 6) {
            plot.new()
            text(x = 0.1, y = 0.5,
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
            text(x = 0.1, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }
        if (i == 15) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Spring", cex = 0.9, font = 2)
        }


        if (i == 16) {
            plot.new()
            text(x = 0.1, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }
        if (i == 20) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Summer", cex = 0.9, font = 2)
        }


        if (i == 21) {
            plot.new()
            text(x = 0.1, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }
        if (i == 25) {
            plot.new()
            text(x = 0.5, y = 0.5,
                 adj  = c(0.5,0.5),
                 srt  = 90, "Autumn", cex = 0.9, font = 2)
        }


        ## actual plots
        if (! i %in% c(6,11,16,21,10,15,20,25)) {

            par("mar"=c(0,0,0,0))

            kk       <- expanded[1,]
            expanded <- expanded[-1, ]

            ## limit plot p-values
            p_lim     <- 0.05

            ## select All/CS  DIR/GLB/trans winter/summer
            subdata <- szatrends_seas[ DATA   == kk$Dataset &
                                           var    == avar &
                                           Season == kk$Seasons, ]

            ## set symbols for plotting
            subdata[ slope.p  < p_lim & preNoon == TRUE,  pch := 16 ]
            subdata[ slope.p >= p_lim & preNoon == TRUE,  pch :=  1 ]
            subdata[ slope.p  < p_lim & preNoon == FALSE, pch := 17 ]
            subdata[ slope.p >= p_lim & preNoon == FALSE, pch :=  2 ]


            # xlim <- range( subdata$SZA,        na.rm = T )
            ## use same axis for all
            xlim <- range(szatrends_seas$SZA, na.rm = T)

            ## test always show zero on plots
            ylim <- range(0, szatrends_seas$slope, na.rm = T)
            # ylim <- c(-2.5, 4.5)

            ylim <- range(szatrends_seas[slope.p < p_lim & SZA < 75, slope] , na.rm = T)


            pam  <- subdata[ preNoon == T ]
            ppm  <- subdata[ preNoon == F ]

            ## plot
            par("mar" = c(0, 0, 0.3, 0.3))


            plot(1, type = "n",
                 cex      = .6,
                 cex.axis = 0.8,
                 cex.lab  = 0.8,
                 cex.main = 0.9,
                 mgp      = c(2, 0.5, 0),
                 xaxt     = "n",
                 xlab     = "",
                 xlim     = xlim,
                 yaxt     = "n",
                 ylab     = "",
                 ylim     = ylim,
            )

            ## y axis
            if (i %in% c(7,12,17,22)){
                axis(2, pretty(ylim), las = 2, cex.axis = 0.8)
            } else {
                axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(2, pretty(ylim), cex.axis = 0.8, labels = NA, tck = -0.03)
            }


            ## x axis
            if (i %in% c(22, 23, 24)) {
                ## bottom row axis
                axis(1, seq(5, 90, 5), las = 1, cex.axis = 0.8, line =  0,   labels = NA)
                axis(1, seq(5, 90, 5), las = 1, cex.axis = 0.8, line = -0.5, tck = 0, lwd = 0)
                ## minor ticks
                axis(1, at = seq(5, 90, 1), labels = NA,
                     tcl = -0.25)
            } else {
                ## major ticks
                axis(1, seq(5, 90, 5), cex.axis = 0.8, labels = NA, tck =  0.03)
                axis(1, seq(5, 90, 5), cex.axis = 0.8, labels = NA, tck = -0.03)
                ## minor ticks
                axis(1, at = seq(5, 90, 1), labels = NA,
                     tcl = -0.25/2)
                axis(1, at = seq(5, 90, 1), labels = NA,
                     tcl = +0.25/2)
            }

            ## zero line
            abline(h = 0, lty = 3)


            ## morning lines
            lines(pam$SZA, pam[[awe]],
                  col  = 2,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## morning points
            points(pam$SZA, pam[[awe]],
                   pch = pam$pch,
                   col = 2,
                   cex = 1)

            ## evening lines
            lines(ppm$SZA, ppm[[awe]],
                  col  = 3,
                  type = "c",
                  lwd  = ccex,
                  cex = 1)
            ## evening points
            points(ppm$SZA, ppm[[awe]],
                   pch = ppm$pch,
                   col = 3,
                   cex = 1)


            if (i %in% c(7)) {
                # legend("top",
                #        legend = c("Morning low stat. sig.", "Evening low stat. sig.",
                #                   "Morning",                "Evening"),
                #        col    = c(2, 3),
                #        pt.cex = 1,
                #        cex    = 0.8,
                #        pch    = c(1, 2, 16, 17),
                #        ncol   = 2,
                #        bty = "n")
                legend("topleft",
                       legend = c("Morning",                "Evening",
                                  "Morning low stat. sig.", "Evening low stat. sig."),
                       col    = c(2, 3),
                       pt.cex = 1,
                       cex    = 0.8,
                       pch    = c(16, 17, 1, 2),
                       ncol   = 1,
                       bty = "n")

            }


            if (i %in% c(22, 23, 24)) {
                mtext(text = bquote("Solar zenith angle (SZA)"),
                      cex  = 0.6,
                      side = 1,
                      line = 1.3)
            }


            if (i %in% c(7,12,17,22)) {
                mtext(text = bquote("Trend [%/y]"),
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
    text(x = 0.5, y = 0.23,
         adj  = c(0.6,0.5),
         "All skies",    cex = 0.9, font = 2)

    # 3
    plot.new()
    text(x = 0.5, y = 0.23,
         adj  = c(0.5, 0.5),
         "Clear skies",  cex = 0.9, font = 2)

    # 4
    plot.new()
    text(x = 0.5, y = 0.23,
         adj  = c(0.5, 0.5),
         "Cloudy skies", cex = 0.9, font = 2)

    # 5
    plot.new()

    par(mfrow = c(1,1))
}
#+ echo=F, include=F


