# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Correlate Horizontal and Inclined CM21 signal **INC ~ HOR**."
#' author:
#'   - Natsis Athanasios^[Laboratory of Atmospheric Physics, AUTH, natsisphysicist@gmail.com]
#'   - Alkiviadis Bais^[Laboratory of Atmospheric Physics, AUTH]
#' abstract:      "Compare Inclined CM21 with Global CM21 to produce a calibration factor for inclined, and new Global measurements to fill calibration period gaps in records."
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#'
#' link-citations:  yes
#' colorlinks:      yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     fig_width:        8
#'     fig_height:       5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#'
#' date: "`r format(Sys.time(), '%F')`"
#' params:
#'    ALL_YEARS: TRUE
#' ---


stop("This should now run now!")
##
## BEWARE !!!! -------------------------------------------
##
## This was made with the assumption that exist a data gap
## on horizontal CHP1. Now you should create a virtual gap
## to work as expected!!!
##



#'
#' # Description
#'
#' **Source code: [`github.com/thanasisn/CM_21_GLB/tree/main/CM21_inclined_global`](https://github.com/thanasisn/CM_21_GLB/tree/main/CM21_inclined_global)**
#'
#' Got measurements from inclined CM-21:
#' : $V_{IN}$
#'
#' Use linear regression to get radiation:
#' : $E_{IN} = β + α * V_{IN}$
#'
#' or
#'
#' Use median of the ratio:
#' : $E_{IN} = a * V_{IN}$
#'
#' Assume:
#' : $E_{IN} = E_{GL}$
#'
#' Produce Global "measurements":
#' : $V_{GL} = E_{IN} / S_{new}$
#'
#' Select an arbitrary $S_{new}$ value for archive usage.
#'
#+ echo=F, include=T


##__  Document Options  --------------------------------------------------------

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "pdf"   )
# knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(fig.pos    = '!h'     )




#+ include=F, echo=F
##__  Run Environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("CM21_incline_global_comparison.R") })
if (!interactive()) {
    pdf( file = paste0("~/CM_21_GLB/RUNTIME/", basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("~/CM_21_GLB/RUNTIME/", basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
    filelock::lock(paste0("~/CM_21_GLB/LOGs/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}


#+ echo=F, include=F
##  External code  -------------------------------------------------------------
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(pander,     quietly = TRUE, warn.conflicts = FALSE)
library(fANCOVA,    quietly = TRUE, warn.conflicts = FALSE)
library(MASS,       quietly = TRUE, warn.conflicts = FALSE)
source("~/CM_21_GLB/Functions_CM21_factor.R")
source("~/CM_21_GLB/Functions_dark_calculation.R")

def.par <- par(no.readonly = TRUE)

##  Use the same dark definitions as horizontal CM-21 --------------------------

## from "~/CM_21_GLB/DEFINITIONS.R"
SUN_FOLDER <- "~/DATA_RAW/SUN/PySolar_LAP/"
DARK_ELEV  <- -10         ## Sun elevation limit to get dark signal (R20, R30)
DSTRETCH   <-  20 * 3600  ## Extend of dark signal for morning and evening of the same day (R30)
DCOUNTLIM  <-  10         ## Number of valid measurements to compute dark (R30)


## Standard deviation filter (apply after other filters)
STD_ret_ap_for = 10   ## apply rule when there are enough data points
STD_relMAX     =  1   ## Standard deviation can not be > STD_relMAX * MAX(daily value)



##  Variables  -----------------------------------------------------------------
tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y"))

## data extend
START_DAY <- "2022-02-21"
END_DAY   <- "2022-06-27"

## exact extend
START_DAY_exact <- as.POSIXct("2022-02-21 11:50")
END_DAY_exact   <- as.POSIXct("2022-06-27 08:40")

## missing data
START_DAY_miss <- as.POSIXct("2022-02-28 06:00")
END_DAY_miss   <- as.POSIXct("2022-06-03 12:00")

## Sun elevation limit
elevation_lim  <- 10

## color values
col_hor <- "green"
col_inc <- "magenta"


##  Load HOR_CM21  -------------------------------------------------------------
HORIZ <- readRDS("~/DATA/Broad_Band/CM21_H_global/LAP_CM21_H_L1_2022.Rds")


##  Load INC_CM  ---------------------------------------------------------------
incfiles <- list.files(path        = "~/DATA_RAW/Bband/CM21_LAP.INC",
                       pattern     = "[0-9]{6}01.LAP",
                       full.names  = TRUE,
                       ignore.case = TRUE,
                       recursive   = TRUE,
                       no..        = TRUE)

stopifnot(length(incfiles) > 0)

dayswecare <- seq.Date(as.Date(START_DAY),
                       as.Date(END_DAY), by = "day")

INCLI         <- data.table()
missing_files <- c()
for (aday in dayswecare) {
    aday  <- as.Date(aday, origin = "1970-01-01")
    found <- grep(paste0(format(aday, "%d%m%y01")), incfiles, ignore.case = T )

    ## check file names
    if (length(found) > 1) {
        stop("Found more file than we should") }
    if (length(found) == 0) {
        missing_files <- c(missing_files, paste0( format(aday, "%d%m%y01")))
        cat("Missing inclined for", format(aday), paste0( format(aday, "%d%m%y01")),"\n")
        next()
    }

    ##  Recreate time stamp for all minutes of day
    D_minutes <- seq(from       = as.POSIXct(paste(aday, "00:00:30 UTC")),
                     length.out = 1440,
                     by         = "min" )

    ## . . Read LAP file  ------------------------------------------------------
    lap <- fread( incfiles[found], na.strings = "-9" )
    lap[ V1 < -8, V1 := NA ]
    lap[ V2 < -8, V2 := NA ]
    stopifnot( dim(lap)[1] == 1440 )

    ## . . Read SUN file  ------------------------------------------------------
    sunfl <- paste0(SUN_FOLDER, "sun_path_", format(aday, "%F"), ".dat.gz")
    if (!file.exists(sunfl)) stop(cat(paste("Missing:", sunfl, "\nRUN! Sun_vector_construction_cron.py\n")))
    sun_temp <- read.table( sunfl,
                            sep         = ";",
                            header      = TRUE,
                            na.strings  = "None",
                            strip.white = TRUE,
                            as.is       = TRUE)

    ##  Daily table to keep
    day_data <- data.table( Date        = D_minutes, # Date of the data point
                            INC_value   = lap$V1,    # Raw value for CM21
                            INC_sd      = lap$V2,    # Raw SD value for CM21
                            # Azimuth     = sun_temp$AZIM,  # Azimuth sun angle
                            Elevat      = sun_temp$ELEV,
                            filein      = incfiles[found])

    ##  Gather data
    INCLI <- rbind(INCLI, day_data)
}


##  Unify data sets
DATA <- merge(HORIZ, INCLI, all = TRUE)
rm(HORIZ, INCLI)

##  Get common data for correlation analysis
DT <- DATA[ !is.na(INC_value) & !is.na(wattGLB), ]


#'
#' # Data Overview
#'
#' Investigate data of horizontal and inclined CM-21.
#'
#' Inclined data are read from "sirena" directly.
#'
#' Global data are produced by me.
#'
#' Data view is extended to:
#'
#' Start day: `r START_DAY`
#'
#' End day:   `r END_DAY`
#'
#' Correlation period:
#'
#' Start day exact: **`r START_DAY_exact`** m:`r hour(START_DAY_exact) * 60 + minute(START_DAY_exact) + 1`
#'
#' End day exact: **`r END_DAY_exact`** m:`r hour(END_DAY_exact) * 60 + minute(END_DAY_exact) + 1`
#'
#' Correlate data with sun elevation **$>`r elevation_lim`^\circ$**
#'
#+ include=T, echo=F

# hour(as.POSIXct("1970-01-01 00:00:30")) * 60 + minute(as.POSIXct("1970-01-01 00:00:30")) + 1
# hour(as.POSIXct("1970-01-01 23:59:30")) * 60 + minute(as.POSIXct("1970-01-01 23:59:30")) + 1


#+ include=F, echo=F
plot(DATA$Date,
     DATA$wattGLB,
     xlim = c(as.POSIXct(START_DAY), as.POSIXct(END_DAY)),
     col  = col_hor,
     pch  = ".",
     xlab = "",
     main = "Global radiation")

plot(DATA$Date,
     DATA$INC_value,
     xlim = c(as.POSIXct(START_DAY), as.POSIXct(END_DAY)),
     col  = col_inc,
     pch  = ".",
     xlab = "",
     main = "Inclined CM-21 signal")



#'
#' ## Common measurements
#'
#' Only simultaneous measurements for correlation.
#'
#+ include=T, echo=F

#+ include=F, echo=F
plot(DT$wattGLB, DT$INC_value,
     pch  = ".",
     main = "Common measurements")

plot(DT$Date,
     DT$wattGLB,
     col  = col_hor,
     pch  = ".",
     xlab = "",
     main = "Global radiation")

plot(DT$Date,
     DT$INC_value,
     col  = col_inc,
     pch  = ".",
     xlab = "",
     main = "Inclined CM-21 signal")


# ##  Daily plot of raw data  ----------------------------------------------------
# #'
# #' ## Daily plot of raw data
# #'
# #+ include=T, echo=F
#
# #+ include=T, echo=F
# for (ad in unique(as.Date(DT$Date))) {
#     pp <- DT[ as.Date(Date) == ad ]
#     ad <- as.Date(ad, origin = "1970-01-01")
#
#     par(mar = c(2,2,2,1))
#
#     layout(rbind(1,2), heights = c(7,1))  # legend on bottom 1/8th of the chart
#
#     plot.new()
#
#     title(main = paste0(ad, " d:", yday(ad), " "), cex.main = .8)
#     par(new = TRUE)
#     plot(pp$Date,
#          pp$wattGLB,
#          col  = col_hor,
#          xlab = "",  ylab = "",
#          yaxt = "n",
#          xaxs = "i",
#          pch  = 19,
#          cex  = 0.3)
#
#     par(new = TRUE)
#     plot(pp$Date,
#          pp$INC_value,
#          col  = col_inc,
#          xlab = "",  ylab = "",
#          yaxt = "n",
#          xaxs = "i",
#          pch  = 19,
#          cex  = 0.2)
#
#     vec <- pp$INC_value / pp$wattGLB
#     vec[!is.finite(vec)] <- NA
#
#     range <- diff(range(vec, na.rm = TRUE))
#     range <- range * 0.01
#     mean  <- mean(vec, na.rm = TRUE)
#     ylim  <- c(mean - range, mean + range)
#
#     vec[vec > ylim[2]] <- NA
#     vec[vec < ylim[1]] <- NA
#
#     par(new = TRUE)
#     plot(pp$Date,
#          vec,
#          col  = "blue",
#          xlab = "",  ylab = "",
#          xaxs = "i",
#          pch  = 19,
#          log  = "y",
#          cex  = 0.4)
#
#     par(new = TRUE)
#     plot(pp$Date,
#          vec,
#          ylim = ylim,
#          col  = "cyan",
#          xlab = "",
#          ylab = "",
#          yaxt = "n",
#          xaxs = "i",
#          pch  = 19,
#          cex  = 0.4)
#
#     par(new = TRUE)
#     plot(pp$INC_value,
#          pp$wattGLB,
#          col  = "red",
#          xlab = "",
#          ylab = "",
#          xaxs = "i",
#          pch  = 19,
#          cex  = 0.2)
#
#     par(mar = c(0, 0, 0, 0))
#     plot.new()
#     legend("center",
#            legend = c("Global Horizontal [W/m^2]",
#                       "Inclined Signal [V]",
#                       "log(Inclined / Horizontal)",
#                       "Inclined / Horizontal"),
#            col  = c(col_hor, col_inc, "blue", "cyan"),
#            pch  = 19,
#            ncol = 2,
#            bty  = "n")
# }



## Compute dark signal correction for inclined CM-21 ---------------------------

#'
#' # Process
#'
#' ## Compute dark signal correction for inclined CM-21.
#'
#' We calculate zero offset as the **median** value of signal with:
#'
#' - Data with Sun elevation angle **bellow $`r DARK_ELEV`^\circ$**.
#' - Data **span for dark signal `r DSTRETCH / 3600`** minutes for each "morning" and "evening".
#' - We **interpolate dark signal** between the "morning" and "evening" offset for each day for the dark correction.
#'
#+ include=T, echo=F


##  Init yearly calculations
globaldata    <- data.table()
NR_extreme_SD <- 0

daystodo      <- unique(as.Date(DATA[!is.na(INC_value), Date]))
pbcount       <- 0

for (ddd in daystodo) {

    theday      <- as.POSIXct( as.Date(ddd, origin = "1970-01-01"))
    test        <- format( theday, format = "%d%m%y06" )
    pbcount     <- pbcount + 1

    daymimutes  <- data.frame(
        Date = seq(as.POSIXct(paste(as.Date(theday), "00:00:30")),
                   as.POSIXct(paste(as.Date(theday), "23:59:30")), by = "min")
    )

    ## get all day from all data
    wholeday    <- DATA[as.Date(Date) == as.Date(theday), ]
    ## use only valid data for dark
    daydata     <- DATA[as.Date(Date) == as.Date(theday) & !is.na(INC_value), ]

    ## fill all minutes for nicer graphs
    daydata     <- merge(daydata, daymimutes, by = "Date", all = TRUE)
    daydata$day <- as.Date(daydata$Date)


    ####  Filter Standard deviation extremes  ##############################
    ## SD can not be greater than the signal
    pre_count     <- daydata[ !is.na(INC_value), .N ]
    ## apply rule if there are enough data
    if (pre_count > STD_ret_ap_for) {
        ## filter SD relative to the signal
        vec <- daydata[, INC_sd < STD_relMAX * max(INC_value, na.rm = TRUE)]
        if (sum(!vec, na.rm = T) > 0) {
            cat("Extreme SD values detected N:",sum(!vec, na.rm = TRUE), "\n")
            cat("Will be ignored from dark calculation\n")
        }
        daydata <- daydata[vec]

        NR_extreme_SD <- NR_extreme_SD + pre_count - daydata[ !is.na(INC_value), .N]
        if (nrow(daydata[!is.na(INC_value)]) < 1) {
            cat('\n')
            cat(paste(theday, "SKIP DAY: No data after extreme SD filtering!!"),"\n\n")
            next()
        }
    }
    ########################################################################

    ####    Calculate Dark signal   ########################################
    suppressWarnings({
        dark_day <- dark_calculations( dates      = daydata$Date,
                                       values     = daydata$INC_value,
                                       elevatio   = daydata$Elevat,
                                       nightlimit = DARK_ELEV,
                                       dstretch   = DSTRETCH)
    })

    if (!((!is.na(dark_day$Mmed) & dark_day$Mcnt >= DCOUNTLIM) |
          (!is.na(dark_day$Emed) & dark_day$Ecnt >= DCOUNTLIM)) ) {
        cat("Can not apply dark\n")
        todays_dark_correction <- NA
        dark_flag              <- "MISSING"
        missingdark            <- NA

        stop("DARK WARNING")
        ## get dark from pre-computed file
        # if (exists("construct")) {
        #     ## can not find date
        #     if (! theday %in% construct$Date) {
        #         todays_dark_correction <- NA
        #         dark_flag              <- "MISSING"
        #         missingdark            <- NA
        #     } else {
        #         ## get data from recomputed dark database
        #         todays_dark_correction <- construct[ Date == theday, DARK]
        #         dark_flag              <- "CONSTRUCTED"
        #     }
        # }

    } else {
        ####    Dark Correction function   #################################
        dark_generator <- dark_function(dark_day    = dark_day,
                                        DCOUNTLIM   = DCOUNTLIM,
                                        type        = "median",
                                        adate       = theday ,
                                        test        = test,
                                        missfiles   = missfiles,
                                        missingdark = missingdark )

        ####    Create dark signal for correction    #######################
        todays_dark_correction <- dark_generator(daydata$Date)
        dark_flag              <- "COMPUTED"
    }

    ####    Apply dark correction    #######################################
    daydata[, INC_valueWdark := INC_value - todays_dark_correction ]

    ####    Get processed and unprocessed data    ##########################
    daydata    <- merge(daydata, wholeday,
                        by = intersect(names(daydata),names(wholeday)),
                        all = TRUE)

    globaldata <- rbind( globaldata, daydata, fill = TRUE )
}



#+ include=F, echo=F
par(def.par)
plot(globaldata$Date,
     globaldata[, INC_valueWdark - INC_value],
     pch = 19,
     cex = 0.3,
     ylab = "Zero offset [V]",
     xlab = "",
     main = "Dark Signal Correction for Inclined CM-21")



## Keep only dark corrected data, data for output  -----------------------------
globaldata[, INC_value      := INC_valueWdark]
globaldata[, INC_valueWdark := NULL]

##  Use only common data for correlation analysis ------------------------------
DT <- globaldata[ !is.na(INC_value) & !is.na(wattGLB), ]

##  Use only exact date range --------------------------------------------------
DT <- DT[Date > START_DAY_exact]
DT <- DT[Date < END_DAY_exact  ]

##  Limit sun elevation  -------------------------------------------------------
DT <- DT[ Elevat >= elevation_lim]



## Correlation Inclined ~ Horizontal CM-21. ------------------------------------
#'
#' \newpage
#'
#' ## Correlation Inclined ~ Horizontal CM-21.
#'
#+ include=T, echo=F

## linear fit
fit   <- lm(DT$INC_value ~ DT$wattGLB  )
Pfit  <- lm(DT$wattGLB   ~ DT$INC_value)

res_threshold <- 2

vec <- abs(fit$residuals) > sd(fit$residuals) * res_threshold
DT$Offending <- vec

## robust linear fit
 rfit <- rlm(DT$INC_value ~ DT$wattGLB  )
Prfit <- rlm(DT$wattGLB   ~ DT$INC_value)



#'
#' ### Linear regression
#'
#' Outliers selection by **$`r res_threshold`σ$ distance**.
#'
#+ include=T, echo=F

par(def.par)
layout(1)
plot.new()

plot(DT$wattGLB, DT$INC_value,
     pch  = 1,
     cex  = 0.5,
     xlab = "Global [W]",
     ylab = "Inclined [V]",
     main = "Common measurements")

points(DT$wattGLB[vec], DT$INC_value[vec],
       pch = "+", col = "magenta")

abline(fit, col = "red", lwd = 2)
abline(rfit, col = "blue", lwd = 2)

legend("bottom",
       bty = "n", lwd = 2, cex = 0.8,
       lty = 1,
       col = c("red", "blue"),
       legend = c(paste("lm: y= ",
                        signif(abs(fit[[1]][1]), 3),
                        if (fit[[1]][2] > 0) "+" else "-",
                        signif(abs(fit[[1]][2]), 4),
                        " * x"),
                  paste("robust lm: y= ",
                        signif(abs(rfit[[1]][1]), 3),
                        if (rfit[[1]][2] > 0) "+" else "-",
                        signif(abs(rfit[[1]][2]), 4),
                        " * x")
       )
)

pander(summary(fit),
       caption = "Linear regression **RED**")

## all points
OffendingPoints <- DT[vec, ]

pander(
    data.frame(table(DT$day[vec])),
    caption = "Outliers points"
)

pander(summary(fit),
       caption = "Robust Linear regression **BLUE**")



##  Distribution of ratios  ----------------------------------------------------
#'
#' \newpage
#'
#' ## Distribution of ratios
#'
#+ include=T, echo=F

ratiolim <- 0.02
vec <- DT$INC_value/DT$wattGLB
vec <- vec[!is.infinite(vec)]
# vec <- vec[abs(vec) < ratiolim]

hist(vec,
     breaks = 2000,
     xlim = c(-0.01,0.03),
     main = "All values INC_value / wattGLB")

abline(v =   mean(vec, na.rem = TRUE), col = "blue" )
abline(v = median(vec, na.rem = TRUE), col = "green")

legend("topright", lty = 1,
       legend = c(paste("Mean:",   signif(  mean(vec, na.rem = TRUE), 6)),
                  paste("Median:", signif(median(vec, na.rem = TRUE), 6))),
       col    = c("blue", "green")
)


##  Removed offending for histogram
vec <- DT$INC_value[!DT$Offending] / DT$wattGLB[!DT$Offending]
vec <- vec[!is.infinite(vec)]
# vec <- vec[abs(vec) < ratiolim]

hist(vec,
     breaks = 2000,
     xlim = c(-0.01,0.03),
     main = "Without outliers INC_value / wattGLB")

abline(v = mean(vec,   na.rem = T), col = "blue" )
abline(v = median(vec, na.rem = T), col = "green" )

legend("topright", lty = 1,
       legend = c(paste("Mean:",   signif(  mean(vec, na.rem = TRUE), 6)),
                  paste("Median:", signif(median(vec, na.rem = TRUE), 6))),
       col    = c("blue", "green")
)



## Daily plot with dark correction ---------------------------------------------
#'
#' \newpage
#'
#' ## Floating scale daily plot after dark correction
#'
#' Ratios are filtered for plotting
#'
#+ include=T, echo=F

for (ad in unique(as.Date(DT$Date))) {
    pp <- DT[ as.Date(Date) == ad ]
    ad <- as.Date(ad, origin = "1970-01-01")

    par(mar = c(2,2,2,1))

    layout(rbind(1,2), heights = c(7,1))  # put legend on bottom 1/8th of the chart
    plot.new()

    title(main = paste0(ad, " d:", yday(ad), " " ), cex.main = .8)
    par(new = TRUE)
    plot(pp$Date,
         pp$wattGLB,
         col  = col_hor,
         xlab = "",  ylab = "",
         # yaxt = "n",
         xaxs = "i",
         pch  = 19,
         cex  = 0.3)

    points(pp$Date[pp$Offending],
           pp$wattGLB[pp$Offending],
           col  = "red",
           pch  = 1,
           cex  = 1)

    par(new = TRUE)
    plot(pp$Date,
         pp$INC_value,
         col  = col_inc,
         xlab = "",
         ylab = "",
         yaxt = "n",
         xaxs = "i",
         pch  = 19,
         cex  = 0.2)

    points(pp$Date[pp$Offending],
           pp$INC_value[pp$Offending],
           col  = "red",
           yaxt = "n",
           xaxt = "n",
           pch  = 1,
           cex  = 1)

    ## ratio plot scale
    vec <- pp$INC_value / pp$wattGLB
    vec[!is.finite(vec)] <- NA

    range <- diff(range(vec, na.rm = TRUE))
    range <- range * 0.01
    mean  <- mean(vec, na.rm = TRUE)
    ylim  <- c(mean - range, mean + range)

    vec[vec > ylim[2]] <- NA
    vec[vec < ylim[1]] <- NA

    # par(new = T)
    # plot(pp$Date,
    #      vec ,
    #      col  = "blue",
    #      xlab = "",  ylab = "",
    #      xaxs = "i",
    #      pch  = 19,
    #      log  = "y",
    #      cex  = 0.4)

    par(new = TRUE)
    plot(pp$Date,
         vec ,
         ylim = ylim,
         col  = "cyan",
         xlab = "",
         ylab = "",
         yaxt = "n",
         xaxt = "n",
         xaxs = "i",
         pch  = 19,
         cex  = 0.4)

    ## LOESS curve
    LOESS_CRITERIO <-  c("aicc", "gcv")[2]
    vec2 <- !is.na(vec)
    FTSE.lo3 <- loess.as(pp$Date[vec2], vec[vec2],
                         degree    = 1,
                         criterion = LOESS_CRITERIO,
                         user.span = NULL,
                         plot      = FALSE)
    FTSE.lo.predict3 <- predict(FTSE.lo3, pp$Date)
    lines(pp$Date, FTSE.lo.predict3, col = "yellow", lwd = 2.5)

    par(new = TRUE)
    plot(pp$INC_value,
         pp$wattGLB ,
         col  = "darkblue",
         xlab = "",
         ylab = "",
         yaxt = "n",
         xaxt = "n",
         xaxs = "i",
         pch  = 19,
         cex  = 0.2)

    par(mar = c(0, 0, 0, 0))
    plot.new()
    legend("center",
           legend = c("Global Horizontal [W/m^2]",
                      "Inclined Signal [V]",
                      "Correlation Inclined ~ Horizontal",
                      "Inclined / Horizontal",
                      "Outliers points",
                      "LOESS"),
           col  = c(col_hor, col_inc,'darkblue', "cyan", "red", "yellow"),
           pch  = c(     19,       19,       19,     19,     1,       NA),
           lty  = c(     NA,       NA,       NA,     NA,    NA,        1),
           ncol = 2,
           bty = "n")

    layout(rbind(1,2), heights=c(7,1))
    par(def.par)
}

# #'
# #' ## Table of offending points
# #'
# #+ include=T, echo=F
#
# OffendingPoints[, Azimuth   := NULL ]
# OffendingPoints[, preNoon   := NULL ]
# OffendingPoints[, Offending := NULL ]
# OffendingPoints[, day       := NULL ]
#
# setorder(OffendingPoints, Date)
#
# #+ echo=F, include=T
# #' \scriptsize
# #+ echo=F, include=T
# pander(OffendingPoints,
#        cap = "Offending Points")
# #'
# #' \normalsize
# #+ echo=F, include=T



## Calibrated daily data -------------------------------------------------------
#'
#' \newpage
#'
#' # Results
#'
#' ## Generate Global data from Inclined.
#'
#' Using the linear fit model
#'
#' With removed outliers data points
#'
#' **Radiometric values are on the same scale now**
#'
#+ include=T, echo=F

## remove offending
DT <- DT[ Offending == FALSE, ]

## Recalculate fits for use !! -------------------------------------------------

## linear regression
fit   <- lm(DT$INC_value ~ DT$wattGLB  )
Pfit  <- lm(DT$wattGLB   ~ DT$INC_value)

## robust linear fit
 rfit <- rlm(DT$INC_value ~ DT$wattGLB  )
Prfit <- rlm(DT$wattGLB   ~ DT$INC_value)


par(def.par)
layout(1)
plot.new()

plot(DT$wattGLB, DT$INC_value,
     pch  = 1,
     cex  = 0.3,
     xlab = "Global [W]",
     ylab = "Inclined [V]",
     main = "Common measurements without outliers")

abline( fit, col = "red",  lwd = 2)
abline(rfit, col = "blue", lwd = 2)

legend("bottom",
       bty = "n", lwd = 2, cex = 0.8,
       lty = 1,
       col = c("red", "blue"),
       legend = c(paste("lm: y= ",
                        signif(abs(fit[[1]][1]), 3),
                        if (fit[[1]][2] > 0) "+" else "-",
                        signif(abs(fit[[1]][2]), 4),
                        " * x"),
                  paste("robust lm: y= ",
                        signif(abs(rfit[[1]][1]), 3),
                        if (rfit[[1]][2] > 0) "+" else "-",
                        signif(abs(rfit[[1]][2]), 4),
                        " * x")
       )
)

pander(summary(fit),
       caption = "Linear regression **RED**")

pander(summary(fit),
       caption = "Robust Linear regression **BLUE**")





##  Calibration factor  --------------------------------------------------------
#'
#' \newpage
#'
#' ## Calibration factor
#'
#+ include=T, echo=F


##  Removed offending for histogram
vec <- DT$INC_value[!DT$Offending] / DT$wattGLB[!DT$Offending]
vec <- vec[!is.infinite(vec)]

#vec <- vec[abs(vec) < ratiolim]

hist(vec,
     breaks = 2000,
     xlim = c(-0.01,0.03),
     main = "Without outliers INC_value / wattGLB")

abline(v = mean(vec,   na.rem = T), col = "blue" )
abline(v = median(vec, na.rem = T), col = "green")

legend("topright", lty = 1,
       legend = c(paste("Mean:",   signif(  mean(vec, na.rem = TRUE), 6)),
                  paste("Median:", signif(median(vec, na.rem = TRUE), 6))),
       col    = c("blue", "green")
)







##  Predict new values  --------------------------------------------------------

## Select model

# ## robust linear model
# LMS <- Prfit

## simple linear model
LMS <- Pfit

vec <- DT$INC_value[!DT$Offending] / DT$wattGLB[!DT$Offending]
vec <- vec[(!is.infinite(vec))]


pp <- data.frame(Median    = median(vec, na.rm = TRUE),
                 Mean      = mean(vec, na.rm = TRUE),
                 Robust_lm = rfit[[1]][2],
                 Simple_lm = fit[[1]][2])
pp <- data.frame(Parameter = names(pp),
                 CF        = unlist(pp[1,]))
CF <- pp

row.names(pp) <- NULL
setorder(pp , CF)

panderOptions("table.alignment.default", "left")
pander(as.data.frame(pp),
       caption = "Calibration factors",
       digits  = 7,
       table.alignment.default = "left")





##  Plot common calibrated  --------------------------------------------------------
#'
#' \newpage
#'
#' ## Plot common calibrated
#'
#'  **Using median as calibration factor**:
#'
#' **`r format( CF$CF[CF$Parameter == "Median"], digits = 10)`**
#'
#+ include=T, echo=F




## Create new data  ------------------------------------------------------------
# DT$INC_watt            <- LMS[[1]][1] + LMS[[1]][2] * DT$INC_value
# DT$INC_watt_sd         <- LMS[[1]][1] + LMS[[1]][2] * DT$INC_sd
# globaldata$INC_watt    <- LMS[[1]][1] + LMS[[1]][2] * globaldata$INC_value
# globaldata$INC_watt_sd <- LMS[[1]][1] + LMS[[1]][2] * globaldata$INC_sd

DT$INC_watt            <- 1 / CF$CF[CF$Parameter == "Median"] * DT$INC_value
DT$INC_watt_sd         <- 1 / CF$CF[CF$Parameter == "Median"] * DT$INC_sd
globaldata$INC_watt    <- 1 / CF$CF[CF$Parameter == "Median"] * globaldata$INC_value
globaldata$INC_watt_sd <- 1 / CF$CF[CF$Parameter == "Median"] * globaldata$INC_sd



for (ad in unique(as.Date(DT$Date))) {
    pp <- DT[ as.Date(Date) == ad ]
    ad <- as.Date(ad, origin = "1970-01-01")

    par(mar = c(2,2,2,1))

    layout(rbind(1,2), heights = c(7,1))  # legend on bottom 1/8th of the chart
    plot.new()

    title(main = paste0(ad, " d:", yday(ad), " "), cex.main = .8)
    par(new = TRUE)
    plot(pp$Date,
         pp$wattGLB,
         col  = col_hor,
         xlab = "",  ylab = "",
         # yaxt = "n",
         xaxs = "i",
         pch  = 19,
         cex  = 0.3)

    points(pp$Date[pp$Offending],
           pp$wattGLB[pp$Offending],
           col  = "red",
           pch  = 1,
           cex  = 1)

    points(pp$Date[pp$Offending],
           pp$INC_watt[pp$Offending],
           col  = "red",
           yaxt = "n",
           xaxt = "n",
           pch  = 1,
           cex  = 1)

    points(pp$Date,
           pp$INC_watt,
           col  = col_inc,
           pch  = 19,
           cex  = 0.2)

    vec <- pp$INC_watt / pp$wattGLB
    vec[!is.finite(vec)] <- NA
    ylim <- range(vec, na.rm = TRUE)

    # range <- diff(range(vec, na.rm = T))
    # range <- range * 0.02
    # mean  <- mean(vec, na.rm = T)
    # ylim  <- c(mean - range, mean + range)
    #
    # vec[vec > ylim[2]] <- NA
    # vec[vec < ylim[1]] <- NA

    par(new = TRUE)
    plot(pp$Date,
         vec ,
         ylim = ylim,
         col  = "cyan",
         xlab = "",
         ylab = "",
         yaxt = "n",
         xaxt = "n",
         xaxs = "i",
         pch  = 19,
         cex  = 0.4)

    ## LOESS curve
    suppressWarnings({
        LOESS_CRITERIO <-  c("aicc", "gcv")[2]
        vec2 <- !is.na(vec)
        FTSE.lo3 <- loess.as(pp$Date[vec2], vec[vec2],
                             degree    = 1,
                             criterion = LOESS_CRITERIO,
                             user.span = NULL,
                             plot      = FALSE)
        FTSE.lo.predict3 <- predict(FTSE.lo3, pp$Date)
        lines(pp$Date, FTSE.lo.predict3, col = "yellow", lwd = 2.5)
    })

    # par(new = T)
    # plot(pp$INC_watt,
    #      pp$wattGLB ,
    #      col  = "darkblue",
    #      xlab = "",
    #      ylab = "",
    #      yaxt = "n",
    #      xaxt = "n",
    #      xaxs = "i",
    #      pch  = 19,
    #      cex  = 0.2)

    par(mar = c(0, 0, 0, 0))
    plot.new()
    legend("center",
           legend = c("Global Horizontal [W/m^2]",
                      "Inclined Signal [W/m^2]",
                      "Correlation Inclined ~ Horizontal",
                      "Inclined / Horizontal",
                      "Outliers points",
                      "LOESS"),
           col  = c(col_hor, col_inc,'darkblue', "cyan", "red", "yellow"),
           pch  = c(     19,       19,       19,     19,     1,       NA),
           lty  = c(     NA,       NA,       NA,     NA,    NA,        1),
           ncol = 2,
           bty  = "n")

    layout(rbind(1,2), heights = c(7,1))
    par(def.par)
}



# ## Gap data --------------------------------------------------------------------
# #'
# #' \newpage
# #'
# #' ## CM-21 calibration data gap inspection
# #'
# #+ include=T, echo=F

gapdata <- globaldata[ is.na(wattGLB) & !is.na(INC_value) ]

## focus on missing data
gapdata <- gapdata[ Date > START_DAY_miss]
gapdata <- gapdata[ Date <   END_DAY_miss]

## create all minutes
gapdata <- merge(gapdata,
                 data.frame(Date = seq(from = min(gapdata$Date),
                                       to   = max(gapdata$Date),
                                       by   = "min")),
                 all = TRUE)
gapdata[, day := as.Date(Date)]


# ## Plot new Global  ------------------------------------------------------------
# for (ad in unique(gapdata$day)) {
#     tmp <- gapdata[ day == ad, ]
#     ad  <- as.Date(ad, origin = "1970-01-01")
#
#     par(mar = c(2,2,2,1))
#
#     if (all(is.na(tmp$INC_watt))) next()
#
#     plot(tmp$Date,
#          tmp$INC_watt, "l")
#
#     title(main = paste0(ad, " d:", yday(ad), " " ), cex.main = .8)
# }


## Sirena data manipulation!! --------------------------------------------------

#'
#' \newpage
#'
#' # Sirena archive data manipulation!!
#'

#'
#+ include=T, echo=F

# 11.96E-6,   4E-2,


tomove <- gapdata[, .(day = min(day)), by = filein]
tomove <- tomove[!is.na(filein)]
tomove[, basename := basename(filein)]
setorder(tomove,day)


tomove[, newbase := sub("01\\.LAP", "06\\.LAP", basename) ]
# movedir <- "~/CM_21_GLB/CM21_inclined_global/export/01/2022/"
# dir.create(movedir, showWarnings = FALSE, recursive = TRUE)
# movedir <- "~/CM_21_GLB/CM21_inclined_global/export/06/2022/"
# dir.create(movedir, showWarnings = FALSE, recursive = TRUE)
#
# cc <- file.copy(from      = tomove$filein,
#                 to        = paste0(movedir, tomove$newbase),
#                 overwrite = FALSE,
#                 copy.mode = TRUE,
#                 copy.date = TRUE)
#
# cc <- file.copy(tomove[day == min(day), filein],
#                 "~/CM_21_GLB/CM21_inclined_global/export/01/2022/",
#                 overwrite = FALSE,
#                 copy.mode = TRUE,
#                 copy.date = TRUE)
# cc <- file.copy(tomove[day == max(day), filein],
#                 "~/CM_21_GLB/CM21_inclined_global/export/01/2022/",
#                 overwrite = FALSE,
#                 copy.mode = TRUE,
#                 copy.date = TRUE)

pp <- tomove[, .(day, basename, newbase)]
setorder(pp, day)
names(pp)[names(pp) == "basename"] <- "Original inclined CM-21"
names(pp)[names(pp) == "newbase" ] <- "Copied from inclined CM-21 to horizontal"


#+ echo=F, include=T
#' \footnotesize
#+ echo=F, include=T
pander(pp,
       cap = "Affected sirena files.")
#'
#' \normalsize
#'
#' For the first file of the period:
#'
#' `r format(pp[day == min(day)])`
#'
#' - Lines after  `r hour(START_DAY_exact) * 60 + minute(START_DAY_exact) + 1`      will be included to the horizontal.
#' - Lines before `r hour(START_DAY_exact) * 60 + minute(START_DAY_exact) + 1 - 10` will be included to the inclined.
#' - The start of the file has the regular CHP-1 global/inclined measurements.
#'
#' For the last file of the period:
#'
#' `r format(pp[day == max(day)])`
#'
#' - Lines before `r hour(END_DAY_exact) * 60 + minute(END_DAY_exact) + 1`     will be included to the horizontal.
#' - Lines after  `r hour(END_DAY_exact) * 60 + minute(END_DAY_exact) + 1 +10` will be included to the inclined.
#' - The end of the file has the regular CHP-1 global/inclined measurements.
#'
#' The intermediate lines will be set to "NA" ("-9") for both instruments.
#'
#' A backup of the whole year for each instrument have been created prior to edits.
#'
#'
#' ## Produce sensitivity for the period
#'
#' $$W = V \times  \frac {1} {`r format(CF$CF[CF$Parameter == "Median"], digits = 10)`}$$
#'
#' $$\frac {1} {`r format(CF$CF[CF$Parameter == "Median"], digits = 10)`} = \frac {gain} {Sensitivity}$$
#'
#' $${gain} = 4E-2$$
#'
#' $${Sensitivity} = `r format(CF$CF[CF$Parameter == "Median"] * 4E-2 , scientific = T, digits = 4, nsmall = 2)`$$
#'
#+ echo=F, include=T


# ## Export new data -------------------------------------------------------------
# outdir <- "~/CM_21_GLB/CM21_inclined_global/export"
# dir.create(outdir, showWarnings = FALSE)
#
# de_Sensitivity <- 600
#
# for (ad in unique(as.Date(gapdata$Date))) {
#     tmp   <- gapdata[ day == ad, ]
#     dateD <- as.Date(ad, origin = "1970-01-01")
#     yyyy  <- year(dateD)
#     doy   <- yday(dateD)
#
#     ## create all minutes of day
#     D_minutes <- seq(as.POSIXct(paste(dateD, "00:00:30"), "%F %T"),
#                      as.POSIXct(paste(dateD, "23:59:30")), by = "mins")
#     tmp <- merge(tmp, data.table(Date = D_minutes), all = TRUE)
#
#     ## daily output file name
#     filename <- paste0(outdir, "/", strftime(dateD, format = "%d%m%y03.test"))
#
#     ## sanity check
#     stopifnot(nrow(tmp) == 1440)
#
#     ## apply conversion to Volt
#     expdt <- data.frame(V1 = tmp$INC_watt    / de_Sensitivity,
#                         V2 = tmp$INC_watt_sd / de_Sensitivity)
#
#     ## round
#     expdt$V1 <- signif( expdt$V1, digits =  7 )
#     expdt$V2 <- signif( expdt$V2, digits = 15 )
#
#     ## write formatted data to file
#     write.table(format(expdt,
#                        digits    = 15,
#                        # width     = 15,
#                        row.names = FALSE,
#                        scietific = c(FALSE, TRUE),
#                        nsmall    = 2 ),
#                 file      = filename,
#                 quote     = FALSE,
#                 col.names = FALSE,
#                 row.names = FALSE,
#                 eol = "\r\n")
#
#     ## set na value
#     expdt[is.na(expdt)] <- "-9"
#
#     ## scientific notation capital
#     system(paste0("sed -i 's|e|E|g' ", filename))
#
#     ## NA to number
#     system(paste0("sed -i 's|NA|-9|g' ", filename))
# }




#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
