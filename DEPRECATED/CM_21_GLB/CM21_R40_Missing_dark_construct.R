# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "CM21 dark trend reconstruction."
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Read previous dark statistics and recreate missing dark cases."
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
#'     fig_width:        6
#'     fig_height:       4
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#' date: "`r format(Sys.time(), '%F')`"
#' params:
#'    ALL_YEARS: TRUE
#' ---


#'
#' **Source code: [github.com/thanasisn/CM_21_GLB](https://github.com/thanasisn/CM_21_GLB)**
#'
#' **Data display: [thanasisn.netlify.app/3-data_display/2-cm21_global/](https://thanasisn.netlify.app/3-data_display/2-cm21_global/)**
#'
#+ echo=F, include=T

warning("Deprecated by ~/BBand_LAP/process/Legacy_CM21_R30_export.R")
stop("No need to run!")

####_  Document options _####

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""      )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"    )
knitr::opts_chunk$set(fig.align  = "center" )
# knitr::opts_chunk$set(fig.pos    = '!h'     )



#+ include=F, echo=F
####  Set environment  ####
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("CM21_R40_") })
if(!interactive()) {
    pdf(  file = paste0("~/CM_21_GLB/RUNTIME/", basename(sub("\\.R$",".pdf", Script.Name))))
    sink( file = paste0("~/CM_21_GLB/RUNTIME/", basename(sub("\\.R$",".out", Script.Name))), split=TRUE)
    filelock::lock(paste0("~/CM_21_GLB/LOGs/",  basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}


#+ echo=F, include=F
####  External code  ####
library(data.table, quietly = T, warn.conflicts = F)
library(pander,     quietly = T, warn.conflicts = F)
library(caTools,    quietly = T, warn.conflicts = F)
source("~/CM_21_GLB/Functions_write_data.R")

####  Variables  ####
source("~/CM_21_GLB/DEFINITIONS.R")
panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )

####  Execution control  ####
## Default
ALL_YEARS <- FALSE
TEST      <- FALSE
## When running
args <- commandArgs( trailingOnly = TRUE )
if ( length(args) > 0 ) {
    if (any(args == "NOTEST"  )) { TEST      <- FALSE }
    if (any(args == "NOTALL"  )) { ALL_YEARS <- FALSE }
    if ( any(args == "ALL")      ) { ALL_YEARS <- TRUE }
    if ( any(args == "ALLYEARS") ) { ALL_YEARS <- TRUE }
}
## When knitting
if (!exists("params")) {
    params <- list( ALL_YEARS = ALL_YEARS)
}
cat(paste("\n**ALL_YEARS:", ALL_YEARS, "**\n"))
cat(paste("\n**TEST     :", TEST,      "**\n"))

tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))




## get first pass statistics
darkDT  <- readRDS(DARKSTORE)
setorder(darkDT, Date)
statist <- darkDT[ dark_flag == "COMPUTED"]

#'
#' List of dark resolution scheme
#'
#+ echo=F, include=T

table(darkDT$dark_flag)



#'
#' ## Info
#'
#' We will use existing daily dark signal values, to infer the dark
#' signal when it can not be computed for a day.
#' This is a result of missing data before sunrise on/and after sunset.
#'
#' \newpage
#' \FloatBarrier
#'
#' ## Dark calculated with the `median` value.
#'
#' The median dark of a period before morning and after night is used as the
#' base of the dark signal correction.
#'
#+ echo=F, include=T

temp <- data.frame(statist$Mmed, statist$Emed)
Dmed <- rowMeans(temp, na.rm = TRUE)

darkmedian <- approxfun( statist$Date, Dmed )


par(mar = c(2,4,2,1))

# plot(statist$Date, Dmed, pch = 19, cex = 0.2,
#      main = "Median", xlab = "", ylab = "Daily dark values")
#

plot(statist$Date, darkmedian(statist$Date), pch = 19, cex = 0.2 ,col="red",
     main="Median reconstruction fill missing")
points(statist$Date, Dmed, pch = 19, cex = 0.2 )

legend("topleft",
       legend = c("Calculated Dark",
                  "Estimated Dark"),
       pch = 19,
       col = c("black","red"),
       bty = "n")




#'
#'
#' ## Dark calculated with the `mean` value.
#'
#' The mean dark of a period before morning and after night is used as the
#' base of the dark signal correction.
#'
#+ echo=F, include=T

temp <- data.frame(statist$Mavg, statist$Eavg)
Dmen <- rowMeans(temp, na.rm = TRUE)

## approximate function for dark
darkmean <- approxfun( statist$Date, Dmed,
                       method = "linear")


par(mar = c(2,4,2,1))

plot(statist$Date, darkmean(statist$Date), pch = 19, cex = 0.2 ,col="red",
     main = "Mean reconstruction fill missing")
points(statist$Date, Dmen, pch = 19, cex = 0.2 )

legend("topleft",
       legend = c("Calculated Dark",
                  "Estimated Dark"),
       pch = 19,
       col = c("black","red"),
       bty = "n")


#'
#' ## Mean Dark from first pass
#'
#+ echo=F, include=T

plot(statist$Date, statist$Dmean, pch = 19, cex = 0.2,
     main = "Average Dark Correction on first pass" )




##### remove zeros #####
statist$Dmean[ statist$Dmean == 0 ] <- NA


sel_1 <- statist$Date < BREAKDATE

scatter.smooth( x = statist$Date[sel_1],
                y = statist$Dmean[sel_1], pch = 19, cex = 0.2,
                xlab = "", xaxt='n', ylab = "Dark signal",
                main = "Mean Daily dark before break")
axis.POSIXct(1, statist$Date[sel_1])


sel_2 <- statist$Date >= BREAKDATE

scatter.smooth( x = statist$Date[sel_2],
                y = statist$Dmean[sel_2], pch = 19, cex = 0.2,
                xlab = "", xaxt = 'n', ylab = "Dark signal",
                main = "Mean Daily dark after break")
axis.POSIXct(1, statist$Date[sel_1])

#'
#' \newpage
#' \FloatBarrier
#'
#' ## Dark calculated as `running mean` value.
#'
#+ echo=F, include=T


# show runmean for different window sizes
x <- statist$Date[  sel_1 ]
y <- statist$Dmean[ sel_1 ]

if ( length(y) > 0 ) {
    col = c("black", "green", "blue", "magenta", "cyan")
    plot(x,y, col=col[1], pch = 19, cex = 0.2, main = "Moving Window Means before break")
    # lines(x, runmean(y, 3), col=col[2], lwd=2)
    lines(x, runmean(y, 8), col = col[2], lwd = 2)
    lines(x, runmean(y,15), col = col[3], lwd = 2)
    lines(x, runmean(y,24), col = col[4], lwd = 2)
    lines(x, runmean(y,50), col = col[5], lwd = 2)
    lab = c("data",  "k=8", "k=15", "k=24", "k=50")
    legend("topright", lab, col = col, lty = 1, cex = 0.8, ncol =3, bty = "n" )
}

x <- statist$Date[  sel_2 ]
y <- statist$Dmean[ sel_2 ]

if ( length(y) > 0 ) {
    col = c("black", "green", "blue", "magenta", "cyan")
    plot(x,y, col=col[1], pch = 19, cex = 0.2, main = "Moving Window Means after break")
    # lines(x, runmean(y, 3), col=col[2], lwd=2)
    lines(x, runmean(y, 8), col = col[2], lwd = 2)
    lines(x, runmean(y,15), col = col[3], lwd = 2)
    lines(x, runmean(y,24), col = col[4], lwd = 2)
    lines(x, runmean(y,50), col = col[5], lwd = 2)
    lab = c("data",  "k=8", "k=15", "k=24", "k=50")
    legend("bottomleft", lab, col = col, lty = 1, cex = 0.8, ncol =3, bty = "n"  )
}


## select running mean window to use
rmn  = 15

#'
#' \newpage
#' \FloatBarrier
#'
#' ## Construct missing Dark
#'
#' **We will use running mean with a window of `r rmn` days,**
#' **and interpolate to get a daily mean value to fill the gaps**
#'
#+ echo=F, include=T

## Running mean for two periods
rnmD        <- c(runmean( statist$Dmean[sel_1], rmn ), runmean( statist$Dmean[sel_2], rmn ) )
runningDark <- data.frame(Date = statist$Date, DARK = rnmD)
runningDark$DARK[is.nan(runningDark$DARK)] <- NA


## Use valid data to interpolate
runningDark_valid <- runningDark[ !is.nan(runningDark$DARK), ]


## Create interpolate function
runningDark_valid_func <- approxfun( runningDark_valid$Date, runningDark_valid$DARK)


plot(statist$Date, statist$Dmean, pch = 19, cex = 0.2, main = paste("Running mean of ", rmn, " days") )
lines(statist$Date, rnmD, "l", col = "red", lwd = 2 )


darkfill <- darkDT[ dark_flag != "COMPUTED"]


#'
#' ## There are `r nrow(darkfill)` days with un-computable dark to fill.
#'
#+ echo=F, include=T

darkfill$Date

#+ echo=F, include=F

darkfill[, DARK := runningDark_valid_func(darkfill$Date)]

#+ echo=F, include=T

write_RDS(object = darkfill,
          file   = DARKCONST )



## END ##
tac = Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
if (interactive()) beepr::beep(7)
