# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Trends of SDR in Thessaloniki "
#' author:
#'   - Natsis Athanasios^[Laboratory of Atmospheric Physics, AUTH, natsisphysicist@gmail.com]
#'   - Alkiviadis Bais^[Laboratory of Atmospheric Physics, AUTH]
#' abstract:
#'   "Study of GHI and DNI radiation for 'clear sky' and all sly conditions."
#'
#' documentclass:  article
#' classoption:    a4paper,oneside
#' fontsize:       10pt
#' geometry:       "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#' link-citations: yes
#' colorlinks:     yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections: no
#'     fig_caption:     no
#'     keep_tex:        yes
#'     latex_engine:    xelatex
#'     toc:             yes
#'     toc_depth:       4
#'     fig_width:       7
#'     fig_height:      4.5
#'   html_document:
#'     toc:             true
#'     keep_md:         yes
#'     fig_width:       7
#'     fig_height:      4.5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#+ echo=F, include=T


## __ Document options ---------------------------------------------------------

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(dev        = c("pdf", "png"))
# knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )  ## !! breaks calculations
knitr::opts_chunk$set(fig.pos    = '!h'     )

#+ include=F, echo=F
## __ Set environment ----------------------------------------------------------
Sys.setenv(TZ = "UTC")
Script.Name <- "./DHI_GHI_3_trends_consistency.R"

if (!interactive()) {
    pdf( file = paste0("./runtime/",  basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("./runtime/",  basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
    filelock::lock(paste0("./runtime/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}


#+ echo=F, include=T
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(pander,     quietly = TRUE, warn.conflicts = FALSE)
library(lubridate,  quietly = TRUE, warn.conflicts = FALSE)
library(ggplot2,    quietly = TRUE, warn.conflicts = FALSE)
library(fANCOVA,    quietly = TRUE, warn.conflicts = FALSE)


panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

## __ Load external functions --------------------------------------------------
## Functions from `https://github.com/thanasisn/IStillBreakStuff/tree/main/FUNCTIONS/R`
source("~/CODE/FUNCTIONS/R/sumNA.R")
source("~/CODE/FUNCTIONS/R/linear_fit_stats.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/CODE/FUNCTIONS/R/data.R")


## __ Source initial scripts ---------------------------------------------------
source("./DHI_GHI_0_variables.R")

## notification function
options(error = function() {
    if (interactive()) {
        system("mplayer /usr/share/sounds/freedesktop/stereo/dialog-warning.oga", ignore.stdout = T, ignore.stderr = T)
        system(paste("notify-send -u normal -t 30000 ", Script.Name, " 'An error occurred!'"))
    }
})


if (!file.exists(I1_longterm) |
    file.mtime(I1_longterm) < file.mtime("./DHI_GHI_0_variables.R") |
    file.mtime(I1_longterm) < file.mtime("./DHI_GHI_00_raw_data.R") |
    file.mtime(I1_longterm) < file.mtime("./DHI_GHI_01_Input_longterm.R"))
{
    # source("./DHI_GHI_01_Input_longterm.R")
    tryCatch(source("./DHI_GHI_01_Input_longterm.R"), exit = function(cond) {
        message( conditionMessage(cond) )
    })
    dummy <- gc()
}

if (!file.exists(I3_trendsconsist) |
    file.mtime(I3_trendsconsist) < file.mtime("./DHI_GHI_0_variables.R") |
    file.mtime(I3_trendsconsist) < file.mtime("./DHI_GHI_00_raw_data.R") |
    file.mtime(I3_trendsconsist) < file.mtime("./DHI_GHI_03_Input_consistency.R"))
{
    # source("./DHI_GHI_01_Input_longterm.R")
    tryCatch(source("./DHI_GHI_03_Input_consistency.R"), exit = function(cond) {
        message( conditionMessage(cond) )
    })
    dummy <- gc()
}

## load data
load(I1_longterm)
load(I3_trendsconsist)

tic <- Sys.time()


## __ Flags --------------------------------------------------------------------

DRAFT <- TRUE
DRAFT <- FALSE

## override plot options
par(pch = ".")

## choose to grid some plots
FIGURESGRID <- TRUE
# FIGURESGRID <- FALSE

## choose loess criterion for span
LOESS_CRITERIO <-  c("aicc", "gcv")[1]

## cex value for side by side
ccex_sbs <- 1.3

#+ echo=F, include=T
#'
#' ## 3. Consistency of trends
#'
#' Where is a **running mean the window is `r running_mean_window_days` days** or
#' `r running_mean_window_days / Days_of_year` years.
#'
#' ### Process
#'
#+ echo=F, include=F



## ____ Scatter plots with SZA all data -----------------------------------------
data_list <- c(  "ALL_1_D_monthly_DESEAS",
                 "CLEAR_1_D_monthly_DESEAS",
                 "CLOUD_1_D_monthly_DESEAS",
                 "ALL_1_daily_DESEAS",
                 "CLEAR_1_daily_DESEAS",
                 "CLEAR_1_daily_DESEAS")

by_var     <- c("Date", "SZA")
for (i in data_list) {
    ## get data and y vars to plot
    Dplot  <- get(i)
    # wecare <- grep("HOR|GLB|DIR", names(Dplot), value = T)
    wecare <- grep("GLB|DIR", names(Dplot), value = T)
    ## loop existing x vars
    for (xvar in names(Dplot)[names(Dplot) %in% by_var]) {
        for (yvar in wecare) {
            if (all(is.na(Dplot[[yvar]]))) next()

            if (grepl("wattGLB", yvar)) {
                col <- "green"
            } else {
                col <- get(paste0(c("col", unlist(strsplit(yvar, split = "_"))[1:2]),
                                  collapse = "_"))
            }

            vect <- Dplot[[yvar]]
            plot(Dplot[[xvar]], vect,
                 pch  = 19,
                 cex  = .3,
                 col  = col,
                 main = paste(i, yvar),
                 xlab = xvar, ylab = yvar)
        }
    }
}

## ____ Histograms Plots all data ----------------------------------------------
for (i in data_list) {
    ## get data and y vars to plot
    Dplot  <- get(i)
    # wecare <- grep("HOR|GLB|DIR", names(Dplot), value = TRUE)
    wecare <- grep("GLB|DIR", names(Dplot), value = TRUE)
    for (yvar in wecare) {
        if (!yvar %in% names(Dplot)) next()
        if (all(is.na(Dplot[[yvar]]))) next()

        if (grepl("wattGLB", yvar)) {
            col <- "green"
        } else {
            col <- get(paste0(c("col", unlist(strsplit(yvar, split = "_"))[1:2]),
                              collapse = "_"))
        }

        hist(Dplot[[yvar]],
             main   = paste(i, yvar),
             xlab   = yvar,
             breaks = 100, col = col)
    }
}
#+ echo=F, include=F
rm(data_list)





## ~ Calculate daily cum sum ---------------------------------------------------

setorder(  ALL_1_daily_DESEAS, Date)
setorder(CLEAR_1_daily_DESEAS, Date)
setorder(CLOUD_1_daily_DESEAS, Date)



## Remove trends before cumulative sum !! --------------------------------------
#'
#' ## Create new data sets with the long term removed
#'
#+ echo=F, include=T

## get linear trends
trends <- readRDS("./figures/tbl_longterm_trends.Rds")

## __ Daily data  --------------------------------------------------------------
dsets <- c("ALL_1_daily_DESEAS",
           "CLEAR_1_daily_DESEAS",
           "CLOUD_1_daily_DESEAS")

for (ad in dsets) {
    DBS  <- get(ad)
    type <- sub("_.*", "", ad)
    temp <- trends[DATA == type]

    for (avaa in temp$var) {
        ## get lm data
        ll   <- temp[var == avaa]
        ## compute trend
        SDRtredn <- ll$intercept + as.numeric(DBS$Date) * ll$slope
        ## remove trend from data
        DBS[[avaa]] <- DBS[[avaa]] - SDRtredn
    }
    ## save new data data table without the trend
    assign(paste0(ad, "_notrend"), DBS)
}

## __ Monthly data  --------------------------------------------------------------
dsets    <- c(  "ALL_1_D_monthly_DESEAS",
              "CLEAR_1_D_monthly_DESEAS",
              "CLOUD_1_D_monthly_DESEAS")


for (ad in dsets) {
    DBS  <- get(ad)
    type <- sub("_.*", "", ad)

    ## get vars to remove trend
    vars <- grep("_att$|_att_des$", names(DBS), value = TRUE)

    for (avaa in vars) {
        ## get lm data
        ll <- lm(DBS[[avaa]] ~ DBS$Date)

        ## compute trend
        SDRtredn <- ll$coefficients[1] + as.numeric(DBS$Date) * ll$coefficients[2]
        ## remove trend from data
        DBS[[avaa]] <- DBS[[avaa]] - SDRtredn
    }
    ## save new data data table without the trend
    assign(paste0(ad, "_notrend"), DBS)
}




## Calculate cusum with zeroing NA
  ALL_1_daily_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
  ALL_1_daily_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
  ALL_1_daily_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLEAR_1_daily_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLEAR_1_daily_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLEAR_1_daily_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLOUD_1_daily_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLOUD_1_daily_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLOUD_1_daily_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]

## Standardized cusum
  ALL_1_daily_DESEAS[, GLB_att_cusumSTD    := cumsum(tidyr::replace_na((GLB_att_des    - mean(GLB_att_des   , na.rm = T)) / sd(GLB_att_des   , na.rm = T), 0))]
  ALL_1_daily_DESEAS[, DIR_att_cusumSTD    := cumsum(tidyr::replace_na((DIR_att_des    - mean(DIR_att_des   , na.rm = T)) / sd(DIR_att_des   , na.rm = T), 0))]
  ALL_1_daily_DESEAS[, DIR_transp_cusumSTD := cumsum(tidyr::replace_na((DIR_transp_des - mean(DIR_transp_des, na.rm = T)) / sd(DIR_transp_des, na.rm = T), 0))]
CLEAR_1_daily_DESEAS[, GLB_att_cusumSTD    := cumsum(tidyr::replace_na((GLB_att_des    - mean(GLB_att_des   , na.rm = T)) / sd(GLB_att_des   , na.rm = T), 0))]
CLEAR_1_daily_DESEAS[, DIR_att_cusumSTD    := cumsum(tidyr::replace_na((DIR_att_des    - mean(DIR_att_des   , na.rm = T)) / sd(DIR_att_des   , na.rm = T), 0))]
CLEAR_1_daily_DESEAS[, DIR_transp_cusumSTD := cumsum(tidyr::replace_na((DIR_transp_des - mean(DIR_transp_des, na.rm = T)) / sd(DIR_transp_des, na.rm = T), 0))]
CLOUD_1_daily_DESEAS[, GLB_att_cusumSTD    := cumsum(tidyr::replace_na((GLB_att_des    - mean(GLB_att_des   , na.rm = T)) / sd(GLB_att_des   , na.rm = T), 0))]
CLOUD_1_daily_DESEAS[, DIR_att_cusumSTD    := cumsum(tidyr::replace_na((DIR_att_des    - mean(DIR_att_des   , na.rm = T)) / sd(DIR_att_des   , na.rm = T), 0))]
CLOUD_1_daily_DESEAS[, DIR_transp_cusumSTD := cumsum(tidyr::replace_na((DIR_transp_des - mean(DIR_transp_des, na.rm = T)) / sd(DIR_transp_des, na.rm = T), 0))]




## Calculate cusum with zeroing NA
  ALL_1_daily_DESEAS_notrend[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
  ALL_1_daily_DESEAS_notrend[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
  ALL_1_daily_DESEAS_notrend[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLEAR_1_daily_DESEAS_notrend[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLEAR_1_daily_DESEAS_notrend[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLEAR_1_daily_DESEAS_notrend[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLOUD_1_daily_DESEAS_notrend[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLOUD_1_daily_DESEAS_notrend[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLOUD_1_daily_DESEAS_notrend[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]

## Standardized cusum
  ALL_1_daily_DESEAS_notrend[, GLB_att_cusumSTD    := cumsum(tidyr::replace_na((GLB_att_des    - mean(GLB_att_des   , na.rm = T)) / sd(GLB_att_des   , na.rm = T), 0))]
  ALL_1_daily_DESEAS_notrend[, DIR_att_cusumSTD    := cumsum(tidyr::replace_na((DIR_att_des    - mean(DIR_att_des   , na.rm = T)) / sd(DIR_att_des   , na.rm = T), 0))]
  ALL_1_daily_DESEAS_notrend[, DIR_transp_cusumSTD := cumsum(tidyr::replace_na((DIR_transp_des - mean(DIR_transp_des, na.rm = T)) / sd(DIR_transp_des, na.rm = T), 0))]
CLEAR_1_daily_DESEAS_notrend[, GLB_att_cusumSTD    := cumsum(tidyr::replace_na((GLB_att_des    - mean(GLB_att_des   , na.rm = T)) / sd(GLB_att_des   , na.rm = T), 0))]
CLEAR_1_daily_DESEAS_notrend[, DIR_att_cusumSTD    := cumsum(tidyr::replace_na((DIR_att_des    - mean(DIR_att_des   , na.rm = T)) / sd(DIR_att_des   , na.rm = T), 0))]
CLEAR_1_daily_DESEAS_notrend[, DIR_transp_cusumSTD := cumsum(tidyr::replace_na((DIR_transp_des - mean(DIR_transp_des, na.rm = T)) / sd(DIR_transp_des, na.rm = T), 0))]
CLOUD_1_daily_DESEAS_notrend[, GLB_att_cusumSTD    := cumsum(tidyr::replace_na((GLB_att_des    - mean(GLB_att_des   , na.rm = T)) / sd(GLB_att_des   , na.rm = T), 0))]
CLOUD_1_daily_DESEAS_notrend[, DIR_att_cusumSTD    := cumsum(tidyr::replace_na((DIR_att_des    - mean(DIR_att_des   , na.rm = T)) / sd(DIR_att_des   , na.rm = T), 0))]
CLOUD_1_daily_DESEAS_notrend[, DIR_transp_cusumSTD := cumsum(tidyr::replace_na((DIR_transp_des - mean(DIR_transp_des, na.rm = T)) / sd(DIR_transp_des, na.rm = T), 0))]







## ~ Calculate monthly cum sum -------------------------------------------------

setorder(  ALL_1_D_monthly_DESEAS, Date)
setorder(CLEAR_1_D_monthly_DESEAS, Date)
setorder(CLOUD_1_D_monthly_DESEAS, Date)



# ## with keeping NAs in place
#   ALL_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(ifelse(is.na(GLB_att_des),    0, GLB_att_des))    + GLB_att_des*0    ]
#   ALL_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(ifelse(is.na(DIR_att_des),    0, DIR_att_des))    + DIR_att_des*0    ]
#   ALL_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(ifelse(is.na(DIR_transp_des), 0, DIR_transp_des)) + DIR_transp_des*0 ]
# CLEAR_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(ifelse(is.na(GLB_att_des),    0, GLB_att_des))    + GLB_att_des*0    ]
# CLEAR_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(ifelse(is.na(DIR_att_des),    0, DIR_att_des))    + DIR_att_des*0    ]
# CLEAR_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(ifelse(is.na(DIR_transp_des), 0, DIR_transp_des)) + DIR_transp_des*0 ]
# CLOUD_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(ifelse(is.na(GLB_att_des),    0, GLB_att_des))    + GLB_att_des*0    ]
# CLOUD_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(ifelse(is.na(DIR_att_des),    0, DIR_att_des))    + DIR_att_des*0    ]
# CLOUD_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(ifelse(is.na(DIR_transp_des), 0, DIR_transp_des)) + DIR_transp_des*0 ]

## Calculate cusum with zeroing NA
  ALL_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
  ALL_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
  ALL_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLEAR_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLEAR_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLEAR_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLOUD_1_D_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLOUD_1_D_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLOUD_1_D_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]



## Calculate cusum with zeroing NA
  ALL_1_D_monthly_DESEAS_notrend[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
  ALL_1_D_monthly_DESEAS_notrend[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
  ALL_1_D_monthly_DESEAS_notrend[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLEAR_1_D_monthly_DESEAS_notrend[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLEAR_1_D_monthly_DESEAS_notrend[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLEAR_1_D_monthly_DESEAS_notrend[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]
CLOUD_1_D_monthly_DESEAS_notrend[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0))]
CLOUD_1_D_monthly_DESEAS_notrend[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0))]
CLOUD_1_D_monthly_DESEAS_notrend[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0))]




## DAILY CUSUM plots -----------------------------------------------------------

## __ Daily CUSUM --------------------------------------------------------------

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Whole day daily cumulative sum
#'
#' All data in this section are referring to daily means.
#'
#+ echo=F, include=T

# vars        <- c("GLB_att", "DIR_att", "DIR_transp")
vars        <- c("GLB_att")
database    <- c(  "ALL_1_daily_DESEAS",
                 "CLEAR_1_daily_DESEAS",
                 "CLOUD_1_daily_DESEAS")

#+ CumulativeDailyCuSum, echo=F, include=T, results="asis"
for (adb in database) {
    DB  <- get(adb)

    cat("\n\\newpage\n")
    cat("\n#### Daily cum sums for", translate(sub("_.*", "", adb)), "\n\n")

    for (avar in vars) {

        ## Plot regular cusums
        wcare <- c("Date", paste0(avar,"_des"), paste0(avar,"_cusum"))
        pdb   <- DB[, ..wcare]
        rm(DB)
        xlim  <- range(pdb$Date)
        ylim  <- range(pdb[[paste0(avar,"_cusum")]],na.rm = T)
        col   <- get(paste0(c("col",
                              unlist(strsplit(avar, split = "_" ))[1:2]),
                            collapse = "_"))

        if (DRAFT == TRUE) {
            par("mar" = c(3, 5, 2,  1))
        } else {
            par("mar" = c(3, 5, 0.5, 0.5))
        }
        par(pch = 19)

        ## scale fonts
        ccex <- ccex_sbs
        par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

        plot(1, type = "n",
             xlab = "",
             xlim = xlim, ylim = ylim,
             xaxt = "n",
             yaxt = "n",
             ylab = "")

        # x axis
        axis.Date(1, pdb$Date)
        axis.Date(1,
                  at = seq(as.Date("1993-01-01"), max(pdb$Date), by = "year"),
                  format = "%Y",
                  labels = NA,
                  tcl = -0.25)


        # y axis
        sfmt <- paste0(pretty(ylim) / 1000,"k")
        sfmt <- sub("^0k$", "0", sfmt)
        axis(2,
             at     = pretty(ylim),
             labels = sfmt,
             las    = 2,
             ylab   = "")


        title(ylab = bquote("Anomaly CUSUM [%]"),
              line = 3.5)



        ## daily from other DT
        lines(pdb$Date, pdb[[paste0(avar,"_cusum")]], col = col, lwd = 2)

        # points(pdb$Date, pdb[[paste0(avar,"_cusum")]], col = "red", lwd = 2)


        if (DRAFT == T) {
            title(paste(sub("_.*","",adb), "mean daily cumulative sum ",
                        translate(avar) ), cex.main = 1)
        } else {
            legend("bottomleft", 0, translate(sub("_.*","",adb)),
                   cex   = 1.1,
                   bty   = "n",
                   xjust = 0.5,      # 0.5 means center adjusted
                   yjust = 0.5,      # 0.5 means center adjusted
                   x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                   y.intersp =  0.2, # adjust character interspacing to effect box height
                   adj = c(0, 0.5))  # adjust string position (default values used here)
            # cex = 1.5,      # change cex if you like (not used here)
            # text.font = 2)  # bold the text if you like (not used here)
            par("mar" = c(3,4,2,1))
        }

        ##  reset fonts
        ccex <- 1
        par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)



        ## test plot for reference
        ## linear model
        lm1 <- lm(pdb[[paste0(avar,"_des")]] ~ pdb$Date)

        plot(pdb$Date, pdb[[paste0(avar,"_des")]],
             ylab = bquote("Daily Anomaly [%]"),
             cex = 0.3,
             col = col)
        abline(h = 0, lty = 2, lwd = 0.8)
        title(paste(sub("_.*","",adb), "mean daily values ",
                    translate(avar) ), cex.main = 1)
        abline(lm1, lwd = 2)


        ## running mean
        rm <- frollmean(pdb[[paste0(avar,"_des")]], round(running_mean_window_days),
                        na.rm = TRUE, algo = "exact", align = "center")
        points(pdb$Date, rm, col = "red", cex = 0.4)

        ## LOESS curve
        vec <- !is.na(pdb[[paste0(avar,"_des")]])
        FTSE.lo3 <- loess.as(pdb$Date[vec], pdb[[paste0(avar,"_des")]][vec],
                             degree = 1,
                             criterion = c("aicc", "gcv")[2], user.span = NULL, plot = F)
        FTSE.lo.predict3 <- predict(FTSE.lo3, pdb$Date)
        lines(pdb$Date, FTSE.lo.predict3, col = "cyan", lwd = 2.5)

        ## decorations
        fit <- lm1[[1]]
        legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
               paste("Trend: ",
                     if (fit[2] > 0) "+" else "-",
                     signif(abs(fit[2]) * Days_of_year, 3),
                     "% per year")
        )


        # hist(pdb$GLB_att_cusum)
        # hist(pdb$GLB_att_des)
        # sum(pdb[ GLB_att_des > 0, GLB_att_des ])
        # sum(pdb[ , GLB_att_des ], na.rm = T)
        # tail(pdb[ , GLB_att_cusum ])

        cat("\n\\newpage\n")
        cat("\n\\footnotesize\n")
        # cat("\n\\small\n")

        testdb <-
            merge(
                merge(
                    merge(
                        merge(
                            pdb[get(paste0(avar,"_des")) > 0, .(PositiveSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)],
                            pdb[get(paste0(avar,"_des")) < 0, .(NegativeSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                        pdb[get(paste0(avar,"_des")) > 0, .(PositiveMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                    pdb[get(paste0(avar,"_des")) < 0, .(NegativeMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]
                ),
                pdb[ , .(Sum = sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date) ]
            )

        cat(pander(testdb))
        cat("\n\\normalsize\n")


        ylim <- range(testdb$PositiveSum, -testdb$NegativeSum)
        plot( testdb$year,  testdb$PositiveSum, type = "l", col = "blue", ylim = ylim)
        lines(testdb$year, -testdb$NegativeSum, type = "l", col = "red")
        title("Sum of negative and positive values by year")

        legend("top", ncol = 2, bty = "n",
               lty = 1, pch = NA, cex = 0.7,
               legend = c("Sum of positive anomaly",
                          "Sum of negative anomaly"),
               col    = c("blue", "red")
        )

        # ylim <- c(-max(abs(testdb$Sum), na.rm = T), max(abs(testdb$Sum), na.rm = T))
        # plot( testdb$year,  testdb$Sum, type = "l", col = "black", ylim = ylim)
        # abline(h = 0, lty = 2, lwd = 0.8)
        # title("Sum of all yearly values")


        gp <- ggplot(data = testdb, aes(x = year, y = Sum)) +
            geom_col(data = testdb[Sum <= 0], fill = "red") +
            geom_col(data = testdb[Sum >= 0], fill = "blue") +
            ggtitle("Sum of all year values") +
            xlab("Year")     +
            ylab("Year Sum") +
            theme_bw()       +
            theme(plot.title = element_text(hjust = 0.5))

        print(gp)

    }
}
#+ echo=F, include=T





## __ Daily CUSUM without trend  -----------------------------------------------


#'
#' \newpage
#' \FloatBarrier
#'
#' ### Whole day daily cumulative sum  without trend
#'
#' All data in this section are referring to daily means.
#'
#' With the corresponding long term trend removed.
#'
#+ echo=F, include=T

# vars        <- c("GLB_att", "DIR_att", "DIR_transp")
vars        <- c("GLB_att")
database    <- c(  "ALL_1_daily_DESEAS_notrend",
                   "CLEAR_1_daily_DESEAS_notrend",
                   "CLOUD_1_daily_DESEAS_notrend")

#+ CumulativeDailyCuSumNOtrend, echo=F, include=T, results="asis"
for (adb in database) {
    DB  <- get(adb)

    cat("\n\\newpage\n")
    cat("\n#### Daily cum sums for", translate(sub("_.*", "", adb)), "\n\n")

    for (avar in vars) {

        ## Plot regular cusums
        wcare <- c("Date", paste0(avar,"_des"), paste0(avar,"_cusum"))
        pdb   <- DB[, ..wcare]
        rm(DB)
        xlim  <- range(pdb$Date)
        ylim  <- range(pdb[[paste0(avar,"_cusum")]],na.rm = T)
        col   <- get(paste0(c("col",
                              unlist(strsplit(avar, split = "_" ))[1:2]),
                            collapse = "_"))

        if (DRAFT == TRUE) {
            par("mar" = c(3, 4, 2, 1))
        } else {
            par("mar" = c(3, 4, 0.5, 0.5))
        }
        par(pch = 19)

        ## scale fonts
        ccex <- ccex_sbs
        par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

        plot(1, type = "n",
             xlab = "",
             xlim = xlim, ylim = ylim,
             xaxt = "n",
             yaxt = "n",
             ylab = "")
        abline(h = 0, lty = 2, lwd = 0.8)

        # x axis
        axis.Date(1, pdb$Date)
        axis.Date(1,
                  at = seq(as.Date("1993-01-01"), max(pdb$Date), by = "year"),
                  format = "%Y",
                  labels = NA,
                  tcl = -0.25)

        # y axis
        sfmt <- paste0(pretty(ylim) / 1000,"k")
        sfmt <- sub("^0k$", "0", sfmt)
        axis(2,
             at     = pretty(ylim),
             labels = sfmt,
             las    = 2,
             ylab   = "")
        title(ylab = bquote("Anomaly CUSUM, trend removed [%]"),
              line = 3)


        ## daily from other DT
        lines(pdb$Date, pdb[[paste0(avar,"_cusum")]], col = col, lwd = 2)



        if (DRAFT == T) {
            title(paste(sub("_.*","",adb), "mean daily cumulative sum ",
                        translate(avar) ), cex.main = 1)
        } else {
            legend("bottomleft", 0, translate(sub("_.*","",adb)),
                   cex   = 2,
                   bty   = "n",
                   xjust = 0.5,      # 0.5 means center adjusted
                   yjust = 0.5,      # 0.5 means center adjusted
                   x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                   y.intersp =  0.2, # adjust character interspacing to effect box height
                   adj = c(0, 0.5))  # adjust string position (default values used here)
            # cex = 1.5,      # change cex if you like (not used here)
            # text.font = 2)  # bold the text if you like (not used here)
            par("mar" = c(3,4,2,1))
        }

        ##  reset fonts
        ccex <- 1
        par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)



        ## test plot for reference
        ## linear model
        lm1 <- lm(pdb[[paste0(avar,"_des")]] ~ pdb$Date)

        plot(pdb$Date, pdb[[paste0(avar,"_des")]],
             ylab = bquote("Daily Anomaly, trend removed [%]"),
             cex = 0.3,
             col = col)
        abline(h = 0, lty = 2, lwd = 0.8)
        title(paste(sub("_.*","",adb), "mean daily values ",
                    translate(avar) ), cex.main = 1)
        abline(lm1, lwd = 2)

        ## running mean
        rm <- frollmean(pdb[[paste0(avar,"_des")]], round(running_mean_window_days),
                        na.rm = TRUE, algo = "exact", align = "center")
        points(pdb$Date, rm, col = "red", cex = 0.4)

        ## LOESS curve
        vec <- !is.na(pdb[[paste0(avar,"_des")]])
        FTSE.lo3 <- loess.as(pdb$Date[vec], pdb[[paste0(avar,"_des")]][vec],
                             degree = 1,
                             criterion = c("aicc", "gcv")[2], user.span = NULL, plot = F)
        FTSE.lo.predict3 <- predict(FTSE.lo3, pdb$Date)
        lines(pdb$Date, FTSE.lo.predict3, col = "cyan", lwd = 2.5)

        ## decorations
        fit <- lm1[[1]]
        legend("top", lty = 1, bty = "n", lwd = 2, cex = 1,
               paste("Trend: ",
                     if (fit[2] > 0) "+" else "-",
                     signif(abs(fit[2]) * Days_of_year, 3),
                     "% per year")
        )



        # hist(pdb$GLB_att_cusum)
        # hist(pdb$GLB_att_des)
        # sum(pdb[ GLB_att_des > 0, GLB_att_des ])
        # sum(pdb[ , GLB_att_des ], na.rm = T)
        # tail(pdb[ , GLB_att_cusum ])

        cat("\n\\newpage\n")
        cat("\n\\footnotesize\n")
        # cat("\n\\small\n")

        testdb <-
            merge(
                merge(
                    merge(
                        merge(
                            pdb[get(paste0(avar,"_des")) > 0, .(PositiveSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)],
                            pdb[get(paste0(avar,"_des")) < 0, .(NegativeSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                        pdb[get(paste0(avar,"_des")) > 0, .(PositiveMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                    pdb[get(paste0(avar,"_des")) < 0, .(NegativeMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]
                ),
                pdb[ , .(Sum = sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date) ]
            )

        cat(pander(testdb))
        cat("\n\\normalsize\n")


        ylim <- range(testdb$PositiveSum, -testdb$NegativeSum)
        plot( testdb$year,  testdb$PositiveSum, type = "l", col = "blue", ylim = ylim)
        lines(testdb$year, -testdb$NegativeSum, type = "l", col = "red")
        title("Sum of negative and positive values by year")

        legend("top", ncol = 2, bty = "n",
               lty = 1, pch = NA, cex = 0.7,
               legend = c("Sum of positive anomaly",
                          "Sum of negative anomaly"),
               col    = c("blue", "red")
        )

        # ylim <- c(-max(abs(testdb$Sum), na.rm = T), max(abs(testdb$Sum), na.rm = T))
        # plot( testdb$year,  testdb$Sum, type = "l", col = "black", ylim = ylim)
        # abline(h = 0, lty = 2, lwd = 0.8)
        # title("Sum of all yearly values")


        gp <- ggplot(data = testdb, aes(x = year, y = Sum)) +
            geom_col(data = testdb[Sum <= 0], fill = "red") +
            geom_col(data = testdb[Sum >= 0], fill = "blue") +
            ggtitle("Sum of all year values") +
            xlab("Year")     +
            ylab("Year Sum") +
            theme_bw()       +
            theme(plot.title = element_text(hjust = 0.5))

        print(gp)

    }
}
#'
#+ echo=F, include=T


## __ Daily Standardized SUSUM plots  ------------------------------------------

#+ CumulativeDailyCuSumSTD, echo=F, include=T, results="asis"
for (adb in database) {
    DB  <- get(adb)

    cat("\n\\newpage\n")
    cat("\n#### Daily cum sums for", translate(sub("_.*", "", adb)), "\n\n")

    for (avar in vars) {

        ## Plot Standardized cusums
        wcare <- c("Date", paste0(avar,"_des"), paste0(avar,"_cusumSTD"))
        pdb   <- DB[, ..wcare]
        rm(DB)
        xlim  <- range(pdb$Date)
        ylim  <- range(pdb[[paste0(avar,"_cusumSTD")]],na.rm = T)
        col   <- get(paste0(c("col",
                              unlist(strsplit(avar, split = "_" ))[1:2]),
                            collapse = "_"))

        par("mar" = c(3,4,2,1))
        par(pch = 19)

        plot(1, type = "n",
             xlab = "",
             xlim = xlim, ylim = ylim,
             xaxt = "n",
             ylab = bquote("STD Cumulative Sum of Anomaly [%]"))
        axis.Date(1, pdb$Date)
        abline(h = 0, lty = 2, lwd = 0.8)

        ## daily from other DT
        lines(pdb$Date, pdb[[paste0(avar,"_cusumSTD")]], col = col, lwd = 2)

        title(paste(sub("_.*","",adb), "mean daily cumulative sum STD",
                    translate(avar) ), cex.main = 1)

    }
}
#'
#+ echo=F, include=T




## MONTHLY daily CUSUM plots ---------------------------------------------------

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Whole day monthly cumulative sum
#'
#' In this section the monthly values have been calculated from daily mean values.
#'
#+ echo=F, include=T

## __ Monthly CUSUM ------------------------------------------------------------


# vars        <- c("GLB_att", "DIR_att", "DIR_transp")
vars        <- c("GLB_att")
database    <- c(  "ALL_1_D_monthly_DESEAS",
                 "CLEAR_1_D_monthly_DESEAS",
                 "CLOUD_1_D_monthly_DESEAS")
lec         <- 0

for (adb in database) {
    DB    <- get(adb)
    avar  <- "GLB_att"
    TYPE  <- sub("_.*", "", adb)
    wcare <- c("Date", avar,
               paste0(avar,"_sd"),
               paste0(avar,"_des"),
               paste0(avar,"_cusum"),
               paste0(avar,"_N")
               )
    # DB[, ..wcare]
    # paste0("Export_",TYPE, "_1_D_monthly_DESEAS.csv")
    write.csv(file = paste0("Export_",TYPE, "_1_D_monthly_DESEAS.csv"),
              x = DB[, ..wcare],
              row.names = F)
}



#+ CumulativeMonthlyCuSum, echo=F, include=T, results="asis"
for (adb in database) {
    DB  <- get(adb)
    lec <- lec + 1

    cat("\n\\newpage\n")
    cat("\n#### Monthly cum sums for", translate(sub("_.*", "", adb)), "\n\n")

    for (avar in vars) {

        ## Plot regular cusums
        wcare <- c("Date", paste0(avar,"_des"), paste0(avar,"_cusum"))
        pdb   <- DB[, ..wcare]
        rm(DB)
        xlim  <- range(pdb$Date)
        ylim  <- range(pdb[[paste0(avar,"_cusum")]],na.rm = T)
        col   <- get(paste0(c("col",
                              unlist(strsplit(avar, split = "_" ))[1:2]),
                            collapse = "_"))

        if (DRAFT == TRUE) {
            par("mar" = c(3, 5, 2,  1))
        } else {
            par("mar" = c(3, 5, 0.5, 0.5))
            par("mar" = c(3, 5, 0.5, 1))
        }
        par(pch = 19)

        ## scale fonts
        ccex <- ccex_sbs
        par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

        plot(1, type = "n",
             xlab = "",
             xlim = xlim, ylim = ylim,
             xaxt = "n",
             yaxt = "n",
             ylab = "")

        # x axis
        axis.Date(1, pdb$Date)
        axis.Date(1,
                  at = seq(as.Date("1993-01-01"), max(pdb$Date), by = "year"),
                  format = "%Y",
                  labels = NA,
                  tcl = -0.25)

        # y axis
        # sfmt <- paste0(pretty(ylim) / 1000,"k")
        # sfmt <- sub("^0k$", "0", sfmt)
        axis(2,
             at     = pretty(ylim),
             # labels = sfmt,
             las    = 2,
             ylab   = "")

        title(ylab = bquote("CUSUM of anomalies [%]"),
              line = 3.5)



        ## daily from other DT
        lines(pdb$Date, pdb[[paste0(avar,"_cusum")]], col = col, lwd = 2)

        # #test
        # points(pdb$Date, pdb[[paste0(avar,"_cusum")]], col = "red")


        if (DRAFT == T) {
            title(paste(sub("_.*","",adb), "mean daily cumulative sum ",
                        translate(avar) ), cex.main = 1)
        } else {
            ## tag the corner
            ## translate(sub("_.*","",adb))
            legend("bottomleft", 0, paste0("(",letters[lec],")"),
                   # cex   = 1.1,
                   cex   = 1.70,
                   bty   = "n",
                   xjust = 0.5,      # 0.5 means center adjusted
                   yjust = 0.5,      # 0.5 means center adjusted
                   x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                   y.intersp =  0.2, # adjust character interspacing to effect box height
                   adj = c(0, 0.5))  # adjust string position (default values used here)
            # text.font = 2  # bold the text if you like (not used here)
            par("mar" = c(3,4,2,1))
        }

        ##  reset fonts
        ccex <- 1
        par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)



        ## test plot for reference
        ## linear model
        lm1 <- lm(pdb[[paste0(avar,"_des")]] ~ pdb$Date)

        plot(pdb$Date, pdb[[paste0(avar,"_des")]],
             ylab = bquote("Monthly Anomaly [%]"),
             cex = 0.3,
             col = col)
        abline(h = 0, lty = 2, lwd = 0.8)
        title(paste(sub("_.*","",adb), "mean monthly values ",
                    translate(avar) ), cex.main = 1)
        abline(lm1, lwd = 2)



        ## running mean
        rm <- frollmean(pdb[[paste0(avar,"_des")]], round(running_mean_window_days),
                        na.rm = TRUE, algo = "exact", align = "center")
        points(pdb$Date, rm, col = "red", cex = 0.5)


        ## LOESS curve
        vec <- !is.na(pdb[[paste0(avar,"_des")]])
        FTSE.lo3 <- loess.as(pdb$Date[vec], pdb[[paste0(avar,"_des")]][vec],
                             degree = 1,
                             criterion = c("aicc", "gcv")[2], user.span = NULL, plot = F)
        FTSE.lo.predict3 <- predict(FTSE.lo3, pdb$Date)
        lines(pdb$Date, FTSE.lo.predict3, col = "cyan", lwd = 2.5)


        ## decorations
        fit <- lm1[[1]]
        legend('top', lty = 1, bty = "n", lwd = 2, cex = 1,
               paste("Trend: ",
                     if (fit[2] > 0) "+" else "-",
                     signif(abs(fit[2]) * Days_of_year, 3),
                     "% per year")
        )


        cat("\n\\newpage\n")
        cat("\n\\footnotesize\n")
        # cat("\n\\small\n")

        testdb <-
            merge(
                merge(
                    merge(
                        merge(
                            pdb[get(paste0(avar,"_des")) > 0, .(PositiveSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)],
                            pdb[get(paste0(avar,"_des")) < 0, .(NegativeSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                        pdb[get(paste0(avar,"_des")) > 0, .(PositiveMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                    pdb[get(paste0(avar,"_des")) < 0, .(NegativeMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]
                ),
                pdb[ , .(Sum = sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date) ]
            )

        cat(pander(testdb))
        cat("\n\\normalsize\n")


        ylim <- range(testdb$PositiveSum, -testdb$NegativeSum)
        plot( testdb$year,  testdb$PositiveSum, type = "l", col = "blue", ylim = ylim)
        lines(testdb$year, -testdb$NegativeSum, type = "l", col = "red")
        title("Sum of negative and positive values by year")

        legend("top", ncol = 2, bty = "n",
               lty = 1, pch = NA, cex = 0.7,
               legend = c("Sum of positive anomaly",
                          "Sum of negative anomaly"),
               col    = c("blue", "red")
        )


        # ylim <- c(-max(abs(testdb$Sum), na.rm = T), max(abs(testdb$Sum), na.rm = T))
        # plot( testdb$year,  testdb$Sum, type = "l", col = "black", ylim = ylim)
        # abline(h = 0, lty = 2, lwd = 0.8)
        # title("Sum of all yearly values")

        # polygon(
        #     c(min(testdb$year), testdb$year , max(testdb$year)) ,
        #     c(min(testdb$Sum) , testdb$Sum , min(testdb$Sum)) ,
        #     col=rgb(0.2,0.1,0.5,0.2) , border=F
        # )


        gp <- ggplot(data = testdb, aes(x = year, y = Sum)) +
            geom_col(data = testdb[Sum <= 0], fill = "red") +
            geom_col(data = testdb[Sum >= 0], fill = "blue") +
            ggtitle("Sum of all year values") +
            xlab("Year")     +
            ylab("Year Sum") +
            theme_bw()       +
            theme(plot.title = element_text(hjust = 0.5))

        print(gp)

        # hist(pdb$GLB_att_cusum)
        # hist(pdb$GLB_att_des)
        # sum(pdb[ GLB_att_des > 0, GLB_att_des ])
        # sum(pdb[ , GLB_att_des ], na.rm = T)
        # tail(pdb[ , GLB_att_cusum ])
        # pdb[ , sum(get(paste0(avar,"_des")), na.rm = T), by = year(Date) ]
    }
}
#'
#+ echo=F, include=T




## __ Monthly CUSUM without trend ----------------------------------------------

#' \newpage
#' \FloatBarrier
#'
#' ### Whole day monthly cumulative sum
#'
#' In this section the monthly values have been calculated from daily mean values.
#' And the monthly long term was removed prior to CUSUM.
#'
#+ echo=F, include=T

# vars        <- c("GLB_att", "DIR_att", "DIR_transp")
vars        <- c("GLB_att")
database    <- c("ALL_1_D_monthly_DESEAS_notrend",
                 "CLEAR_1_D_monthly_DESEAS_notrend",
                 "CLOUD_1_D_monthly_DESEAS_notrend")
lec          <- 0

#+ CumulativeMonthlyCuSumNOtrend, echo=F, include=T, results="asis"
# , fig.height = 5.5, fig.width = 7
for (adb in database) {
    DB  <- get(adb)
    lec <- lec + 1

    cat("\n\\newpage\n")
    cat("\n#### Monthly cum sums for", translate(sub("_.*", "", adb)), "\n\n")

    for (avar in vars) {

        ## Plot regular cusums
        wcare <- c("Date", paste0(avar,"_des"), paste0(avar,"_cusum"))
        pdb   <- DB[, ..wcare]
        rm(DB)
        xlim  <- range(pdb$Date)
        ylim  <- range(pdb[[paste0(avar,"_cusum")]],na.rm = T)
        col   <- get(paste0(c("col",
                              unlist(strsplit(avar, split = "_" ))[1:2]),
                            collapse = "_"))

        if (DRAFT == TRUE) {
            par("mar" = c(3, 5, 2,  1))
        } else {
            # par("mar" = c(3, 5, 3.4, 0.5))
            par("mar" = c(3, 5, 0, 0.5))
        }
        par(pch = 19)


        ## scale fonts
        ccex <- ccex_sbs
        par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)

        # options(repr.plot.width  = 7,
        #         repr.plot.height = 7)
        # knitr::opts_chunk$set(dev = 'pdf')
        # knitr::opts_chunk$set(fig.height = 7)
        # knitr::opts_chunk$set(fig.width = 7)

        plot(1, type = "n",
             xlab = "",
             xlim = xlim, ylim = ylim,
             xaxt = "n",
             yaxt = "n",
             ylab = "")
        abline(h = 0, lty = 2, lwd = 0.8)

        # x axis
        axis.Date(1, pdb$Date)
        axis.Date(1,
                  at = seq(as.Date("1993-01-01"), max(pdb$Date), by = "year"),
                  format = "%Y",
                  labels = NA,
                  tcl = -0.25)


        # y axis
        # sfmt <- paste0(pretty(ylim) / 1000,"k")
        # sfmt <- sub("^0k$", "0", sfmt)
        axis(2,
             at     = pretty(ylim),
             # labels = sfmt,
             las    = 2,
             ylab   = "")
        title(ylab = bquote("CUSUM of anomalies [%]"),
              line = 3.5)


        ## daily from other DT
        lines(pdb$Date, pdb[[paste0(avar,"_cusum")]], col = col, lwd = 2)



        if (DRAFT == T) {
            title(paste(sub("_.*","",adb), "mean daily cumulative sum ",
                        translate(avar) ), cex.main = 1)
        } else {
            letters[lec]
            ## tag plot with type
            ## translate(sub("_.*","",adb))
            legend("bottomleft", 0, paste0("(",letters[lec],")"),
                   cex   = 1.70,
                   bty   = "n",
                   xjust = 0.5,      # 0.5 means center adjusted
                   yjust = 0.5,      # 0.5 means center adjusted
                   x.intersp = -0.5, # adjust character interspacing as you like to effect box width
                   y.intersp =  0.2, # adjust character interspacing to effect box height
                   adj = c(0, 0.5))  # adjust string position (default values used here)
            # cex = 1.5,      # change cex if you like (not used here)
            # text.font = 2)  # bold the text if you like (not used here)
            par("mar" = c(3,4,2,1))
        }

        ##  reset fonts
        ccex <- 1
        par(cex.lab = ccex, cex.axis = ccex, cex.main = ccex, cex = ccex)



        ## test plot for reference
        ## linear model
        lm1 <- lm(pdb[[paste0(avar,"_des")]] ~ pdb$Date)

        plot(pdb$Date, pdb[[paste0(avar,"_des")]],
             ylab = bquote("Monthly Anomaly, trend removed [%]"),
             cex = 0.3,
             col = col)
        abline(h = 0, lty = 2, lwd = 0.8)
        title(paste(sub("_.*","",adb), "mean monthly values ",
                    translate(avar) ), cex.main = 1)
        abline(lm1, lwd = 2)



        ## running mean
        rm <- frollmean(pdb[[paste0(avar,"_des")]], round(running_mean_window_days),
                        na.rm = TRUE, algo = "exact", align = "center")
        points(pdb$Date, rm, col = "red", cex = 0.5)


        ## LOESS curve
        vec <- !is.na(pdb[[paste0(avar,"_des")]])
        FTSE.lo3 <- loess.as(pdb$Date[vec], pdb[[paste0(avar,"_des")]][vec],
                             degree = 1,
                             criterion = c("aicc", "gcv")[2], user.span = NULL, plot = F)
        FTSE.lo.predict3 <- predict(FTSE.lo3, pdb$Date)
        lines(pdb$Date, FTSE.lo.predict3, col = "cyan", lwd = 2.5)



        ## decorations
        fit <- lm1[[1]]
        legend('top', lty = 1, bty = "n", lwd = 2, cex = 1,
               paste("Trend: ",
                     if (fit[2] > 0) "+" else "-",
                     signif(abs(fit[2]) * Days_of_year, 3),
                     "% per year")
        )

        cat("\n\\newpage\n")
        cat("\n\\footnotesize\n")
        # cat("\n\\small\n")

        testdb <-
            merge(
                merge(
                    merge(
                        merge(
                            pdb[get(paste0(avar,"_des")) > 0, .(PositiveSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)],
                            pdb[get(paste0(avar,"_des")) < 0, .(NegativeSum  =  sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                        pdb[get(paste0(avar,"_des")) > 0, .(PositiveMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]),
                    pdb[get(paste0(avar,"_des")) < 0, .(NegativeMean = mean(get(paste0(avar,"_des")), na.rm = T)), by = year(Date)]
                ),
                pdb[ , .(Sum = sum(get(paste0(avar,"_des")), na.rm = T)), by = year(Date) ]
            )

        cat(pander(testdb))
        cat("\n\\normalsize\n")


        ylim <- range(testdb$PositiveSum, -testdb$NegativeSum)
        plot( testdb$year,  testdb$PositiveSum, type = "l", col = "blue", ylim = ylim)
        lines(testdb$year, -testdb$NegativeSum, type = "l", col = "red")
        title("Sum of negative and positive values by year")

        legend("top", ncol = 2, bty = "n",
               lty = 1, pch = NA, cex = 0.7,
               legend = c("Sum of positive anomaly",
                          "Sum of negative anomaly"),
               col    = c("blue", "red")
        )


        # ylim <- c(-max(abs(testdb$Sum), na.rm = T), max(abs(testdb$Sum), na.rm = T))
        # plot( testdb$year,  testdb$Sum, type = "l", col = "black", ylim = ylim)
        # abline(h = 0, lty = 2, lwd = 0.8)
        # title("Sum of all yearly values")

        # polygon(
        #     c(min(testdb$year), testdb$year , max(testdb$year)) ,
        #     c(min(testdb$Sum) , testdb$Sum , min(testdb$Sum)) ,
        #     col=rgb(0.2,0.1,0.5,0.2) , border=F
        # )


        gp <- ggplot(data = testdb, aes(x = year, y = Sum)) +
            geom_col(data = testdb[Sum <= 0], fill = "red") +
            geom_col(data = testdb[Sum >= 0], fill = "blue") +
            ggtitle("Sum of all year values") +
            xlab("Year")     +
            ylab("Year Sum") +
            theme_bw()       +
            theme(plot.title = element_text(hjust = 0.5))

        print(gp)

        # hist(pdb$GLB_att_cusum)
        # hist(pdb$GLB_att_des)
        # sum(pdb[ GLB_att_des > 0, GLB_att_des ])
        # sum(pdb[ , GLB_att_des ], na.rm = T)
        # tail(pdb[ , GLB_att_cusum ])
        # pdb[ , sum(get(paste0(avar,"_des")), na.rm = T), by = year(Date) ]
    }
}
#+ echo=F, include=T







## ~ Calculate monthly sum sum by SZA ------------------------------------------

## set data order before cusum
setorder(  ALL_3_monthly_DESEAS, Date)
setorder(CLEAR_3_monthly_DESEAS, Date)
setorder(CLOUD_3_monthly_DESEAS, Date)

ALL_3_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0)), by = .(SZA, preNoon)]
ALL_3_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0)), by = .(SZA, preNoon)]
ALL_3_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0)), by = .(SZA, preNoon)]
CLEAR_3_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0)), by = .(SZA, preNoon)]
CLEAR_3_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0)), by = .(SZA, preNoon)]
CLEAR_3_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0)), by = .(SZA, preNoon)]
CLOUD_3_monthly_DESEAS[, GLB_att_cusum    := cumsum(tidyr::replace_na(GLB_att_des,    0)), by = .(SZA, preNoon)]
CLOUD_3_monthly_DESEAS[, DIR_att_cusum    := cumsum(tidyr::replace_na(DIR_att_des,    0)), by = .(SZA, preNoon)]
CLOUD_3_monthly_DESEAS[, DIR_transp_cusum := cumsum(tidyr::replace_na(DIR_transp_des, 0)), by = .(SZA, preNoon)]





#### Plot Cumulative sums  -----------------------------------------------------

#'
#' \newpage
#' \FloatBarrier
#'
#' ### Cumulative sums by SZA and part of day
#'
#' Use deseasonalized monthly values to calculate cumulative sums
#' by a SZA bin and a period of day.
#'
#+ echo=F, include=F

plotsza     <- c( 63 )
# plotpreNoon <- c("am","pm","am+pm", "daily")
plotpreNoon <- c("am","pm","am+pm")
# plotpreNoon <- c("am","pm")

plotpNcol   <- c(2, 3, 4, 5)
# vars        <- c("GLB_att", "DIR_att", "DIR_transp")
vars        <- c("GLB_att")
database    <- c(  "ALL_3_monthly_DESEAS",
                   "CLEAR_3_monthly_DESEAS",
                   "CLOUD_3_monthly_DESEAS")

#+ CumulativeMonthlySZACuSum, echo=F, include=T, results="asis"
for (adb in database) {
    DB  <- get(adb)

    cat("\n\\newpage\n")
    cat("\n\\FloatBarrier\n")
    cat("\n#### Monthly cum sums for", translate(sub("_.*", "", adb)), "by SZA\n\n")

    for (asza in plotsza) {
        for (avar in vars) {
            wcare <- c("Date", "preNoon", grep(avar, names(DB), value = T))

            pdb   <- DB[ SZA == asza ]
            pdb   <- pdb[, ..wcare]

            ## Plot cum sum
            xlim  <- range(pdb$Date)
            ylim  <- range(pdb[[paste0(avar,"_cusum")]], na.rm = T)

            par("mar" = c(3,4,2,1))
            par(pch = 19)

            plot(1, type = "n",
                 xlab = "",
                 xlim = xlim, ylim = ylim,
                 xaxt = "n",
                 ylab = bquote("Cumulative Sum Anomaly [%]" ) )
            axis.Date(1, pdb$Date)
            abline(h = 0, lty = 2, lwd = 0.8)

            ## for a sza
            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                lines(pp$Date, pp[[paste0(avar,"_cusum")]], col = plotpNcol[i], lwd = 2)
            }

            legend("top", legend = plotpreNoon, col = plotpNcol,
                   lty = 1, bty = "n", ncol = 3, cex = 0.8)

            title(paste(sub("_.*","",adb), "monthly cumulative sum ",
                        translate(avar), "for SZA", asza,""), cex.main = 1)

            cat("\n \n \n")


            ## plot anomaly with linear fit
            mainvar <- paste0(avar,"_des")
            xlim  <- range(pdb$Date)
            ylim  <- range(pdb[[mainvar]], na.rm = T)

            plot(1, type = "n",
                 xlab = "",
                 xlim = xlim, ylim = ylim,
                 xaxt = "n",
                 ylab = bquote("Seasonal Anomaly [%]" ) )
            axis.Date(1, pdb$Date)
            abline(h = 0, lty = 2, lwd = 0.8)

            ## for a sza
            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                points(pp$Date, pp[[mainvar]], col = plotpNcol[i], cex = 0.5)
            }

            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                abline(lm(pp[[mainvar]] ~ pp$Date), col = plotpNcol[i], lwd = 2)
            }

            legend("top", legend = plotpreNoon, col = plotpNcol, title = "Linear fit",
                   lty = 1, bty = "n", ncol = 3, cex = 0.8)



            ## plot anomaly with loess smoothing
            mainvar <- paste0(avar,"_des")
            xlim  <- range(pdb$Date)
            ylim  <- range(pdb[[mainvar]], na.rm = T)

            plot(1, type = "n",
                 xlab = "",
                 xlim = xlim, ylim = ylim,
                 xaxt = "n",
                 ylab = bquote("Seasonal Anomaly [%]" ) )
            axis.Date(1, pdb$Date)
            abline(h = 0, lty = 2, lwd = 0.8)

            ## for a sza
            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                points(pp$Date, pp[[mainvar]], col = plotpNcol[i], cex = 0.5)
            }

            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                pp <- pp[ !is.na(pp[[mainvar]]) ]

                LOESS_CRITERIO <-  c("aicc", "gcv")[2]
                FTSE.lo3 <- loess.as(pp$Date, pp[[mainvar]],
                                     degree    = 1,
                                     criterion = LOESS_CRITERIO,
                                     user.span = NULL,
                                     plot      = FALSE)
                FTSE.lo.predict3 <- predict(FTSE.lo3, pp$Date)
                lines(pp$Date, FTSE.lo.predict3, col = plotpNcol[i], lwd = 2.5)
            }

            legend("top", legend = plotpreNoon, col = plotpNcol, title = "LOESS",
                   lty = 1, bty = "n", ncol = 3, cex = 0.8)


        }
    }
}
#'
#+ echo=F, include=T





## Plot direct only time scale -------------------------------------------------


plotsza     <- c( 63 )
plotpreNoon <- c("am", "pm", "am+pm")
# plotpreNoon <- c("am", "pm")

plotpNcol   <- c(2, 3, 4, 5)
# vars        <- c("GLB_att", "DIR_att", "DIR_transp")
vars        <- c("DIR_att", "DIR_transp")
database    <- c(  "ALL_3_monthly_DESEAS",
                   "CLEAR_3_monthly_DESEAS",
                   "CLOUD_3_monthly_DESEAS")

#+  CumulativeSZASumDir, echo=F, include=T, results="asis"
for (adb in database) {
    DB  <- get(adb)

    cat("\n\\newpage\n")
    cat("\n\\FloatBarrier\n")
    cat("\n#### Monthly cum sums for", translate(sub("_.*", "", adb)), "by SZA\n\n")

    for (asza in plotsza) {
        for (avar in vars) {
            wcare <- c("Date", "preNoon", grep(avar, names(DB), value = T))

            pdb   <- DB[ SZA == asza ]
            pdb   <- pdb[, ..wcare]

            pdb   <- pdb[!is.na(pdb[[avar]])]

            if (nrow(pdb) == 0) next()

            ## start empty plot
            xlim  <- range(pdb$Date)
            ylim  <- range(pdb[[paste0(avar,"_cusum")]], na.rm = T)

            par("mar" = c(3,4,2,1))
            par(pch = 19)

            plot(1, type = "n",
                 xlab = "",
                 xlim = xlim, ylim = ylim,
                 xaxt = "n",
                 ylab = bquote("Cumulative Sum of Anomaly [%]" ) )
            axis.Date(1, pdb$Date)
            abline(h = 0, lty = 2, lwd = 0.8)

            ## for a sza
            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                lines(pp$Date, pp[[paste0(avar,"_cusum")]], col = plotpNcol[i], lwd = 2)
            }

            legend("top", legend = plotpreNoon, col = plotpNcol,
                   lty = 1, bty = "n", ncol = 3, cex = 0.8)

            title(paste(sub("_.*","",adb), "monthly cumulative sum ",
                        translate(avar), "for SZA", asza,""), cex.main = 1)

            cat("\n \n \n")




            ## plot anomaly with linear fit
            mainvar <- paste0(avar,"_des")
            xlim  <- range(pdb$Date)
            ylim  <- range(pdb[[mainvar]], na.rm = T)

            plot(1, type = "n",
                 xlab = "",
                 xlim = xlim, ylim = ylim,
                 xaxt = "n",
                 ylab = bquote("Anomaly [%]" ) )
            axis.Date(1, pdb$Date)
            abline(h = 0, lty = 2, lwd = 0.8)

            ## for a sza
            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                points(pp$Date, pp[[mainvar]], col = plotpNcol[i], cex = 0.5)
            }

            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                abline(lm(pp[[mainvar]] ~ pp$Date), col = plotpNcol[i], lwd = 2)
            }

            legend("top", legend = plotpreNoon, col = plotpNcol, title = "Linear fit",
                   lty = 1, bty = "n", ncol = 3, cex = 0.8)



            ## plot anomaly with loess smoothing
            mainvar <- paste0(avar,"_des")
            xlim  <- range(pdb$Date)
            ylim  <- range(pdb[[mainvar]], na.rm = T)

            plot(1, type = "n",
                 xlab = "",
                 xlim = xlim, ylim = ylim,
                 xaxt = "n",
                 ylab = bquote("Seasonal Anomaly [%]" ) )
            axis.Date(1, pdb$Date)
            abline(h = 0, lty = 2, lwd = 0.8)

            ## for a sza
            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                points(pp$Date, pp[[mainvar]], col = plotpNcol[i], cex = 0.5)
            }

            for (i in 1:length(plotpreNoon) ) {
                pp <- pdb[preNoon == plotpreNoon[i]]
                pp <- pp[ !is.na(pp[[mainvar]]) ]

                LOESS_CRITERIO <-  c("aicc", "gcv")[2]
                FTSE.lo3 <- loess.as(pp$Date, pp[[mainvar]],
                                     degree    = 1,
                                     criterion = LOESS_CRITERIO,
                                     user.span = NULL,
                                     plot      = FALSE)
                FTSE.lo.predict3 <- predict(FTSE.lo3, pp$Date)
                lines(pp$Date, FTSE.lo.predict3, col = plotpNcol[i], lwd = 2.5)
            }

            legend("top", legend = plotpreNoon, col = plotpNcol, title = "LOESS",
                   lty = 1, bty = "n", ncol = 3, cex = 0.8)

        }
    }
}
#'
#+ echo=F, include=T




#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n", Sys.time(), Sys.info()["login"],
            Sys.info()["nodename"], basename(Script.Name), difftime(tac,tic,units = "mins")))
