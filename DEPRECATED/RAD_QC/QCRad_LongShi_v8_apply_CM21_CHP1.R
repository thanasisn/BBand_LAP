# /* Copyright (C) 2022 Athanasios Natsis <natsisthanasis@gmail.com> */
#' ---
#' title:       "QCRad methodology apply."
#' author:      "Natsis Athanasios"
#' institute:   "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' abstract:    "Data quality for radiation measurements as described by
#'               CN Long and Y Shi, September 2006, DOE/SC-ARM/TR-074.
#'               - The QCRad Value Added Product Surface
#'               Radiation Measurement Quality Control Testing Including
#'               Climatology_Long2006.pdf"
#'
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
#'     keep_md:          no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     fig_width:        8
#'     fig_height:       5
#'   html_document:
#'     toc:        true
#'     fig_width:  9
#'     fig_height: 4
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#'
#' **Source code: [github.com/thanasisn/RAD_QC](https://github.com/thanasisn/RAD_QC)**
#'
#' ### Notes ###
#'
#' Run for each year and apply Quality control on radiation data based on ID.
#'
#' The chosen levels and filters have to be evaluated with the available data.
#'
#' reads: QCRad_LongShi_v8_id_*
#'
#' exports:
#'
#' - `Broad_Band/QCRad_LongShi/QCRad_LongShi_v8_apply_CM21_CHP1_YYYY.Rds`
#' - Plots at runtime or render
#'
#' TODO:
#'
#' - plot combination of flag for each point
#' - plot cumulative graphs like the old
#' - Plot daily graphs with all available flags
#'
#+ echo=F, include=T

####_  Document options _####
knitr::opts_chunk$set(echo       =  FALSE   )
knitr::opts_chunk$set(comment    = ""       )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.height = "30%"    )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(cache      =  FALSE   )
# knitr::opts_chunk$set(fig.pos    = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "./QCRad_LongShi_v8_apply_CM21_CHP1.R"
lockfile    <- paste0("~/RAD_QC/RUNTIME/",
                      basename(sub("\\.R$",".stopfile", Script.Name)))
if (!interactive()) {
    pdf( file = paste0("~/RAD_QC/RUNTIME/", basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("~/RAD_QC/RUNTIME/", basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
}

library(scales)
library(data.table)
library(pander)
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/RAD_QC/Functions_write_data.R")
source("~/CODE/FUNCTIONS/R/execlock.R")

# mylock(lockfile)


####_  Variables init  _####
DATA_BASE <- "~/DATA/Broad_Band/QCRad_LongShi/"
IN_PREFIX <- "LAP_QCRad_LongShi_v8_id_CM21_CHP1_"
cachedata <- "~/RAD_QC/temp_data.Rds"

#### ~ ~ Execution control ~ ~ ####


## use cache data for development only
FORCE_NEW_DATA <- TRUE
FORCE_NEW_DATA <- FALSE

TEST_01  <- FALSE
TEST_02  <- FALSE
TEST_03  <- FALSE
TEST_04  <- FALSE
TEST_05  <- FALSE
TEST_06  <- FALSE
TEST_07  <- FALSE
TEST_08  <- FALSE
TEST_09  <- FALSE

TEST_01  <- TRUE
TEST_02  <- TRUE
TEST_03  <- TRUE
TEST_04  <- TRUE
TEST_05  <- TRUE
TEST_06  <- TRUE
# TEST_07  <- TRUE
TEST_08  <- TRUE
TEST_09  <- TRUE

## mostly for daily plots
# DO_PLOTS     <- FALSE
DO_PLOTS     <- TRUE   ## <- too many plots to render
if (interactive()) {
    DO_PLOTS <- FALSE
}

ALL_YEAR <- TRUE
ALL_YEAR <- FALSE

#+ echo=F, include=T
#### ~ ~ Load all data ~ ~ ####
fileslist <- list.files(path    = DATA_BASE,
                        pattern = paste0(IN_PREFIX, ".*.Rds"),
                        full.names = TRUE)
fileslist <- sort(fileslist)
## read data or load cached

if (FORCE_NEW_DATA || !file.exists(cachedata)) {
    DATA <- data.table()
    for (afl in fileslist) {
        tmp  <- readRDS(afl)
        DATA <- rbind(DATA, tmp, fill = TRUE)
        rm(tmp)
    }
    write_RDS(DATA, cachedata, clean = TRUE)
} else {
    DATA <- readRDS(cachedata)
    cat("\n\n**USING CACHED DATA!!**\n\n")
}


if (!ALL_YEAR) {
    cat("Partial data run")
    DATA <- DATA[year(Date) >= 2024, ]
}



## Obstacles definitions
load("~/RAD_QC/Obstacles.Rda")


#'
#' ### INPUT DATA
#'
#' **Data range: `r paste(range(DATA$Date), collapse = " to ")`**
#'
#+ echo=F, include=T
pp <- data.frame(basename(fileslist), file.mtime(fileslist))
names(pp) <- c("Filename", "File mtime")
pander(pp, caption = "Input files")



## . . Limits definitions  ####
#'
#' ### Some filters definitions
#'
#+ echo=T, include=T
QS <- list()
QS$sun_elev_min     <- -2 * 0.103 # 0. Drop all data when sun is below this point
QS$sun_elev_no_neg  <-  0         # 0. Don't allow negative values below this sun angle





####  1. PHYSICALLY POSSIBLE LIMITS PER BSRN  ----------------------------------
#' \FloatBarrier
#' \newpage
#' ## 1. PHYSICALLY POSSIBLE LIMITS PER BSRN
#'
#' Test values are within physical/logical limits.
#'
#' Direct upper constrain is a closeness to TSI at TOA. Shouldn't be any hits.
#' or need to remove data.
#'
#' Global upper constrain is an modeled GHI value.
#'
#' These limit should not be met, they are defined neat the maximum observed
#' values of the data set.
#'
#+ echo=TEST_01, include=T
if (TEST_01) {
    cat(paste("\n1. Physically Possible Limits.\n\n"))

    QS$dir_SWdn_min <-  -4  # Minimum direct value to consider valid measurement
    QS$dir_SWdn_dif <- 327  # Closeness to to TSI
    QS$glo_SWdn_min <-  -4  # Minimum global value to consider valid measurement
    QS$glo_SWdn_off <- 160  # Global departure offset above the model
    QS$glo_SWdn_amp <- 1.3  # Global departure factor above the model

    ## . . Direct --------------------------------------------------------------
    DATA[wattDIR < QS$dir_SWdn_min,
         QCF_DIR_01 := "Physical possible limit min (5)"]
    DATA[TSIextEARTH_comb - wattDIR < QS$dir_SWdn_dif,
         QCF_DIR_01 := "Physical possible limit max (6)"]

    ## . . Global --------------------------------------------------------------
    DATA[wattGLB < QS$glo_SWdn_min,
         QCF_GLB_01 := "Physical possible limit min (5)"]
    DATA[, Glo_max_ref := TSIextEARTH_comb * QS$glo_SWdn_amp * cosde(SZA)^1.2 + QS$glo_SWdn_off]
    DATA[wattGLB > Glo_max_ref,
         QCF_GLB_01 := "Physical possible limit max (6)"]
}

#+ echo=F, include=T, results="asis"
if (TEST_01) {

    cat(pander(table(DATA$QCF_DIR_01, exclude = NULL)))
    cat("\n\n")
    cat(pander(table(DATA$QCF_GLB_01, exclude = NULL)))
    cat("\n\n")

    range(DATA[, TSIextEARTH_comb - wattDIR ], na.rm = T)

    hist(DATA[,  TSIextEARTH_comb - wattDIR ], breaks = 100)

    range(DATA[, Glo_max_ref - wattGLB ], na.rm = T)

    hist(DATA[,  Glo_max_ref - wattGLB ], breaks = 100)

    if (DO_PLOTS) {

        test <- DATA[!is.na(QCF_DIR_01)]
        for (ad in sort(unique(as.Date(test$Date)))) {
            pp <- DATA[ as.Date(Date) == ad, ]
            ylim <- range(pp$TSIextEARTH_comb - QS$dir_SWdn_dif, pp$wattDIR, na.rm = T)
            plot(pp$Date, pp$wattDIR, "l", col = "blue",
                 ylim = ylim, xlab = "", ylab = "wattDIR")
            # lines(pp$Date, pp[, 1.2 * TSIextEARTH_comb * 0.678 * cosde(SZA) ])
            # lines(pp$Date, pp[, 0.8 * TSIextEARTH_comb * cosde(SZA)  ])
            # lines(pp$Date, pp[, 1.2 * TSIextEARTH_comb ^ (0.678 * cosde(SZA)) ])
            title(paste("1_", as.Date(ad, origin = "1970-01-01")))
            ## plot limits
            lines(pp$Date, pp$TSIextEARTH_comb - QS$dir_SWdn_dif, col = "red")
            ## mark offending data
            # points(pp[!is.na(QCF_DIR_01), Date],
            #        pp[!is.na(QCF_DIR_01), wattDIR],
            #        col = "red", pch = 1)
        }

        test <- DATA[ !is.na(QCF_GLB_01) ]
        for (ad in sort(unique(as.Date(c(test$Date))))) {
            pp <- DATA[ as.Date(Date) == ad, ]
            ylim <- range(pp$Glo_max_ref, pp$wattGLB, na.rm = T)
            plot(pp$Date, pp$wattGLB, "l", col = "green",
                 ylim = ylim, xlab = "", ylab = "wattGLB")
            title(paste("1_", as.Date(ad, origin = "1970-01-01")))
            ## plot limits
            lines(pp$Date, pp$Glo_max_ref, col = "red")
            ## mark offending data
            # points(pp[!is.na(QCF_DIR_01), Date],
            #        pp[!is.na(QCF_DIR_01), wattDIR],
            #        col = "red", pch = 1)
        }
    }
    # DATA$Glo_max_ref <- NULL
}
#' -----------------------------------------------------------------------------




####  2. EXTREMELY RARE LIMITS PER BSRN  ---------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 2. EXTREMELY RARE LIMITS PER BSRN
#'
#' These should be a little more restrictive than 1. in order to start
#' catching erroneous values.
#'
#' The choose of those settings may be optimized with an iterative process.
#'
#+ echo=TEST_02, include=T
if (TEST_02) {
    cat(paste("\n2. Extremely Rare Limits.\n\n"))
    # Upper modeled values
    QS$Dir_SWdn_amp     <-    0.91 # Direct departure factor above the model
    QS$Dir_SWdn_off     <- -140    # Direct departure offset above the model
    QS$Glo_SWdn_amp     <- 1.18    # Global departure factor above the model
    QS$Glo_SWdn_off     <- 40      # Global departure offset above the model
    # Minimum accepted values
    QS$dir_SWdn_min_ext <-   -2    # Extremely Rare Minimum Limits
    QS$glo_SWdn_min_ext <-   -2    # Extremely Rare Minimum Limits
    # Compute reference values
    DATA[, Direct_max := TSIextEARTH_comb * QS$Dir_SWdn_amp * cosde(SZA)^0.2 + QS$Dir_SWdn_off]
    DATA[, Global_max := TSIextEARTH_comb * QS$Glo_SWdn_amp * cosde(SZA)^1.2 + QS$Glo_SWdn_off]
    # Ignore too low values near horizon
    DATA[Direct_max < 3, Direct_max := NA]
    DATA[Global_max < 3, Direct_max := NA]

    ## . . Direct --------------------------------------------------------------
    DATA[wattDIR < QS$dir_SWdn_min_ext, QCF_DIR_02 := "Extremely rare limits min (3)"]
    DATA[wattDIR > Direct_max,          QCF_DIR_02 := "Extremely rare limits max (4)"]

    ## . . Global --------------------------------------------------------------
    DATA[wattGLB < QS$glo_SWdn_min_ext, QCF_GLB_02 := "Extremely rare limits min (3)"]
    DATA[wattGLB > Global_max,          QCF_GLB_02 := "Extremely rare limits max (4)"]
}

#+ echo=F, include=T, results="asis"
if (TEST_02) {

    cat(pander(table(DATA$QCF_DIR_02, exclude = TRUE)))
    cat("\n\n")
    cat(pander(table(DATA$QCF_GLB_02, exclude = TRUE)))
    cat("\n\n")

    range(DATA[, Direct_max - wattDIR])
    hist(DATA[, Direct_max - wattDIR], breaks = 100)

    range(DATA[, Global_max - wattGLB])
    hist(DATA[, Global_max - wattGLB], breaks = 100)

    if (DO_PLOTS) {

        test <- DATA[ !is.na(QCF_DIR_02) ]
        # test <- DATA[ wattDIR > Direct_max -50 ]
        for (ad in sort(unique(as.Date(test$Date)))) {
            pp <- DATA[ as.Date(Date) == ad, ]
            ylim <- range(pp$Direct_max, pp$wattDIR, na.rm = T)
            plot(pp$Date, pp$wattDIR, "l", col = "blue",
                 ylim = ylim, xlab = "", ylab = "wattDIR")
            title(paste("2_", as.Date(ad, origin = "1970-01-01")))
            ## plot limits
            lines(pp$Date, pp$Direct_max, col = "red")
            ## mark offending data
            points(pp[!is.na(QCF_DIR_02), Date],
                   pp[!is.na(QCF_DIR_02), wattDIR],
                   col = "red", pch = 1)
        }

        test <- DATA[ !is.na(QCF_GLB_02) ]
        # test <- DATA[ wattGLB > Global_max]
        for (ad in sort(unique(as.Date(c(test$Date))))) {
            pp <- DATA[ as.Date(Date) == ad, ]
            ylim <- range(pp$Global_max, pp$wattGLB, na.rm = T)
            plot(pp$Date, pp$wattGLB, "l", col = "green",
                 ylim = ylim, xlab = "", ylab = "wattGLB")
            title(paste("2_", as.Date(ad, origin = "1970-01-01")))
            ## plot limits
            lines(pp$Date, pp$Global_max, col = "red")
            ## mark offending data
            points(pp[!is.na(QCF_GLB_02), Date],
                   pp[!is.na(QCF_GLB_02), wattGLB],
                   col = "magenta", pch = 1)
        }
    }
    # DATA$Direct_max <- NULL
    # DATA$Global_max <- NULL
}
#' -----------------------------------------------------------------------------




####  3. COMPARISON TESTS PER BSRN “non-definitive”  ---------------------------
#' \FloatBarrier
#' \newpage
#' ## 3. COMPARISON TESTS PER BSRN “non-definitive”
#'
#'
#+ echo=TEST_03, include=T
if (TEST_03) {
    cat(paste("\n3. Comparison tests.\n\n"))

    QS$dif_rati_po1  <-  0.03
    QS$dif_rati_po2  <-  0.08
    QS$dif_sza_break <- 75
    QS$dif_rati_pr1  <-  1.03
    QS$dif_rati_pr2  <-  1.06
    QS$dif_watt_lim  <-  10

    ## . . Proposed filter -----------------------------------------------------
    DATA[DiffuseFraction_Kd  > QS$dif_rati_pr1  &
         SZA                <= QS$dif_sza_break &
         wattGLB             > QS$dif_watt_lim,
         QCF_BTH_03_1 := "Diffuse ratio comp max (11)"]
    DATA[DiffuseFraction_Kd  > QS$dif_rati_pr2  &
         SZA                 > QS$dif_sza_break &
         wattGLB             > QS$dif_watt_lim,
         QCF_BTH_03_1 := "Diffuse ratio comp max (11)"]

    ## . . Extra filters by me -------------------------------------------------
    DATA[DiffuseFraction_Kd  < QS$dif_rati_po1  &
         SZA                <= QS$dif_sza_break &
         wattGLB             > QS$dif_watt_lim,
         QCF_BTH_03_2 := "Diffuse ratio comp min (12)"]
    DATA[DiffuseFraction_Kd < QS$dif_rati_po1  &
         SZA                > QS$dif_sza_break &
         wattGLB            > QS$dif_watt_lim,
         QCF_BTH_03_2 := "Diffuse ratio comp min (12)"]
}

#+ echo=F, include=T, results="asis"
if (TEST_03) {

    cat(pander(table(DATA$QCF_BTH_03_1, exclude = TRUE)))
    cat("\n\n")
    cat(pander(table(DATA$QCF_BTH_03_2, exclude = TRUE)))
    cat("\n\n")

    years <- DATA[ !is.na(DiffuseFraction_Kd), unique(year(Date)) ]
    for (ay in years) {
        pp <- DATA[year(Date) == ay]
        ylim <- c(-0.5, 1.5)

        par(mar = c(4,4,2,1))
        plot( pp$SZA, pp$DiffuseFraction_Kd,
              ylab = "Diffuse fraction", xlab = "SZA", ylim = ylim,
              cex = .1)
        title(paste("3_", ay))

        segments(               0, QS$dif_rati_pr1, QS$dif_sza_break, QS$dif_rati_pr1, col = "red" )
        segments(QS$dif_sza_break, QS$dif_rati_pr2,               93, QS$dif_rati_pr2, col = "red" )

        segments(               0, QS$dif_rati_po1, QS$dif_sza_break, QS$dif_rati_po1, col = "blue" )
        segments(QS$dif_sza_break, QS$dif_rati_po2,               93, QS$dif_rati_po2, col = "blue" )

        points( pp[!is.na(QCF_BTH_03_1), SZA], pp[!is.na(QCF_BTH_03_1), DiffuseFraction_Kd],
                cex = .2, col = "red")
        points( pp[!is.na(QCF_BTH_03_2), SZA], pp[!is.na(QCF_BTH_03_2), DiffuseFraction_Kd],
                cex = .2, col = "cyan")


        par(mar = c(4,4,2,1))
        plot( pp$Azimuth, pp$DiffuseFraction_Kd,
              ylim = ylim,
              ylab = "Diffuse fraction", xlab = "Azimuth",
              cex = .1)
        title(paste("3_", ay))

        points( pp[!is.na(QCF_BTH_03_1), Azimuth], pp[!is.na(QCF_BTH_03_1), DiffuseFraction_Kd],
                cex = .2, col = "red")
        points( pp[!is.na(QCF_BTH_03_2), Azimuth], pp[!is.na(QCF_BTH_03_2), DiffuseFraction_Kd],
                cex = .2, col = "cyan")
    }

    if (DO_PLOTS) {
        tmp <- DATA[ !is.na(QCF_BTH_03_1) | !is.na(QCF_BTH_03_2) ]
        for (ad in sort(unique(c(as.Date(tmp$Date))))) {
            pp   <- DATA[ as.Date(Date) == ad, ]
            layout(matrix(c(1,2), 2, 1, byrow = TRUE))
            par(mar = c(2,4,2,1))

            plot( pp$Date, pp$DiffuseFraction_Kd, "l",
                  col = "cyan", ylab = "Diffuse Fraction", xlab = "")

            abline(h = QS$dif_rati_pr1, col = "red")
            abline(h = QS$dif_rati_pr2, col = "red", lty = 2)
            abline(h = QS$dif_rati_po1, col = "blue")
            abline(h = QS$dif_rati_po2, col = "blue", lty = 2)

            title(paste("3_1_2", as.Date(ad, origin = "1970-01-01")))

            par(mar = c(2,4,1,1))
            ylim <- range(pp$wattGLB, pp$wattDIR, na.rm = T)
            plot( pp$Date, pp$wattGLB, "l",
                  ylim = ylim, col = "green", ylab = "", xlab = "")
            lines(pp$Date, pp$wattDIR, col = "blue" )

            points(pp[!is.na(QCF_BTH_03_1), Date],
                   pp[!is.na(QCF_BTH_03_1), wattDIR],
                   ylim = ylim, col = "red")
            points(pp[!is.na(QCF_BTH_03_1), Date],
                   pp[!is.na(QCF_BTH_03_1), wattGLB],
                   ylim = ylim, col = "red")
            points(pp[!is.na(QCF_BTH_03_2), Date],
                   pp[!is.na(QCF_BTH_03_2), wattDIR],
                   ylim = ylim, col = "magenta")
            points(pp[!is.na(QCF_BTH_03_2), Date],
                   pp[!is.na(QCF_BTH_03_2), wattGLB],
                   ylim = ylim, col = "magenta")
        }
    }
}
#' -----------------------------------------------------------------------------




####  4. Climatological (configurable) Limits  ---------------------------------
#' \FloatBarrier
#' \newpage
#' ## 4. Climatological (configurable) Limits
#'
#' Limits the maximum expected irradiance based on climatological
#' observations levels and the value of TSI.
#'
#' Some hits on first limits are expected and need manual evaluation.
#'
#' Hits on second limit should be problematic data.
#'
#' For GHI this may limit the radiation enhancement cases.
#'
#' Exclusions should be done case by case.
#'
#+ echo=TEST_04, include=T
if (TEST_04) {
    cat("\n## 4. Climatological (configurable) Limits.\n\n")

    QS$clim_lim_C3 <- 0.77
    QS$clim_lim_D3 <- 0.81
    QS$clim_lim_C1 <- 1.14
    QS$clim_lim_D1 <- 1.32

    ## . . Direct --------------------------------------------------------------
    DATA[, Dir_First_Clim_lim := TSIextEARTH_comb * QS$clim_lim_C3 * cosde(SZA)^0.2 + 10]
    DATA[wattDIR > Dir_First_Clim_lim,
         QCF_DIR_04_1 := "First climatological limit (17)"]
    DATA[, Dir_Secon_Clim_lim := TSIextEARTH_comb * QS$clim_lim_D3 * cosde(SZA)^0.2 + 15]
    DATA[wattDIR > Dir_Secon_Clim_lim,
         QCF_DIR_04_2 := "Second climatological limit (16)"]

    ## . . Global --------------------------------------------------------------
    DATA[, Glo_First_Clim_lim := TSIextEARTH_comb * QS$clim_lim_C1 * cosde(SZA)^1.2 + 60]
    DATA[wattGLB > Glo_First_Clim_lim,
         QCF_GLB_04_1 := "First climatological limit (17)"]
    DATA[, Glo_Secon_Clim_lim := TSIextEARTH_comb * QS$clim_lim_D1 * cosde(SZA)^1.2 + 60]
    DATA[wattGLB > Glo_Secon_Clim_lim,
         QCF_GLB_04_2 := "Second climatological limit (16)"]
}

#+ echo=F, include=T, results="asis"
if (TEST_04) {

    cat(pander(table(DATA$QCF_GLB_04_1, exclude = NULL)))
    cat("\n\n")
    cat(pander(table(DATA$QCF_GLB_04_2, exclude = NULL)))
    cat("\n\n")
    cat(pander(table(DATA$QCF_DIR_04_1, exclude = NULL)))
    cat("\n\n")
    cat(pander(table(DATA$QCF_DIR_04_2, exclude = NULL)))
    cat("\n\n")

    hist(DATA[, wattDIR - Dir_First_Clim_lim], breaks = 100,
         main = "Departure Direct from first climatological limti")

    hist(DATA[, wattDIR - Dir_Secon_Clim_lim], breaks = 100,
         main = "Departure Direct from second climatological limit")

    hist(DATA[, wattGLB - Glo_First_Clim_lim], breaks = 100,
         main = "Departure Direct from first climatological limti")

    hist(DATA[, wattGLB - Glo_Secon_Clim_lim], breaks = 100,
         main = "Departure Direct from second climatological limit")

    if (DO_PLOTS) {

        ## test direct first limit
        temp1 <- DATA[ !is.na(QCF_DIR_04_1) ]
        for (ad in sort(unique(as.Date(temp1$Date)))) {
            pp <- DATA[ as.Date(Date) == ad, ]
            if (any(!is.na(pp$wattDIR))) {
                ylim <- range(pp$Dir_First_Clim_lim, pp$wattDIR, na.rm = T)
                plot(pp$Date, pp$wattDIR, "l", col = "blue",
                     ylim = ylim, xlab = "", ylab = "wattDIR")
                title(paste("4_1", as.Date(ad, origin = "1970-01-01")))
                ## plot limits
                lines(pp$Date, pp$Dir_First_Clim_lim, col = "pink")
                lines(pp$Date, pp$Dir_Secon_Clim_lim, col = "red" )
                ## mark offending data
                points(pp[wattDIR > Dir_First_Clim_lim, Date],
                       pp[wattDIR > Dir_First_Clim_lim, wattDIR],
                       col = "red", pch = 1)
            }
        }

        ## test direct second limit
        temp2 <- DATA[ !is.na(QCF_DIR_04_2) ]
        # extra <- sample(unique(as.Date(DATA[!is.na(wattDIR), Date])),5)
        for (ad in sort(unique(c(as.Date(temp2$Date))))) {
            pp <- DATA[ as.Date(Date) == ad, ]
            if (any(!is.na(pp$wattDIR))) {
                ylim <- range(pp$Dir_Secon_Clim_lim, pp$wattDIR, na.rm = T)
                plot(pp$Date, pp$wattDIR, "l", col = "blue",
                     ylim = ylim, xlab = "", ylab = "wattDIR")
                title(paste("4_2", as.Date(ad, origin = "1970-01-01")))
                ## plot limits
                lines(pp$Date, pp$Dir_First_Clim_lim, col = "pink")
                lines(pp$Date, pp$Dir_Secon_Clim_lim, col = "red" )
                ## mark offending data
                points(pp[wattDIR > Dir_First_Clim_lim, Date],
                       pp[wattDIR > Dir_First_Clim_lim, wattDIR],
                       col = "red", pch = 1)
            }
        }

        ## test global first limit
        temp1 <- DATA[ !is.na(QCF_GLB_04_1) ]
        for (ad in sort(unique(as.Date(temp1$Date)))) {
            pp <- DATA[ as.Date(Date) == ad, ]
            if (any(!is.na(pp$wattGLB))) {
                ylim <- range(pp$Glo_First_Clim_lim, pp$wattGLB, na.rm = T)
                plot(pp$Date, pp$wattGLB, "l", col = "green",
                     ylim = ylim, xlab = "", ylab = "wattGLB")
                title(paste("4_1", as.Date(ad, origin = "1970-01-01")))
                ## plot limits
                lines(pp$Date, pp$Glo_First_Clim_lim, col = "pink")
                lines(pp$Date, pp$Glo_Secon_Clim_lim, col = "red" )
                ## mark offending data
                points(pp[wattGLB > Glo_First_Clim_lim, Date],
                       pp[wattGLB > Glo_First_Clim_lim, wattGLB],
                       col = "magenta", pch = 1)
            }
        }

        ## test global second limit
        temp2 <- DATA[ !is.na(QCF_GLB_04_2) ]
        for (ad in sort(unique(c(as.Date(temp2$Date))))) {
            pp <- DATA[ as.Date(Date) == ad, ]
            if (any(!is.na(pp$wattGLB))) {
                ylim <- range(pp$Glo_Secon_Clim_lim, pp$wattGLB, na.rm = T)
                plot(pp$Date, pp$wattGLB, "l", col = "green",
                     ylim = ylim, xlab = "", ylab = "wattGLB")
                title(paste("4_2", as.Date(ad, origin = "1970-01-01")))
                ## plot limits
                lines(pp$Date, pp$Glo_First_Clim_lim, col = "pink")
                lines(pp$Date, pp$Glo_Secon_Clim_lim, col = "red" )
                ## mark offending data
                points(pp[wattGLB > Glo_Secon_Clim_lim, Date],
                       pp[wattGLB > Glo_Secon_Clim_lim, wattGLB],
                       col = "magenta", pch = 1)
            }
        }

    }
    # DATA$Dir_First_Clim_lim <- NULL
    # DATA$Glo_First_Clim_lim <- NULL
    # DATA$Dir_Secon_Clim_lim <- NULL
    # DATA$Glo_Secon_Clim_lim <- NULL
}
#' -----------------------------------------------------------------------------




####  5. Tracker is off test  --------------------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 5. Tracker is off test
#'
# This test use a diffuse model will be implemented when one is produced
# and accepted. For now we omit it to protect from over-fitting prior to
# make one such model.
#
#+ echo=TEST_05, include=T
if (TEST_05) {
    cat(paste("\n5. Tracking test.\n\n"))
    ## criteria
    QS$Tracking_min_elev <-   15
    QS$ClrSW_lim         <-    0.85
    QS$glo_min           <-   25
    ## Global Clear SW model
    QS$ClrSW_a           <- 1050.5
    QS$ClrSW_b           <-    1.095
    ## Clear Sky Sort-Wave model
    DATA[, ClrSW_ref2 := ( QS$ClrSW_a / sun_dist ^ 2 ) * cosde(SZA) ^ QS$ClrSW_b ]
    # DATA[, ClrSW_ref1 := TSIextEARTH_comb * cosde(SZA) ^ QS$ClrSW_b ]

    ## . . Direct --------------------------------------------------------------
    DATA[wattGLB / ClrSW_ref2 > QS$ClrSW_lim &
         DIF_HOR / wattGLB    > QS$ClrSW_lim &
         wattGLB              > QS$glo_min   &
         Elevat               > QS$Tracking_min_elev,
         QCF_DIR_05 := "Possible no tracking (24)"]
}

#+ echo=F, include=T, results="asis"
if (TEST_05) {

    cat(pander(table(DATA$QCF_DIR_05, exclude = T)))
    cat("\n\n")

    hist(DATA[, ClrSW_ref2 - wattDIR ], breaks = 100)
    hist(DATA[, wattGLB / ClrSW_ref2 ], breaks = 100)
    hist(DATA[, DIF_HOR / wattGLB    ], breaks = 100)

    if (DO_PLOTS) {
        tmp <- DATA[ !is.na(QCF_DIR_05), unique(as.Date(Date)) ]
        # tmp <- sample(DATA[!is.na(wattDIR), unique(as.Date(Date))], 10)
        for (ad in sort(tmp)) {
            pp <- DATA[ as.Date(Date) == ad, ]
            ylim <- range(pp$ClrSW_ref2, pp$wattDIR, pp$wattGLB, na.rm = T)
            plot(pp$Date, pp$wattDIR, "l", col = "blue",
                 ylim = ylim, xlab = "", ylab = "wattDIR")
            lines(pp$Date, pp$wattGLB, col = "green")
            title(paste("5_", as.Date(ad, origin = "1970-01-01")))
            ## plot limits
            # lines(pp$Date, pp$ClrSW_ref1, col = "pink")
            lines(pp$Date, pp$ClrSW_ref2, col = "cyan")
            ## mark offending data
            points(pp[!is.na(QCF_DIR_05), Date],
                   pp[!is.na(QCF_DIR_05), wattDIR],
                   col = "red", pch = 1)
        }
    }
    # DATA$ClrSW_ref2 <- NULL
}
#' -----------------------------------------------------------------------------




####  6. Rayleigh Limit Diffuse Comparison  ------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 6. Rayleigh Limit Diffuse Comparison
#'
#' Compare inferred diffuse radiation with a modeled value of diffuse,
#' based on SZA and atmospheric pressure.
#'
#' The upper limit denotes no tracking of CHP-1.
#'
#' Reasons:
#' - Difference of Sun observation angle due to different instruments location.
#' - Cases of instrument windows cleaning
#'
#+ echo=TEST_06, include=T
if (TEST_06) {
    cat(paste("\n6. Rayleigh Limit Diffuse Comparison.\n\n"))
    # criteria
    QS$Rayleigh_upper_lim <- 500   # Upper departure diffuse limit
    QS$Rayleigh_lower_lim <-  -3   # Lower departure diffuse limit
    QS$Rayleigh_dif_glo_r <-   0.8 # Low limit diffuse/global < threshold
    QS$Rayleigh_glo_min   <-  50   # Low limit minimum global
    # model
    Rayleigh_diff <- function(SZA, Pressure) {
        a    <-   209.3
        b    <-  -708.3
        c    <-  1128.7
        d    <-  -911.2
        e    <-   287.85
        f    <-     0.046725
        mu_0 <- cosde(SZA)
        return( a * mu_0     +
                b * mu_0 ^ 2 +
                c * mu_0 ^ 3 +
                d * mu_0 ^ 4 +
                e * mu_0 ^ 5 +
                f * mu_0 * Pressure )
    }
    DATA[, RaylDIFF  := Rayleigh_diff(SZA = SZA, Pressure = pressure) ]

    ## . . Both ----------------------------------------------------------------
    DATA[DIF_HOR - RaylDIFF > QS$Rayleigh_upper_lim,
         QCF_BTH_06_1 := "Rayleigh diffuse limit (18)" ]
    DATA[DIF_HOR - RaylDIFF < QS$Rayleigh_lower_lim,
         QCF_BTH_06_2 := "Rayleigh diffuse limit (18)" ]

}

#+ echo=F, include=T, results="asis"
if (TEST_06) {

    cat(pander(table(DATA$QCF_BTH_06_1, exclude = TRUE)))
    cat("\n\n")
    cat(pander(table(DATA$QCF_BTH_06_2, exclude = TRUE)))
    cat("\n\n")

    hist( DATA[, DIF_HOR - RaylDIFF ], breaks = 100 )

    if ( any(!is.na(DATA$QCF_BTH_06_1)) ) {
        hist( DATA[ !is.na(QCF_BTH_06_1), DIF_HOR - RaylDIFF ], breaks = 100)
    }

    if ( any(!is.na(DATA$QCF_BTH_06_2)) ) {
        hist( DATA[ !is.na(QCF_BTH_06_2), DIF_HOR - RaylDIFF ], breaks = 100)
    }

    if (DO_PLOTS) {
        ## plot on upper limit

        tmp <- DATA[ !is.na(QCF_BTH_06_1) ]

        for (ad in sort(unique(c(as.Date(tmp$Date))))) {

            pp   <- DATA[ as.Date(Date) == ad, ]

            layout(matrix(c(1,2), 2, 1, byrow = TRUE))
            par(mar = c(2,4,2,1))

            ylim <- range(pp$DIF_HOR, pp$RaylDIFF, na.rm = T)
            plot( pp$Date, pp$DIF_HOR, "l",
                  ylim = ylim, col = "cyan", ylab = "Diffuse", xlab = "")
            lines(pp$Date, pp$RaylDIFF, col = "magenta" )
            lines(pp$Date, pp$RaylDIFF + QS$Rayleigh_upper_lim, col = "red" )

            title(paste("6_1", as.Date(ad, origin = "1970-01-01")))

            par(mar = c(2,4,1,1))
            ylim <- range(pp$wattGLB, pp$wattDIR, na.rm = T)
            plot( pp$Date, pp$wattGLB, "l",
                  ylim = ylim, col = "green", ylab = "", xlab = "")
            lines(pp$Date, pp$wattDIR, col = "blue" )

            points(pp[!is.na(QCF_BTH_06_1), Date],
                   pp[!is.na(QCF_BTH_06_1), wattDIR],
                   ylim = ylim, col = "red")
            points(pp[!is.na(QCF_BTH_06_1), Date],
                   pp[!is.na(QCF_BTH_06_1), wattGLB],
                   ylim = ylim, col = "red")
        }

        ## plot on lower limit
        DATA[ !is.na(QCF_BTH_06_2) , .N]
        DATA[ !is.na(QCF_BTH_06_2) &
                  (DIF_HOR / wattGLB < QS$Rayleigh_dif_glo_r) , .N]
        DATA[ !is.na(QCF_BTH_06_2) &
                  (DIF_HOR / wattGLB < QS$Rayleigh_dif_glo_r) &
                  wattGLB > QS$Rayleigh_glo_min , .N]

        tmp <- DATA[!is.na(QCF_BTH_06_2) &
                        (DIF_HOR / wattGLB < QS$Rayleigh_dif_glo_r) &
                        wattGLB > QS$Rayleigh_glo_min ]

        for (ad in sort(unique(c(as.Date(tmp$Date))))) {

            pp   <- DATA[ as.Date(Date) == ad, ]

            layout(matrix(c(1,2), 2, 1, byrow = TRUE))
            par(mar = c(2,4,2,1))

            ylim <- range(pp$DIF_HOR, pp$RaylDIFF, na.rm = T)
            plot( pp$Date, pp$DIF_HOR, "l",
                  ylim = ylim, col = "cyan", ylab = "Diffuse", xlab = "")
            lines(pp$Date, pp$RaylDIFF, col = "magenta" )
            lines(pp$Date, pp$RaylDIFF + QS$Rayleigh_lower_lim, col = "red" )

            title(paste("6_2", as.Date(ad, origin = "1970-01-01")))

            par(mar = c(2,4,1,1))
            ylim <- range(pp$wattGLB, pp$wattDIR, na.rm = T)
            plot( pp$Date, pp$wattGLB, "l",
                  ylim = ylim, col = "green", ylab = "", xlab = "")
            lines(pp$Date, pp$wattDIR, col = "blue" )

            points(pp[!is.na(QCF_BTH_06_2), Date],
                   pp[!is.na(QCF_BTH_06_2), wattDIR],
                   ylim = ylim, col = "red")
            points(pp[!is.na(QCF_BTH_06_2), Date],
                   pp[!is.na(QCF_BTH_06_2), wattGLB],
                   ylim = ylim, col = "red")
        }
    }
}
#' -----------------------------------------------------------------------------




####  7. Test for obstacles  ---------------------------------------------------
#'
#' \newpage
#' ## 7. Test for obstacles
#'
#' This is deactivated
#'
#+ echo=TEST_07, include=T
if (TEST_07) {
    cat(paste("\n7. Obstacles test.\n\n"))

    ## . . Direct ----------------------------------------------------------####

    source("./QCRad_Obstacles_definition_v2.R")

    ## get biology building tag
    # biol     <- biolog_build(DATA$Azimuth, DATA$Elevat )
    # ## apply filter for biology building
    # ## this is not pretty we are using the indexes to mark data
    # ## have to parse all the original data although the filter is applicable
    # ## for a specific range of Azimuth angles
    # building <- which(biol$type == "bellow")
    # existing <- which(is.na(DATA_year$QCF_DIR))
    # exclude  <- building %in% existing
    #
    # DATA_year$QCF_DIR[    building[exclude] ] <- "Biology Building (22)"
    # DATA_year$QCF_DIR_07[ building[exclude] ] <- "Biology Building (22)"
    #
    # ## Pole abstraction is a possibility, should combine with Direct to decide
    # suspects <- DATA_year$Azimuth > Pole_az_lim[1] & DATA_year$Azimuth < Pole_az_lim[2]
    # DATA_year$QCF_DIR[    suspects ]          <- "Possible Direct Obstruction (23)"
    # DATA_year$QCF_DIR_07[ suspects ]          <- "Possible Direct Obstruction (23)"



}

#+ echo=F, include=T
if (TEST_07) {

}
#' -----------------------------------------------------------------------------




####  8. Test for inverted values  ---------------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 8. Test for inverted values
#'
#' Test the ratio of Diffuse / Global radiation.
#' When the Diffuse is too lower than Global, (less than a % limit).
#'
#' This denotes obstacles on the mornings mostly, or very low
#' signals when Sun is near the horizon.
#' Due to the time difference of sun shine, due to geometry, location and
#' obstacles.
#'
#' And possible cases of Instrument windows cleaning shadowing.
#'
#' Probably these value should be removed for CS when occurring on low
#' elevation angles, as the measurements can not be considered to reflect
#' the same condition of Sun visibility.
#'
#' Additional criteria is needed for any data drop.
#'
#+ echo=TEST_08, include=T
if (TEST_08) {
    cat(paste("\n8. Inversion test.\n\n"))

    QS$dir_glo_invert  <- 5  # Diffuse Inversion test: DIRhor - GLBhor > lim[%]
    QS$dir_glo_glo_off <- 5  # Diffuse Inversion test: apply for GLBhor > offset

    ## . . Both ----------------------------------------------------------------
    DATA[, Relative_diffuse := 100 * (wattHOR - wattGLB) / wattGLB ]
    DATA[ is.infinite(Relative_diffuse), Relative_diffuse := NA]

    DATA[Relative_diffuse > QS$dir_glo_invert  &
                  wattGLB > QS$dir_glo_glo_off ,
         QCF_BTH_08_1 := "Direct > global soft (14)" ]

    DATA[Relative_diffuse > QS$dir_glo_invert ,
         QCF_BTH_08_2 := "Direct > global hard (15)" ]
}

#+ echo=F
if (TEST_08) {

    cat(pander(table(DATA$QCF_BTH_08_1, exclude = TRUE)))
    cat("\n\n")
    cat(pander(table(DATA$QCF_BTH_08_2, exclude = TRUE)))
    cat("\n\n")

    hist(DATA[ !is.na(QCF_BTH_08_1), Relative_diffuse], breaks = 100)
    hist(DATA[ !is.na(QCF_BTH_08_2), Relative_diffuse], breaks = 100)

    hist(DATA[ Relative_diffuse > QS$dir_glo_invert & Elevat  > 3,                  Elevat])
    hist(DATA[ Relative_diffuse > QS$dir_glo_invert & Elevat  > 3,                  wattHOR - wattGLB])
    hist(DATA[ Relative_diffuse > QS$dir_glo_invert & wattGLB > QS$dir_glo_glo_off, Elevat])
    hist(DATA[ Relative_diffuse > QS$dir_glo_invert & wattGLB > QS$dir_glo_glo_off, wattHOR - wattGLB])

    if (DO_PLOTS) {

        ## plot softer limit
        test <- DATA[ !is.na(QCF_BTH_08_1) ]
        xlim <- range( DATA[ Elevat > 0, Azimuth ] )
        for (ad in unique(as.Date(test$Date))) {
            pp   <- DATA[ as.Date(Date) == ad, ]
            ylim <- range(pp$wattGLB, pp$wattHOR, na.rm = T)
            plot( pp$Azimuth, pp$wattHOR, "l",
                  xlim = xlim, ylim = ylim, col = "blue", ylab = "", xlab = "")
            lines(pp$Azimuth, pp$wattGLB, col = "green" )
            title(paste("8_1", as.Date(ad, origin = "1970-01-01")))
            points(pp[!is.na(QCF_BTH_08_1), Azimuth],
                   pp[!is.na(QCF_BTH_08_1), wattHOR],
                   ylim = ylim, col = "red")
            points(pp[!is.na(QCF_BTH_08_1), Azimuth],
                   pp[!is.na(QCF_BTH_08_1), wattGLB],
                   ylim = ylim, col = "magenta")
        }

        ## plot harder limit
        test <- DATA[ !is.na(QCF_BTH_08_2) ]
        xlim <- range( DATA[ Elevat > 0, Azimuth ] )
        for (ad in unique(as.Date(test$Date))) {
            pp   <- DATA[ as.Date(Date) == ad, ]
            ylim <- range(pp$wattGLB, pp$wattHOR, na.rm = T)
            plot( pp$Azimuth, pp$wattHOR, "l",
                  xlim = xlim, ylim = ylim, col = "blue", ylab = "", xlab = "")
            lines(pp$Azimuth, pp$wattGLB, col = "green" )
            title(paste("8_2", as.Date(ad, origin = "1970-01-01")))
            points(pp[!is.na(QCF_BTH_08_2), Azimuth],
                   pp[!is.na(QCF_BTH_08_2), wattHOR],
                   ylim = ylim, col = "red")
            points(pp[!is.na(QCF_BTH_08_2), Azimuth],
                   pp[!is.na(QCF_BTH_08_2), wattGLB],
                   ylim = ylim, col = "magenta")
        }

        # test <- DATA[ , Relative_diffuse < -200 ]
        # for (ad in unique(as.Date(DATA[test,Date]))) {
        #     pp   <- DATA[ as.Date(Date) == ad, ]
        #     tt   <- pp[, Relative_diffuse < -200 ]
        #     ylim <- range(pp$wattGLB, pp$wattHOR, na.rm = T)
        #     plot( pp$Date, pp$wattHOR, "l",
        #           ylim = ylim, col = "blue", ylab = "", xlab = "")
        #     lines(pp$Date, pp$wattGLB, col = "green" )
        #     title(as.Date(ad, origin = "1970-01-01"))
        #     points(pp[tt, Date],
        #            pp[tt, wattHOR],
        #            ylim = ylim, col = "blue")
        #     points(pp[tt, Date],
        #            pp[tt, wattGLB],
        #            ylim = ylim, col = "green")
        # }

    }
    DATA$Relative_diffuse <- NULL
}
#' -----------------------------------------------------------------------------






####  9. Clearness index test  ---------------------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 9. Clearness index test
#'
#' This filter is mine, and is applied on GHI data.
#'
#' Data near elevation 0 are caused by the cos(SZA) while calculating
#' kt = GLB / (cos(sza) * TSI).
#'
#' For larger elevation angles manual inspection is needed.
#'
#+ echo=TEST_09, include=T
if (TEST_09) {
    cat(paste("\n9. Clearness index (global/TSI) test.\n\n"))

    QS$CL_idx_max <-  1.13  # Upper Clearness index accepted level
    QS$CL_idx_min <- -0.001 # Lower Clearness index accepted level
    QS$CL_idx_ele <-  8     # Apply for elevations above this angle

    ## . . Global --------------------------------------------------------------
    DATA[Clearness_Kt > QS$CL_idx_max & Elevat > QS$CL_idx_ele,
         QCF_GLB_09 := "Clearness index limit max (19)" ]
    DATA[Clearness_Kt < QS$CL_idx_min & Elevat > QS$CL_idx_ele,
         QCF_GLB_09 := "Clearness index limit min (20)" ]


}

#+ echo=F, include=T, results="asis"
if (TEST_09) {

    cat(pander(table(DATA$QCF_GLB_09, exclude = TRUE)))
    cat("\n\n")

    range(DATA[Elevat > QS$CL_idx_ele, Clearness_Kt], na.rm = T)
    hist( DATA[Elevat > QS$CL_idx_ele, Clearness_Kt], breaks = 100 )

    if (any(!is.na(DATA$QCF_GLB_09))) {
        hist(DATA[!is.na(QCF_GLB_09), wattGLB],      breaks = 100)
        hist(DATA[!is.na(QCF_GLB_09), Elevat ],      breaks = 100)
        hist(DATA[!is.na(QCF_GLB_09), Clearness_Kt], breaks = 100)
    }


    if (DO_PLOTS) {

        tmp <- DATA[ !is.na(QCF_GLB_09) ]

        ## plot offending years
        for (ay in unique(year(tmp$Date))) {
            pp <- DATA[year(Date) == ay]

            ylim = c(-0.5, 2)
            plot(pp$Elevat, pp$Clearness_Kt,
                 main = ay, pch = 19, cex = 0.1,
                 ylim = ylim, xlab = "Elevation", ylab = "Clearness index Kt" )

            abline(v = QS$CL_idx_ele, col = "yellow")

            points(pp[Clearness_Kt > QS$CL_idx_max & Elevat > QS$CL_idx_ele, Elevat],
                   pp[Clearness_Kt > QS$CL_idx_max & Elevat > QS$CL_idx_ele, Clearness_Kt],
                   pch = 19, cex = 0.3, col = "red")
            abline(h = QS$CL_idx_max, col = "magenta", lwd = 0.5)

            points(pp[Clearness_Kt < QS$CL_idx_min & Elevat > QS$CL_idx_ele, Elevat],
                   pp[Clearness_Kt < QS$CL_idx_min & Elevat > QS$CL_idx_ele, Clearness_Kt],
                   pch = 19, cex = 0.3, col = "blue")
            abline(h = QS$CL_idx_min, col = "cyan", lwd = 0.5)
        }

        ## plot offending days
        for (ad in sort(unique(c(as.Date(tmp$Date))))) {
            pp   <- DATA[ as.Date(Date) == ad, ]
            ylim <- range(pp$wattDIR, pp$wattGLB, na.rm = T)
            plot(pp$Date, pp$wattGLB, "l", col = "green",
                 ylim = ylim, xlab = "", ylab = "wattGLB")
            lines(pp$Date, pp$wattDIR, col = "blue")
            title(paste("9_", as.Date(ad, origin = "1970-01-01")))
            ## mark offending data
            points(pp[!is.na(QCF_GLB_09), Date],
                   pp[!is.na(QCF_GLB_09), wattGLB],
                   col = "red", pch = 1)
            ## no applicable to direct!!
            # points(pp[!is.na(QCF_GLB_09), Date],
            #        pp[!is.na(QCF_GLB_09), wattDIR],
            #        col = "red", pch = 1)
        }
    }
}
#' -----------------------------------------------------------------------------














#### ~ ~  Export Data ~ ~ ####
#+ echo=F, include=T, results="asis"
for (YYYY in unique(year(DATA$Date))) {
    write_RDS(object = DATA[ year(Date) == YYYY, ],
              file   = paste0(DATA_BASE,
                              basename(sub("\\.R$","_", Script.Name)), YYYY))
}




#' **END**
#+ include=T, echo=F
# myunlock(lockfile)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
