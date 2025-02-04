#!/usr/bin/env Rscript
# /* Copyright (C) 2016 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:       "QCRad methodology application."
#' author:      "Natsis Athanasios"
#' institute:   "AUTH"
#' affiliation: "Laboratory of Atmospheric Physics"
#' abstract:    "Data quality for radiation measurements as described by
#'               CN Long and Y Shi, September 2006, DOE/SC-ARM/TR-074.
#'               Quality Control Procedures/The QCRad Value Added Product Surface
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
#'     latex_engine:     xelatex
#'     toc:              yes
#'     fig_width:        8
#'     fig_height:       5
#' date: "`r format(Sys.time(), '%F')`"
#' ---

#+ echo=F, include=T

####_  Document options _####

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""       )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
# knitr::opts_chunk$set(fig.pos    = '!h'    )

####_ Notes _####

##
## Run for each year and apply Quality control on radiation data.
## reads:   from level 1 data
##          pressure
##          TSI
## exports: characterized data with QCRad algorithm
##          strict QCRad dat
##          all bad data points
##

#+ include=F, echo=F
####  Set environment  ####
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("QCRad_appl_") })
if (!interactive()) {
    pdf(    file = paste0("~/Aerosols/REPORTS/runtime/", basename(sub("\\.R$",".pdf",  Script.Name))))
    filelock::lock(paste0("~/Aerosols/REPORTS/runtime/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}


#+ echo=F, include=T
####  External code  ####
#library(RAerosols,  warn.conflicts = F, quietly = T)
library(scales,     warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
source("~/CODE/FUNCTIONS/R/trig_deg.R")
source("~/RAD_QC/Functions_write_data.R")


### Variables init ###

## data files pattern
CHP1_BASE_IN   <- "/home/athan/DATA/Broad_Band/LAP_CHP1"
CM21_BASE_IN   <- "/home/athan/DATA/Broad_Band/CM21_H_global/"
PRES           <- "/home/athan/DATA/WEATHER/Pressure_M1.Rds"

OUTPUT_BASE    <- "/home/athan/DATA/Broad_Band/LAP_QCRad_CM21_CHP1_"
OUTPUT_STRICT  <- "/home/athan/DATA/Broad_Band/LAP_CM21_QCRad_STRICT_"
PLOTS_OUT      <- "/home/athan/Aerosols/DATA/Graphs/Level_2/QCRad_id/"
SUSPECTS_EXP   <- "/home/athan/DATA/Broad_Band/LAP_QCRad_SUSPECTS"

## other inputs
tsi_build_Rdat <- "/home/athan/DATA/SUN/TSI_COMPOSITE_legacy.Rds"
template_file  <- "/home/athan/DATA/Broad_Band/LAP_CHP1_L1_2016.Rds"

## date to start run
PROJECT_START  <- as.POSIXct("1993-01-01")  ## when both instruments were operational
# LAST_DAY_EXPR  <- as.POSIXct("2022-07-07")  ## day of last manual data clean
LAST_DAY_EXPR  <- Sys.time()                ## Ignore limit on last day


## . . Limits definitions  ####
QS <- list(
    sun_elev_min     =  -2 * 0.103, # 0. Drop all data when sun is below this point
    sun_elev_no_neg  =   0,         # 0. Don't allow negative values below this sun angle
    glo_SWdn_min     =  -4,         # 1. MIN Physically Possible Limits
    dir_SWdn_min     =  -4,         # 1. MIN Physically Possible Limits
    glo_SWdn_min_ext =  -2,         # 2. MIN Extremely Rare Minimum Limits
    dir_SWdn_min_ext =  -2,         # 2. MIN Extremely Rare Minimum Limits
    dif_rati_min     =   0.001,     # 3. (12) extra comparison to check data
    dif_rati_max     =   1.01,      # 3. (13) extra comparison to check data 1
    clim_lim_C3      =   0.81,      # 4. Direct Climatological (configurable) Limit first level
    clim_lim_D3      =   0.90,      # 4. Direct Climatological (configurable) Limit second level
    clim_lim_C1      =   1.15,      # 4. Global Climatological (configurable) Limit first level
    clim_lim_D1      =   1.35,      # 4. Global Climatological (configurable) Limit second level
    ClrSW_a          = 1050.5,      # 5. Tracker off test Clear Sky factor a
    ClrSW_b          =   1.095,     # 5. Tracker off test Clear Sky factor b
    ClrSW_lim        =   0.85,      # 5. Tracker off test Threshold
    CL_idx_max       =   1.3,       # 8. Clearness index test
    CL_idx_min       =   -0.001,    # 8. Clearness index test
    NULL
)
## Obstacles definitions
load("~/RAD_QC/Obstacles.Rda")

DO_TEST_01 <- TRUE   # Physically Possible Limits
DO_TEST_02 <- TRUE   # Extremely Rare Limits
DO_TEST_03 <- TRUE   # Comparison tests
DO_TEST_04 <- TRUE   # Climatological (configurable) Limits.
DO_TEST_05 <- TRUE   # Tracking check
DO_TEST_06 <- TRUE   # Rayleigh Limit Diffuse Comparison
DO_TEST_07 <- TRUE   # Obstacles removal
DO_TEST_08 <- TRUE   # Test for inverted values
DO_TEST_09 <- TRUE   # Test clearness index limits

TESTING_NP <- 10000
TESTING    <- FALSE
# TESTING = T

# DO_TEST_01 <- F   # Physically Possible Limits
# DO_TEST_02 <- F   # Extremely Rare Limits
# DO_TEST_03 <- F   # Comparison tests
# DO_TEST_04 <- F   # Climatological (configurable) Limits.
# DO_TEST_05 <- F   # Tracking check
# DO_TEST_06 <- F   # Rayleigh Limit Diffuse Comparison
DO_TEST_07 <- F   # Obstacles removal
# DO_TEST_08 <- F   # Test for inverted values
# DO_TEST_09 <- F   # Test clearness index limits


## plot options
palete_rand = c("#995732", "#BFE46C", "#CB45BE", "#675CA8",
                "#85BA0C", "#01855B", "#43F44D", "#AB4243",
                "#521DCC", "#6E3F65", "#6D3D19", "#E85571",
                "#18652C", "#867AF6", "#87DCA8")
# pie(rep(1, length(palete_rand)), col = palete_rand)


## Keep all bad data categories here! ##
categories = c(
    paste("Extremely rare limits min (3)"),
    paste("Extremely rare limits max (4)"),
    paste("Physical possible limit min (5)"),
    paste("Physical possible limit max (6)"),
    paste("Diffuse ratio comp max (11)"),
    paste("Diffuse ratio comp min (12)"),
    paste("Diffuse ratio comp max (13)"),  ##<-?
    paste("Direct > global soft (14)"),
    paste("Direct > global hard (15)"),
    paste("Second climatological limit (16)"),
    paste("First climatological limit (17)"),
    paste("Rayleigh diffuse limit (18)"),
    paste("Clearness index limit max (19)"),
    paste("Clearness index limit min (20)"),
    paste("Obstacle Biology build (21)"),
    paste("Biology Building (22)"),
    paste("Possible Direct Obstruction (23)"),
    paste("No tracking possible (24)")
)



## . Load TSI data  ------------------------------------------------------- ####
tsi_build     <- data.table(readRDS( file = tsi_build_Rdat ))
names(tsi_build)[names(tsi_build) == "Date"] <- "nominal_dates"
tsi_build     <- tsi_build[, c("nominal_dates", "TSIextEARTH_comb", "tsi_1au_comb", "sun_dist")]
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## . Load atmospheric pressure data --------------------------------------- ####
pressure      <- data.table(readRDS(PRES))
pressure$Date <- pressure$Date + 30
names(pressure)[names(pressure) == "Source"] <- "Pressure_Source"
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


## Will Loop for all years ##

daysinfo_gather <- data.table()
SUS_DATA_gather <- data.table()

## get year range
yearSTA <- as.numeric( format(PROJECT_START, format = "%Y") )
yearEND <- as.numeric( format(x = as.POSIXct(Sys.Date()), format = "%Y")     )

## override years
# yearSTA <- 2014
# yearEND <- 2014

# yearSTA <- 1993
# yearEND <- 1993


## graph options
par(mar = c(2,4,1.1,.5))
par(pch = 19)
xlim <- c(18, 92)


#+ echo=F, include=T, results="asis"
## loop years and read data
for (YY in yearSTA:yearEND) {

    cat("\n\n\\FloatBarrier\n\n")
    cat("\\newpage\n\n")
    cat("\n## Year:", YY, "\n\n")

    ## start graphical device to output not in the pdf report
    if (!interactive()) {
        png(paste0(PLOTS_OUT, "QCRad_id_v6_CM21_", YY, "_%04d.png"),
            pointsize = 18, width = 960, height = 720)
    }


    ####  Get Direct if available  #####
    year_file1 <- paste0(CHP1_BASE_IN, "_L1_", YY, ".Rds")
    cat( paste(year_file1), "\n\n")

    if ( !file.exists(year_file1) ) {
        warning("Missing input file ", year_file1)
        ## create dummy data
        CHP1_year <- data.table(readRDS(template_file))
        cols      <- grep("Date" ,names(CHP1_year), value = T, invert = T)

        Date      <- seq.POSIXt( as.POSIXct(paste0(YY,"-01-01 00:00")),
                                 as.POSIXct(paste0(YY,"-12-31 23:59")), by = "mins" )
        Date30    <- Date + 30
        temp      <- data.table(Date, Date30)
        temp[, (cols) := NA ]
        CHP1_year <- temp
        rm(temp)
    } else {
        CHP1_year <- data.table(readRDS(year_file1))
    }

    if (TESTING) { CHP1_year <- CHP1_year[ 1:TESTING_NP, ] }

    ## drop some columns
    CHP1_year <- subset( CHP1_year, select = c(-Async,
                                               -AsynStep,
                                               -CHP1value,
                                               -CHP1sd,
                                               -DumDarkCHP1,
                                               -Date,
                                               -rel_Time,
                                               -rel_Elev,
                                               -Times
    ))

    names(CHP1_year)[names(CHP1_year) == "Date30"] <- "Date"

    ## Drop night data
    CHP1_year <- CHP1_year[ Elevat > QS$sun_elev_min,    ]
    cat(sprintf(" %6d   %s\n", nrow(CHP1_year), "Records without night from CHP-1"),"\n")

    ## don't allow negative values when sun is too low
    sel <- CHP1_year[ Elevat < QS$sun_elev_no_neg & wattDIR < 0, .N ]
    CHP1_year[ Elevat < QS$sun_elev_no_neg & wattDIR < 0, wattDIR        := 0 ]
    CHP1_year[ Elevat < QS$sun_elev_no_neg & wattDIR < 0, wattDIR_tmp_cr := 0 ]
    CHP1_year[ Elevat < QS$sun_elev_no_neg & wattDIR < 0, wattHOR        := 0 ]
    CHP1_year[ Elevat < QS$sun_elev_no_neg & wattDIR < 0, wattHOR_tmp_cr := 0 ]
    cat(sprintf(" %6d   %s\n", sel, "Negative Records from CHP1 near sunset sunrise set to zero!"),"\n")



    ####  Get Global  #####
    year_file <- paste0(CM21_BASE_IN, "LAP_CM21_H_L1_", YY, ".Rds")
    cat( paste(year_file), "\n\n")
    if ( !file.exists(year_file) ) { stop("Missing input file ", year_file) }

    CM21_year <- data.table(readRDS(year_file))

    if (TESTING) { CM21_year <- CM21_year[ 1:TESTING_NP, ] }


    ## Drop night data
    CM21_year <- CM21_year[ Elevat > QS$sun_elev_min ]
    cat(sprintf( " %6d   %s\n", nrow(CM21_year), "Records without night from CM-21"),"\n")


    ## Don't allow negative values when sun is too low
    sel <- CM21_year[ Elevat < QS$sun_elev_no_neg & wattGLB < 0, .N ]
    CM21_year[        Elevat < QS$sun_elev_no_neg & wattGLB < 0, wattGLB := 0 ]
    cat(sprintf( " %6d   %s\n", sel, "Negative Records from CM21 near sunset sunrise set to zero!"),"\n")

    ## unify data
    DATA_year <- merge( CHP1_year, CM21_year, all  = TRUE  )
    DATA_year <- merge( DATA_year, tsi_build, by.x = "Date", by.y = "nominal_dates", all.x = T)
    DATA_year <- merge( DATA_year, pressure,  by.x = "Date", by.y = "Date", all.x = T)
    ## forget this year source data
    rm(CM21_year, CHP1_year)


    ## init QCRad flag quality flag for the measurements
    DATA_year[ , QCF_DIR := NA ]
    DATA_year[ , QCF_GLB := NA ]
    levels(DATA_year$QCF_DIR) <- categories
    levels(DATA_year$QCF_GLB) <- categories


    ## create diffuse irradiance
    DATA_year[ , wattDIF            := DATA_year$wattGLB - DATA_year$wattHOR ]

    ## create clearness index k_t
    DATA_year[ , Clearness_Kt       := wattGLB / ( cosde(SZA) * TSIextEARTH_comb ) ]

    ## Diffuse fraction k_d
    DATA_year[ , DiffuseFraction_Kd := wattDIF / wattGLB ]

    ## create a time of day representation
    DATA_year$Times <- as.POSIXct(strftime(DATA_year$Date, format = "%H:%M:%S"), format = "%H:%M:%S" )

    ## replace infinite values
    DATA_year[ is.infinite(DiffuseFraction_Kd) & DiffuseFraction_Kd > 0, DiffuseFraction_Kd := NA ]
    DATA_year[ is.infinite(DiffuseFraction_Kd) & DiffuseFraction_Kd < 0, DiffuseFraction_Kd := NA ]



    #### ~ ~ ~ ~ START OF FLAGGING ~ ~ ~  ~ ####################################

    if (DO_TEST_01) {
        ####  1. PHYSICALLY POSSIBLE LIMITS PER BSRN  ##########################
        cat(paste("\n1. Physically Possible Limits.\n\n"))

        ## . . Direct ------------------------------------------------------####
        DSWdn_min <- DATA_year$wattDIR  <  QS$dir_SWdn_min
        DSWdn_max <- DATA_year$wattDIR  >  DATA_year$TSIextEARTH_comb

        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & DSWdn_min ] <- "Physical possible limit min (5)"
        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & DSWdn_max ] <- "Physical possible limit max (6)"

        ## . . Global ------------------------------------------------------####
        GSWdn_min                 <- DATA_year$wattGLB  <  QS$glo_SWdn_min
        Global_max_physical_limit <- DATA_year$TSIextEARTH_comb * 1.5 * cosde(DATA_year$SZA)^1.2 + 100
        GSWdn_max                 <- DATA_year$wattGLB  >  Global_max_physical_limit

        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & GSWdn_min ] <- "Physical possible limit min (5)"
        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & GSWdn_max ] <- "Physical possible limit max (6)"

        ## . . Info --------------------------------------------------------####
        cat(sprintf( " %6d    %s\n\n",      sum(DSWdn_max, na.rm = T), "Direct Records above physical limit (6)"))
        cat(sprintf( " %6d    %s [%s]\n\n", sum(DSWdn_min, na.rm = T), "Direct Records below physical limit (5)", QS$dir_SWdn_min))
        cat(sprintf( " %6d    %s\n\n",      sum(GSWdn_max, na.rm = T), "Global Records above physical limit (6)"))
        cat(sprintf( " %6d    %s [%s]\n\n", sum(GSWdn_min, na.rm = T), "Global Records below physical limit (5)", QS$glo_SWdn_min))

        rm(DSWdn_max, DSWdn_min, GSWdn_max, GSWdn_min)
    } ##END if DO_TEST_01


    if (DO_TEST_02) {
        ####  2. EXTREMELY RARE LIMITS PER BSRN  ###############################
        cat(paste("\n2. Extremely Rare Limits.\n\n"))

        ## . . Direct ------------------------------------------------------####
        Direct_max_extremely_rare <- DATA_year$TSIextEARTH_comb * 0.95 * cosde(DATA_year$SZA)^0.2 + 10
        DSWdn_min_ext             <- DATA_year$wattDIR  <  QS$dir_SWdn_min_ext
        DSWdn_max_ext             <- DATA_year$wattDIR  >  Direct_max_extremely_rare

        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & DSWdn_min_ext ] <- "Extremely rare limits min (3)"
        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & DSWdn_max_ext ] <- "Extremely rare limits max (4)"

        ## . . Global ------------------------------------------------------####
        Global_max_extremely_rare <- DATA_year$TSIextEARTH_comb * 1.2 * cosde(DATA_year$SZA)^1.2 + 50
        GSWdn_min_ext             <- DATA_year$wattGLB  <  QS$glo_SWdn_min_ext
        GSWdn_max_ext             <- DATA_year$wattGLB  >  Global_max_extremely_rare

        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & GSWdn_min_ext ] <- "Extremely rare limits min (3)"
        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & GSWdn_max_ext ] <- "Extremely rare limits max (4)"

        ## . . Info --------------------------------------------------------####
        cat(sprintf(" %6d    %s\n\n",      sum(DSWdn_max_ext, na.rm = T), "Direct records above extremely rare limit max (4)"))
        cat(sprintf(" %6d    %s [%s]\n\n", sum(DSWdn_min_ext, na.rm = T), "Direct records below extremely rare limit min (3)", QS$dir_SWdn_min_ext))
        cat(sprintf(" %6d    %s\n\n",      sum(GSWdn_max_ext, na.rm = T), "Global records above extremely rare limit max (4)"))
        cat(sprintf(" %6d    %s [%s]\n\n", sum(GSWdn_min_ext, na.rm = T), "Global records below extremely rare limit min (3)", QS$glo_SWdn_min_ext))

        rm(DSWdn_max_ext, DSWdn_min_ext, GSWdn_max_ext, GSWdn_min_ext)
    } ##END if DO_TEST_02


    if (DO_TEST_03) {
        ####  3. COMPARISON TESTS PER BSRN “non-definitive” ####################
        cat(paste("\n3. Comparison tests.\n\n"))

        ## . . Proposed filter ---------------------------------------------####
        DFR_A    <- DATA_year$DiffuseFraction_Kd >   1.05 &
                    DATA_year$SZA                <= 75    &
                    DATA_year$wattGLB            >  50
        DFR_B    <- DATA_year$DiffuseFraction_Kd >   1.10 &
                    DATA_year$SZA                >  75    &
                    DATA_year$SZA                <  93    &
                    DATA_year$wattGLB            >  50
        DFR_prop <- DFR_A | DFR_B

        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & DFR_prop ] <- "Diffuse ratio comp max (11)"
        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & DFR_prop ] <- "Diffuse ratio comp max (11)"

        ## . . Extra filters by me -----------------------------------------####
        DFR_low <- DATA_year$DiffuseFraction_Kd < QS$dif_rati_min
        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & DFR_low ] <- "Diffuse ratio comp min (12)"
        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & DFR_low ] <- "Diffuse ratio comp min (12)"

        DFR_hig <- DATA_year$DiffuseFraction_Kd > QS$dif_rati_max
        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & DFR_hig ] <- "Diffuse ratio comp max (13)"
        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & DFR_hig ] <- "Diffuse ratio comp max (13)"

        ## . . Info --------------------------------------------------------####
        cat(sprintf(" %6d    %s\n\n", sum(DFR_prop, na.rm = T), "Records above diffuse ratio propose limit     (11)"))
        cat(sprintf(" %6d    %s\n\n", sum(DFR_low,  na.rm = T), "Records below our extra diffuse ratio limit   (12)"))
        cat(sprintf(" %6d    %s\n\n", sum(DFR_hig,  na.rm = T), "Records above our extra diffuse ratio limit   (13)"))

        rm(DFR_prop,DFR_low,DFR_hig,DFR_A,DFR_B)
    } ##END if DO_TEST_03


    if (DO_TEST_04) {
        ####  4. Climatological (configurable) Limits  #########################
        cat(paste("\n4. Climatological (configurable) Limits.\n\n"))

        ## . . Direct ------------------------------------------------------####
        second_level_D <- DATA_year$TSIextEARTH_comb * QS$clim_lim_D3 * cosde( DATA_year$SZA )**0.2 + 15
        first_level_D  <- DATA_year$TSIextEARTH_comb * QS$clim_lim_C3 * cosde( DATA_year$SZA )**0.2 + 10
        CL_first_D     <- DATA_year$wattDIR > first_level_D
        CL_secon_D     <- DATA_year$wattDIR > second_level_D

        ## Apply second limit first as it is looser than first
        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & CL_secon_D ] <- "Second climatological limit (16)"
        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & CL_first_D ] <- "First climatological limit (17)"

        ## . . Global ------------------------------------------------------####
        second_level_G <- DATA_year$TSIextEARTH_comb * QS$clim_lim_D1 * cosde( DATA_year$SZA )**1.2 + 60
        first_level_G  <- DATA_year$TSIextEARTH_comb * QS$clim_lim_C1 * cosde( DATA_year$SZA )**1.2 + 55
        CL_first_G     <- DATA_year$wattGLB > first_level_G
        CL_secon_G     <- DATA_year$wattGLB > second_level_G

        ## Apply second limit first as it is looser than first
        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & CL_secon_G ] <- "Second climatological limit (16)"
        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & CL_first_G ] <- "First climatological limit (17)"

        ## . . Info --------------------------------------------------------####
        cat(sprintf( " %6d    %s\n\n", sum(CL_secon_D, na.rm = T), "Records above Second climatological limit     (16)"))
        cat(sprintf( " %6d    %s\n\n", sum(CL_first_D, na.rm = T), "Records above First climatological limit      (17)"))
        cat(sprintf( " %6d    %s\n\n", sum(CL_secon_G, na.rm = T), "Records above Second climatological limit     (16)"))
        cat(sprintf( " %6d    %s\n\n", sum(CL_first_G, na.rm = T), "Records above First climatological limit      (17)"))

        rm(CL_secon_D, CL_first_D, CL_secon_G, CL_first_G)
    } ##END if DO_TEST_04


    if (DO_TEST_05) {
        ####  5. Tracker off test  #############################################
        cat(paste("\n5. Tracking test.\n\n"))
        ## . . Direct ------------------------------------------------------####
        ## This test use a diffuse model will be implemented when one is produced
        ## and accepted. For now we omit it to protect from over-fitting prior to
        ## make one such model.

        ## Create Clear Sky Sort-Wave model
        DATA_year[ , ClrSW := ( QS$ClrSW_a / sun_dist**2 ) * cosde(SZA)**QS$ClrSW_b ]

        sel <- DATA_year[ is.na(DATA_year$QCF_DIR) &
                              wattGLB / ClrSW   > QS$ClrSW_lim &
                              wattDIF / wattGLB > QS$ClrSW_lim, .N ]
        ## apply test
        DATA_year[ is.na(DATA_year$QCF_DIR) &
                       wattGLB / ClrSW   > QS$ClrSW_lim &
                       wattDIF / wattGLB > QS$ClrSW_lim,
                   QCF_DIR := "No tracking possible (24)" ]

        ## . . Info --------------------------------------------------------####
        cat(sprintf( " %6d    %s\n\n", sel, "Records marked with possible tracking problem    (24)"))
    } ##END if TEST DO_TEST_05


    if (DO_TEST_06) {
        ####  6. Rayleigh Limit Diffuse Comparison  ############################
        cat(paste("\n6. Rayleigh Limit Diffuse Comparison.\n\n"))

        a =   209.3
        b =  -708.3
        c =  1128.7
        d =  -911.2
        e =   287.85
        f =     0.046725

        mu_0 <- cosde(DATA_year$SZA)
        Rayleigh_diff <- a * mu_0    +
                         b * mu_0**2 +
                         c * mu_0**3 +
                         d * mu_0**4 +
                         e * mu_0**5 +
                         f * mu_0 * DATA_year$pressure

        selg <-  DATA_year$wattGLB > 50
        seld <- (DATA_year$wattDIF / DATA_year$wattGLB) < 0.8
        selr <-  DATA_year$wattDIF < (Rayleigh_diff - 1.0)

        Rayleigh_lim <- selg & seld & selr

        ## . . Both --------------------------------------------------------####
        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & Rayleigh_lim ] <- "Rayleigh diffuse limit (18)"
        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & Rayleigh_lim ] <- "Rayleigh diffuse limit (18)"

        ## . .  Info -------------------------------------------------------####
        cat(sprintf( " %6d    %s\n\n", sum(Rayleigh_lim, na.rm = T), "Rayleigh diffuse limit              (18)"))
        rm(selg,seld,selr,Rayleigh_lim)
    } ##END if DO_TEST_06


    if (DO_TEST_07) {
        ####  7. Test for obstacles  ###########################################
        cat(paste("\n7. Obstacles test.\n\n"))
        ## . . Direct ------------------------------------------------------####

        ## get biology building tag
        biol     <- biolog_build(DATA_year$Azimuth, DATA_year$Elevat )
        ## apply filter for biology building
        ## this is not pretty we are using the indexes to mark data
        ## have to parse all the original data although the filter is applicable
        ## for a specific range of Azimuth angles
        building <- which(biol$type == "bellow")
        existing <- which(is.na(DATA_year$QCF_DIR))
        exclude  <- building %in% existing

        DATA_year$QCF_DIR[ building[exclude] ] <- "Biology Building (22)"

        ## Pole obstraction is a possibility, should combine with Direct to decide
        suspects <- DATA_year$Azimuth > Pole_az_lim[1] & DATA_year$Azimuth < Pole_az_lim[2]
        DATA_year$QCF_DIR[ suspects ] <- "Possible Direct Obstruction (23)"

        ## . . Info --------------------------------------------------------####
        cat(sprintf( " %6d    %s\n\n", length(exclude),          "Direct Records obscured by biology building          (22)"))
        cat(sprintf( " %6d    %s\n\n", sum(suspects, na.rm = T), "Direct maybe obscured near sunset                    (23)"))
        rm(biol,building,existing,exclude)
    }


    if (DO_TEST_08) {
        ####  8. Test for inversed values  #####################################
        cat(paste("\n8. Inversion test.\n\n"))

        INV_hard = DATA_year$wattHOR > DATA_year$wattGLB
        INV_soft = DATA_year$wattHOR > DATA_year$wattGLB & DATA_year$wattGLB > 50

        ## . . Both --------------------------------------------------------####
        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & INV_soft ] <- "Direct > global soft (14)"
        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & INV_soft ] <- "Direct > global soft (14)"

        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & INV_hard ] <- "Direct > global hard (15)"
        DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) & INV_hard ] <- "Direct > global hard (15)"

        ## . . Info --------------------------------------------------------####
        cat(sprintf( " %6d    %s\n\n", sum(INV_soft, na.rm = T), "Records with inverse radiation Direct > global soft  (14)"))
        cat(sprintf( " %6d    %s\n\n", sum(INV_hard, na.rm = T), "Records with inverse radiation Direct > global hard  (15)"))
    } ##END if DO_TEST_08


    if (DO_TEST_09) {
        ####  8. Clearness index test  #########################################
        cat(paste("\n9. Clearness index (global/TSI) test.\n\n"))

        ## . . Extra filters by me -----------------------------------------####
        clid_over  <- DATA_year$Clearness_Kt > QS$CL_idx_max
        clid_under <- DATA_year$Clearness_Kt < QS$CL_idx_min

        ## . . Global ------------------------------------------------------####
        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & clid_over  ] <- "Clearness index limit max (19)"
        DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) & clid_under ] <- "Clearness index limit min (20)"

        ## . . Info --------------------------------------------------------####
        cat(sprintf( " %6d    %s\n\n", sum(clid_over,  na.rm = T), "Records with extreme clearness index                 (19)"))
        cat(sprintf( " %6d    %s\n\n", sum(clid_under, na.rm = T), "Records with negative clearness index                (20)"))
    } ##END if DO_TEST_09



    ####  All unflagged data are good  #########################################
    DATA_year$QCF_DIR[ is.na(DATA_year$QCF_DIR) ] <- "good"
    DATA_year$QCF_GLB[ is.na(DATA_year$QCF_GLB) ] <- "good"

    ## make them factors
    wecare <- grep("QCF_.*", names(DATA_year), value = TRUE )
    for (avar in wecare) {
        DATA_year[[avar]] <- as.factor(DATA_year[[avar]])
    }

    ## put good first for coloring
    DATA_year$QCF_DIR <- relevel(DATA_year$QCF_DIR, "good" )
    DATA_year$QCF_GLB <- relevel(DATA_year$QCF_GLB, "good" )

    #### ~ ~ ~ ~ END OF FLAGGING ~ ~ ~  ~ ######################################



    #### ~ ~ ~ ~  Plot each test action  ~ ~ ~ ~   #############################

    ## get valid data vectors
    Dgood <- DATA_year$QCF_DIR == "good"
    Ggood <- DATA_year$QCF_GLB == "good"

    if (all(DO_TEST_01, DO_TEST_02)) {
        if (any(!is.na(DATA_year$wattDIR))) {
            ## . . Plot Direct irradiance and tests 1. and 2. --------------####
            ylim <- range( QS$dir_SWdn_min,
                           DATA_year$wattDIR,
                           DATA_year$TSIextEARTH_comb,
                           Direct_max_extremely_rare,
                           na.rm = T)

            ####  plot direct by SZA  ####
            cat("\n\n")
            plot( DATA_year$SZA[Dgood], DATA_year$wattDIR[Dgood],
                  cex = .1,
                  xlim = xlim,  ylim = ylim,
                  xlab = "SZA", ylab = "Direct Irradiance" )
            ## 1. Physical possible limit max (6)
            points(DATA_year$SZA, DATA_year$TSIextEARTH_comb, cex = .2,  col = alpha("red",  0.05))
            ## 2. Extremely rare limits max (4)
            points(DATA_year$SZA, Direct_max_extremely_rare,  cex = .2,  col = alpha("blue", 0.05))
            ## 1. Physical possible limit min (5)
            abline(h = QS$dir_SWdn_min,                       lwd = 1.5, col = "red")
            ## 2. Extremely rare limits min (3)
            abline(h = QS$dir_SWdn_min_ext,                   lwd = 1.5, col = "blue")

            ## plot flagged
            rare <- which(DATA_year$QCF_DIR %in%
                              c("Extremely rare limits min (3)", "Extremely rare limits max (4)"))
            phys <- which(DATA_year$QCF_DIR %in%
                              c("Physical possible limit min (5)", "Physical possible limit max (6)"))

            points(DATA_year$SZA[rare], DATA_year$wattDIR[rare], cex = .7, col = "cyan")
            points(DATA_year$SZA[phys], DATA_year$wattDIR[phys], cex = .7, col = "magenta")

            title(main = paste("Direct Beam Physical limits tests 1. and 2.",YY))
            legend("topright",
                   legend = c("Global measurements", "Physical limits", "Rare limits", "Rare measurements", "Extreme measurements" ),
                   col    = c("black",               "red"            , "blue",        "cyan",              "magenta"),
                   pch = 19, bty = "n", cex = 0.8 )

            ####  plot direct by azimuth  ####
            cat("\n\n")
            plot( DATA_year$Azimuth[Dgood], DATA_year$wattDIR[Dgood],
                  cex = .1,
                  ylim = ylim,
                  xlab = "Azimuth", ylab = "Direct Irradiance" )
            ## 1. Physical possible limit max (6)
            points(DATA_year$Azimuth, DATA_year$TSIextEARTH_comb,  cex = .1,  col = alpha("red", 0.05))
            ## 2. Extremely rare limits max (4)
            points(DATA_year$Azimuth, Direct_max_extremely_rare,   cex = .1,  col = alpha("blue",0.05))
            ## 1. Physical possible limit min (5)
            abline(h = QS$dir_SWdn_min,                            lwd = 1.5, col = "red")
            ## 2. Extremely rare limits min (3)
            abline(h = QS$dir_SWdn_min_ext,                        lwd = 1.5, col = "blue")

            ## plot flagged
            rare <- which( DATA_year$QCF_DIR %in%
                               c("Extremely rare limits min (3)", "Extremely rare limits max (4)"))
            phys <- which( DATA_year$QCF_DIR %in%
                               c("Physical possible limit min (5)", "Physical possible limit max (6)"))

            points(DATA_year$Azimuth[rare], DATA_year$wattDIR[rare], cex = .7, col = "cyan")
            points(DATA_year$Azimuth[phys], DATA_year$wattDIR[phys], cex = .7, col = "magenta")

            title(main = paste("Direct Beam Physical limits tests 1. and 2.", YY))
            legend("topright",
                   legend = c("Global measurements", "Physical limits", "Rare limits", "Rare measurements", "Extreme measurements"),
                   col    = c("black",               "red",             "blue",        "cyan",              "magenta"),
                   pch = 19, bty = "n", cex = 0.8 )

            ## clean
            rm(Direct_max_extremely_rare, rare, phys)
        }


        if (any(!is.na(DATA_year$wattGLB))) {
            ## . . Plot Global irradiance and test 1. and 2. ---------------####
            ylim <- range(QS$glo_SWdn_min,
                          DATA_year$wattGLB,
                          Global_max_physical_limit,
                          Global_max_extremely_rare,
                          na.rm = TRUE)

            ####  plot global by SZA  ####
            cat("\n\n")
            plot(  DATA_year$SZA[Ggood], DATA_year$wattGLB[Ggood],
                   cex = .1,
                   xlim = xlim,  ylim = ylim,
                   xlab = "SZA", ylab = "Global Irradiance")
            ## 1. Physical possible limit max (6)
            points(DATA_year$SZA, Global_max_physical_limit, cex = .1,  col = alpha("red",  0.05))
            ## 2. Extremely rare limits max (4)
            points(DATA_year$SZA, Global_max_extremely_rare, cex = .1,  col = alpha("blue", 0.05))
            ## 1. Physical possible limit min (5)
            abline(h = QS$glo_SWdn_min,                      lwd = 1.5, col = "red")
            ## 2. "Extremely rare limits min (3)"
            abline(h = QS$glo_SWdn_min_ext,                  lwd = 1.5, col = "blue")

            ## plot flagged
            rare <- which(DATA_year$QCF_GLB %in%
                              c("Extremely rare limits min (3)", "Extremely rare limits max (4)"))
            phys <- which(DATA_year$QCF_GLB %in%
                              c("Physical possible limit min (5)", "Physical possible limit max (6)"))

            points(DATA_year$SZA[rare], DATA_year$wattGLB[rare], cex = .7, col = "cyan")
            points(DATA_year$SZA[phys], DATA_year$wattGLB[phys], cex = .7, col = "magenta")

            title(main = paste("Global Physical limits tests 1. and 2.",YY))
            legend("topright",
                   legend = c("Global measurements", "Physical limits", "Rare limits", "Rare measurements", "Extreme measurements" ),
                   col    = c("black",               "red"            , "blue",        "cyan",              "magenta"),
                   pch = 19, bty = "n", cex = 0.8 )


            ####  plot global by azimuth  ####
            cat("\n\n")
            plot(  DATA_year$Azimuth[Ggood], DATA_year$wattGLB[Ggood],
                   cex  = .1,
                   ylim = ylim,
                   xlab = "Azimuth", ylab = "Global Irradiance" )
            ## 1. Physical possible limit max (6)
            points(DATA_year$Azimuth, Global_max_physical_limit, cex = .1,  col = alpha("red", 0.05))
            ## 2. Extremely rare limits max (4)
            points(DATA_year$Azimuth, Global_max_extremely_rare, cex = .1,  col = alpha("blue",0.05))
            ## 1. Physical possible limit min (5)
            abline(h = QS$glo_SWdn_min,                        lwd = 1.5, col = "red")
            ## 2. "Extremely rare limits min (3)"
            abline(h = QS$glo_SWdn_min_ext,                    lwd = 1.5, col = "blue")

            ## plot flagged
            rare <- which(DATA_year$QCF_GLB %in%
                              c("Extremely rare limits min (3)",   "Extremely rare limits max (4)"))
            phys <- which(DATA_year$QCF_GLB %in%
                              c("Physical possible limit min (5)", "Physical possible limit max (6)"))

            points(DATA_year$Azimuth[rare], DATA_year$wattGLB[rare], cex = .7, col = "cyan"   )
            points(DATA_year$Azimuth[phys], DATA_year$wattGLB[phys], cex = .7, col = "magenta")

            title(main = paste("Global Physical limits tests 1. and 2.",YY))
            legend("topright",
                   legend = c("Global measurements", "Physical limits", "Rare limits", "Rare measurements", "Extreme measurements"),
                   col    = c("black",               "red"            , "blue",        "cyan",              "magenta"),
                   pch = 19, bty = "n", cex = 0.8 )

            ## clean
            rm(Global_max_physical_limit, Global_max_extremely_rare, rare, phys)
        }
    } ##END if all(DO_TEST_01, DO_TEST_02)


    if (DO_TEST_03 & !all(is.na(DATA_year$DiffuseFraction_Kd))) {
        ## . . Plot comparison test 3. -------------------------------------####

        ## Direct diffuse fraction problems
        yrange = range( DATA_year$DiffuseFraction_Kd, na.rm = T )
        if (yrange[1] < -1) yrange[1] = -1
        if (yrange[2] >  2) yrange[2] =  2
        ## the factor is the same for all radiations
        hard <- which( DATA_year$QCF_DIR %in%   "Diffuse ratio comp max (11)" )
        soft <- which( DATA_year$QCF_DIR %in% c("Diffuse ratio comp min (12)", "Diffuse ratio comp max (13)"))

        ####  Diffuse Fraction by SZA  ####
        cat("\n\n")
        plot( DATA_year$SZA[Ggood | Dgood], DATA_year$DiffuseFraction_Kd[Ggood | Dgood],
              ylim = yrange,
              ylab = "Diffuse fraction",
              cex = .1)
        ## 3. Diffuse ratio comp max (11)
        segments( 0, 1.05, 75, 1.05, lwd = 2, col = "red" )
        segments(75, 1.10, 93, 1.10, lwd = 2, col = "red" )
        ## 3. Diffuse ratio comp min (12)
        abline( h = QS$dif_rati_max, lwd = 2, col = "blue")
        ## 3. Diffuse ratio comp max (13)
        abline( h = QS$dif_rati_min, lwd = 2, col = "blue")

        ## plot flagged
        points(DATA_year$SZA[hard], DATA_year$DiffuseFraction_Kd[hard],
               col = "magenta", cex = 0.5)
        points(DATA_year$SZA[soft], DATA_year$DiffuseFraction_Kd[soft],
               col = "cyan",    cex = 0.5)

        title(main = paste("Comparison test 3.", YY))
        legend("topleft",
               legend = c("Global measurements", "Max diff proposed", "Our limits", "Rare measurements", "Extreme measurements"),
               col    = c("black",               "red",               "blue",       "cyan",              "magenta"),
               pch = 19, bty = "n", cex = 0.8 )


        ####  Diffuse fraction by azimuth  ####
        cat("\n\n")
        plot( DATA_year$Azimuth[Ggood | Dgood], DATA_year$DiffuseFraction_Kd[Ggood | Dgood],
              ylim = yrange,
              ylab = "Diffuse fraction", xlab = "Azimuth",
              cex = .1)

        ## 3. Diffuse ratio comp max (11)
        segments(0, 1.05, 360, 1.05, lwd = 2, lty = 2, col = "red")
        segments(0, 1.10, 360, 1.10, lwd = 2, lty = 2, col = "red")
        ## 3. Diffuse ratio comp min (12)
        abline(h = QS$dif_rati_max, lwd = 2, col = "blue")
        ## 3. Diffuse ratio comp max (13)
        abline(h = QS$dif_rati_min, lwd = 2, col = "blue")

        ## plot flagged
        points(DATA_year$Azimuth[hard], DATA_year$DiffuseFraction_Kd[hard],
               col = "magenta", cex = 0.5)
        points(DATA_year$Azimuth[soft], DATA_year$DiffuseFraction_Kd[soft],
               col = "cyan",    cex = 0.5)

        title(main = paste("Comparison test 3.", YY))
        legend("topright",
               legend = c("Global measurements", "Max diff proposed", "Our limits", "Rare measurements", "Extreme measurements"),
               col    = c("black",               "red",               "blue",       "cyan",              "magenta"),
               pch = 19, bty = "n", cex = 0.8 )

        rm(hard, soft)
    }##END if DO_TEST_03


    if (DO_TEST_04 & !all(is.na(DATA_year$wattDIR))) {
        ## . . Plot climatological test 4. ---------------------------------####
        if (any(!is.na(DATA_year$wattDIR))) {
            ## For Direct
            ylim <- range(second_level_D,
                          first_level_D,
                          DATA_year$wattDIR,
                          na.rm = T)
            hard <- which(DATA_year$QCF_DIR %in% "Second climatological limit (16)")
            soft <- which(DATA_year$QCF_DIR %in% "First climatological limit (17)")

            ####  plot direct by SZA  ####
            cat("\n\n")
            plot( DATA_year$SZA[Dgood], DATA_year$wattDIR[Dgood],
                  cex = .1,
                  xlim = xlim,  ylim = ylim,
                  xlab = "SZA", ylab = "Direct Irradiance")
            ## 4. Second climatological limit (16)
            points(DATA_year$SZA, second_level_D, cex = .2, col = alpha("red",  0.05))
            ## 4. First climatological limit (17)
            points(DATA_year$SZA, first_level_D,  cex = .2, col = alpha("blue", 0.05))

            ## plot flagged
            points(DATA_year$SZA[soft], DATA_year$wattDIR[soft], cex = .7, col = "cyan")
            points(DATA_year$SZA[hard], DATA_year$wattDIR[hard], cex = .7, col = "magenta")

            title(main = paste("Direct Beam climatological test 4.", YY))
            legend("topright",
                   legend = c("Global measurements", "Second limit", "First limit", "First measurements", "Second measurements" ),
                   col    = c("black",               "red",          "blue",        "cyan",               "magenta"),
                   pch = 19, bty = "n", cex = 0.8 )

            ####  plot direct by Azimuth  ####
            cat("\n\n")
            plot( DATA_year$Azimuth[Dgood], DATA_year$wattDIR[Dgood],
                  cex = .1,
                  ylim = ylim,
                  xlab = "Azimuth", ylab = "Direct Irradiance" )
            ## 4. Second climatological limit (16)
            points(DATA_year$Azimuth, second_level_D, cex = .2,  col = alpha("red",  0.05))
            ## 4. First climatological limit (17)
            points(DATA_year$Azimuth, first_level_D,  cex = .2,  col = alpha("blue", 0.05))

            ## plot flagged
            points(DATA_year$Azimuth[soft], DATA_year$wattDIR[soft], cex = .7, col = "cyan")
            points(DATA_year$Azimuth[hard], DATA_year$wattDIR[hard], cex = .7, col = "magenta")

            title(main = paste("Direct Beam climatological test 4.", YY))
            legend("topright",
                   legend = c("Global measurements", "Second limit", "First limit", "First measurements", "Second measurements" ),
                   col    = c("black",               "red",          "blue",        "cyan",               "magenta"),
                   pch = 19, bty = "n", cex = 0.8 )
        }


        if (any(!is.na(DATA_year$wattGLB))) {
            ## For global
            ylim <- range(second_level_G,
                          first_level_G,
                          DATA_year$wattGLB,
                          na.rm = TRUE)
            hard <- which(DATA_year$QCF_GLB %in% "Second climatological limit (16)")
            soft <- which(DATA_year$QCF_GLB %in% "First climatological limit (17)")

            ####  plot global by SZA  ####
            cat("\n\n")
            plot( DATA_year$SZA[Ggood], DATA_year$wattGLB[Ggood],
                  cex = .1,
                  xlim = xlim,  ylim = ylim,
                  xlab = "SZA", ylab = "Global Irradiance" )
            ## 4. Second climatological limit (16)
            points(DATA_year$SZA, second_level_G, cex = .2,  col = alpha("red", 0.05))
            ## 4. First climatological limit (17)
            points(DATA_year$SZA, first_level_G,  cex = .2,  col = alpha("blue",0.05))

            ## plot flagged
            points(DATA_year$SZA[soft], DATA_year$wattGLB[soft], cex = .7, col = "cyan")
            points(DATA_year$SZA[hard], DATA_year$wattGLB[hard], cex = .7, col = "magenta")

            title(main = paste("Global climatological test 4.",YY))
            legend("topright",
                   legend = c("Global measurements", "Second limit", "First limit", "First measurements", "Second measurements"),
                   col    = c("black",               "red",          "blue",        "cyan",               "magenta"),
                   pch = 19, bty = "n", cex = 0.8 )

            ####  plot global by Azimuth  ####
            cat("\n\n")
            plot( DATA_year$Azimuth[Ggood], DATA_year$wattGLB[Ggood],
                  cex = .1,
                  ylim = ylim,
                  xlab = "Azimuth", ylab = "Global Irradiance" )
            ## 4. Second climatological limit (16)
            points(DATA_year$Azimuth, second_level_G, cex = .2,  col = alpha("red", 0.05))
            ## 4. First climatological limit (17)
            points(DATA_year$Azimuth, first_level_G,  cex = .2,  col = alpha("blue",0.05))

            ## plot flagged
            points(DATA_year$Azimuth[soft], DATA_year$wattGLB[soft], cex = .7, col = "cyan")
            points(DATA_year$Azimuth[hard], DATA_year$wattGLB[hard], cex = .7, col = "magenta")

            title(main = paste("Global climatological test 4.",YY))
            legend("topright",
                   legend = c("Global measurements", "Second limit", "First limit", "First measurements", "Second measurements"),
                   col    = c("black",               "red",          "blue",        "cyan",               "magenta"),
                   pch = 19, bty = "n", cex = 0.8 )

            ## clean
            rm(second_level_G, first_level_G, second_level_D, first_level_D, hard, soft)
        }
    }##END if DO_TEST_04


    if (DO_TEST_05 & !all(is.na(DATA_year$wattDIR))) {
        ## . . Plot Tracker off test 5. ------------------------------------####

        ## plot by SZA
        cat("\n\n")
        plot( DATA_year$SZA[Dgood], DATA_year$wattDIR[Dgood],
              cex = .1,
              xlim = xlim,
              xlab = "SZA", ylab = "Direct Irradiance" )
        title(main = paste("Tracker off test 5.",YY))

        ## plot flagged
        ss = which(DATA_year$QCF_DIR == "No tracking possible (24)")
        points( DATA_year$SZA[ss], DATA_year$wattDIF[ss],
                cex = .5, col = "cyan" )

        ## plot by Azimuth
        plot( DATA_year$Azimuth[Dgood], DATA_year$wattDIR[Dgood],
              cex = .1,
              xlab = "Azimuth", ylab = "Direct Irradiance" )
        title(main = paste("Tracker off test 5.",YY))
        points( DATA_year$Azimuth[ss], DATA_year$wattDIF[ss],
                cex = .5, col = "cyan" )
    }##END if DO TEST 05


    if (DO_TEST_06 & !all(is.na(DATA_year$wattDIF)) ) {
        ## . . Plot Rayleigh Limit Diffuse test 6. -------------------------####

        ## plot by SZA
        cat("\n\n")
        plot( DATA_year$SZA[Dgood | Ggood], DATA_year$wattDIF[Dgood | Ggood],
              cex = .1,
              xlim = xlim,
              xlab = "SZA", ylab = "Diffuse Irradiance" )
        title(main = paste("Rayleigh Limit Diffuse Comparison test 6.", YY))

        ## plot flagged
        ss <- which(DATA_year$QCF_DIR == "Rayleigh diffuse limit (18)")
        points( DATA_year$SZA[ss], DATA_year$wattDIF[ss],
                cex = .7, col = "magenta" )
        legend("topright",
               legend = c("Diffuse (inferred)", "Rayleigh limit" ),
               col    = c("black",              "magenta"),
               pch = 19, bty = "n", cex = 0.8 )

        ## plot by Azimuth
        cat("\n\n")
        plot( DATA_year$Azimuth[Dgood | Ggood], DATA_year$wattDIF[Dgood | Ggood],
              cex = .1,
              xlab = "Azimuth", ylab = "Diffuse Irradiance" )
        title(main = paste("Rayleigh Limit Diffuse Comparison test 6.", YY))

        ## plot flagged
        ss <- which(DATA_year$QCF_DIR == "Rayleigh diffuse limit (18)")
        points( DATA_year$Azimuth[ss], DATA_year$wattDIF[ss],
                cex = .7, col = "magenta" )
        legend("topright",
               legend = c("Diffuse (inferred)", "Rayleigh limit" ),
               col    = c("black",              "magenta"),
               pch = 19, bty = "n", cex = 0.8 )
    }##END if DO_TEST_06


    if (DO_TEST_07) {
        ## . . Plot obstacle test 7. ---------------------------------------####
        if (nrow(DATA_year[ QCF_DIR %in% c("Biology Building (22)", "Possible Direct Obstruction (23)")]) > 0 ) {

            cat("\n\n")
            plot( DATA_year$Azimuth, DATA_year$Elevat,
                  cex = .1,
                  xlab = "Azimuth", ylab = "Elevat" )
            title(main = paste("Obstacle test 7.", YY))

            ## plot flagged
            ss = which(DATA_year$QCF_DIR == "Biology Building (22)")
            points( DATA_year$Azimuth[ss], DATA_year$Elevat[ss],
                    cex = .1, col = "magenta" )

            ss = which(DATA_year$QCF_DIR == "Possible Direct Obstruction (23)")
            points( DATA_year$Azimuth[ss], DATA_year$Elevat[ss],
                    cex = .1, col = "cyan" )

            legend("topright",
                   legend = c("Direct measurements","Biology Build", "Possible pole obstruction"),
                   col    = c("black",              "magenta",       "Cyan"      ),
                   pch = 19, bty = "n", cex = 0.8 )
        }
    }


    if (DO_TEST_08 & !all(is.na(DATA_year$wattDIF))) {
        ## . . Plot Diffuse inversion test 8. ------------------------------####

        ####  plot direct by SZA  ####
        cat("\n\n")
        plot( DATA_year$SZA[Dgood | Ggood], DATA_year$wattDIF[Dgood | Ggood],
              cex = .1,
              xlim = xlim,
              xlab = "SZA", ylab = "Diffuse Irradiance" )
        title(main = paste("Diffuse inversion test 8.",YY))

        ## plot flagged
        ss <- which(DATA_year$QCF_DIR == "Direct > global soft (14)")
        tt <- which(DATA_year$QCF_DIR == "Direct > global hard (15)")

        points( DATA_year$SZA[ss], DATA_year$wattDIF[ss],
                cex = .7, col = "magenta" )

        points( DATA_year$SZA[tt], DATA_year$wattDIF[tt],
                cex = .7, col = "cyan" )

        legend("topright",
               legend = c("Diffuse (inferred)", "Inversion hard", "Inversion soft"),
               col    = c("black",              "magenta",        "cyan"  ),
               pch = 19, bty = "n", cex = 0.8 )


        ####  plot direct by Azimuth  ####
        cat("\n\n")
        plot( DATA_year$Azimuth[Dgood | Ggood], DATA_year$wattDIF[Dgood | Ggood],
              cex = .1,
              xlab = "Azimuth", ylab = "Diffuse Irradiance")
        title(main = paste("Diffuse inversion test 8.",YY))

        ## plot flagged
        ss <- which(DATA_year$QCF_DIR == "Direct > global soft (14)")
        tt <- which(DATA_year$QCF_DIR == "Direct > global hard (15)")

        points( DATA_year$Azimuth[ss], DATA_year$wattDIF[ss],
                cex = .7, col = "magenta" )

        points( DATA_year$Azimuth[tt], DATA_year$wattDIF[tt],
                cex = .7, col = "cyan" )

        legend("topright",
               legend = c("Diffuse (inferred)", "Inversion hard", "Inversion soft" ),
               col    = c("black",              "magenta",        "cyan"  ),
               pch = 19, bty = "n", cex = 0.8 )
    }##END if DO_TEST_08


    if (DO_TEST_09 & !all(is.na(DATA_year$Clearness_Kt)) ) {
        ## . . Clearness index test 9. -------------------------------------####
        ylim <- range(DATA_year$Clearness_Kt, na.rm = T)
        if (ylim[1] < -1 ) ylim[1] = -1
        if (ylim[2] >  3 ) ylim[2] =  3

        dddd <- DATA_year$QCF_GLB %in%
                    c("Clearness index limit max (19)", "Clearness index limit min (20)")

        ## by SZA
        cat("\n\n")
        plot(DATA_year$SZA[!dddd], DATA_year$Clearness_Kt[!dddd],
             cex = .1,
             ylim = ylim, xlim = xlim,
             xlab = "SZA", ylab = "Clearness Index (Global)" )

        ppmax <- DATA_year$QCF_GLB == "Clearness index limit max (19)"
        ppmin <- DATA_year$QCF_GLB == "Clearness index limit min (20)"

        points( DATA_year$SZA[ppmax], DATA_year$Clearness_Kt[ppmax],
                cex = .7, col = "cyan" )

        points( DATA_year$SZA[ppmin], DATA_year$Clearness_Kt[ppmin],
                cex = .7, col = "magenta")

        legend("topleft",
               legend = c("Clearness index", "Extreme clearness index", "Too low clearness index"),
               col    = c("black",           "cyan",                    "magenta"                ),
               pch = 19, bty = "n", cex = 0.8 )
        title(main = paste("Clearness index test 9.", YY))

        ## by Azimuth
        cat("\n\n")
        plot(DATA_year$Azimuth[!dddd], DATA_year$Clearness_Kt[!dddd],
             cex = .1,
             ylim = ylim,
             xlab = "Azimuth", ylab = "Clearness Index (Global)" )

        ppmax <- DATA_year$QCF_GLB == "Clearness index limit max (19)"
        ppmin <- DATA_year$QCF_GLB == "Clearness index limit min (20)"

        points( DATA_year$Azimuth[ppmax], DATA_year$Clearness_Kt[ppmax],
                cex = .7, col = "cyan")

        points( DATA_year$Azimuth[ppmin], DATA_year$Clearness_Kt[ppmin],
                cex = .7, col = "magenta")

        legend("topleft",
               legend = c("Clearness index", "Extreme clearness index", "Too low clearness index"),
               col    = c("black",           "cyan",                    "magenta"                ),
               pch = 19, bty = "n", cex = 0.8 )
        title(main = paste("Clearness index test 9.", YY))
    }##END if DO_TEST_09




    #### ~ ~ ~ ~ Plot problematic data ~ ~ ~ ~   ##############################

    ## . . Plot all problems on Direct ------------------------------------####

    ## by SZA
    if (!all(is.na(DATA_year$wattDIR))) {
        cat("\n\n")
        plot( DATA_year$SZA, DATA_year$wattDIR,
              cex = .1,
              xlab = "SZA", ylab = "Direct Irradiance" )

        ## plot flagged
        prob <- !(DATA_year$QCF_DIR %in% c("good", "Diffuse ratio comp min (12)"))
        points( DATA_year$SZA[prob], DATA_year$wattDIR[prob],
                cex = 0.7,
                col = palete_rand[ DATA_year$QCF_DIR[prob] ])

        ## annotations
        cnames = unique(DATA_year$QCF_DIR[prob])
        if ( length(cnames) > 0 ) {
            legend("topright",
                   title  = "Suspicious measurements",
                   legend = cnames,
                   col    = palete_rand[cnames],
                   pch = 19, bty = "n", cex = 0.7 ) }
        title(paste("All suspects on Direct (excl. Diffuse ratio comp min (12)) for", YY))
    }



    ## by Azimuth
    if (!all(is.na(DATA_year$wattDIR))) {
        cat("\n\n")
        plot( DATA_year$Azimuth, DATA_year$wattDIR,
              cex = .1,
              xlab = "Azimuth", ylab = "Direct Irradiance" )

        ## plot flagged
        prob <- !(DATA_year$QCF_DIR %in% c("good", "Diffuse ratio comp min (12)"))
        points( DATA_year$Azimuth[prob], DATA_year$wattDIR[prob],
                cex = 0.7,
                col = palete_rand[ DATA_year$QCF_DIR[prob] ])

        ## annotations
        cnames = unique(DATA_year$QCF_DIR[prob])
        if (length(cnames) > 0) {
            legend("topleft",
                   title  = "Suspicious measurements",
                   legend = cnames,
                   col    = palete_rand[cnames],
                   pch = 19, bty = "n", cex = 0.7 ) }
        title(paste("All suspects on Direct (excl. Diffuse ratio comp min (12)) for", YY))
    }


    ## . . Plot all problems on Global -------------------------------------####

    ## by SZA
    cat("\n\n")
    plot( DATA_year$SZA, DATA_year$wattGLB,
          cex = .1,
          xlab = "SZA", ylab = "Global Irradiance" )

    ## plot flagged
    prob <- !(DATA_year$QCF_GLB %in% c("good", "Diffuse ratio comp min (12)"))
    points( DATA_year$SZA[prob], DATA_year$wattGLB[prob],
            cex = 0.7,
            col = palete_rand[ DATA_year$QCF_GLB[prob] ])

    ## annotations
    cnames <- unique(DATA_year$QCF_GLB[prob])
    if (length(cnames) > 0) {
        legend("topright",
               title = "Suspicious measurements",
               legend = cnames,
               col = palete_rand[cnames],
               pch = 19, bty = "n", cex = 0.7 ) }
    title(paste("All suspects on Global (excl. Diffuse ratio comp min (12)) for",YY))

    ## by Azimuth
    cat("\n\n")
    plot( DATA_year$Azimuth, DATA_year$wattGLB,
          cex = .1,
          xlab = "Azimuth", ylab = "Global Irradiance" )

    ## plot flagged
    prob <- !(DATA_year$QCF_GLB %in% c("good", "Diffuse ratio comp min (12)"))
    points( DATA_year$Azimuth[prob], DATA_year$wattGLB[prob],
            cex = 0.7,
            col = palete_rand[ DATA_year$QCF_GLB[prob] ])

    ## annotations
    cnames <- unique(DATA_year$QCF_GLB[prob])
    if (length(cnames) > 0) {
        legend("topleft",
               title  = "Suspicious measurements",
               legend = cnames,
               col    = palete_rand[cnames],
               pch = 19, bty = "n", cex = 0.7 ) }
    title(paste("All suspects on Direct (excl. Diffuse ratio comp min (12)) for", YY))



    ## . . Plot all problems on Diffuse ------------------------------------####
    if (!all(is.na(DATA_year$wattDIF))) {
        ## by SZA
        cat("\n\n")
        plot( DATA_year$SZA, DATA_year$wattDIF,
              cex = .1,
              xlab = "SZA", ylab = "Diffuse Irradiance" )

        ## plot flagged
        prob1 <- !(DATA_year$QCF_DIR %in% c("good"))
        prob2 <- !(DATA_year$QCF_GLB %in% c("good"))

        points(DATA_year$SZA[prob1], DATA_year$wattDIF[prob1],
               cex = 0.7,
               col = palete_rand[ DATA_year$QCF_DIR[prob1] ])
        points(DATA_year$SZA[prob2], DATA_year$wattDIF[prob2],
               cex = 0.7,
               col = palete_rand[ DATA_year$QCF_GLB[prob2] ])

        ## annotations
        cnames1 <- unique(DATA_year$QCF_DIR[prob1])
        cnames2 <- unique(DATA_year$QCF_GLB[prob2])
        cnames  <- unique(cnames1, cnames2)
        if (length(cnames) > 0) {
            legend("topright",
                   title  = "Suspicious measurements",
                   legend = cnames,
                   col    = palete_rand[cnames],
                   pch = 19, bty = "n", cex = 0.7 ) }
        title(paste("Suspects on Diffuse for",YY))
    }


    ## . . Plot all problems on Diffuse fraction ---------------------------####
    if (!all(is.na(DATA_year$DiffuseFraction_Kd))) {
        ## by SZA
        cat("\n\n")
        plot( DATA_year$SZA, DATA_year$DiffuseFraction_Kd,
              cex = .1,
              xlab = "SZA", ylab = "Diffuse fraction" )

        ## plot flagged
        prob1 <- !(DATA_year$QCF_DIR %in% c("good"))
        prob2 <- !(DATA_year$QCF_GLB %in% c("good"))

        points( DATA_year$SZA[prob1], DATA_year$DiffuseFraction_Kd[prob1],
                cex = 0.7,
                col = palete_rand[ DATA_year$QCF_DIR[prob1] ])
        points( DATA_year$SZA[prob2], DATA_year$DiffuseFraction_Kd[prob2],
                cex = 0.7,
                col = palete_rand[ DATA_year$QCF_GLB[prob2] ])

        ## annotations
        cnames1 <- unique(DATA_year$QCF_DIR[prob1])
        cnames2 <- unique(DATA_year$QCF_GLB[prob2])
        cnames  <- unique(cnames1, cnames2)
        if (length(cnames) > 0) {
            legend("topleft",
                   title  = "Suspicious measurements",
                   legend = cnames,
                   col    = palete_rand[cnames],
                   pch = 19, bty = "n", cex = 0.7 ) }
        title(paste("Suspects on Diffuse fraction for",YY))
    }


    cat(pander::pander(table(DATA_year$QCF_DIR)))
    cat("\n\n")

    cat(pander::pander(table(DATA_year$QCF_GLB)))
    cat("\n\n")


    ## . . Plot sun path for direct ----------------------------------------####

    isgood <- DATA_year$QCF_DIR %in% c("good", "Diffuse ratio comp min (12)") &
              !is.na(DATA_year$wattDIR)
    if (all(isgood)) {
        cat("\n\n")
        plot(DATA_year$Azimuth[isgood], DATA_year$Elevat[isgood],
             xlab = "Azimuth", ylab = "Sun Elevation",
             col = alpha("black", 0.3), pch = 15, cex = 0.5)

        isgood <- !(DATA_year$QCF_DIR %in% c("good", "Diffuse ratio comp min (12)")) &
                  !is.na(DATA_year$wattDIR)
        points(DATA_year$Azimuth[isgood], DATA_year$Elevat[isgood],
               pch = 15, cex = 0.65, col = palete_rand[ DATA_year$QCF_DIR[isgood] ])

        cnames <- unique(DATA_year$QCF_DIR[isgood])
        if (length(cnames) >0) {
            legend("topleft",
                   legend = cnames,
                   col = palete_rand[cnames],
                   pch = 19, bty = "n", cex = 0.7)
            }
        title(paste("Problems on Direct (excl. Diffuse ratio comp min (12)) for", YY))
    }


    ## . . Plot sun path for global ----------------------------------------####

    isgood <- DATA_year$QCF_GLB %in% c("good", "Diffuse ratio comp min (12)") &
              !is.na(DATA_year$wattGLB)
    cat("\n\n")
    plot(DATA_year$Azimuth[isgood], DATA_year$Elevat[isgood],
         xlab = "Azimuth", ylab = "Sun Elevation",
         col = alpha("black", 0.3), pch = 15, cex = 0.5 )

    isgood <- !(DATA_year$QCF_GLB %in% c("good", "Diffuse ratio comp min (12)")) &
              !is.na(DATA_year$wattGLB)
    points(DATA_year$Azimuth[isgood], DATA_year$Elevat[isgood],
           pch = 15, cex = 0.65, col = palete_rand[ DATA_year$QCF_GLB[isgood] ])

    cnames <- unique(DATA_year$QCF_GLB[isgood])
    if (length(cnames) > 0 ) {
        legend("topleft",
               legend = cnames,
               col = palete_rand[cnames],
               pch = 19, bty = "n", cex = 0.7 ) }
    title(paste("Problems on Global (excl. Diffuse ratio comp min (12)) for", YY))


    # tempplto = aggregate_CF( DATA_year$Date30, DATA_year[ ,c("wattGLB","Azimuth","Elevat") ], n_minutes = 1 )
    #
    # colorss = colorRampPalette(colors = c("blue","red"))
    # collors = heat.colors(length(unique( (tempplto$wattGLB %/% 2)*2 )))
    # collors = colorss(length(unique( (tempplto$wattGLB %/% 2)*2 )))
    #
    # ccv = collors[as.factor( (tempplto$wattGLB %/% 2)*2 )]
    # plot(tempplto$Azimuth,tempplto$Elevat,col=ccv,pch=15,cex=0.6)
    #
    # doy = as.numeric(format( tempplto$Group.1,format = "%j"))
    #
    # plot( doy,
    #       (((decimal_date(tempplto$Group.1) %% 1) * 365)-doy+1) *24*60,
    #       col = ccv , pch = 15,cex=.8)
    #
    # image(xtabs(tempplto$wattGLB ~ tempplto$Azimuth+DATA_year$Elevat))
    # require(lattice)
    # levelplot( tempplto$wattGLB ~ tempplto$Azimuth * DATA_year$Elevat )
    # m <- with(tempplto, tapply(wattGLB, list(Azimuth, Elevat), I))


    #### ~ ~ ~ ~  Data export ~ ~ ~ ~ ##########################################
    cat(paste("\nExport Data.\n\n"))

    ## . . gather all suspect points for export ----------------------------####
    suspect_choose  <- DATA_year$QCF_DIR != "good" | DATA_year$QCF_GLB != "good"
    SUS_DATA        <- DATA_year[suspect_choose,]
    SUS_DATA        <- SUS_DATA[order(SUS_DATA$Date30),]
    SUS_DATA_gather <- rbind(SUS_DATA_gather, SUS_DATA)

    # ## gather all suspect dates for export
    # daysinfo        <- SUS_DATA[,c("Date30","QCF_DIR","QCF_GLB")]
    # daysinfo$Day    <- as.Date(daysinfo$Date30)
    # daysinfo$Date30 <- NULL
    # daysinfo        <- daysinfo[order(daysinfo$Day),]
    # daysinfo        <- unique(daysinfo)
    # daysinfo_gather <- rbind(daysinfo_gather, daysinfo)


    ## . . Prepare main data for export ------------------------------------####

    ## Drop some columns ##
    DATA_year <- subset( DATA_year, select = c(-pressure,
                                               -Times,
                                               -ClrSW
    ))


    #'
    #' ## Do some filtering (data drop)
    #'
    #' ### 1. PHYSICALLY POSSIBLE LIMITS PER BSRN
    #' Drop all data with
    #' "Physical possible limit min (5)" or  "Physical possible limit max (6)"
    #'

    sel_d <- DATA_year$QCF_DIR %in% c("Physical possible limit min (5)", "Physical possible limit max (6)")
    sel_g <- DATA_year$QCF_GLB %in% c("Physical possible limit min (5)", "Physical possible limit max (6)")
    DATA_year$wattDIR[sel_d] <- NA
    DATA_year$wattGLB[sel_g] <- NA

    cat(sprintf(" %6d  %s\n", sum(sel_d, na.rm = T), "Direct Records removed due to: 'Physical possible limit min (5)' and 'Physical possible limit max (6)'"))
    cat(sprintf(" %6d  %s\n", sum(sel_g, na.rm = T), "Global Records removed due to: 'Physical possible limit min (5)' and 'Physical possible limit max (6)'"))


    #'
    #' ### 4. Climatological (configurable) Limits
    #'
    #' Drop all data with
    #' "Second climatological limit (16)"
    #'

    sel_d <- DATA_year$QCF_DIR %in% "Second climatological limit (16)"
    sel_g <- DATA_year$QCF_GLB %in% "Second climatological limit (16)"
    DATA_year$wattDIR[sel_d] <- NA
    DATA_year$wattGLB[sel_g] <- NA

    cat(sprintf(" %6d  %s\n", sum(sel_d, na.rm = T), "Direct Records removed due to: 'Second climatological limit (16)'"))
    cat(sprintf(" %6d  %s\n", sum(sel_g, na.rm = T), "Global Records removed due to: 'Second climatological limit (16)'"))

    #'
    #' ### 8. Clearness index test
    #'
    #' this values are mostly due to too low global values in retention with
    #' cos(SZA) and TSI
    #'

    sel_g <- DATA_year$QCF_GLB %in% c("Clearness index limit max (19)", "Clearness index limit min (20)")
    DATA_year$wattGLB[sel_g] <- NA
    cat(sprintf( " %6d  %s\n", sum(sel_g, na.rm = T), "Global Records removed due clearness index limits      (19) (20)'"))


    ## Drop empty records
    empty     <- is.na(DATA_year$wattDIR) & is.na(DATA_year$wattGLB)
    DATA_year <- DATA_year[ !empty, ]

    ## save data identification
    DATA_year <- DATA_year[ DATA_year$Date < LAST_DAY_EXPR , ]
    DATA_year <- DATA_year[ DATA_year$Date > PROJECT_START , ]


    ## . . Export main data ------------------------------------------------####
    if ( !TESTING & dim(DATA_year)[1] > 0 ) {
        write_RDS(object = DATA_year,
                  file   = paste0(OUTPUT_BASE, YY))
    }
    ##------------------------------------------------------------------------##


    ## . . Strict output for clear sky use ? -------------------------------####
    allow <- c( "good", "Possible Direct Obstruction (23)")
    sels  <- DATA_year$QCF_DIR %in% allow | DATA_year$QCF_GLB %in% allow
    STRICT_data <- DATA_year[sels,]

    STRICT_data <- subset(STRICT_data, select = c( -CHP1temp,
                                                   -CHP1tempSD,
                                                   -CHP1tempUNC,
                                                   -chp1TempCF,
                                                   -TSIextEARTH_comb
                                                   ))

    ## . . Export strict data ----------------------------------------------####
    if ( !TESTING & dim(STRICT_data)[1] > 0 ) {
        write_RDS(object = STRICT_data, paste0(OUTPUT_STRICT,YY)) }
    ##------------------------------------------------------------------------##

}

# ##-- Export a record of the bad data ---------------------------------------##
# if (!TESTING) myRtools::write_RDS( SUS_DATA_gather, SUSPECTS_EXP )
# ##--------------------------------------------------------------------------##



#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
