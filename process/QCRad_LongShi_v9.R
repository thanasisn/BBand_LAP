# /* #!/opt/R/4.2.3/bin/Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Radiation Quality Control **QCRad** "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
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
#'     toc_depth:        4
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
#' **QCRad**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
#' The chosen levels and filters have to be evaluated with the available data.
#'
#' For now this is a copy of
#' "~/RAD_QC/QCRad_LongShi_v8_id_CM21_CHP1.R" and
#' "~/RAD_QC/QCRad_LongShi_v8_apply_CM21_CHP1.R"
#'
#' ## Difference in implementation!
#'
#' Here the previous flags do not exclude the next.
#' In the original if a flag was set then the next filter will ignore the point.
#'
#' TODO:
#'
#' - plot combination of flag for each point
#' - plot cumulative graphs like the old
#' - Plot daily graphs with all available flags
#'
#+ echo=F, include=T

#+ echo=F, include=T
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"  )
knitr::opts_chunk$set(fig.align  = "center")
knitr::opts_chunk$set(fig.pos    = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/process/QCRad_LongShi_v9.R"
qc_ver      <- 9

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")
mylock(DB_lock)


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(scales,     warn.conflicts = FALSE, quietly = TRUE)


##  Variables  -----------------------------------------------------------------
sun_elev_min     <-  -2 * 0.103  ## Drop ALL radiation data when sun is below this point


##  Execution control  ---------------------------------------------------------

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
TEST_07  <- TRUE  ## TODO
TEST_08  <- TRUE
TEST_09  <- TRUE

## mostly for daily plots
DO_PLOTS     <- TRUE
if (interactive()) {
    DO_PLOTS <- FALSE
}

# TEST
DO_PLOTS <- TRUE

## __ Select a part of data to plot  -------------------------------------------
PARTIAL    <- FALSE
PARTIAL    <- TRUE
PLOT_FIRST <- as_date("2022-01-01")
PLOT_LAST  <- as_date("2024-03-31")

## gather configurations for quality control
QS <<- list()

# ##  Create a test database  ----------------------------------------------------
# TEST_DB <- TRUE
# if (TEST_DB) {
#     source("~/BBand_LAP/DEFINITIONS.R")
#     cat("\n * * * Using a temp DB * * * \n\n")
#     ## copy data to temp
#     tyear <- 2017
#     dir.create(test_DB_DIR, showWarnings = FALSE, recursive = TRUE)
#     system(paste( "cp -rv --update ", DB_HASH_fl, test_DB_HASH_fl))
#     system(paste( "cp -rv --update ", DB_META_fl, test_DB_META_fl))
#     system(paste0("rsync -avr ", DB_DIR, "/", tyear, "/ ", test_DB_DIR, "/", tyear))
#     ## replace paths with test paths
#     DB_DIR     <- test_DB_DIR
#     DB_lock    <- test_DB_lock
#     DB_META_fl <- test_DB_META_fl
#     DB_HASH_fl <- test_DB_HASH_fl
# }



##  Create a new variable to the whole database  -------------------------------

## use this columns as indicator
## make it NA to reprocess all
InitVariableBBDB("QCv9_01_dir_flag", as.character(NA))

# OVERWRITEVariableBBDB("QCv9_01_dir_flag", as.character(NA))

## list data base files
filelist <- data.table(
    names = list.files(DB_DIR,
                       pattern = "*.parquet",
                       recursive  = TRUE,
                       full.names = TRUE))
dd      <- dirname(filelist$names)
dd      <- tstrsplit(dd, "/")

filelist$flmonth <- as.numeric(unlist(dd[length(dd)]))
filelist$flyear  <- as.numeric(unlist(dd[length(dd)-1]))


## find what needs touching
BB <- opendata()
temp_to_do <- data.table(BB |>
                             filter(is.na(QCv9_01_dir_flag)) |>
                             select(year, month) |>
                             unique()            |>
                             collect()
)
rm(BB)

## select what data set files to touch
filelist <- filelist[temp_to_do, on = .(flmonth = month, flyear = year)]
rm(temp_to_do, dd)



## ~ ~ Apply Filters ~ ~  ------------------------------------------------------
#'
#' # Apply Filters
#'
#' Go through all files in data base and apply flags.
#'
#+ include=T
for (af in filelist$names) {
    datapart <- data.table(read_parquet(af))
    datapart[, month := as.integer(month(Date))]
    datapart[, year  := as.integer(year(Date)) ]

    cat("Load: ", af, "\n")

    ##  Create strict radiation data  ------------------------------------------

    ## __ Daytime radiation only  ----------------------------------------------

    ## use this as a general processing marker for this script
    datapart$QCv9_01_dir_flag <- "pass"

    ## Direct beam DNI
    datapart[Elevat > sun_elev_min           &
                 is.na(chp1_bad_data_flag)   &
                 Async_tracker_flag == FALSE,
             DIR_strict := DIR_wpsm]
    ## DHI
    datapart[Elevat > sun_elev_min           &
                 is.na(chp1_bad_data_flag)   &
                 Async_tracker_flag == FALSE,
             HOR_strict := HOR_wpsm]
    ## GHI
    datapart[Elevat > sun_elev_min           &
                 is.na(cm21_bad_data_flag),
             GLB_strict := GLB_wpsm]

    ## __ Negative radiation to zero  ------------------------------------------
    datapart[DIR_strict < 0, DIR_strict := 0]
    datapart[HOR_strict < 0, HOR_strict := 0]
    datapart[GLB_strict < 0, GLB_strict := 0]

    ## __ Diffuse radiation  ---------------------------------------------------
    ## DHI = GHI – DNI cos(z)
    datapart[, DIFF_strict := GLB_strict - HOR_strict]

    ## __ Clearness Index  -----------------------------------------------------
    datapart[, ClearnessIndex_kt := GLB_strict / (cosde(SZA) * TSI_TOA)]

    ## __ Diffuse fraction  ----------------------------------------------------
    datapart[, DiffuseFraction_kd := DIFF_strict / GLB_strict]

    ## replace infinite values
    datapart[is.infinite(DiffuseFraction_kd), DiffuseFraction_kd := NA]



    ## 1. Physically possible limits per BSRN  ---------------------------------
    #'
    #' ## 1. Physically possible limits per BSRN
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

    QS$dir_SWdn_min <-  -4    # Minimum direct value to consider valid measurement
    QS$dir_SWdn_dif <- 327    # Closeness to to TSI
    QS$glo_SWdn_min <-  -4    # Minimum global value to consider valid measurement
    QS$glo_SWdn_off <- 160    # Global departure offset above the model
    QS$glo_SWdn_amp <-   1.3  # Global departure factor above the model

    if (TEST_01) {
        testN        <- 1
        flagname_DIR <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_dir_flag")
        flagname_GLB <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_glb_flag")
        cat(paste("\n1. Physically Possible Limits", flagname_DIR, flagname_GLB, "\n\n"))

        InitVariableBBDB(flagname_DIR, as.character(NA))
        InitVariableBBDB(flagname_GLB, as.character(NA))

        ## __ Direct  ----------------------------------------------------------
        datapart[DIR_strict < QS$dir_SWdn_min,
                 (flagname_DIR) := "Physical possible limit min (5)"]
        datapart[TSI_TOA - DIR_strict < QS$dir_SWdn_dif,
                 (flagname_DIR) := "Physical possible limit max (6)"]

        ## __ Global  ----------------------------------------------------------
        datapart[GLB_strict < QS$glo_SWdn_min,
                 (flagname_GLB) := "Physical possible limit min (5)"]
        datapart[, Glo_max_ref := TSI_TOA * QS$glo_SWdn_amp * cosde(SZA)^1.2 + QS$glo_SWdn_off]
        datapart[GLB_strict > Glo_max_ref,
                 (flagname_GLB) := "Physical possible limit max (6)"]

        rm(list = ls(pattern = "flagname_.*"))
        dummy <- gc()
    }



    ## 2. Extremely rare limits per BSRN  --------------------------------------
    #'
    #' ## 2. Extremely rare limits per BSRN
    #'
    #' These should be a little more restrictive than 1. in order to start
    #' catching erroneous values.
    #'
    #' The choose of those settings may be optimized with an iterative process.

    # Upper modeled values
    QS$Dir_SWdn_amp     <-    0.91  # Direct departure factor above the model
    QS$Dir_SWdn_off     <- -140     # Direct departure offset above the model
    QS$Glo_SWdn_amp     <-    1.18  # Global departure factor above the model
    QS$Glo_SWdn_off     <-   40     # Global departure offset above the model
    # Minimum accepted values
    QS$dir_SWdn_min_ext <-   -2     # Extremely Rare Minimum Limits
    QS$glo_SWdn_min_ext <-   -2     # Extremely Rare Minimum Limits
    # Ignore too low values near horizon
    QS$dir_SWdn_too_low <-    3     # Ideal w/m^2
    QS$glo_SWdn_too_low <-    3     # Ideal w/m^2

    if (TEST_02) {
        testN        <- 2
        flagname_DIR <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_dir_flag")
        flagname_GLB <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_glb_flag")
        cat(paste("\n2. Extremely Rare Limits", flagname_DIR, flagname_GLB, "\n\n"))

        InitVariableBBDB(flagname_DIR, as.character(NA))
        InitVariableBBDB(flagname_GLB, as.character(NA))

        # Compute reference values
        datapart[, Direct_max := TSI_TOA * QS$Dir_SWdn_amp * cosde(SZA)^0.2 + QS$Dir_SWdn_off]
        datapart[, Global_max := TSI_TOA * QS$Glo_SWdn_amp * cosde(SZA)^1.2 + QS$Glo_SWdn_off]
        # Ignore too low values near horizon
        datapart[Direct_max < QS$dir_SWdn_too_low, Direct_max := NA]
        datapart[Global_max < QS$glo_SWdn_too_low, Direct_max := NA]

        ## __ Direct  ----------------------------------------------------------
        datapart[DIR_strict < QS$dir_SWdn_min_ext,
                 (flagname_DIR) := "Extremely rare limits min (3)"]
        datapart[DIR_strict > Direct_max,
                 (flagname_DIR) := "Extremely rare limits max (4)"]

        ## __ Global  ----------------------------------------------------------
        datapart[GLB_strict < QS$glo_SWdn_min_ext,
                 (flagname_GLB) := "Extremely rare limits min (3)"]
        datapart[GLB_strict > Global_max,
                 (flagname_GLB) := "Extremely rare limits max (4)"]

        rm(list = ls(pattern = "flagname_.*"))
        dummy <- gc()
    }



    ## 3. Comparison tests per BSRN “non-definitive”  --------------------------
    #'
    #' ## 3. Comparison tests per BSRN “non-definitive”
    #'

    QS$dif_rati_po1  <-  0.03  # DiffuceFraction low limit
    QS$dif_rati_po2  <-  0.08  # My DiffuceFraction low limit
    QS$dif_sza_break <- 75     # SZA break point
    QS$dif_rati_pr1  <-  1.03  # DiffuceFraction upper limit
    QS$dif_rati_pr2  <-  1.06  # My DiffuceFraction upper limit
    QS$dif_watt_lim  <- 10     # Filter only when GLB is above that

    if (TEST_03) {
        testN        <- 3
        flagname_UPP <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_upp_flag")
        flagname_LOW <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_low_flag")
        cat(paste("\n3. Comparison tests", flagname_UPP, flagname_LOW, "\n\n"))

        InitVariableBBDB(flagname_UPP, as.character(NA))
        InitVariableBBDB(flagname_LOW, as.character(NA))

        ## __ Proposed filter  -------------------------------------------------
        datapart[DiffuseFraction_kd  > QS$dif_rati_pr1  &
                     SZA            <= QS$dif_sza_break &
                     GLB_strict      > QS$dif_watt_lim,
                 (flagname_UPP) := "Diffuse ratio comp max (11)"]
        datapart[DiffuseFraction_kd  > QS$dif_rati_pr2  &
                     SZA             > QS$dif_sza_break &
                     GLB_strict      > QS$dif_watt_lim,
                 (flagname_UPP) := "Diffuse ratio comp max (11)"]

        ## __ Extra filters by me  ---------------------------------------------
        datapart[DiffuseFraction_kd  < QS$dif_rati_po1  &
                     SZA            <= QS$dif_sza_break &
                     GLB_strict      > QS$dif_watt_lim,
                 (flagname_LOW) := "Diffuse ratio comp min (12)"]
        datapart[DiffuseFraction_kd  < QS$dif_rati_po2  &
                     SZA             > QS$dif_sza_break &
                     GLB_strict      > QS$dif_watt_lim,
                 (flagname_LOW) := "Diffuse ratio comp min (12)"]

        rm(list = ls(pattern = "flagname_.*"))
        dummy <- gc()
    }



    ## 4. Climatological (configurable) Limits  --------------------------------
    #'
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

    QS$clim_lim_C3 <- 0.77
    QS$clim_lim_D3 <- 0.81
    QS$clim_lim_C1 <- 1.14
    QS$clim_lim_D1 <- 1.32

    if (TEST_04) {
        testN        <- 4
        flagname_DIR <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_dir_flag")
        flagname_GLB <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_glb_flag")
        cat("\n4. Climatological (configurable) Limits", flagname_DIR, flagname_GLB, "\n\n")

        InitVariableBBDB(flagname_DIR, as.character(NA))
        InitVariableBBDB(flagname_GLB, as.character(NA))

        ## __ Direct -----------------------------------------------------------
        datapart[, Dir_First_Clim_lim := TSI_TOA * QS$clim_lim_C3 * cosde(SZA)^0.2 + 10]
        datapart[DIR_strict > Dir_First_Clim_lim,
                 (flagname_DIR) := "First climatological limit (17)"]
        datapart[, Dir_Secon_Clim_lim := TSI_TOA * QS$clim_lim_D3 * cosde(SZA)^0.2 + 15]
        datapart[DIR_strict > Dir_Secon_Clim_lim,
                 (flagname_DIR) := "Second climatological limit (16)"]

        ## __ Global -----------------------------------------------------------
        datapart[, Glo_First_Clim_lim := TSI_TOA * QS$clim_lim_C1 * cosde(SZA)^1.2 + 60]
        datapart[GLB_strict > Glo_First_Clim_lim,
                 (flagname_GLB) := "First climatological limit (17)"]
        datapart[, Glo_Secon_Clim_lim := TSI_TOA * QS$clim_lim_D1 * cosde(SZA)^1.2 + 60]
        datapart[GLB_strict > Glo_Secon_Clim_lim,
                 (flagname_GLB) := "Second climatological limit (16)"]

        rm(list = ls(pattern = "flagname_.*"))
        dummy <- gc()
    }



    ## 5. Tracker is off test  -------------------------------------------------
    #'
    #' ## 5. Tracker is off test
    #'
    #' This test use a diffuse model. A better one will be implemented when one
    #' is produced and accepted.
    #'

    ## criteria
    QS$Tracking_min_elev <-    5
    QS$ClrSW_lim         <-    0.85
    QS$glo_min           <-   25
    ## Global Clear SW model
    QS$ClrSW_a           <- 1050.5
    QS$ClrSW_b           <-    1.095

    if (TEST_05) {
        testN        <- 5
        flagname_DIR <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_dir_flag")
        cat(paste("\n5. Tracking test", flagname_DIR, "\n\n"))

        InitVariableBBDB(flagname_DIR, as.character(NA))

        ## Clear Sky Sort-Wave model
        datapart[, ClrSW_ref2 := (QS$ClrSW_a / Sun_Dist_Astropy^2) * cosde(SZA)^QS$ClrSW_b]

        ## __ Direct -----------------------------------------------------------
        datapart[GLB_strict  / ClrSW_ref2 > QS$ClrSW_lim &
                 DIFF_strict / GLB_strict > QS$ClrSW_lim &
                 GLB_strict               > QS$glo_min   &
                 Elevat                   > QS$Tracking_min_elev,
             (flagname_DIR) := "Possible no tracking (24)"]

        rm(list = ls(pattern = "flagname_.*"))
        dummy <- gc()
    }



    ## 6. Rayleigh Limit Diffuse Comparison  -----------------------------------
    #'
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

    # criteria
    QS$Rayleigh_upper_lim <- 500    # Upper departure diffuse limit
    QS$Rayleigh_lower_lim <-  -3    # Lower departure diffuse limit
    QS$Rayleigh_dif_glo_r <-   0.8  # Low limit diffuse/global < threshold
    QS$Rayleigh_glo_min   <-  50    # Low limit minimum global

    # Reference model
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
                    f * mu_0 * Pressure)
    }

    if (TEST_06) {
        cat(paste("\n6. Rayleigh Limit Diffuse Comparison.\n\n"))

        testN        <- 6
        flagname_BTH <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_bth_flag")

        InitVariableBBDB(flagname_BTH, as.character(NA))


        datapart[, RaylDIFF  := Rayleigh_diff(SZA = SZA, Pressure = Pressure)]

        ## __ Both  ------------------------------------------------------------
        datapart[DIFF_strict - RaylDIFF > QS$Rayleigh_upper_lim,
                 (flagname_BTH) := "Rayleigh diffuse limit (18)"]
        datapart[DIFF_strict - RaylDIFF < QS$Rayleigh_lower_lim,
                 (flagname_BTH) := "Rayleigh diffuse limit (18)"]

        rm(list = ls(pattern = "flagname_.*"))
        dummy <- gc()
    }



    ## 7. Test for obstacles  --------------------------------------------------
    #'
    #' ## 7. Test for obstacles
    #'
    #' This is deactivated
    #'
    if (TEST_07) {
        cat(paste("\n7. Obstacles test.\n\n"))

        ## . . . Direct --------------------------------------------------------

        # source("./QCRad_Obstacles_definition_v2.R")

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



    ## 8. Test for inverted values  --------------------------------------------
    #'
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

    QS$dir_glo_invert  <- 5  # Diffuse Inversion test: DIRhor - GLBhor > lim[%]
    QS$dir_glo_glo_off <- 5  # Diffuse Inversion test: apply for GLBhor > offset

    if (TEST_08) {
        cat(paste("\n8. Inversion test.\n\n"))

        testN        <- 8
        flagname_BTH <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_bth_flag")

        InitVariableBBDB(flagname_BTH, as.character(NA))

        ## __ Both  ------------------------------------------------------------
        datapart[, Relative_diffuse := 100 * (HOR_strict  - GLB_strict) / GLB_strict ]
        datapart[ is.infinite(Relative_diffuse), Relative_diffuse := NA]

        datapart[Relative_diffuse > QS$dir_glo_invert  &
                 GLB_strict       > QS$dir_glo_glo_off,
                 (flagname_BTH) := "Direct > global soft (14)"]
        datapart[Relative_diffuse > QS$dir_glo_invert,
                 (flagname_BTH) := "Direct > global hard (15)" ]

        rm(list = ls(pattern = "flagname_.*"))
        dummy <- gc()
    }



    ## 9. Clearness index test  ------------------------------------------------
    #'
    #' ## 9. Clearness index test
    #'
    #' This filter is mine, and is applied on GHI data.
    #'
    #' Data near elevation 0 are caused by the cos(SZA) while calculating
    #' kt = GLB / (cos(sza) * TSI).
    #'
    #' For larger elevation angles manual inspection is needed.

    QS$CL_idx_max <-  1.13   # Upper Clearness index accepted level
    QS$CL_idx_min <- -0.001  # Lower Clearness index accepted level
    QS$CL_idx_ele <-  8      # Apply for elevations above this angle

    if (TEST_09) {
        cat(paste("\n9. Clearness index (global/TSI) test.\n\n"))

        testN        <- 9
        flagname_GLB <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_glb_flag")

        InitVariableBBDB(flagname_GLB, as.character(NA))

        ## __ Global  ----------------------------------------------------------
        datapart[ClearnessIndex_kt > QS$CL_idx_max & Elevat > QS$CL_idx_ele,
             (flagname_GLB) := "Clearness index limit max (19)" ]
        datapart[ClearnessIndex_kt < QS$CL_idx_min & Elevat > QS$CL_idx_ele,
             (flagname_GLB) := "Clearness index limit min (20)" ]

        rm(list = ls(pattern = "flagname_.*"))
        dummy <- gc()
    }

    summary(datapart)

    ## store actual data
    datapart <- as_tibble(datapart)
    write_parquet(x = datapart, sink = af)
    cat("Save: ", af, "\n\n")

    ## store filters parameters
    saveRDS(object = QS,
            file   = sub("\\.R", "_parameters.Rds",Script.Name))

    ## clean
    rm(datapart)
    dummy <- gc()
}
#+ include=T, echo=F
myunlock(DB_lock)



## ~ ~ Inspect quality control results ~ ~ -------------------------------------
#'
#' # Inspect quality control results
#'
#+ include=T, echo=F


## open data base for plots
BB <- opendata()
## load filter parameters
QS <- readRDS(sub("\\.R", "_parameters.Rds",Script.Name))

## __ Part of data we care for  ------------------------------------------------
if (PARTIAL == TRUE) {
    BB <- BB |> filter(as_date(Date) >= PLOT_FIRST &
                       as_date(Date) <= PLOT_LAST) |>
        compute()

    cat("\n\n PARTIAL PLOT ", format(PLOT_FIRST), "--", format(PLOT_LAST), "\n\n")
}



####  1. Physically possible limits per BSRN  ----------------------------------
#' \FloatBarrier
#' \newpage
#' ## 1. Physically possible limits per BSRN
#'
#+ echo=F, include=T, results="asis"
if (TEST_01) {

    testN        <- 1
    flagname_DIR <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_dir_flag")
    flagname_GLB <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_glb_flag")

    cat(pander(table(collect(select(BB, !!flagname_DIR)), useNA = "always"),
               caption = flagname_DIR))
    cat(" \n \n")

    cat(pander(table(collect(select(BB, !!flagname_GLB)), useNA = "always"),
               caption = flagname_GLB))
    cat(" \n \n")

    ## TODO display limits on graphs
    test <- BB |>
        mutate(test = TSI_TOA - DIR_strict) |>
        select(test) |> collect()

    cat("\n", range(test$test, na.rm = T), "\n")

    hist(test$test, breaks = 100,
         main = "TSI_TOA - DIR_strict",
         xlab = "")
    cat(" \n \n")


    ## TODO display limits on graphs
    test <- BB |>
        mutate(test = Glo_max_ref - GLB_strict) |>
        select(test) |> collect()

    cat("\n", range(test$test, na.rm = T), "\n")

    hist(test$test, breaks = 100,
         main = "Glo_max_ref - GLB_strict",
         xlab = "")
    cat(" \n \n")


    if (DO_PLOTS) {

        if (!interactive()) {
            pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V", qc_ver, "_F", testN, ".pdf"))
        }

        test <- BB |> filter(!QCv9_01_dir_flag %in% c(NA, "pass")) |> collect() |> as.data.table()
        ## TODO
        if (nrow(test) == 0) {
            cat("\nNO CASES FOR DIRECT QCv9_01_dir_flag\n\n")
        }
        for (ad in sort(unique(as.Date(test$Date)))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )
            ylim <- range(pp$TSI_TOA - QS$dir_SWdn_dif, pp$DIR_strict, na.rm = T)
            plot(pp$Date, pp$DIR_strict, "l", col = "blue",
                 ylim = ylim, xlab = "", ylab = "DIR_strict")

            title(paste("#1", as.Date(ad, origin = "1970-01-01")))

            ## plot limits
            lines(pp$Date, pp$TSI_TOA - QS$dir_SWdn_dif, col = "red")
            ## mark offending data
            points(pp[!is.na(get(flagname_DIR)), DIR_strict, Date],
                   col = "red", pch = 1)
        }

        ## Plot Global radiation
        test <- BB |> filter(!is.na(QCv9_01_glb_flag) ) |> collect() |> as.data.table()
        if (nrow(test) == 0) {
            cat("\nNO CASES FOR GLOBAL QCv9_01_glb_flag\n\n")
        }
        for (ad in sort(unique(as.Date(c(test$Date))))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )
            ylim <- range(pp$Glo_max_ref, pp$GLB_strict, na.rm = T)
            plot(pp$Date, pp$GLB_strict, "l", col = "green",
                 ylim = ylim, xlab = "", ylab = "GLB")
            title(paste("#1", as.Date(ad, origin = "1970-01-01")))
            ## plot limits
            lines(pp$Date, pp$Glo_max_ref, col = "red")
            ## mark offending data
            points(pp[!is.na(get(flagname_GLB)), GLB_strict, Date],
                   col = "red", pch = 1)
        }
    }
    rm(list = ls(pattern = "flagname_.*"))
    dummy <- gc()
    if (!interactive()) dummy <- dev.off()
}
#+ echo=F, include=T



####  2. Extremely rare limits per BSRN  ---------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 2. Extremely rare limits per BSRN
#'
#+ echo=F, include=T, results="asis"
if (TEST_02) {

    testN        <- 2
    flagname_DIR <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_dir_flag")
    flagname_GLB <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_glb_flag")

    cat(pander(table(collect(select(BB, !!flagname_DIR)), useNA = "always"),
               caption = flagname_DIR))
    cat(" \n \n")

    cat(pander(table(collect(select(BB, !!flagname_GLB)), useNA = "always"),
               caption = flagname_GLB))
    cat("\n \n")

    test <- BB |>
        mutate(dir = Direct_max - DIR_strict,
               glo = Global_max - GLB_strict) |>
        select(dir, glo) |> collect()

    cat("\n", range(test$dir, na.rm = TRUE), "\n")

    hist(test$dir, breaks = 100,
         main = "Direct_max - DIR_strict")
    abline(v = QS$dir_SWdn_too_low)
    abline(v = QS$dir_SWdn_min_ext, col = "red")
    cat("\n \n")

    cat("\n", range(test$glo, na.rm = TRUE), "\n")

    hist(test$glo, breaks = 100,
         main = "Global_max - GLB_strict")
    abline(v = QS$glo_SWdn_too_low)
    abline(v = QS$glo_SWdn_min_ext, col = "red")
    cat("\n \n")

    if (DO_PLOTS) {

        if (!interactive()) {
            pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V", qc_ver, "_F", testN, ".pdf"))
        }

        ## Direct
        test <- BB |> filter(!is.na(QCv9_02_dir_flag)) |> collect() |> as.data.table()
        for (ad in sort(unique(as.Date(test$Date)))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )
            ylim <- range(pp$Direct_max, pp$DIR_strict, na.rm = T)
            plot(pp$Date, pp$DIR_strict, "l", col = "blue",
                 ylim = ylim, xlab = "", ylab = "wattDIR")
            title(paste("#2", as.Date(ad, origin = "1970-01-01"),
                        "N:", pp[!is.na(QCv9_02_dir_flag), .N]))
            ## plot limits
            lines(pp$Date, pp$Direct_max, col = "red")
            ## mark offending data
            points(pp[!is.na(QCv9_02_dir_flag), DIR_strict, Date],
                   col = "red", pch = 1)
        }

        ## Global
        test <- BB |> filter(!is.na(QCv9_02_glb_flag)) |> collect() |> as.data.table()
        for (ad in sort(unique(as.Date(c(test$Date))))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )
            ylim <- range(pp$Global_max, pp$GLB_strict, na.rm = T)
            plot(pp$Date, pp$GLB_strict, "l", col = "green",
                 ylim = ylim, xlab = "", ylab = "GLB")
            title(paste("#2", as.Date(ad, origin = "1970-01-01"),
                        "N:", pp[!is.na(QCv9_02_glb_flag), .N] ))
            ## plot limits
            lines(pp$Date, pp$Global_max, col = "red")
            ## mark offending data
            points(pp[!is.na(QCv9_02_glb_flag), GLB_strict, Date],
                   col = "magenta", pch = 1)
        }
    }
    rm(list = ls(pattern = "flagname_.*"))
    dummy <- gc()
    if (!interactive()) dummy <- dev.off()
}
#+ echo=F, include=T



####  3. Comparison tests per BSRN “non-definitive”  ---------------------------
#' \FloatBarrier
#' \newpage
#' ## 3. Comparison tests per BSRN “non-definitive”
#'
#+ echo=F, include=T, results="asis"
if (TEST_03) {

    testN        <- 3
    flagname_UPP <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_upp_flag")
    flagname_LOW <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_low_flag")

    cat(pander(table(collect(select(BB, !!flagname_UPP)), useNA = "always"),
               caption = flagname_UPP))
    cat(" \n \n")

    cat(pander(table(collect(select(BB, !!flagname_LOW)), useNA = "always"),
               caption = flagname_LOW))
    cat(" \n \n")

    years <- (BB |> filter(!is.na(DiffuseFraction_kd)) |>
                  select(year) |> unique() |> collect() |> pull())
    for (ay in years) {
        pp <- data.table(BB |> filter(year(Date) == ay & Elevat > 0) |> collect())
        ylim <- c(-30, 1.1)
        ylim <- c(-0.5, 1.5)

        par(mar = c(4, 4, 2, 1))

        ## plot limits by SZA
        plot(pp$SZA, pp$DiffuseFraction_kd,
             ylab = "Not Diffuse fraction", xlab = "SZA", ylim = ylim,
             cex = .1)
        title(paste("#3", ay))

        segments(               0, QS$dif_rati_pr1, QS$dif_sza_break, QS$dif_rati_pr1, col = "red" )
        segments(QS$dif_sza_break, QS$dif_rati_pr2,               93, QS$dif_rati_pr2, col = "red" )

        segments(               0, QS$dif_rati_po1, QS$dif_sza_break, QS$dif_rati_po1, col = "blue" )
        segments(QS$dif_sza_break, QS$dif_rati_po2,               93, QS$dif_rati_po2, col = "blue" )

        points(pp[!is.na(get(flagname_UPP)), DiffuseFraction_kd, SZA],
               cex = .2, col = "red")
        points(pp[!is.na(get(flagname_LOW)), DiffuseFraction_kd, SZA],
               cex = .2, col = "cyan")
        cat(" \n \n")

        ## plot limits by date
        plot(pp$Date, pp$DiffuseFraction_kd,
             ylab = "Not Diffuse fraction", xlab = "SZA", ylim = ylim,
             cex = .1)
        title(paste("#3", ay))

        points(pp[!is.na(get(flagname_UPP)), DiffuseFraction_kd, Date],
               cex = .2, col = "red")
        points(pp[!is.na(get(flagname_LOW)), DiffuseFraction_kd, Date],
               cex = .2, col = "cyan")
        cat(" \n \n")

        ## plot limits by Azimuth
        plot(pp$Azimuth, pp$DiffuseFraction_kd,
             ylim = ylim,
             ylab = "Not Diffuse fraction", xlab = "Azimuth",
             cex = .1)
        title(paste("#3", ay))

        points(pp[!is.na(get(flagname_UPP)), DiffuseFraction_kd, Azimuth],
               cex = .2, col = "red")
        points(pp[!is.na(get(flagname_LOW)), DiffuseFraction_kd, Azimuth],
               cex = .2, col = "cyan")
        cat(" \n \n")
    }

    if (DO_PLOTS) {

        if (!interactive()) {
            pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V", qc_ver, "_F", testN, ".pdf"))
        }

        tmp <- BB |> filter(!is.na(QCv9_03_upp_flag) | !is.na(QCv9_03_low_flag)) |> collect() |> as.data.table()

        for (ad in sort(unique(c(as.Date(tmp$Date))))) {

            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )

            layout(matrix(c(1, 2), 2, 1, byrow = TRUE))
            par(mar = c(2,4,2,1))

            plot(pp$Date, pp$DiffuseFraction_kd, "l",
                 col = "cyan", ylab = "Not Diffuse Fraction", xlab = "")

            abline(h = QS$dif_rati_pr1, col = "red")
            abline(h = QS$dif_rati_pr2, col = "red", lty = 2)
            abline(h = QS$dif_rati_po1, col = "blue")
            abline(h = QS$dif_rati_po2, col = "blue", lty = 2)

            title(paste("#3", as.Date(ad, origin = "1970-01-01")))

            par(mar = c(2, 4, 1, 1))
            ylim <- range(pp$GLB_strict, pp$DIR_strict, na.rm = T)
            plot( pp$Date, pp$GLB_strict, "l",
                  ylim = ylim, col = "green", ylab = "", xlab = "")
            lines(pp$Date, pp$DIR_strict, col = "blue" )

            points(pp[!is.na(QCv9_03_upp_flag), Date],
                   pp[!is.na(QCv9_03_upp_flag), DIR_strict],
                   ylim = ylim, col = "red")
            points(pp[!is.na(QCv9_03_upp_flag), Date],
                   pp[!is.na(QCv9_03_upp_flag), GLB_strict],
                   ylim = ylim, col = "red")
            points(pp[!is.na(QCv9_03_low_flag), Date],
                   pp[!is.na(QCv9_03_low_flag), DIR_strict],
                   ylim = ylim, col = "magenta")
            points(pp[!is.na(QCv9_03_low_flag), Date],
                   pp[!is.na(QCv9_03_low_flag), GLB_strict],
                   ylim = ylim, col = "magenta")
            ## reset layout
            layout(1)
        }
    }
    rm(list = ls(pattern = "flagname_.*"))
    dummy <- gc()
    if (!interactive()) dummy <- dev.off()
}
#+ echo=F, include=T



####  4. Climatological (configurable) Limits  ---------------------------------
#' \FloatBarrier
#' \newpage
#' ## 4. Climatological (configurable) Limits
#'
#+ echo=F, include=T, results="asis"
if (TEST_04) {

    testN        <- 4
    flagname_DIR <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_dir_flag")
    flagname_GLB <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_glb_flag")

    cat(pander(table(collect(select(BB, !!flagname_DIR)), useNA = "always"),
               caption = flagname_DIR))
    cat(" \n \n")

    cat(pander(table(collect(select(BB, !!flagname_GLB)), useNA = "always"),
               caption = flagname_GLB))
    cat(" \n \n")

    test <- data.table(BB |>
                           filter(!is.na(get(flagname_DIR)) |
                                      !is.na(get(flagname_GLB))) |>
                           select(Date,
                                  DIR_strict,
                                  GLB_strict,
                                  Dir_First_Clim_lim, Dir_Secon_Clim_lim,
                                  Glo_First_Clim_lim, Glo_Secon_Clim_lim,
                                  !!flagname_DIR, !!flagname_GLB) |>
                           collect())

    hist(test[, DIR_strict - Dir_First_Clim_lim], breaks = 100,
         main = "Departure Direct from first climatological limti")
    cat(" \n \n")

    hist(test[, DIR_strict - Dir_Secon_Clim_lim], breaks = 100,
         main = "Departure Direct from second climatological limit")
    cat(" \n \n")

    hist(test[, GLB_strict - Glo_First_Clim_lim], breaks = 100,
         main = "Departure Direct from first climatological limti")
    cat(" \n \n")

    hist(test[, GLB_strict - Glo_Secon_Clim_lim], breaks = 100,
         main = "Departure Direct from second climatological limit")
    cat(" \n \n")


    ## Yearly plots for Direct
    years <- (BB |> filter(!is.na(DIR_wpsm)) |>
                  select(year) |> unique() |> collect() |> pull())

    ## common scale
    vars <- c("Dir_First_Clim_lim", "Dir_Secon_Clim_lim", "DIR_wpsm")
    ylim <- c(BB |> summarise(across(all_of(vars), ~ min(., na.rm = T))) |> collect() |> min(),
              BB |> summarise(across(all_of(vars), ~ max(., na.rm = T))) |> collect() |> max())

    for (ay in years) {
        pp <- data.table(BB |> filter(year(Date) == ay & Elevat > 0) |> collect())

        ## plot direct by SZA
        plot(pp$SZA, pp$DIR_wpsm,
             cex  = .1,
             ylim = ylim,
             xlab = "SZA",
             ylab = "Direct Irradiance")

        ## 4. Second climatological limit (16)
        points(pp$SZA, pp$Dir_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
        ## 4. First climatological limit (17)
        points(pp$SZA, pp$Dir_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

        ## plot flagged
        points(pp[QCv9_04_dir_flag == "First climatological limit (17)",  DIR_wpsm, SZA], cex = .7, col = "cyan"   )
        points(pp[QCv9_04_dir_flag == "Second climatological limit (16)", DIR_wpsm, SZA], cex = .7, col = "magenta")

        title(main = paste("Direct Beam climatological test 4.", ay))
        legend("topright",
               legend = c("Direct measurements", "Second limit", "First limit", "First hit", "Second hit" ),
               col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")


        ## plot direct by Azimuth
        plot(pp$Azimuth, pp$DIR_wpsm,
             cex  = .1,
             ylim = ylim,
             xlab = "Azimuth",
             ylab = "Direct Irradiance")

        ## 4. Second climatological limit (16)
        points(pp$Azimuth, pp$Dir_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
        ## 4. First climatological limit (17)
        points(pp$Azimuth, pp$Dir_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

        ## plot flagged
        points(pp[QCv9_04_dir_flag == "First climatological limit (17)",  DIR_wpsm, Azimuth], cex = .7, col = "cyan"   )
        points(pp[QCv9_04_dir_flag == "Second climatological limit (16)", DIR_wpsm, Azimuth], cex = .7, col = "magenta")

        title(main = paste("Direct Beam climatological test 4.", ay))
        legend("topright",
               legend = c("Direct measurements", "Second limit", "First limit", "First hit", "Second hit" ),
               col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")


        ## plot direct by Date
        plot(pp$Date, pp$DIR_wpsm,
             cex  = .1,
             ylim = ylim,
             xlab = "",
             ylab = "Direct Irradiance")

        ## 4. Second climatological limit (16)
        points(pp$Date, pp$Dir_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
        ## 4. First climatological limit (17)
        points(pp$Date, pp$Dir_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

        ## plot flagged
        points(pp[QCv9_04_dir_flag == "First climatological limit (17)",  DIR_wpsm, Date], cex = .7, col = "cyan"   )
        points(pp[QCv9_04_dir_flag == "Second climatological limit (16)", DIR_wpsm, Date], cex = .7, col = "magenta")

        title(main = paste("Direct Beam climatological test 4.", ay))
        legend("topright",
               legend = c("Direct measurements", "Second limit", "First limit", "First hit", "Second hit" ),
               col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")
    }


    ## Yearly plots for Global
    years <- (BB |> filter(!is.na(GLB_wpsm)) |>
                  select(year) |> unique() |> collect() |> pull())

    ## common scale
    vars <- c("Glo_First_Clim_lim", "Glo_Secon_Clim_lim", "GLB_wpsm")
    ylim <- c(BB |> summarise(across(all_of(vars), ~ min(., na.rm = T))) |> collect() |> min(),
              BB |> summarise(across(all_of(vars), ~ max(., na.rm = T))) |> collect() |> max())

    for (ay in years) {
        pp <- data.table(BB |> filter(year(Date) == ay & Elevat > 0) |> collect())

        ## plot direct by SZA
        plot(pp$SZA, pp$GLB_wpsm,
             cex  = .1,
             ylim = ylim,
             xlab = "SZA",
             ylab = "Global Irradiance")

        ## 4. Second climatological limit (16)
        points(pp$SZA, pp$Glo_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
        ## 4. First climatological limit (17)
        points(pp$SZA, pp$Glo_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

        ## plot flagged
        points(pp[QCv9_04_glb_flag == "First climatological limit (17)",  GLB_wpsm, SZA], cex = .7, col = "cyan"   )
        points(pp[QCv9_04_glb_flag == "Second climatological limit (16)", GLB_wpsm, SZA], cex = .7, col = "magenta")

        title(main = paste("Global climatological test 4.", ay))
        legend("topright",
               legend = c("Global measurements", "Second limit", "First limit", "First hit", "Second hit" ),
               col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")


        ## plot direct by Azimuth
        plot(pp$Azimuth, pp$GLB_wpsm,
             cex  = .1,
             ylim = ylim,
             xlab = "Azimuth",
             ylab = "Global Irradiance")

        ## 4. Second climatological limit (16)
        points(pp$Azimuth, pp$Glo_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
        ## 4. First climatological limit (17)
        points(pp$Azimuth, pp$Glo_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

        ## plot flagged
        points(pp[QCv9_04_glb_flag == "First climatological limit (17)",  GLB_wpsm, Azimuth], cex = .7, col = "cyan"   )
        points(pp[QCv9_04_glb_flag == "Second climatological limit (16)", GLB_wpsm, Azimuth], cex = .7, col = "magenta")

        title(main = paste("Global Beam climatological test 4.", ay))
        legend("topright",
               legend = c("Global measurements", "Second limit", "First limit", "First hit", "Second hit" ),
               col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")


        ## plot direct by Date
        plot(pp$Date, pp$GLB_wpsm,
             cex  = .1,
             ylim = ylim,
             xlab = "",
             ylab = "Global Irradiance")

        ## 4. Second climatological limit (16)
        points(pp$Date, pp$Glo_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
        ## 4. First climatological limit (17)
        points(pp$Date, pp$Glo_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

        ## plot flagged
        points(pp[QCv9_04_glb_flag == "First climatological limit (17)",  GLB_wpsm, Date], cex = .7, col = "cyan"   )
        points(pp[QCv9_04_glb_flag == "Second climatological limit (16)", GLB_wpsm, Date], cex = .7, col = "magenta")

        title(main = paste("Direct Beam climatological test 4.", ay))
        legend("topright",
               legend = c("Direct measurements", "Second limit", "First limit", "First hit", "Second hit" ),
               col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")
    }


    if (DO_PLOTS) {

        if (!interactive()) {
            pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V", qc_ver, "_F", testN, ".pdf"))
        }

        ## test direct limits
        temp1 <- data.table(BB |>
                                filter(!is.na(get(flagname_DIR))) |>
                                select(Date,
                                       DIR_strict,
                                       Dir_First_Clim_lim, Dir_Secon_Clim_lim,
                                       !!flagname_DIR) |>
                                collect())

        for (ad in sort(unique(as.Date(temp1$Date)))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )
            if (any(!is.na(pp$DIR_strict))) {
                ylim <- range(pp$Dir_First_Clim_lim,
                              pp$Dir_Secon_Clim_lim,
                              pp$DIR_strict, na.rm = T)
                plot(pp$Date, pp$DIR_strict, "l", col = "blue",
                     ylim = ylim, xlab = "", ylab = "wattDIR")
                title(paste("#4", as.Date(ad, origin = "1970-01-01")))
                ## plot limits
                lines(pp$Date, pp$Dir_First_Clim_lim, col = "pink")
                lines(pp$Date, pp$Dir_Secon_Clim_lim, col = "red" )
                ## mark offending data
                points(pp[!is.na(get(flagname_DIR)), DIR_strict, Date],
                       col = "red", pch = 1)
            }
        }


        ## test global first limit
        temp1 <- data.table(BB |>
                                filter(!is.na(get(flagname_GLB))) |>
                                select(Date,
                                       GLB_strict,
                                       Glo_First_Clim_lim, Glo_Secon_Clim_lim,
                                       !!flagname_GLB) |>
                                collect())

        for (ad in sort(unique(as.Date(temp1$Date)))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )
            if (any(!is.na(pp$GLB_strict))) {
                ylim <- range(pp$Glo_First_Clim_lim,
                              pp$Glo_Secon_Clim_lim,
                              pp$GLB_strict, na.rm = T)

                plot(pp$Date, pp$GLB_strict, "l", col = "green",
                     ylim = ylim, xlab = "", ylab = "wattGLB")
                title(paste("#4", as.Date(ad, origin = "1970-01-01")))
                ## plot limits
                lines(pp$Date, pp$Glo_First_Clim_lim, col = "pink")
                lines(pp$Date, pp$Glo_Secon_Clim_lim, col = "red" )
                ## mark offending data
                points(pp[!is.na(get(flagname_GLB)), GLB_strict, Date],
                col = "red", pch = 1)
            }
        }
    }
    rm(list = ls(pattern = "flagname_.*"))
    dummy <- gc()
    if (!interactive()) dummy <- dev.off()
}
#+ echo=F, include=T



####  5. Tracker is off test  --------------------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 5. Tracker is off test
#'
#+ echo=F, include=T, results="asis"
if (TEST_05) {

    testN        <- 5
    flagname_DIR <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_dir_flag")

    cat(pander(table(collect(select(BB, !!flagname_DIR)), useNA = "always"),
               caption = flagname_DIR))
    cat(" \n \n")

    test <- data.table(BB |>
                           filter(Elevat > 0) |>
                           select(Date,
                                  DIR_strict, GLB_strict, DIFF_strict,
                                  ClrSW_ref2, !!flagname_DIR) |>
                           collect())

    hist(test[GLB_strict / ClrSW_ref2 < 2,
              GLB_strict / ClrSW_ref2], breaks = 100)
    abline(v = QS$ClrSW_lim, col = "red", lty = 3)
    cat(" \n \n")

    hist(test[DIFF_strict / GLB_strict > -0.5,
              DIFF_strict / GLB_strict], breaks = 100)
    abline(v = QS$ClrSW_lim, col = "red", lty = 3)
    cat(" \n \n")

    hist(test[, GLB_strict], breaks = 100)
    abline(v = QS$glo_min, col = "red", lty = 3)
    cat(" \n \n")


    if (DO_PLOTS) {

        if (!interactive()) {
            pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V", qc_ver, "_F", testN, ".pdf"))
        }

        tmp <- BB |>
            filter(!is.na(get(flagname_DIR))) |>
            select(Date) |>
            collect() |>
            as.data.table()

        for (ad in sort(unique(as.Date(tmp$Date)))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )
            ylim <- range(pp$ClrSW_ref2, pp$DIR_strict, pp$GLB_strict, pp$HOR_strict, na.rm = T)
            plot(pp$Date, pp$DIR_strict, "l", col = "blue",
                 ylim = ylim, xlab = "", ylab = "wattDIR")
            lines(pp$Date, pp$GLB_strict, col = "green")
            lines(pp$Date, pp$HOR_strict, col = "cyan")
            title(paste("#5", as.Date(ad, origin = "1970-01-01")))
            ## plot limits
            # lines(pp$Date, pp$ClrSW_ref1, col = "pink")
            lines(pp$Date, pp$ClrSW_ref2, col = "magenta")
            ## mark offending data
            points(pp[!is.na(get(flagname_DIR)), DIR_strict, Date],
                   col = "red", pch = 1)
        }
    }
    rm(list = ls(pattern = "flagname_.*"))
    dummy <- gc()
    if (!interactive()) dummy <- dev.off()
}
#+ echo=F, include=T


####  6. Rayleigh Limit Diffuse Comparison  ------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 6. Rayleigh Limit Diffuse Comparison
#'
#+ echo=F, include=T, results="asis"
if (TEST_06) {

    testN        <- 6
    flagname_BTH <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_bth_flag")

    cat(pander(table(collect(select(BB, !!flagname_BTH)), useNA = "always"),
               caption = flagname_BTH))
    cat("\n\n")

    test <- BB |> select(DIFF_strict, RaylDIFF) |> collect() |> as.data.table()
    hist( test[, DIFF_strict - RaylDIFF ], breaks = 100 )
    abline(v = QS$Rayleigh_lower_lim, lty = 3, col = "red")
    abline(v = QS$Rayleigh_upper_lim, lty = 3, col = "red")
    cat("\n\n")


    ## Yearly plots for Diffuse
    years <- (BB |> filter(!is.na(DIFF_strict)) |>
                  select(year) |> unique() |> collect() |> pull())

    for (ay in years) {
        pp <- data.table(BB |> filter(year(Date) == ay & Elevat > 0) |> collect())

        ignore <- grep("QCv9_0[1-5]", names(pp), value = T)

        # points(pp[!is.na(get(flagname_BTH))  ]

        ## plot by SZA
        plot(pp$SZA, pp$DIFF_strict,
             cex = .1,
             xlab = "SZA", ylab = "Diffuse Irradiance" )
        title(main = paste("Rayleigh Limit Diffuse Comparison test 6.", ay))

        ## plot flagged
        points(pp[!is.na(get(flagname_BTH)),  DIFF_strict, SZA], cex = .7, col = alpha("magenta", 0.2))

        legend("topright",
               legend = c("Diffuse (inferred)", "Rayleigh limit" ),
               col    = c("black",              "magenta"),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")


        ## plot by Azimuth
        plot(pp$Azimuth, pp$DIFF_strict,
             cex = .1,
             xlab = "Azimuth", ylab = "Diffuse Irradiance" )
        title(main = paste("Rayleigh Limit Diffuse Comparison test 6.", ay))

        ## plot flagged
        points(pp[!is.na(get(flagname_BTH)),  DIFF_strict, Azimuth], cex = .7, col = alpha("magenta", 0.2))

        legend("topright",
               legend = c("Diffuse (inferred)", "Rayleigh limit" ),
               col    = c("black",              "magenta"),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")


        ## plot by Date
        plot(pp$Date, pp$DIFF_strict,
             cex = .1,
             xlab = "SZA", ylab = "Diffuse Irradiance" )
        title(main = paste("Rayleigh Limit Diffuse Comparison test 6.", ay))

        ## plot flagged
        points(pp[!is.na(get(flagname_BTH)),  DIFF_strict, Date], cex = .7, col = alpha("magenta", 0.2))

        legend("topright",
               legend = c("Diffuse (inferred)", "Rayleigh limit" ),
               col    = c("black",              "magenta"),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")

    }




    stop("DDD")
    if (DO_PLOTS) {

        if (!interactive()) {
            pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V", qc_ver, "_F", testN, ".pdf"))
        }

        ## plot on upper limit

        tmp <- BB |>
            filter(!is.na(get(flagname_BTH))) |>
            select(Date) |>
            collect() |>
            as.data.table()

        for (ad in sort(unique(c(as.Date(tmp$Date))))) {

            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )

            layout(matrix(c(1, 2), 2, 1, byrow = TRUE))
            par(mar = c(2, 4, 2, 1))

            ylim <- range(pp$DIFF_strict, pp$RaylDIFF, na.rm = T)
            if (ylim[1] < -10) ylim[1] <- -10
            plot(pp$Date, pp$DIFF_strict, "l",
                 ylim = ylim, col = "cyan", ylab = "Diffuse", xlab = "")
            lines(pp$Date, pp$RaylDIFF, col = "magenta" )
            lines(pp$Date, pp$RaylDIFF + QS$Rayleigh_upper_lim, col = "red" )

            title(paste("#6", as.Date(ad, origin = "1970-01-01")))

            par(mar = c(2,4,1,1))
            ylim <- range(pp$GLB_strict, pp$DIR_strict, na.rm = T)
            plot(pp$Date, pp$GLB_strict, "l",
                 ylim = ylim, col = "green", ylab = "", xlab = "")
            lines(pp$Date, pp$DIR_strict, col = "blue" )

            points(pp[!is.na(get(flagname_BTH)), DIR_strict, Date],
                   ylim = ylim, col = "pink")
            points(pp[!is.na(get(flagname_BTH)), GLB_strict, Date],
                   ylim = ylim, col = "magenta")

            layout(1,1)
        }
    }
    rm(list = ls(pattern = "flagname_.*"))
    dummy <- gc()
    if (!interactive()) dummy <- dev.off()
}
#+ echo=F, include=T



####  7. Test for obstacles  ---------------------------------------------------
#'
#' \newpage
#' ## 7. Test for obstacles
#'
#+ echo=F, include=T, results="asis"
if (TEST_07) {

}
#+ echo=F, include=T



####  8. Test for inverted values  ---------------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 8. Test for inverted values
#'
#+ echo=F, include=T, results="asis"
if (TEST_08) {

    testN        <- 8
    flagname_BTH <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_bth_flag")

    cat(pander(table(collect(select(BB, !!flagname_BTH)), useNA = "always")))

    test <- BB |>
        filter(Elevat > 0) |>
        select(!!flagname_BTH, Relative_diffuse, Elevat, GLB_strict, HOR_strict) |>
        collect() |> data.table()

    hist(test[Relative_diffuse < 10, Relative_diffuse], breaks = 100)
    abline(v = QS$dir_glo_invert, lty = 3, col = "red")

    hist(test[Relative_diffuse > QS$dir_glo_invert & Elevat  > 3, Elevat], breaks = 100)
    hist(test[Relative_diffuse > QS$dir_glo_invert & Elevat  > 3, HOR_strict - GLB_strict], breaks = 100)
    hist(test[Relative_diffuse > QS$dir_glo_invert & GLB_strict > QS$dir_glo_glo_off, Elevat], breaks = 100)
    hist(test[Relative_diffuse > QS$dir_glo_invert & GLB_strict > QS$dir_glo_glo_off, HOR_strict - GLB_strict], breaks = 100)


    if (DO_PLOTS) {

        if (!interactive()) {
            pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V", qc_ver, "_F", testN, ".pdf"))
        }

        tmp <- BB |>
            filter(!is.na(get(flagname_BTH))) |>
            select(Date) |>
            collect()    |>
            as.data.table()

        for (ad in unique(as.Date(tmp$Date))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )
            ylim <- range(pp$GLB_strict, pp$HOR_strict, na.rm = T)

            plot( pp$Azimuth, pp$HOR_strict, "l",
                  ylim = ylim, col = "blue", ylab = "", xlab = "")
            lines(pp$Azimuth, pp$GLB_strict, col = "green")
            title(paste("#8", as.Date(ad, origin = "1970-01-01")))

            points(pp[!is.na(get(flagname_BTH)), HOR_strict, Azimuth],
                   col = "red")
            points(pp[!is.na(get(flagname_BTH)), GLB_strict, Azimuth],
                   ylim = ylim, col = "magenta")
        }
    }
    rm(list = ls(pattern = "flagname_.*"))
    dummy <- gc()
    if (!interactive()) dummy <- dev.off()
}
#+ echo=F, include=T



####  9. Clearness index test  -------------------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 9. Clearness index test
#'
#+ echo=F, include=T, results="asis"
if (TEST_09) {

    testN        <- 9
    flagname_GLB <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_glb_flag")

    cat(pander(table(collect(select(BB, !!flagname_GLB)), useNA = "always")))
    cat("\n\n")

    test <- BB |>
        filter(Elevat > 0 & !is.na(ClearnessIndex_kt) & ClearnessIndex_kt > 0) |>
        select(!!flagname_GLB, ClearnessIndex_kt, Elevat,
               GLB_strict) |>
        collect() |> data.table()

    range(test[Elevat > QS$CL_idx_ele, ClearnessIndex_kt], na.rm = T)
    hist( test[Elevat > QS$CL_idx_ele, ClearnessIndex_kt], breaks = 100)
    abline(v = QS$CL_idx_max, lty = 3, col = "red")
    abline(v = QS$CL_idx_min, lty = 3, col = "red")

    if (any(!is.na(test$QCv9_09_glb_flag))) {
        hist(test[!is.na(QCv9_09_glb_flag), GLB_strict],        breaks = 100)
        hist(test[!is.na(QCv9_09_glb_flag), Elevat ],           breaks = 100)
        hist(test[!is.na(QCv9_09_glb_flag), ClearnessIndex_kt], breaks = 100)
    }


    if (DO_PLOTS) {

        if (!interactive()) {
            pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V", qc_ver, "_F", testN, ".pdf"))
        }

        tmp <- BB |>
            filter(!is.na(get(flagname_GLB))) |>
            select(Date) |>
            collect()    |>
            as.data.table()

        ## TODO plot offending years
        for (ay in unique(year(tmp$Date))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )

            ylim = c(-0.5, 2)
            plot(pp$Elevat, pp$ClearnessIndex_kt,
                 pch = 19, cex = 0.1,
                 ylim = ylim, xlab = "Elevation", ylab = "Clearness index Kt" )

            abline(v = QS$CL_idx_ele, col = "yellow")
            title(paste("#9", as.Date(ad, origin = "1970-01-01")))

            points(pp[ClearnessIndex_kt > QS$CL_idx_max & Elevat > QS$CL_idx_ele, Elevat],
                   pp[ClearnessIndex_kt > QS$CL_idx_max & Elevat > QS$CL_idx_ele, ClearnessIndex_kt],
                   pch = 19, cex = 0.3, col = "red")
            abline(h = QS$CL_idx_max, col = "magenta", lwd = 0.5)

            points(pp[ClearnessIndex_kt < QS$CL_idx_min & Elevat > QS$CL_idx_ele, Elevat],
                   pp[ClearnessIndex_kt < QS$CL_idx_min & Elevat > QS$CL_idx_ele, ClearnessIndex_kt],
                   pch = 19, cex = 0.3, col = "blue")
            abline(h = QS$CL_idx_min, col = "cyan", lwd = 0.5)
        }

        ## TODO plot offending days
        for (ad in sort(unique(c(as.Date(tmp$Date))))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > sun_elev_min)   |>
                    collect()
            )

            ylim <- range(pp$DIR_strict, pp$GLB_strict, na.rm = T)
            plot(pp$Date, pp$GLB_strict, "l", col = "green",
                 ylim = ylim, xlab = "", ylab = "wattGLB")
            lines(pp$Date, pp$DIR_strict, col = "blue")
            title(paste("#9", as.Date(ad, origin = "1970-01-01")))
            ## mark offending data
            points(pp[!is.na(get(flagname_GLB)), GLB_strict, Date],
                   col = "red", pch = 1)
            ## no applicable to direct!!
            # points(pp[!is.na(QCF_GLB_09), Date],
            #        pp[!is.na(QCF_GLB_09), wattDIR],
            #        col = "red", pch = 1)
        }
    }
    rm(list = ls(pattern = "flagname_.*"))
    dummy <- gc()
    if (!interactive()) dummy <- dev.off()
}
#+ echo=F, include=T




tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
