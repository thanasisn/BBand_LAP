#!/opt/R/4.2.3/bin/Rscript
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
#'  **QCRad**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
#' The chosen levels and filters have to be evaluated with the available data.
#'
#'
#' TODO:
#'
#' - plot combination of flag for each point
#' - plot cumulative graphs like the old
#' - Plot daily graphs with all available flags
#'
#+ echo=F, include=T


#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(fig.pos    = '!h'     )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/process/QCRad_LongShi_v9.R"
qc_ver      <- 9

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
source("~/CODE/FUNCTIONS/R/trig_deg.R")
# mylock(DB_lock)


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)

## __  Variables  --------------------------------------------------------------
sun_elev_min     <-  -2 * 0.103  ## Drop  radiation data when sun is below this point


## __  Execution control  ------------------------------------------------------

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
DO_PLOTS     <- TRUE
if (interactive()) {
    DO_PLOTS <- FALSE
}





##  Create a test database  ----------------------------------------------------
TEST_DB <- TRUE
if (TEST_DB) {
    source("~/BBand_LAP/DEFINITIONS.R")
    cat("\n * * * Using a temp DB * * * \n\n")
    ## copy data to temp
    tyear <- 2017
    dir.create(test_DB_DIR, showWarnings = FALSE, recursive = TRUE)
    system(paste( "cp -rv --update ", DB_HASH_fl, test_DB_HASH_fl))
    system(paste( "cp -rv --update ", DB_META_fl, test_DB_META_fl))
    system(paste0("rsync -avr ", DB_DIR, "/", tyear, "/ ", test_DB_DIR, "/", tyear))
    ## replace paths with test paths
    DB_DIR     <- test_DB_DIR
    DB_lock    <- test_DB_lock
    DB_META_fl <- test_DB_META_fl
    DB_HASH_fl <- test_DB_HASH_fl
}


##  Create a new variable to the whole database  -------------------------------





##  Interactive tests  ---------------------------------------------------------


BB <- opendata()

## gather Quality control settings
QS <- list()


##  Create strict radiation data  ----------------------------------------------


## __ Daytime radiation only  --------------------------------------------------
BB <- BB |>
    mutate(DIR_strict =
               if_else(Elevat > sun_elev_min &
                           is.na(chp1_bad_data_flag) &
                           Async_tracker_flag == FALSE,
                       DIR_wpsm, NA )) |> compute()
BB <- BB |>
    mutate(HOR_strict =
               if_else(Elevat > sun_elev_min &
                           is.na(chp1_bad_data_flag) &
                           Async_tracker_flag == FALSE,
                       HOR_wpsm, NA )) |> compute()
BB <- BB |>
    mutate(GLB_strict =
               if_else(Elevat > sun_elev_min &
                           is.na(cm21_bad_data_flag),
                       GLB_wpsm, NA )) |> compute()


## __ Negative radiation to zero  ----------------------------------------------
BB <- BB |> mutate(DIR_strict =
                       if_else(DIR_strict < 0, 0, DIR_strict)) |> compute()
BB <- BB |> mutate(HOR_strict =
                       if_else(HOR_strict < 0, 0, HOR_strict)) |> compute()
BB <- BB |> mutate(GLB_strict =
                       if_else(GLB_strict < 0, 0, GLB_strict)) |> compute()


## __ Diffuse radiation  -------------------------------------------------------
BB <- BB |> mutate(DIFF_strict = GLB_strict - DIR_strict) |> compute()

## __ Clearness Index  ---------------------------------------------------------
BB <- BB |> mutate(ClearnessIndex_kt = GLB_strict / (cosde(SZA) * TSI_TOA)) |> compute()




BB |>  filter(is.na(DIR_wpsm)) |> summarise(n()) |> collect()
BB |>  filter(is.na(DIR_strict)) |> summarise(n()) |> collect()
BB |>  filter(DIR_strict == 0) |> summarise(n()) |> collect()


BB |> select(chp1_bad_data_flag) |> collect() |> table(useNA = "always")
BB |> select(Async_tracker_flag) |> collect() |> table(useNA = "always")

ss <- BB |> mutate(day = as_date(Date)) |> select(day) |> unique() |> pull() |> sample(3)

for (ad in ss) {
    pp <- BB |> filter(as_date(Date) == as_date(ad)) |> collect()
    summary(pp)

    plot(pp$Date, pp$DIR_strict)
    plot(pp$Date, pp$HOR_strict)
    plot(pp$Date, pp$GLB_strict)
}


## for chp1 and cm21
## - drop all night data
## - remove negative values when sun is too low

names(BB)

DIR_strict
GLB_strict
HOR_strict




## 1. PHYSICALLY POSSIBLE LIMITS PER BSRN  -------------------------------------
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

    testN        <- 1
    flagname_dir <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_dir_flag")
    flagname_glo <- paste0("QCv", qc_ver, "_", sprintf("%02d", testN), "_glo_flag")

    InitVariableBBDB(flagname_dir, as.character(NA))
    InitVariableBBDB(flagname_glo, as.character(NA))

    stop()

    QS$dir_SWdn_min <-  -4  # Minimum direct value to consider valid measurement
    QS$dir_SWdn_dif <- 327  # Closeness to to TSI
    QS$glo_SWdn_min <-  -4  # Minimum global value to consider valid measurement
    QS$glo_SWdn_off <- 160  # Global departure offset above the model
    QS$glo_SWdn_amp <- 1.3  # Global departure factor above the model

    ## __ Direct --------------------------------------------------------------
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


























myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
