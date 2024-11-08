# /* #!/opt/R/4.2.3/bin/Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Radiation Quality Control **QCRad** "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Data quality for radiation measurements as described by
#'                 CN Long and Y Shi, September 2006, DOE/SC-ARM/TR-074.
#'                 - The QCRad Value Added Product Surface
#'                 Radiation Measurement Quality Control Testing Including
#'                 Climatology_Long2006.pdf"
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
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#'
#+ echo=F, include=T

stop("This is a reference file")

#+ echo=F, include=T
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )

## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name  <- "~/BBand_LAP/process/QCRad_LongShi_v9.R"

##  Variables  -----------------------------------------------------------------
## gather configurations for quality control
QS <<- list()
QS$sun_elev_min <- -2 * 0.103  ## Drop ALL radiation data when sun is below this point


##  Execution control  ---------------------------------------------------------

## mostly for daily plots
DO_PLOTS     <- TRUE
if (interactive()) {
    DO_PLOTS <- FALSE
}


# Daily plots
DO_PLOTS       <- TRUE
# Ignore previous flagged points in plots (not fully implemented yet)
IGNORE_FLAGGED <- TRUE   ## TRUE is the default of the original
IGNORE_FLAGGED <- FALSE


## __ Select a part of data to plot  -------------------------------------------
PARTIAL    <- FALSE
PARTIAL    <- TRUE
PLOT_FIRST <- as_date("2023-01-01")
PLOT_LAST  <- as_date("2024-01-01")



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
QS$Rayleigh_lower_lim <-  -1    # Lower departure diffuse limit
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
  return( a * mu_0      +
            b * mu_0^2 +
            c * mu_0^3 +
            d * mu_0^4 +
            e * mu_0^5 +
            f * mu_0 * Pressure)
}

if (QS$TEST_06) {
  testN        <- 6
  flagname_BTH <- paste0("QCv9_", sprintf("%02d", testN), "_bth_flag")
  cat(paste("\n6. Rayleigh Limit Diffuse Comparison", flagname_BTH, "\n\n"))

  datapart[, RaylDIFF  := Rayleigh_diff(SZA = SZA, Pressure = Pressure)]

  ## __ Both  ------------------------------------------------------------
  datapart[DIFF_strict - RaylDIFF > QS$Rayleigh_upper_lim,
           (flagname_BTH) := "Rayleigh diffuse limit upper (18)"]
  datapart[DIFF_strict - RaylDIFF < QS$Rayleigh_lower_lim,
           (flagname_BTH) := "Rayleigh diffuse limit lower broad (18)"]

  ## extra restrictions by me
  datapart[DIFF_strict - RaylDIFF     < QS$Rayleigh_lower_lim &
             DIFF_strict / GLB_wpsm < QS$Rayleigh_dif_glo_r &
             GLB_wpsm               > QS$Rayleigh_glo_min,
           (flagname_BTH) := "Rayleigh diffuse limit lower narrow (18)"]



  rm(list = ls(pattern = "flagname_.*"))
  dummy <- gc()
}



## 7. Test for obstacles  --------------------------------------------------
#'
#' ## 7. Test for obstacles
#'
#' This is deactivated
#'
if (QS$TEST_07) {
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

if (QS$TEST_08) {
  cat(paste("\n8. Inversion test.\n\n"))

  testN        <- 8
  flagname_BTH <- paste0("QCv9_", sprintf("%02d", testN), "_bth_flag")

  ## __ Both  ------------------------------------------------------------
  datapart[, Relative_diffuse := 100 * (HOR_strict  - GLB_strict) / GLB_strict ]
  datapart[ is.infinite(Relative_diffuse), Relative_diffuse := NA]

  datapart[Relative_diffuse > QS$dir_glo_invert  &
             GLB_strict       > QS$dir_glo_glo_off,
           (flagname_BTH) := "Direct > global soft (14)"]
  datapart[Relative_diffuse > QS$dir_glo_invert,
           (flagname_BTH) := "Direct > global hard (15)"]

  rm(list = ls(pattern = "flagname_.*"))
  dummy <- gc()
}



## 9. Transmittance test  ------------------------------------------------
#'
#' ## 9. Transmittance test
#'
#' This filter is mine, and is applied on GHI data.
#'
#' Data near elevation 0 are caused by the cos(SZA) while calculating
#' T = GLB / (cos(sza) * TSI).
#'
#' For larger elevation angles manual inspection is needed.

QS$CL_idx_max <-  1.13   # Upper Transmittance accepted level
QS$CL_idx_min <- -0.001  # Lower Transmittance accepted level
QS$CL_idx_ele <-  8      # Apply for elevations above this angle

if (QS$TEST_09) {
  cat(paste("\n9. Transmittance (global/TSI) test.\n\n"))

  testN        <- 9
  flagname_GLB <- paste0("QCv9_", sprintf("%02d", testN), "_glb_flag")

  # InitVariableBBDB(flagname_GLB, as.character(NA))

  ## __ Global  ----------------------------------------------------------
  datapart[Transmittance_GLB > QS$CL_idx_max & Elevat > QS$CL_idx_ele,
           (flagname_GLB) := "Clearness index limit max (19)" ]
  datapart[Transmittance_GLB < QS$CL_idx_min & Elevat > QS$CL_idx_ele,
           (flagname_GLB) := "Clearness index limit min (20)" ]

  rm(list = ls(pattern = "flagname_.*"))
  dummy <- gc()
}



## 10. Erroneous sun position  ---------------------------------------------
#'
#' ## 10. Erroneous sun position
#'
#' This filter is mine.
#'
#' Create a statistical range to check SZA and Azimuth.
#' Implement monthly of by doy.
#'

if (QS$TEST_10) {
  cat(paste("\n10. Erroneous sun position test.\n\n"))

  testN        <- 10
  flagname_ALL <- paste0("QCv9_", sprintf("%02d", testN), "_all_flag")

  rm(list = ls(pattern = "flagname_.*"))
  dummy <- gc()
}








## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ----



## ~ ~ Inspect quality control results ~ ~ -------------------------------------
#'
#' # Inspect quality control results
#'
#+ include=T, echo=F








####  6. Rayleigh Limit Diffuse Comparison  ------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 6. Rayleigh Limit Diffuse Comparison
#'
#+ echo=F, include=T, results="asis"
if (QS$TEST_06) {

    test <- BB |> select(DIFF_strict, RaylDIFF) |> collect() |> as.data.table()
    hist(test[, DIFF_strict - RaylDIFF ], breaks = 100)
    abline(v = QS$Rayleigh_lower_lim, lty = 3, col = "red")
    abline(v = QS$Rayleigh_upper_lim, lty = 3, col = "red")
    cat(" \n \n")

    ## Info flags to ignore in plots
    if (IGNORE_FLAGGED) {
        ignore <- grep(paste0("QCv9_0[1-", testN - 1 ,"]"), names(BB), value = T)
        cat("**Plots will ignore previoysly flaged points: ", ignore, "**\n")
    }

    ## Yearly plots for Diffuse
    years <- (BB |> filter(!is.na(DIFF_strict)) |>
                  select(year) |> unique() |> collect() |> pull())

    for (ay in years) {
        pp <- data.table(BB |> filter(year(Date) == ay & Elevat > 0) |> collect())

        ## Ignore previously flagged
        pp[, Ignore := FALSE]
        if (IGNORE_FLAGGED) {
            pp[
                pp[, rowSums(!is.na(.SD)) > 0 &
                       !is.na(get(flagname_BTH)), .SDcols = patterns("^QCv9_0[1-5]")],
                Ignore := TRUE
            ]
        }

        ## plot by SZA
        plot(pp$SZA, pp$DIFF_strict,
             cex = .1,
             xlab = "SZA", ylab = "Diffuse Irradiance" )
        title(main = paste("Rayleigh Limit Diffuse Comparison test 6.", ay))

        ## plot flagged
        points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  & !Ignore, DIFF_strict, SZA], cex = .7, col = alpha("magenta", 0.09))
        points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower narrow (18)" & !Ignore, DIFF_strict, SZA], cex = .7, col = alpha("red", 0.4))

        legend("topright",
               legend = c("Diffuse (inferred)", "Rayleigh limit broad", "Rayleigh limit narrow"),
               col    = c("black",              "magenta",             "red"),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")


        ## plot by Azimuth
        plot(pp$Azimuth, pp$DIFF_strict,
             cex = .1,
             xlab = "Azimuth", ylab = "Diffuse Irradiance" )
        title(main = paste("Rayleigh Limit Diffuse Comparison test 6.", ay))

        ## plot flagged
        points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  & !Ignore, DIFF_strict, Azimuth], cex = .7, col = alpha("magenta", 0.09))
        points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower narrow (18)" & !Ignore, DIFF_strict, Azimuth], cex = .7, col = alpha("red", 0.7))

        legend("topright",
               legend = c("Diffuse (inferred)", "Rayleigh limit broad", "Rayleigh limit narrow"),
               col    = c("black",              "magenta",             "red"),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")


        ## plot by Date
        plot(pp$Date, pp$DIFF_strict,
             cex = .1,
             xlab = "SZA", ylab = "Diffuse Irradiance" )
        title(main = paste("Rayleigh Limit Diffuse Comparison test 6.", ay))

        ## plot flagged
        points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  & !Ignore, DIFF_strict, Date], cex = .7, col = alpha("magenta", 0.09))
        points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower narrow (18)" & !Ignore, DIFF_strict, Date], cex = .7, col = alpha("red", 0.7))

        legend("topright",
               legend = c("Diffuse (inferred)", "Rayleigh limit broad", "Rayleigh limit narrow"),
               col    = c("black",              "magenta",             "red"),
               pch = 19, bty = "n", cex = 0.8 )
        cat(" \n \n")

    }


    ## Daily plots
    if (DO_PLOTS) {

        if (!interactive()) {
            pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V9_F", testN, ".pdf"))
        }

        ## plot on upper limit

        tmp <- BB |>
            filter(get(flagname_BTH) == "Rayleigh diffuse limit lower narrow (18)") |>
            select(Date) |>
            collect() |>
            as.data.table()

        for (ad in sort(unique(c(as.Date(tmp$Date))))) {

            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > QS$sun_elev_min)   |>
                    collect()
            )

            ## Ignore previously flagged
            pp[, Ignore := FALSE]
            if (IGNORE_FLAGGED) {
                pp[
                    pp[, rowSums(!is.na(.SD)) > 0 &
                           !is.na(get(flagname_BTH)),
                       .SDcols = patterns(paste0("^QCv9_0[1-", testN - 1, "]"))],
                    Ignore := TRUE
                ]
            }

            ## skip if all points are ignored
            if (all(pp$Ignore) == TRUE) next()

            ## start figure
            layout(matrix(c(1, 2), 2, 1, byrow = TRUE))
            par(mar = c(2, 4, 2, 1))

            ## plot limits
            ylim <- range(pp$DIFF_strict, pp$RaylDIFF, na.rm = T)
            if (ylim[1] < -10) ylim[1] <- -10
            plot(pp$Date, pp$DIFF_strict, "l",
                 ylim = ylim, col = "cyan", ylab = "Diffuse", xlab = "")
            lines(pp$Date, pp$RaylDIFF, col = "magenta" )
            lines(pp$Date, pp$RaylDIFF + QS$Rayleigh_upper_lim, col = "red" )

            title(paste("#6", as.Date(ad, origin = "1970-01-01")))

            ## plots data
            par(mar = c(2,4,1,1))
            ylim <- range(pp$GLB_strict, pp$DIR_strict, na.rm = T)
            plot(pp$Date, pp$GLB_strict, "l",
                 ylim = ylim, col = "green", ylab = "", xlab = "")
            lines(pp$Date, pp$DIR_strict, col = "blue" )

            points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  & !Ignore, DIR_strict, Date], ylim = ylim, col = alpha("magenta", 0.5))
            points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  & !Ignore, GLB_strict, Date], ylim = ylim, col = alpha("magenta", 0.5))
            points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower narrow (18)" & !Ignore, DIR_strict, Date], ylim = ylim, col = "red")
            points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower narrow (18)" & !Ignore, GLB_strict, Date], ylim = ylim, col = "red")

            layout(1, 1)
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
if (QS$TEST_07) {

}
#+ echo=F, include=T



####  8. Test for inverted values  ---------------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 8. Test for inverted values
#'
#+ echo=F, include=T, results="asis"
if (QS$TEST_08) {
    testN        <- 8
    flagname_BTH <- paste0("QCv9_", sprintf("%02d", testN), "_bth_flag")

    cat(pander(table(collect(select(BB, !!flagname_BTH)), useNA = "always"),
               caption = flagname_BTH))
    cat(" \n \n")

    test <- BB |>
        filter(Elevat > 0) |>
        select(!!flagname_BTH, Relative_diffuse, Elevat, GLB_strict, HOR_strict) |>
        collect() |> data.table()

    hist(test[Relative_diffuse < 10, Relative_diffuse], breaks = 100)
    abline(v = QS$dir_glo_invert, lty = 3, col = "red")
    cat(" \n \n")

    hist(test[Relative_diffuse > QS$dir_glo_invert & Elevat  > 3, Elevat], breaks = 100)
    hist(test[Relative_diffuse > QS$dir_glo_invert & Elevat  > 3, HOR_strict - GLB_strict], breaks = 100)
    hist(test[Relative_diffuse > QS$dir_glo_invert & GLB_strict > QS$dir_glo_glo_off, Elevat], breaks = 100)
    hist(test[Relative_diffuse > QS$dir_glo_invert & GLB_strict > QS$dir_glo_glo_off, HOR_strict - GLB_strict], breaks = 100)



    ## Info flags to ignore in plots
    if (IGNORE_FLAGGED) {
        ignore <- grep(paste0("QCv9_0[1-", testN - 1 ,"]"), names(BB), value = T)
        cat("**Plots will ignore previoysly flaged points: ", ignore, "**\n")
    }

    ## Yearly plots for Diffuse
    years <- (BB |> filter(!is.na(flagname_BTH)) |>
                  select(year) |> unique() |> collect() |> pull())

    for (ay in years) {
        pp <- data.table(BB |> filter(year(Date) == ay & Elevat > 0) |> collect())

        ## Ignore previously flagged
        pp[, Ignore := FALSE]
        if (IGNORE_FLAGGED) {
            pp[
                pp[, rowSums(!is.na(.SD)) > 0 &
                       !is.na(get(flagname_BTH)),
                   .SDcols = patterns(paste0("^QCv9_0[1-", testN - 1, "]"))],
                Ignore := TRUE
            ]
        }

        ## skip if all points are ignored
        if (pp[!is.na(get(flagname_BTH)) & !Ignore, .N] == 0) next()

        ## plot by sky position
        plot(pp$Azimuth, pp$Elevat,
             ylab = "Elevation", xlab = "Azimuth",
             cex = .1)
        title(paste("#8", ay, flagname_BTH))

        points(pp[!is.na(get(flagname_BTH)) & !Ignore, Elevat, Azimuth],
               cex = .2, col = "red")
        cat(" \n \n")
    }



    if (DO_PLOTS) {

        if (!interactive()) {
            pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V9_F", testN, ".pdf"))
        }

        tmp <- BB |>
            filter(!is.na(get(flagname_BTH))) |>
            select(Date) |>
            collect()    |>
            as.data.table()

        for (ad in unique(as.Date(tmp$Date))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > QS$sun_elev_min)   |>
                    collect()
            )

            ## Ignore previously flagged
            pp[, Ignore := FALSE]
            if (IGNORE_FLAGGED) {
                pp[
                    pp[, rowSums(!is.na(.SD)) > 0 &
                           !is.na(get(flagname_BTH)),
                       .SDcols = patterns(paste0("^QCv9_0[1-", testN - 1, "]"))],
                    Ignore := TRUE
                ]
            }

            ## skip if all points are ignored
            if (pp[!is.na(get(flagname_BTH)) & !Ignore, .N] == 0) next()

            table(pp$QCv9_08_bth_flag)
            table(pp$Ignore)

            pp[!is.na(get(flagname_BTH)) & !Ignore, .N]

            pp[!is.na(get(flagname_BTH))]

            ylim <- range(pp$GLB_strict, pp$HOR_strict, na.rm = T)

            plot( pp$Azimuth, pp$HOR_strict, "l",
                  ylim = ylim, col = "blue", ylab = "", xlab = "")
            lines(pp$Azimuth, pp$GLB_strict, col = "green")
            title(paste("#8", as.Date(ad, origin = "1970-01-01")))

            points(pp[!is.na(get(flagname_BTH)) & !Ignore, HOR_strict, Azimuth],
                   col = "red")
            points(pp[!is.na(get(flagname_BTH)) & !Ignore, GLB_strict, Azimuth],
                   col = "magenta")
            ## TODO plot flags separate

            cat(" \n \n")
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
if (QS$TEST_09) {
    testN        <- 9
    flagname_GLB <- paste0("QCv9_", sprintf("%02d", testN), "_glb_flag")

    cat(pander(table(collect(select(BB, !!flagname_GLB)), useNA = "always")))
    cat("\n\n")

    test <- BB |>
        filter(Elevat > 0 & !is.na(Transmittance_GLB) & Transmittance_GLB > 0) |>
        select(!!flagname_GLB, Transmittance_GLB, Elevat,
               GLB_strict) |>
        collect() |> data.table()

    range(test[Elevat > QS$CL_idx_ele, Transmittance_GLB], na.rm = T)
    hist( test[Elevat > QS$CL_idx_ele, Transmittance_GLB], breaks = 100)
    abline(v = QS$CL_idx_max, lty = 3, col = "red")
    abline(v = QS$CL_idx_min, lty = 3, col = "red")

    if (any(!is.na(test$QCv9_09_glb_flag))) {
        hist(test[!is.na(QCv9_09_glb_flag), GLB_strict],        breaks = 100)
        hist(test[!is.na(QCv9_09_glb_flag), Elevat ],           breaks = 100)
        hist(test[!is.na(QCv9_09_glb_flag), Transmittance_GLB], breaks = 100)
    }


    if (DO_PLOTS) {

        if (!interactive()) {
            pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V9_F", testN, ".pdf"))
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
                                 Elevat > QS$sun_elev_min)   |>
                    collect()
            )

            ylim = c(-0.5, 2)
            plot(pp$Elevat, pp$Transmittance_GLB,
                 pch = 19, cex = 0.1,
                 ylim = ylim, xlab = "Elevation", ylab = "Clearness index Kt" )

            abline(v = QS$CL_idx_ele, col = "yellow")
            title(paste("#9", as.Date(ad, origin = "1970-01-01")))

            points(pp[Transmittance_GLB > QS$CL_idx_max & Elevat > QS$CL_idx_ele, Elevat],
                   pp[Transmittance_GLB > QS$CL_idx_max & Elevat > QS$CL_idx_ele, Transmittance_GLB],
                   pch = 19, cex = 0.3, col = "red")
            abline(h = QS$CL_idx_max, col = "magenta", lwd = 0.5)

            points(pp[Transmittance_GLB < QS$CL_idx_min & Elevat > QS$CL_idx_ele, Elevat],
                   pp[Transmittance_GLB < QS$CL_idx_min & Elevat > QS$CL_idx_ele, Transmittance_GLB],
                   pch = 19, cex = 0.3, col = "blue")
            abline(h = QS$CL_idx_min, col = "cyan", lwd = 0.5)
        }

        ## TODO plot offending days
        for (ad in sort(unique(c(as.Date(tmp$Date))))) {
            pp <- data.table(
                BB |> filter(as.Date(Date) == as.Date(ad) &
                                 Elevat > QS$sun_elev_min)   |>
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




#' **END**
#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
