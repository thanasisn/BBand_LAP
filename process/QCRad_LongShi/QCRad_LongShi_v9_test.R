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
#'   html_document:
#'     toc:        true
#'     fig_width:  9
#'     fig_height: 4
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
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---
#+ include=F

#'
#' **QCRad**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'

stop("This is a reference file")

#+ echo=F, include=T
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.cap   = " empty caption ")
knitr::opts_chunk$set(fig.pos   = "!ht"   )

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


#+ results="asis", echo=FALSE
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
