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

#' **QCRad T02**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#'
#+ echo=F, include=T

#+ echo=F, include=T
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name  <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R"
Script.ID    <- "Q1"
parameter_fl <- "~/BBand_LAP/SIDE_DATA/QCRad_LongShi_v10_duck_parameters.Rds"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/",   basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/LOGs/duck/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

##  Variables  -----------------------------------------------------------------
if (file.exists(parameter_fl)) {
  QS <<- readRDS(parameter_fl)
} else {
  stop("File not initialiazed")
}

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
PLOT_FIRST <- as_date("1993-01-01")
PLOT_LAST  <- as_date("2024-01-01")



flagname_DIR <- "QCv10_02_dir_flag"
flagname_GLB <- "QCv10_02_glb_flag"

if (Sys.info()["nodename"] == "sagan") {

  ##  Open dataset  ------------------------------------------------------------
  con <- dbConnect(duckdb(dbdir = DB_DUCK))

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

  categories <- c("empty",
                  "pass",
                  "Extremely rare limits min (3)",
                  "Extremely rare limits max (4)")


  cat(paste("\n2. Extremely Rare Limits", flagname_DIR, flagname_GLB, "\n\n"))

  ## __ Make null columns to update all values  --------------------------------

  ## TODO just once
  remove_column(con, "LAP", flagname_DIR)
  remove_column(con, "LAP", flagname_GLB)

  ## create categorical if not existing
  make_categorical_column(flagname_DIR, categories, con, "LAP")
  make_categorical_column(flagname_GLB, categories, con, "LAP")



  stop("wait")

  ## Select some data
  ADD <- tbl(con, "LAP")             |>
    filter(Elevat > QS$sun_elev_min) |>
    filter(!is.na(TSI_TOA))          |>
    filter(!is.na(SZA))              |>
    select(TSI_TOA, SZA, Date, GLB_strict, DIR_strict) |> collect() |> data.table()


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




  ## __  Store used filters parameters  ----------------------------------------
saveRDS(object = QS,
        file   = parameter_fl)
}


stop()

## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ----



## ~ ~ Inspect quality control results ~ ~ -------------------------------------
#'
#' # Inspect quality control results
#'
#+ include=T, echo=F


## TODO when plotting ignore previous flagged data or not, but fully apply flag


####  2. Extremely rare limits per BSRN  ---------------------------------------
#' \FloatBarrier
#' \newpage
#' ## 2. Extremely rare limits per BSRN
#'
#+ echo=F, include=T, results="asis"
if (QS$TEST_02) {
  testN        <- 2
  flagname_DIR <- paste0("QCv9_", sprintf("%02d", testN), "_dir_flag")
  flagname_GLB <- paste0("QCv9_", sprintf("%02d", testN), "_glb_flag")

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
      pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V9_F", testN, ".pdf"))
    }

    ## Direct
    test <- BB |> filter(!is.na(QCv9_02_dir_flag)) |> collect() |> as.data.table()
    for (ad in sort(unique(as.Date(test$Date)))) {
      pp <- data.table(
        BB |> filter(as.Date(Date) == as.Date(ad) &
                       Elevat > QS$sun_elev_min)   |>
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
                       Elevat > QS$sun_elev_min)   |>
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






#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
