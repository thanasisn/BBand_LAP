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

#' **QCRad T01**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#' The chosen levels and filters have to be evaluated with the available data.
#'
#'
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
Script.Name  <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T01_v10.R"
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
stop()
QS$sun_elev_min <- 0  ## Drop ALL radiation data when sun is below this point

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

##  Open dataset  ------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_DUCK, read_only = TRUE))


if (Sys.info()["nodename"] == "sagan") {

  ##  Open dataset  ------------------------------------------------------------
  con <- dbConnect(duckdb(dbdir = DB_DUCK))

  ## 1. Physically possible limits per BSRN  -----------------------------------
  #'
  #' ## 1. Physically possible limits per BSRN
  #'
  #' Test values are within physical/logical limits.
  #'
  #' Direct upper constrain is a closeness to TSI at TOA. Shouldn't be any hits.
  #' or need to remove data.
  #'
  #' Global upper constrain is an modelled GHI value.
  #'
  #' These limit should not be met, they are defined neat the maximum observed
  #' values of the data set.

  QS$dir_SWdn_min <-  -4    # Minimum direct value to consider valid measurement
  QS$dir_SWdn_dif <- 327    # Closeness to to TSI
  QS$glo_SWdn_min <-  -4    # Minimum global value to consider valid measurement
  QS$glo_SWdn_off <- 160    # Global departure offset above the model
  QS$glo_SWdn_amp <-   1.3  # Global departure factor above the model

  flagname_DIR <- "QCv10_01_dir_flag"
  flagname_GLB <- "QCv10_01_glb_flag"
  cat(paste("\n1. Physically Possible Limits", flagname_DIR, flagname_GLB, "\n\n"))

  ##  Make null columns as above
  make_null_column(con, "LAP", flagname_DIR, "character")
  make_null_column(con, "LAP", flagname_GLB, "character")

  DT <- tbl(con, "LAP") |>
    filter(Elevat > QS$sun_elev_min)  ## sun is up

  ##  Flag direct  -------------------------------------------------------------
  ADD <- DT |>
    filter(is.na(!!flagname_DIR)) |>
    mutate(!!flagname_DIR := case_when(

      DIR_strict           < QS$dir_SWdn_min ~ "Physical possible limit min (5)",
      TSI_TOA - DIR_strict < QS$dir_SWdn_dif ~ "Physical possible limit max (6)",

      .default = "passed"
    ))
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")

  ##  Flag global  -------------------------------------------------------------
  ADD <- DT |>
    mutate(Glo_max_ref := TSI_TOA * QS$glo_SWdn_amp * cos(SZA*pi/180)^1.2 + QS$glo_SWdn_off) |>
    mutate(!!flagname_GLB := case_when(

      GLB_strict < QS$glo_SWdn_min ~ "Physical possible limit min (5)",
      GLB_strict > Glo_max_ref     ~ "Physical possible limit max (6)",

      .default = "passed"
    ))
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")

  ##  Store filters parameters  ------------------------------------------------
  saveRDS(object = QS,
          file   = parameter_fl)

}






DT <- tbl(con, "LAP") |>
  filter(Elevat > QS$sun_elev_min)  ## sun is up

DT |>
  filter(!is.na(!!flagname_DIR)) |>
  select(!!flagname_DIR) |>
  group_by(!!flagname_DIR) |> tally()


DT |>
  filter(!is.na(!!flagname_GLB)) |>
  select(!!flagname_GLB) |>
  group_by(!!flagname_GLB) |> tally()



stop()
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



stop()


## clean
rm(datapart)
dummy <- gc()




## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ----



## ~ ~ Inspect quality control results ~ ~ -------------------------------------
#'
#' # Inspect quality control results
#'
#+ include=T, echo=F


## load filter parameters
QS <- readRDS(parameter_fl)

## TODO when plotting ignore previous flagged data or not, but fully apply flag


####  1. Physically possible limits per BSRN  ----------------------------------
#' \FloatBarrier
#' \newpage
#' ## 1. Physically possible limits per BSRN
#'
#+ echo=F, include=T, results="asis"
if (QS$TEST_01) {
  testN        <- 1
  flagname_DIR <- paste0("QCv10_", sprintf("%02d", testN), "_dir_flag")
  flagname_GLB <- paste0("QCv10_", sprintf("%02d", testN), "_glb_flag")

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
      pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V9_F", testN, ".pdf"))
    }

    test <- BB |> filter(!QCv10_01_dir_flag %in% c(NA, "pass")) |> collect() |> as.data.table()
    test <- BB |> filter(!is.na(QCv10_01_dir_flag)) |> collect() |> as.data.table()

    ## TODO
    if (nrow(test) == 0) {
      cat("\nNO CASES FOR DIRECT QCv10_01_dir_flag\n\n")
    }
    for (ad in sort(unique(as.Date(test$Date)))) {
      pp <- data.table(
        BB |> filter(as.Date(Date) == as.Date(ad) &
                       Elevat > QS$sun_elev_min)   |>
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
    test <- BB |> filter(!is.na(QCv10_01_glb_flag) ) |> collect() |> as.data.table()
    if (nrow(test) == 0) {
      cat("\nNO CASES FOR GLOBAL QCv10_01_glb_flag\n\n")
    }
    for (ad in sort(unique(as.Date(c(test$Date))))) {
      pp <- data.table(
        BB |> filter(as.Date(Date) == as.Date(ad) &
                       Elevat > QS$sun_elev_min)   |>
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





#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
