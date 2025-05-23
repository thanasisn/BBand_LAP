#!/usr/bin/env Rscript
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
#' **QCRad T02**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'

#+ include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.cap   = " empty caption ")
knitr::opts_chunk$set(fig.pos   = "!h"    )
knitr::opts_chunk$set(tidy = TRUE,
                      tidy.opts = list(
                        indent       = 4,
                        blank        = FALSE,
                        comment      = FALSE,
                        args.newline = TRUE,
                        arrow        = TRUE)
                      )

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name  <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T02_v10.R"
Script.ID    <- "Q2"
parameter_fl <- "~/BBand_LAP/SIDE_DATA/QCRad_LongShi_v10_duck_parameters.Rds"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/",   basename(sub("\\.R$", ".pdf", Script.Name))))
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

#+ include=T, echo=F, results="asis"
##  Variables  -----------------------------------------------------------------
if (file.exists(parameter_fl)) {
  QS <<- readRDS(parameter_fl)
} else {
  stop("File not initialized")
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
DAILY_PLOTS_DIR <- "~/BBand_LAP/REPORTS/REPORTS/QCRad_LongShi/"

flagname_DIR     <- "QCv10_02_dir_flag"
flagname_GLB     <- "QCv10_02_glb_flag"
QS$plot_elev_T02 <- 2


##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_BROAD))

## 2. Extremely rare limits per BSRN  ------------------------------------------
#'
#' ## 2. Extremely rare limits per BSRN
#'
#' These should be a little more restrictive than 1. in order to start
#' catching erroneous values.
#'
#' The choose of those settings may be optimized with an iterative process.
#'

# Upper modelled values
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
# datapart[Direct_max_ref < QS$dir_SWdn_too_low, Direct_max_ref := NA]
# datapart[Global_max < QS$glo_SWdn_too_low, Direct_max_ref := NA]

if (Sys.info()["nodename"] == "sagan") {

  cat(paste("\n2. Extremely Rare Limits", flagname_DIR, flagname_GLB, "\n\n"))

  ## __ Make categorical columns  ----------------------------------------------
  categories <- c("empty",
                  "pass",
                  "Extremely rare limits min (3)",
                  "Extremely rare limits max (4)")

  ## remove existing
  remove_column(con, "LAP", flagname_DIR)
  remove_column(con, "LAP", flagname_GLB)

  ## create categorical if not existing
  make_categorical_column(flagname_DIR, categories, con, "LAP")
  make_categorical_column(flagname_GLB, categories, con, "LAP")


  ## __ Direct  ----------------------------------------------------------------
  ADD <- tbl(con, "LAP")                                   |>
    filter(Elevat > QS$sun_elev_min)                       |>
    filter(!is.na(TSI_TOA))                                |>
    select(Date, SZA, TSI_TOA, DIR_strict, !!flagname_DIR) |>
    arrow::to_arrow()                                      |>
    mutate(

      Direct_max_ref := case_when(
        TSI_TOA * QS$Dir_SWdn_amp * cos(SZA*pi/180)^0.2 + QS$Dir_SWdn_off > 9000 ~ 9000,
        TSI_TOA * QS$Dir_SWdn_amp * cos(SZA*pi/180)^0.2 + QS$Dir_SWdn_off < 9000 ~ TSI_TOA * QS$Dir_SWdn_amp * cos(SZA*pi/180)^0.2 + QS$Dir_SWdn_off
      )
    ) |>
    mutate(

      !!flagname_DIR := case_when(
        DIR_strict <  QS$dir_SWdn_min_ext ~ "Extremely rare limits min (3)",
        DIR_strict >= Direct_max_ref      ~ "Extremely rare limits max (4)",

        .default = "pass"
      )
    )

  ## this needs a lot of memory, could do it in batches
  ADD <- ADD |> collect() |> data.table()
  res <- update_table(con, ADD, "LAP", "Date")
  rm(ADD); dummy <- gc()


  ## __ Global  ----------------------------------------------------------------
  ADD <- tbl(con, "LAP")                                   |>
    filter(Elevat > QS$sun_elev_min)                       |>
    filter(!is.na(TSI_TOA))                                |>
    select(Date, SZA, TSI_TOA, GLB_strict, !!flagname_GLB) |>
    arrow::to_arrow()                                      |>
    mutate(

      Global_max := case_when(
        TSI_TOA * QS$Glo_SWdn_amp * cos(SZA * pi / 180)^1.2 + QS$Glo_SWdn_off > 9000 ~ 9000,
        TSI_TOA * QS$Glo_SWdn_amp * cos(SZA * pi / 180)^1.2 + QS$Glo_SWdn_off < 9000 ~ TSI_TOA * QS$Glo_SWdn_amp * cos(SZA * pi / 180)^1.2 + QS$Glo_SWdn_off,
      )
    ) |>
    mutate(

      !!flagname_GLB := case_when(
        GLB_strict <  QS$glo_SWdn_min_ext ~ "Extremely rare limits min (3)",
        GLB_strict >= Global_max          ~ "Extremely rare limits max (4)",

        .default = "pass"
      )
    )

  ## this needs a lot of memory, could do it in batches
  ADD <- ADD |> collect() |> data.table()
  res <- update_table(con, ADD, "LAP", "Date")
  rm(ADD); dummy <- gc()

  ## __  Store used filters parameters  ----------------------------------------
  saveRDS(object = QS,
          file   = parameter_fl)
}

#+ echo=F
##  Plots  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ----

##  Open dataset
con <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))

## Select data to plot
DT <- tbl(con, "LAP")                  |>
  filter(Day    > QCrad_plot_date_min) |>
  filter(Day    < QCrad_plot_date_max) |>
  filter(Elevat > QS$plot_elev_T02)

## TODO when plotting ignore previous flagged data or not, but fully apply flag

#' \FloatBarrier
#' \newpage
#'
#' ### Statistics
#'
#+ echo=F, include=T, results="asis"

cat(pander(DT |> select(!!flagname_DIR) |> pull() |> table(),
           caption = flagname_DIR))
cat(" \n \n")

cat(pander(DT |> select(!!flagname_GLB) |> pull() |> table(),
           caption = flagname_GLB))
cat("\n \n")

test <- DT |>
  mutate(dir = Direct_max_ref - DIR_strict,
         glo = Global_max - GLB_strict) |>
  select(dir, glo) |> collect()

cat("\n", range(test$dir, na.rm = TRUE), "\n")

hist(test$dir, breaks = 100,
     main = "Direct_max_ref - DIR_strict")
abline(v = QS$dir_SWdn_too_low)
abline(v = QS$dir_SWdn_min_ext, col = "red")
cat("\n \n")

cat("\n", range(test$glo, na.rm = TRUE), "\n")

hist(test$glo, breaks = 100,
     main = "Global_max - GLB_strict")
abline(v = QS$glo_SWdn_too_low)
abline(v = QS$glo_SWdn_min_ext, col = "red")
cat("\n \n")

## __  Daily plots  -----------------------------------------------------------
#'
#' ### Daily plots
#'
#+ echo=F, include=T, results="asis"
if (DO_PLOTS) {

  DO_PDF <- (!interactive() | isTRUE(getOption('knitr.in.progress')))

  if (DO_PDF) {
    afile <- paste0(DAILY_PLOTS_DIR, "/",
                    sub("\\.R$", "_daily", basename(Script.Name)),
                    ".pdf")
    cat(paste0("[", basename(afile), "](", path.expand(afile),")"),"\n")
    pdf(file = afile)
  }

  ## Direct
  choose <- setdiff(
    DT |> select(QCv10_02_dir_flag) |> distinct() |> pull() |> as.character(),
    c("empty", "pass")
  )
  test <- DT |>
    filter(QCv10_02_dir_flag %in% choose) |>
    select(Day) |>
    collect() |> data.table()

  for (ad in sort(unique(test$Day))) {
    ddd <- as.Date(ad, origin = origin)
    pp  <- DT |> filter(Day == ddd) |> collect() |> data.table()
    setorder(pp, Date)

    ylim <- range(pp$Direct_max_ref, pp$DIR_strict, na.rm = T)
    plot(pp$Date, pp$DIR_strict, "l", col = "blue",
         ylim = ylim, xlab = "", ylab = "wattDIR")
    title(paste("#2", as.Date(ad, origin = "1970-01-01"),
                "N:", pp[!QCv10_02_dir_flag %in% c("empty", "pass"), .N]))

    ## plot limits
    lines(pp$Date, pp$Direct_max_ref, col = "red")
    ## mark offending data
    points(pp[!QCv10_02_dir_flag %in% c("empty", "pass"),
              DIR_strict, Date],
           col = "red", pch = 1)
  }

  ## Global
  choose <- setdiff(
    DT |> select(QCv10_02_glb_flag) |> distinct() |> pull() |> as.character(),
    c("empty", "pass")
  )
  test <- DT |> filter(QCv10_02_glb_flag %in% choose) |> collect() |> data.table()

  for (ad in sort(unique(test$Day))) {
    ddd <- as.Date(ad, origin = origin)
    pp  <- DT |> filter(Day == ddd) |> collect() |> data.table()
    setorder(pp, Date)

    ylim <- range(pp$Global_max, pp$GLB_strict, na.rm = T)
    plot(pp$Date, pp$GLB_strict, "l", col = "green",
         ylim = ylim, xlab = "", ylab = "GLB")
    title(paste("#2", as.Date(ad, origin = "1970-01-01"),
                "N:", pp[!QCv10_02_glb_flag %in% c("empty", "pass"), .N]))
    ## plot limits
    lines(pp$Date, pp$Global_max, col = "red")
    ## mark offending data
    points(pp[!QCv10_02_glb_flag %in% c("empty", "pass"), GLB_strict, Date],
           col = "magenta", pch = 1)
  }
  if (DO_PDF) dummy <- dev.off()
}

#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
goodbye()
