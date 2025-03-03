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
#' **QCRad T04**
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
knitr::opts_chunk$set(fig.pos   = "!h"     )
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
Script.Name  <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T05_v10.R"
Script.ID    <- "Q5"
parameter_fl <- "~/BBand_LAP/SIDE_DATA/QCRad_LongShi_v10_duck_parameters.Rds"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)

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

flagname_DIR     <- "QCv10_05_dir_flag"
QS$plot_elev_T05 <- 2


##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_BROAD))

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

if (Sys.info()["nodename"] == "sagan") {

  cat(paste("\n5. Tracking test", flagname_DIR, "\n\n"))

  ## __ Make categorical columns  ----------------------------------------------
  categories <- c("empty",
                  "pass",
                  "Possible no tracking (24)")

  remove_column(con, "LAP", flagname_DIR)
  make_categorical_column(flagname_DIR, categories, con, "LAP")

  ## __ Direct -----------------------------------------------------------------
  ADD <- tbl(con, "LAP")                        |>
    filter(Elevat > QS$sun_elev_min)            |>
    select(Date, SZA, Sun_Dist_Astropy, Elevat,
           DIR_strict, DIFF_strict, GLB_strict,
           !!flagname_DIR)                      |>
    to_arrow()                                  |>
    mutate(

      ## Clear Sky Sort-Wave model
      ClrSW_ref := case_when(
        (QS$ClrSW_a / Sun_Dist_Astropy^2) * cos(SZA * pi / 180)^QS$ClrSW_b > 9000
        ~ 9000,
        (QS$ClrSW_a / Sun_Dist_Astropy^2) * cos(SZA * pi / 180)^QS$ClrSW_b < 9000
        ~ (QS$ClrSW_a / Sun_Dist_Astropy^2) * cos(SZA * pi / 180)^QS$ClrSW_b
      ),

    ) |>
    mutate(

      !!flagname_DIR := case_when(
        GLB_strict  / ClrSW_ref > QS$ClrSW_lim           &
          DIFF_strict / GLB_strict > QS$ClrSW_lim         &
          GLB_strict               > QS$glo_min           &
          Elevat                   > QS$Tracking_min_elev ~ "Possible no tracking (24)",

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
  filter(Elevat > QS$plot_elev_T05)

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

test <- DT |>
  select(DIR_strict, GLB_strict, DIFF_strict, ClrSW_ref) |>
  collect() |> data.table()


hist(test[GLB_strict / ClrSW_ref < 2,
          GLB_strict / ClrSW_ref], breaks = 100)
abline(v = QS$ClrSW_lim, col = "red", lty = 3)
cat(" \n \n")

hist(test[DIFF_strict / GLB_strict > -0.5,
          DIFF_strict / GLB_strict], breaks = 100)
abline(v = QS$ClrSW_lim, col = "red", lty = 3)
cat(" \n \n")

hist(test[, GLB_strict], breaks = 100)
abline(v = QS$glo_min, col = "red", lty = 3)
cat(" \n \n")


## __  Daily plots  -----------------------------------------------------------
#'
#' ### Daily plots
#'
#+ echo=F, include=T, results="asis"
if (DO_PLOTS) {

  DO_PDF <- (!interactive() | isTRUE(getOption("knitr.in.progress")))

  if (DO_PDF) {
    afile <- paste0(DAILY_PLOTS_DIR, "/",
                    sub("\\.R$", "_daily", basename(Script.Name)),
                    ".pdf")
    cat(paste0("[", basename(afile), "](", path.expand(afile),")"),"\n")
    pdf(file = afile)
  }

  choose <- setdiff(
    DT |> select(!!flagname_DIR) |> distinct() |> pull() |> as.character(),
    c("empty", "pass")
  )
  tmp <- DT |>
    filter(QCv10_05_dir_flag %in% choose) |>
    filter(!is.na(DIR_strict)) |>
    select(Day) |>
    distinct()  |> collect() |> data.table()

  for (ad in sort(unique(tmp$Day))) {
    ad <- as.Date(ad, origin = origin)
    pp <- DT |>
      filter(Day == ad) |>
      select(Date,
             DIR_strict, GLB_strict, HOR_strict,
             ClrSW_ref,
             !!flagname_DIR) |>
      collect() |> data.table()
    setorder(pp, Date)

    ylim <- range(pp$ClrSW_ref, pp$DIR_strict, pp$GLB_strict, pp$HOR_strict, na.rm = TRUE)
    plot(pp$Date, pp$DIR_strict, "l", col = "blue",
         ylim = ylim, xlab = "", ylab = "wattDIR")
    lines(pp$Date, pp$GLB_strict, col = "green")
    lines(pp$Date, pp$HOR_strict, col = "cyan")
    title(paste("#5", as.Date(ad, origin = "1970-01-01")))
    ## plot limits
    lines(pp$Date, pp$ClrSW_ref, col = "magenta")
    ## mark offending data
    points(pp[!get(flagname_DIR) %in% c("empty", "pass"), DIR_strict, Date],
           col = "red", pch = 1)
  }
  if (DO_PDF) dummy <- dev.off()
}

#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
goodbye()
