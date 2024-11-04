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

#' **QCRad T04**
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
Script.Name  <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T05_v10.R"
Script.ID    <- "Q5"
parameter_fl <- "~/BBand_LAP/SIDE_DATA/QCRad_LongShi_v10_duck_parameters.Rds"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/",   basename(sub("\\.R$", ".pdf", Script.Name))))
    # sink(file = paste0("~/BBand_LAP/REPORTS/LOGs/duck/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
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

flagname_DIR <- "QCv10_05_dir_flag"

if (Sys.info()["nodename"] == "sagan") {

  ##  Open dataset  ------------------------------------------------------------
  con <- dbConnect(duckdb(dbdir = DB_DUCK))

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

  cat(paste("\n5. Tracking test", flagname_DIR, "\n\n"))

  ## __ Make categorical columns  ----------------------------------------------
  categories <- c("empty",
                  "pass",
                  "Possible no tracking (24)")

  remove_column(con, "LAP", flagname_DIR)
  make_categorical_column(flagname_DIR, categories, con, "LAP")

  stop("kkkkkkk")

  ## Clear Sky Sort-Wave model
  datapart[, ClrSW_ref2 := (QS$ClrSW_a / Sun_Dist_Astropy^2) * cosde(SZA)^QS$ClrSW_b]

  ## __ Direct -----------------------------------------------------------
  datapart[GLB_strict  / ClrSW_ref2 > QS$ClrSW_lim &
             DIFF_strict / GLB_strict > QS$ClrSW_lim &
             GLB_strict               > QS$glo_min   &
             Elevat                   > QS$Tracking_min_elev,
           (flagname_DIR) := "Possible no tracking (24)"]




  ## this needs a lot of memory, could do it in batches
  ADD <- ADD |> collect() |> data.table()
  res <- update_table(con, ADD, "LAP", "Date")
  rm(ADD); dummy <- gc()

  ## __  Store used filters parameters  ----------------------------------------
  saveRDS(object = QS,
          file   = parameter_fl)
}

##  Plots  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ----

##  Open dataset
con <- dbConnect(duckdb(dbdir = DB_DUCK, read_only = TRUE))

## Select data to plot
DT <- tbl(con, "LAP")                  |>
  filter(Day    > QCrad_plot_date_min) |>
  filter(Day    < QCrad_plot_date_max) |>
  filter(Elevat > QCrad_plot_elev_T4)

# DT |> select(!!flagname_GLB) |> distinct() |> collect()
# DT |> select(!!flagname_DIR) |> distinct()
# DT |> select(!!flagname_GLB) |> pull() |> table()

## TODO when plotting ignore previous flagged data or not, but fully apply flag

#' \FloatBarrier
#' \newpage
#' ## 4. Climatological (configurable) Limits
#'
#+ echo=F, include=T

## __  Stats  ------------------------------------------------------------------
#' ### Stats
#+ echo=F, include=T


## __  Yearly plots  -----------------------------------------------------------
#' ### Yearly plots
#+ echo=F, include=T



#+ echo=F, include=T

## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
