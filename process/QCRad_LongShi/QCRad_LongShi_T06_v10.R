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
Script.Name  <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T06_v10.R"
Script.ID    <- "Q6"
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

flagname_BTH <- "QCv10_06_bth_flag"

if (Sys.info()["nodename"] == "sagan") {

  ##  Open dataset  ------------------------------------------------------------
  con <- dbConnect(duckdb(dbdir = DB_DUCK))

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

    cat(paste("\n6. Rayleigh Limit Diffuse Comparison", flagname_BTH, "\n\n"))

    ## __ Make categorical columns  --------------------------------------------
    categories <- c("empty",
                    "pass",
                    "Rayleigh diffuse limit upper (18)",
                    "Rayleigh diffuse limit lower broad (18)",
                    "Rayleigh diffuse limit lower narrow (18)")

    remove_column(con, "LAP", flagname_DIR)
    make_categorical_column(flagname_DIR, categories, con, "LAP")



    stop("kkkkk")

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
  filter(Elevat > QCrad_plot_elev_T05)


## TODO when plotting ignore previous flagged data or not, but fully apply flag

#' \FloatBarrier
#' \newpage
#' ## 4. Climatological (configurable) Limits
#'
#+ echo=F, include=T

## __  Statistics  -------------------------------------------------------------
#' ### Statistics
#+ echo=F, include=T



## __  Daily plots  ------------------------------------------------------------
#' ### Daily plots
#+ echo=F, include=T


## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
