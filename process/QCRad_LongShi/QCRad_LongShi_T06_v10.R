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
require(scales,     warn.conflicts = FALSE, quietly = TRUE)

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
# IGNORE_FLAGGED <- FALSE

flagname_BTH     <- "QCv10_06_bth_flag"
QS$plot_elev_T06 <- 2

if (Sys.info()["nodename"] == "sagan") {

  ##  Open dataset  ------------------------------------------------------------
  con <- dbConnect(duckdb(dbdir = DB_DUCK))

  ## 6. Rayleigh Limit Diffuse Comparison  -------------------------------------
  #'
  #' ## 6. Rayleigh Limit Diffuse Comparison
  #'
  #' Compare inferred diffuse radiation with a modelled value of diffuse,
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
    mu_0 <- cos(SZA*pi/180)
    return( a * mu_0      +
              b * mu_0^2 +
              c * mu_0^3 +
              d * mu_0^4 +
              e * mu_0^5 +
              f * mu_0 * Pressure)
  }

  cat(paste("\n6. Rayleigh Limit Diffuse Comparison", flagname_BTH, "\n\n"))

  ## __ Make categorical columns  -----------------------------------------------
  categories <- c("empty",
                  "pass",
                  "Rayleigh diffuse limit upper (18)",
                  "Rayleigh diffuse limit lower broad (18)",
                  "Rayleigh diffuse limit lower narrow (18)")

  remove_column(con, "LAP", flagname_BTH)
  make_categorical_column(flagname_BTH, categories, con, "LAP")



  ## __ Both  ------------------------------------------------------------------

  ADD <- tbl(con, "LAP")                                   |>
    filter(Elevat > QS$sun_elev_min)                       |>
    filter(!is.na(TSI_TOA))                                |>
    filter(!is.na(Pressure))                               |>
    select(Date, SZA, TSI_TOA, Pressure,
           DIFF_strict, GLB_strict, !!flagname_BTH)        |>
    to_arrow()                                             |>
    mutate(

      RaylDIFF := case_when(
        Rayleigh_diff(SZA = SZA, Pressure = Pressure) > 9000 ~ 9000,
        Rayleigh_diff(SZA = SZA, Pressure = Pressure) < 9000 ~
          Rayleigh_diff(SZA = SZA, Pressure = Pressure)
      ),

    ) |>
    mutate(

      !!flagname_BTH := case_when(
        DIFF_strict - RaylDIFF > QS$Rayleigh_upper_lim ~
          "Rayleigh diffuse limit upper (18)",
        DIFF_strict - RaylDIFF < QS$Rayleigh_lower_lim ~
          "Rayleigh diffuse limit lower broad (18)",
        DIFF_strict - RaylDIFF     < QS$Rayleigh_lower_lim &
          DIFF_strict / GLB_strict < QS$Rayleigh_dif_glo_r &
          GLB_strict               > QS$Rayleigh_glo_min ~
          "Rayleigh diffuse limit lower narrow (18)",

        .default = "pass"
      )
    )

  ## this needs a lot of memory, could do it in batches
  ADD <- ADD |> collect() |> data.table()
  res <- update_table(con, ADD, "LAP", "Date")

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
  filter(Elevat > QS$plot_elev_T06)

## TODO when plotting ignore previous flagged data or not, but fully apply flag

#' \FloatBarrier
#' \newpage
#' ## 4. Climatological (configurable) Limits
#'
#+ echo=F, include=T

## __  Statistics  -------------------------------------------------------------
#' ### Statistics
#+ echo=F, include=T, sesults="asis"
cat(pander(DT |> select(!!flagname_BTH) |> pull() |> table(),
           caption = flagname_BTH))
cat(" \n \n")

pp <- DT |>
  filter(!is.na(DIFF_strict)) |>
  filter(!is.na(RaylDIFF))    |>
  select(DIFF_strict, RaylDIFF) |>
  collect() |> data.table()

hist(pp[, DIFF_strict - RaylDIFF ], breaks = 100)
abline(v = QS$Rayleigh_lower_lim, lty = 3, col = "red")
abline(v = QS$Rayleigh_upper_lim, lty = 3, col = "red")
cat(" \n \n")



## __  Yearly plots  -----------------------------------------------------------
#' ### Yearly plots
#+ echo=F, include=T, results="asis"

## Info flags to ignore in plots
ignore <- grep("QCv10_0[1-5]", DT |> colnames(), value = T)

## Yearly plots for Diffuse
years <- DT |>
  filter(!is.na(DIFF_strict)) |>
  select(year) |> distinct()  |>
  collect()    |> pull()

for (ay in years) {

  pp <- DT |>
    filter(year == ay) |>
    select(Date, SZA, Azimuth,
           DIFF_strict,
           !!flagname_BTH,
           all_of(ignore)) |>
    collect() |> data.table()

  ## Ignore previously flagged
  pp[, Ignore := FALSE]
  if (IGNORE_FLAGGED) {
    pp[
      !QCv10_01_dir_flag %in% c("empty", "pass") |
        !QCv10_01_glb_flag %in% c("empty", "pass") |
        !QCv10_02_dir_flag %in% c("empty", "pass") |
        !QCv10_02_glb_flag %in% c("empty", "pass") |
        !QCv10_03_upp_flag %in% c("empty", "pass") |
        !QCv10_03_low_flag %in% c("empty", "pass") |
        !QCv10_03_obs_flag %in% c("empty", "pass") |
        !QCv10_04_dir_flag %in% c("empty", "pass") |
        !QCv10_04_glb_flag %in% c("empty", "pass") |
        !QCv10_05_dir_flag %in% c("empty", "pass") ,
      Ignore := TRUE]

    # ni <- pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  & !Ignore]
    # ig <- pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  &  Ignore]

    # pp[
    #   pp[, rowSums(!is.na(.SD)) > 0 &
    #        !is.na(get(flagname_BTH)), .SDcols = patterns("^QCv9_0[1-5]")],
    #   Ignore := TRUE
    # ]
  }

  ## plot by SZA
  plot(pp$SZA, pp$DIFF_strict,
       cex = .1,
       xlab = "SZA", ylab = "Diffuse Irradiance" )
  title(main = paste("Rayleigh Limit Diffuse Comparison test 6.", ay))

  ## plot flagged
  points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  & !Ignore, DIFF_strict, SZA], cex = .7, col = alpha("magenta", 0.09))
  points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower narrow (18)" & !Ignore, DIFF_strict, SZA], cex = .7, col = alpha("red",     0.4))
  points(pp[get(flagname_BTH) == "Rayleigh diffuse limit upper (18)"        & !Ignore, DIFF_strict, SZA], cex = .7, col = alpha("blue",    0.5))


  legend("topright",
         legend = c("Diffuse (inferred)", "Rayleigh limit broad", "Rayleigh limit narrow", "Rayleigh limit upper"),
         col    = c("black",              "magenta",              "red",                   "blue"),
         pch = 19, bty = "n", cex = 0.8 )
  cat(" \n \n")


  ## plot by Azimuth
  plot(pp$Azimuth, pp$DIFF_strict,
       cex = .1,
       xlab = "Azimuth", ylab = "Diffuse Irradiance" )
  title(main = paste("Rayleigh Limit Diffuse Comparison test 6.", ay))

  ## plot flagged
  points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  & !Ignore, DIFF_strict, Azimuth], cex = .7, col = alpha("magenta", 0.09))
  points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower narrow (18)" & !Ignore, DIFF_strict, Azimuth], cex = .7, col = alpha("red",     0.7))
  points(pp[get(flagname_BTH) == "Rayleigh diffuse limit upper (18)"        & !Ignore, DIFF_strict, Azimuth], cex = .7, col = alpha("blue",    0.5))

  legend("topright",
         legend = c("Diffuse (inferred)", "Rayleigh limit broad", "Rayleigh limit narrow", "Rayleigh limit upper"),
         col    = c("black",              "magenta",              "red",                   "blue"),
         pch = 19, bty = "n", cex = 0.8 )


  ## plot by Date
  plot(pp$Date, pp$DIFF_strict,
       cex = .1,
       xlab = "SZA", ylab = "Diffuse Irradiance" )
  title(main = paste("Rayleigh Limit Diffuse Comparison test 6.", ay))

  ## plot flagged
  points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  & !Ignore, DIFF_strict, Date], cex = .7, col = alpha("magenta", 0.09))
  points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower narrow (18)" & !Ignore, DIFF_strict, Date], cex = .7, col = alpha("red",     0.7))
  points(pp[get(flagname_BTH) == "Rayleigh diffuse limit upper (18)"        & !Ignore, DIFF_strict, Date], cex = .7, col = alpha("blue",    0.5))

  legend("topright",
         legend = c("Diffuse (inferred)", "Rayleigh limit broad", "Rayleigh limit narrow", "Rayleigh limit upper"),
         col    = c("black",              "magenta",              "red",                   "blue"),
         pch = 19, bty = "n", cex = 0.8 )

}


## __  Daily plots  ------------------------------------------------------------
#' ### Daily plots
#+ echo=F, include=T, results="asis"
if (DO_PLOTS) {

  if (!interactive()) {
    afile <- paste0("~/BBand_LAP/REPORTS/REPORTS/",
                    sub("\\.R$", "", basename(Script.Name)),
                    ".pdf")
    pdf(file = afile)
  }

  choose <- setdiff(
    DT |> select(!!flagname_BTH) |> distinct() |> pull() |> as.character(),
    c("empty", "pass")
  )
  tmp <- DT |>
    filter(QCv10_06_bth_flag %in% choose) |>
    filter(!is.na(DIR_strict)) |>
    select(Day) |>
    distinct()  |> collect() |> data.table()

  for (ad in sort(unique(tmp$Day))) {
    ad <- as.Date(ad, origin = origin)
    pp <- DT |>
      filter(Day == ad) |>
      select(Date, SZA, Azimuth,
             RaylDIFF,
             DIFF_strict, DIR_strict, GLB_strict,
             !!flagname_BTH,
             all_of(ignore)) |>
      collect() |> data.table()
    setorder(pp, Date)

    ## Ignore previously flagged
    pp[, Ignore := FALSE]
    if (IGNORE_FLAGGED) {
      pp[
        !QCv10_01_dir_flag %in% c("empty", "pass") |
          !QCv10_01_glb_flag %in% c("empty", "pass") |
          !QCv10_02_dir_flag %in% c("empty", "pass") |
          !QCv10_02_glb_flag %in% c("empty", "pass") |
          !QCv10_03_upp_flag %in% c("empty", "pass") |
          !QCv10_03_low_flag %in% c("empty", "pass") |
          !QCv10_03_obs_flag %in% c("empty", "pass") |
          !QCv10_04_dir_flag %in% c("empty", "pass") |
          !QCv10_04_glb_flag %in% c("empty", "pass") |
          !QCv10_05_dir_flag %in% c("empty", "pass") ,
        Ignore := TRUE]
    }


    ## skip if all points are ignored
    if (all(pp$Ignore) == TRUE) next()

    ## start figure
    layout(matrix(c(1, 2), 2, 1, byrow = TRUE))
    par(mar = c(2, 4, 2, 1))

    ## plot limits
    ylim <- range(pp$DIFF_strict, pp$RaylDIFF + QS$Rayleigh_upper_lim, na.rm = T)
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

    points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  & !Ignore, DIR_strict, Date], col = alpha("magenta", 0.4))
    points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower broad (18)"  & !Ignore, GLB_strict, Date], col = alpha("magenta", 0.4))
    points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower narrow (18)" & !Ignore, DIR_strict, Date], col = alpha("red",     0.5))
    points(pp[get(flagname_BTH) == "Rayleigh diffuse limit lower narrow (18)" & !Ignore, DIR_strict, Date], col = alpha("red",     0.5))
    points(pp[get(flagname_BTH) == "Rayleigh diffuse limit upper (18)"        & !Ignore, GLB_strict, Date], col = alpha("blue",    0.5))
    points(pp[get(flagname_BTH) == "Rayleigh diffuse limit upper (18)"        & !Ignore, GLB_strict, Date], col = alpha("blue",    0.5))

    layout(1, 1)
  }
}
if (!interactive()) dummy <- dev.off()
#+ echo=F, include=T

## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
