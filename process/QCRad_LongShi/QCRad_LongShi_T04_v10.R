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
knitr::opts_chunk$set(fig.pos   = '!h'    )
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
Script.Name  <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T04_v10.R"
Script.ID    <- "Q4"
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

flagname_DIR     <- "QCv10_04_dir_flag"
flagname_GLB     <- "QCv10_04_glb_flag"
QS$plot_elev_T04 <- 2

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_DUCK))

## 4. Climatological (configurable) Limits  ------------------------------------
#'
#' ## 4. Climatological (configurable) Limits
#'
#' Limits the maximum expected irradiance based on climatological
#' observations levels and the value of TSI.
#'
#' Some hits on first limits are expected and need manual evaluation.
#'
#' Hits on second limit should be problematic data.
#'
#' For GHI this may limit the radiation enhancement cases.
#'
#' Exclusions should be done case by case.

QS$clim_lim_F3_fct <-  0.77
QS$clim_lim_F3_off <- 10
QS$clim_lim_S3_fct <-  0.81
QS$clim_lim_S3_off <- 15

QS$clim_lim_F1_fct <-  1.14
QS$clim_lim_F1_off <- 60
QS$clim_lim_S1_fct <-  1.32
QS$clim_lim_S1_off <- 60

if (Sys.info()["nodename"] == "sagan") {

  cat("\n4. Climatological (configurable) Limits", flagname_DIR, flagname_GLB, "\n\n")

  ## __ Make categorical columns  ----------------------------------------------
  categories <- c("empty",
                  "pass",
                  "Second climatological limit (16)",
                  "First climatological limit (17)")

  ## remove existing
  remove_column(con, "LAP", flagname_DIR)
  remove_column(con, "LAP", flagname_GLB)

  ## create categorical if not existing
  make_categorical_column(flagname_DIR, categories, con, "LAP")
  make_categorical_column(flagname_GLB, categories, con, "LAP")


  ## __ Direct -----------------------------------------------------------------
  ADD <- tbl(con, "LAP")                                   |>
    filter(Elevat > QS$sun_elev_min)                       |>
    filter(!is.na(TSI_TOA))                                |>
    select(Date, SZA, TSI_TOA,
           DIR_strict, !!flagname_DIR)                     |>
    to_arrow()                                             |>
    mutate(

      Dir_First_Clim_lim := case_when(
        TSI_TOA * QS$clim_lim_F3_fct * cos(SZA*pi/180)^0.2 + QS$clim_lim_F3_off > 9000 ~ 9000,
        TSI_TOA * QS$clim_lim_F3_fct * cos(SZA*pi/180)^0.2 + QS$clim_lim_F3_off < 9000 ~ TSI_TOA * QS$clim_lim_F3_fct * cos(SZA*pi/180)^0.2 + QS$clim_lim_F3_off
      ),

      Dir_Secon_Clim_lim := case_when(
        TSI_TOA * QS$clim_lim_S3_fct * cos(SZA*pi/180)^0.2 + QS$clim_lim_S3_off > 9000 ~ 9000,
        TSI_TOA * QS$clim_lim_S3_fct * cos(SZA*pi/180)^0.2 + QS$clim_lim_S3_off < 9000 ~ TSI_TOA * QS$clim_lim_S3_fct * cos(SZA*pi/180)^0.2 + QS$clim_lim_S3_off
      ),

    ) |>
    mutate(

      !!flagname_DIR := case_when(
        DIR_strict > Dir_First_Clim_lim ~ "First climatological limit (17)",
        DIR_strict > Dir_Secon_Clim_lim ~ "Second climatological limit (16)",

        .default = "pass"
      )
    )

  ## this needs a lot of memory, could do it in batches
  ADD <- ADD |> collect() |> data.table()
  res <- update_table(con, ADD, "LAP", "Date")
  rm(ADD); dummy <- gc()


  ## __ Global -----------------------------------------------------------------
  ADD <- tbl(con, "LAP")                                   |>
    filter(Elevat > QS$sun_elev_min)                       |>
    filter(!is.na(TSI_TOA))                                |>
    select(Date, SZA, TSI_TOA,
           GLB_strict, !!flagname_GLB)                     |>
    to_arrow()                                             |>
    mutate(

      Glo_First_Clim_lim := case_when(
        TSI_TOA * QS$clim_lim_F1_fct * cos(SZA*pi/180)^1.2 + QS$clim_lim_F1_off > 9000 ~ 9000,
        TSI_TOA * QS$clim_lim_F1_fct * cos(SZA*pi/180)^1.2 + QS$clim_lim_F1_off < 9000 ~ TSI_TOA * QS$clim_lim_F1_fct * cos(SZA*pi/180)^1.2 + QS$clim_lim_F1_off
      ),

      Glo_Secon_Clim_lim := case_when(
        TSI_TOA * QS$clim_lim_S1_fct * cos(SZA*pi/180)^1.2 + QS$clim_lim_S1_off > 9000 ~ 9000,
        TSI_TOA * QS$clim_lim_S1_fct * cos(SZA*pi/180)^1.2 + QS$clim_lim_S1_off < 9000 ~ TSI_TOA * QS$clim_lim_S1_fct * cos(SZA*pi/180)^1.2 + QS$clim_lim_S1_off
      ),

    ) |>
    mutate(

      !!flagname_GLB := case_when(
        GLB_strict > Glo_First_Clim_lim ~ "First climatological limit (17)",
        GLB_strict > Glo_Secon_Clim_lim ~ "Second climatological limit (16)",

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
con <- dbConnect(duckdb(dbdir = DB_DUCK, read_only = TRUE))

## Select data to plot
DT <- tbl(con, "LAP")                  |>
  filter(Day    > QCrad_plot_date_min) |>
  filter(Day    < QCrad_plot_date_max) |>
  filter(Elevat > QS$plot_elev_T04)

## TODO when plotting ignore previous flagged data or not, but fully apply flag

#' \FloatBarrier
#' \newpage
#' ## 4. Climatological (configurable) Limits
#'
#+ echo=F, include=T, results="asis"

## __  Statistics  -------------------------------------------------------------
#' ### Statistics
#+ echo=F, include=T, sesults="asis"
cat(pander(DT |> select(!!flagname_DIR) |> pull() |> table(),
           caption = flagname_DIR))
cat(" \n \n")

cat(pander(DT |> select(!!flagname_GLB) |> pull() |> table(),
           caption = flagname_GLB))
cat(" \n \n")


test <- DT |>
  filter(!QCv10_04_dir_flag %in% c("empty", "pass") |
           !QCv10_04_glb_flag %in% c("empty", "pass") ) |>
  select(Date,
         DIR_strict,
         GLB_strict,
         Dir_First_Clim_lim, Dir_Secon_Clim_lim,
         Glo_First_Clim_lim, Glo_Secon_Clim_lim,
         !!flagname_DIR, !!flagname_GLB) |>
  collect() |> data.table()

hist(test[, DIR_strict - Dir_First_Clim_lim], breaks = 100,
     main = "Departure Direct from first climatological limit")
cat(" \n \n")

hist(test[, DIR_strict - Dir_Secon_Clim_lim], breaks = 100,
     main = "Departure Direct from second climatological limit")
cat(" \n \n")

hist(test[, GLB_strict - Glo_First_Clim_lim], breaks = 100,
     main = "Departure Direct from first climatological limti")
cat(" \n \n")

hist(test[, GLB_strict - Glo_Secon_Clim_lim], breaks = 100,
     main = "Departure Direct from second climatological limit")
cat(" \n \n")


## __  Yearly plots  -----------------------------------------------------------
#' ### Yearly plots
#+ echo=F, include=T, results="asis"

## Yearly plots for Direct
years <- DT |> filter(!is.na(DIR_strict)) |>
            select(year) |> distinct() |> pull()

## common scale for plots
vars <- c("Dir_First_Clim_lim", "Dir_Secon_Clim_lim", "DIR_strict")
ylim <- c(DT |> summarise(across(all_of(vars), ~ min(., na.rm = T))) |> collect() |> min(na.rm = T),
          DT |> summarise(across(all_of(vars), ~ max(., na.rm = T))) |> collect() |> max(na.rm = T))

for (ay in years) {
  pp <- tbl(con, "LAP") |>
    filter(Elevat > 0)  |>
    filter(year == ay)  |>
    select(Date, SZA, Azimuth,
           DIR_strict,
           Dir_Secon_Clim_lim, Dir_First_Clim_lim,
           !!flagname_DIR) |>
    collect() |> data.table()

  ## plot direct by SZA
  plot(pp$SZA, pp$DIR_strict,
       cex  = .1,
       ylim = ylim,
       xlab = "SZA",
       ylab = "Direct Irradiance")

  ## 4. Second climatological limit (16)
  points(pp$SZA, pp$Dir_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
  ## 4. First climatological limit (17)
  points(pp$SZA, pp$Dir_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

  ## plot flagged
  points(pp[get(flagname_DIR) == "First climatological limit (17)",  DIR_strict, SZA], cex = .7, col = "cyan"   )
  points(pp[get(flagname_DIR) == "Second climatological limit (16)", DIR_strict, SZA], cex = .7, col = "magenta")

  title(main = paste("Direct Beam climatological test 4.", ay))
  legend("topright",
         legend = c("Direct measurements", "Second limit", "First limit", "First hit", "Second hit" ),
         col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
         pch = 19, bty = "n", cex = 0.8 )
  cat(" \n \n")


  ## plot direct by Azimuth
  plot(pp$Azimuth, pp$DIR_strict,
       cex  = .1,
       ylim = ylim,
       xlab = "Azimuth",
       ylab = "Direct Irradiance")

  ## 4. Second climatological limit (16)
  points(pp$Azimuth, pp$Dir_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
  ## 4. First climatological limit (17)
  points(pp$Azimuth, pp$Dir_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

  ## plot flagged
  points(pp[get(flagname_DIR) == "First climatological limit (17)",  DIR_strict, Azimuth], cex = .7, col = "cyan"   )
  points(pp[get(flagname_DIR) == "Second climatological limit (16)", DIR_strict, Azimuth], cex = .7, col = "magenta")

  title(main = paste("Direct Beam climatological test 4.", ay))
  legend("topright",
         legend = c("Direct measurements", "Second limit", "First limit", "First hit", "Second hit" ),
         col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
         pch = 19, bty = "n", cex = 0.8 )
  cat(" \n \n")


  ## plot direct by Date
  plot(pp$Date, pp$DIR_strict,
       cex  = .1,
       ylim = ylim,
       xlab = "",
       ylab = "Direct Irradiance")

  ## 4. Second climatological limit (16)
  points(pp$Date, pp$Dir_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
  ## 4. First climatological limit (17)
  points(pp$Date, pp$Dir_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

  ## plot flagged
  points(pp[get(flagname_DIR) == "First climatological limit (17)",  DIR_strict, Date], cex = .7, col = "cyan"   )
  points(pp[get(flagname_DIR) == "Second climatological limit (16)", DIR_strict, Date], cex = .7, col = "magenta")

  title(main = paste("Direct Beam climatological test 4.", ay))
  legend("topright",
         legend = c("Direct measurements", "Second limit", "First limit", "First hit", "Second hit" ),
         col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
         pch = 19, bty = "n", cex = 0.8 )
  cat(" \n \n")
}



## Yearly plots for Global
years <- DT |> filter(!is.na(DIR_strict)) |>
  select(year) |> distinct() |> pull()

## common scale
vars <- c("Glo_First_Clim_lim", "Glo_Secon_Clim_lim", "DIR_strict")
ylim <- c(DT |> summarise(across(all_of(vars), ~ min(., na.rm = T))) |> collect() |> min(na.rm = T),
          DT |> summarise(across(all_of(vars), ~ max(., na.rm = T))) |> collect() |> max(na.rm = T))

for (ay in years) {
  pp <- tbl(con, "LAP") |>
    filter(Elevat > 0)  |>
    filter(year == ay)  |>
    select(Date, SZA, Azimuth,
           DIR_strict,
           Glo_Secon_Clim_lim, Glo_First_Clim_lim,
           !!flagname_GLB) |>
    collect() |> data.table()

  ## plot direct by SZA
  plot(pp$SZA, pp$DIR_strict,
       cex  = .1,
       ylim = ylim,
       xlab = "SZA",
       ylab = "Global Irradiance")

  ## 4. Second climatological limit (16)
  points(pp$SZA, pp$Glo_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
  ## 4. First climatological limit (17)
  points(pp$SZA, pp$Glo_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

  ## plot flagged
  points(pp[get(flagname_GLB) == "First climatological limit (17)",  DIR_strict, SZA], cex = .7, col = "cyan"   )
  points(pp[get(flagname_GLB) == "Second climatological limit (16)", DIR_strict, SZA], cex = .7, col = "magenta")

  title(main = paste("Global climatological test 4.", ay))
  legend("topright",
         legend = c("Global measurements", "Second limit", "First limit", "First hit", "Second hit" ),
         col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
         pch = 19, bty = "n", cex = 0.8 )
  cat(" \n \n")


  ## plot direct by Azimuth
  plot(pp$Azimuth, pp$DIR_strict,
       cex  = .1,
       ylim = ylim,
       xlab = "Azimuth",
       ylab = "Global Irradiance")

  ## 4. Second climatological limit (16)
  points(pp$Azimuth, pp$Glo_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
  ## 4. First climatological limit (17)
  points(pp$Azimuth, pp$Glo_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

  ## plot flagged
  points(pp[get(flagname_GLB) == "First climatological limit (17)",  DIR_strict, Azimuth], cex = .7, col = "cyan"   )
  points(pp[get(flagname_GLB) == "Second climatological limit (16)", DIR_strict, Azimuth], cex = .7, col = "magenta")

  title(main = paste("Global Beam climatological test 4.", ay))
  legend("topright",
         legend = c("Global measurements", "Second limit", "First limit", "First hit", "Second hit" ),
         col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
         pch = 19, bty = "n", cex = 0.8 )
  cat(" \n \n")


  ## plot direct by Date
  plot(pp$Date, pp$DIR_strict,
       cex  = .1,
       ylim = ylim,
       xlab = "",
       ylab = "Global Irradiance")

  ## 4. Second climatological limit (16)
  points(pp$Date, pp$Glo_Secon_Clim_lim, cex = .2, col = alpha("red",  0.01))
  ## 4. First climatological limit (17)
  points(pp$Date, pp$Glo_First_Clim_lim, cex = .2, col = alpha("blue", 0.01))

  ## plot flagged
  points(pp[get(flagname_GLB) == "First climatological limit (17)",  DIR_strict, Date], cex = .7, col = "cyan"   )
  points(pp[get(flagname_GLB) == "Second climatological limit (16)", DIR_strict, Date], cex = .7, col = "magenta")

  title(main = paste("Direct Beam climatological test 4.", ay))
  legend("topright",
         legend = c("Direct measurements", "Second limit", "First limit", "First hit", "Second hit" ),
         col    = c("black",               "red",          "blue",        "cyan",      "magenta"    ),
         pch = 19, bty = "n", cex = 0.8 )
  cat(" \n \n")
}


## __  Daily plots  -----------------------------------------------------------
#' ### Daily plots
#+ echo=F, include=T, results="asis"
if (DO_PLOTS) {

  if (!interactive()) {
    afile <- paste0(DAILY_PLOTS_DIR, "/",
                    sub("\\.R$", "_daily", basename(Script.Name)),
                    ".pdf")
    pdf(file = afile)
  }

  choose <- setdiff(
    DT |> select(!!flagname_DIR) |> distinct() |> pull() |> as.character(),
    c("empty", "pass")
  )
  temp1 <- DT |>
    filter(QCv10_04_dir_flag %in% choose) |>
    filter(!is.na(DIR_strict)) |>
    select(Day) |>
    distinct()  |> collect() |> data.table()

  for (ad in sort(unique(temp1$Day))) {
    ad <- as.Date(ad, origin = origin)
    pp <- DT |>
      filter(Day == ad) |>
      # filter(!is.na(DIR_strict)) |>
      select(Date,
             DIR_strict,
             Dir_First_Clim_lim, Dir_Secon_Clim_lim,
             !!flagname_DIR) |>
      collect() |> data.table()
    setorder(pp, Date)

    ylim <- range(pp$Dir_First_Clim_lim,
                  pp$Dir_Secon_Clim_lim,
                  pp$DIR_strict, na.rm = T)
    plot(pp$Date, pp$DIR_strict, "l", col = "blue",
         ylim = ylim, ylab = "DIR_strict", xlab = "")
    title(paste("#4", as.Date(ad, origin = "1970-01-01")))
    ## plot limits
    lines(pp$Date, pp$Dir_First_Clim_lim, col = "pink")
    lines(pp$Date, pp$Dir_Secon_Clim_lim, col = "red" )
    ## mark offending data
    points(pp[!get(flagname_DIR) %in% c("empty", "pass"), DIR_strict, Date],
           col = "red", pch = 1)
  }


  ## test global first limit
  choose <- setdiff(
    DT |> select(!!flagname_GLB) |> distinct() |> pull() |> as.character(),
    c("empty", "pass")
  )
  temp1 <- DT |>
    filter(QCv10_04_glb_flag %in% choose) |>
    filter(!is.na(GLB_strict)) |>
    select(Day) |>
    distinct()  |> collect() |> data.table()

  for (ad in sort(unique(temp1$Day))) {
    ad <- as.Date(ad, origin = origin)
    pp <- DT |>
      filter(Day == ad) |>
      # filter(!is.na(GLB_strict)) |>
      select(Date,
             GLB_strict,
             Glo_First_Clim_lim, Glo_Secon_Clim_lim,
             !!flagname_GLB) |>
      collect() |> data.table()
    setorder(pp, Date)

    ylim <- range(pp$Glo_First_Clim_lim,
                  pp$Glo_Secon_Clim_lim,
                  pp$GLB_strict, na.rm = T)

    plot(pp$Date, pp$GLB_strict, "l", col = "green",
         ylim = ylim, xlab = "", ylab = "GLB_strict")
    title(paste("#4", as.Date(ad, origin = "1970-01-01")))
    ## plot limits
    lines(pp$Date, pp$Glo_First_Clim_lim, col = "pink")
    lines(pp$Date, pp$Glo_Secon_Clim_lim, col = "red" )
    ## mark offending data
    points(pp[!get(flagname_GLB) %in% c("empty", "pass"), GLB_strict, Date],
           col = "red", pch = 1)
  }
}
if (!interactive()) dummy <- dev.off()

#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
