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
Script.Name  <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T03_v10.R"
Script.ID    <- "Q3"
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

## __ Select a part of data to plot  -------------------------------------------
PARTIAL    <- FALSE
PARTIAL    <- TRUE
PLOT_FIRST <- as_date("1993-01-01")
PLOT_LAST  <- as_date("2024-01-01")



flagname_UPP <- "QCv10_03_upp_flag"
flagname_LOW <- "QCv10_03_low_flag"
flagname_OBS <- "QCv10_03_obs_flag"

if (Sys.info()["nodename"] == "sagan") {

  ##  Open dataset  ------------------------------------------------------------
  con <- dbConnect(duckdb(dbdir = DB_DUCK))

  ## 3. Comparison tests per BSRN “non-definitive”  --------------------------
  #'
  #' ## 3. Comparison tests per BSRN “non-definitive”
  #'
  #' Aplies to both, but probably more relevant for global
  #'

  QS$dif_rati_min  <-  0.001 # DiffuseFraction_kd low limit this make obstacles stand out
  QS$dif_rati_po1  <-  0.03  # DiffuseFraction_kd low limit
  QS$dif_rati_po2  <-  0.08  # My DiffuseFraction_kd low limit
  QS$dif_sza_break <- 75     # SZA break point
  QS$dif_rati_pr1  <-  1.03  # DiffuseFraction_kd upper limit
  QS$dif_rati_pr2  <-  1.06  # My DiffuseFraction_kd upper limit
  QS$dif_watt_lim  <- 10     # Filter only when GLB is above that

  cat(paste("\n3. Comparison tests", flagname_UPP, flagname_LOW, flagname_OBS, "\n\n"))

  ## __ Make categorical columns  ----------------------------------------------
  categories <- c("empty",
                  "pass",
                  "Diffuse ratio comp max (11)",
                  "Diffuse ratio comp min (12)")

  remove_column(con, "LAP", flagname_UPP)
  remove_column(con, "LAP", flagname_LOW)
  make_categorical_column(flagname_UPP, categories, con, "LAP")
  make_categorical_column(flagname_LOW, categories, con, "LAP")

  categories <- c("empty",
                  "pass",
                  "Diffuse ratio obstacle min (13)")

  remove_column(con, "LAP", flagname_OBS)
  make_categorical_column(flagname_OBS, categories, con, "LAP")


  ## __ Proposed filter  -------------------------------------------------
  ADD <- tbl(con, "LAP")                              |>
    filter(Elevat > QS$sun_elev_min)                  |>
    filter(!is.na(GLB_strict))                        |>
    filter(!is.na(DiffuseFraction_kd))                |>
    select(Date, GLB_strict, SZA, DiffuseFraction_kd) |>
    mutate(

      !!flagname_UPP := case_when(

        DiffuseFraction_kd  > QS$dif_rati_pr1  &
          SZA              <= QS$dif_sza_break &
          GLB_strict        > QS$dif_watt_lim  ~ "Diffuse ratio comp max (11)",

        DiffuseFraction_kd  > QS$dif_rati_pr2  &
          SZA               > QS$dif_sza_break &
          GLB_strict        > QS$dif_watt_lim  ~ "Diffuse ratio comp max (11)",

        ## test
        SZA               > QS$dif_sza_break ~ "Diffuse ratio comp max (11)",

        .default = "pass"
      ))
  res <- update_table(con, ADD, "LAP", "Date")


  ## __ Extra filters by me  ---------------------------------------------
  ADD <- tbl(con, "LAP")                              |>
    filter(Elevat > QS$sun_elev_min)                  |>
    filter(!is.na(GLB_strict))                        |>
    filter(!is.na(DiffuseFraction_kd))                |>
    select(Date, GLB_strict, SZA, DiffuseFraction_kd) |>
    mutate(

      !!flagname_LOW := case_when(

        DiffuseFraction_kd  < QS$dif_rati_po1  &
          SZA              <= QS$dif_sza_break &
          GLB_strict        > QS$dif_watt_lim  ~ "Diffuse ratio comp min (12)",

        DiffuseFraction_kd  < QS$dif_rati_po2  &
          SZA               > QS$dif_sza_break &
          GLB_strict        > QS$dif_watt_lim  ~ "Diffuse ratio comp min (12)",

        .default = "pass"
      ))
  res <- update_table(con, ADD, "LAP", "Date")


  ## __ This is good for systematic obstacle highlight  ------------------
  ADD <- tbl(con, "LAP")               |>
    filter(Elevat > QS$sun_elev_min)   |>
    filter(!is.na(GLB_strict))         |>
    filter(!is.na(DiffuseFraction_kd)) |>
    select(Date, DiffuseFraction_kd)   |>
    mutate(

      !!flagname_OBS := case_when(

        DiffuseFraction_kd  < QS$dif_rati_min  ~ "Diffuse ratio obstacle min (13)",

        .default = "pass"
      ))
  res <- update_table(con, ADD, "LAP", "Date")


  ## __  Store used filters parameters  ----------------------------------------
  saveRDS(object = QS,
          file   = parameter_fl)
}


stop("kdfsdk")

## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ----



##  Open dataset
con <- dbConnect(duckdb(dbdir = DB_DUCK, read_only = TRUE))



## Check that flags exist
tbl(con, "LAP") |> colnames() %in% c(flagname_GLB, flagname_DIR)


DT <- tbl(con, "LAP") |>
  filter(Elevat > QS$sun_elev_min)  ## sun is up

DT |>
  # filter(!!flagname_DIR != "empty") |>
  select(!!flagname_DIR) |>
  group_by(!!flagname_DIR) |> tally()


DT |>
  # filter(!is.na(!!flagname_GLB)) |>
  select(!!flagname_GLB) |>
  group_by(!!flagname_GLB) |> tally()

dd <- DT |> head() |> collect() |> data.table()


tbl(con, "LAP") |> colnames() %in% "Global_max"
tbl(con, "LAP") |> filter(!is.na(Global_max))

tbl(con, "LAP") |> filter(is.na(Global_max))

tbl(con, "LAP") |> summarise(mean(Global_max, na.rm = T))
tbl(con, "LAP") |> summarise(max(Global_max, na.rm = T))




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




## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
