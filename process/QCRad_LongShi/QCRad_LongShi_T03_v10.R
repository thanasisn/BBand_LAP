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

        ## FIXME test
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


stop("remove test above")

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



# ####  3. Comparison tests per BSRN “non-definitive”  ---------------------------
# #' \FloatBarrier
# #' \newpage
# #' ## 3. Comparison tests per BSRN “non-definitive”
# #'
# #+ echo=F, include=T, results="asis"
#
# flagname_UPP <- paste0("QCv9_", sprintf("%02d", testN), "_upp_flag")
# flagname_LOW <- paste0("QCv9_", sprintf("%02d", testN), "_low_flag")
# flagname_OBS <- paste0("QCv9_", sprintf("%02d", testN), "_obs_flag")
#
# cat(pander(table(collect(select(BB, !!flagname_UPP)), useNA = "always"),
#            caption = flagname_UPP))
# cat(" \n \n")
#
# cat(pander(table(collect(select(BB, !!flagname_LOW)), useNA = "always"),
#            caption = flagname_LOW))
# cat(" \n \n")
#
# cat(pander(table(collect(select(BB, !!flagname_OBS)), useNA = "always"),
#            caption = flagname_LOW))
# cat(" \n \n")
#
# years <- (BB |> filter(!is.na(DiffuseFraction_kd)) |>
#             select(year) |> unique() |> collect() |> pull())
# for (ay in years) {
#   pp <- data.table(BB |> filter(year(Date) == ay & Elevat > 0) |> collect())
#   ylim <- c(-30, 1.1)
#   ylim <- c(-0.5, 1.5)
#
#   par(mar = c(4, 4, 2, 1))
#
#   ## plot limits by SZA
#   plot(pp$SZA, pp$DiffuseFraction_kd,
#        ylab = "Not Diffuse fraction", xlab = "SZA", ylim = ylim,
#        cex = .1)
#   title(paste("#3", ay))
#
#   segments(               0, QS$dif_rati_pr1, QS$dif_sza_break, QS$dif_rati_pr1, col = "red" )
#   segments(QS$dif_sza_break, QS$dif_rati_pr2,               93, QS$dif_rati_pr2, col = "red" )
#
#   segments(               0, QS$dif_rati_po1, QS$dif_sza_break, QS$dif_rati_po1, col = "blue")
#   segments(QS$dif_sza_break, QS$dif_rati_po2,               93, QS$dif_rati_po2, col = "blue")
#
#   points(pp[!is.na(get(flagname_UPP)), DiffuseFraction_kd, SZA],
#          cex = .2, col = "red")
#   points(pp[!is.na(get(flagname_LOW)), DiffuseFraction_kd, SZA],
#          cex = .2, col = "cyan")
#   points(pp[!is.na(get(flagname_OBS)), DiffuseFraction_kd, SZA],
#          cex = .3, col = "#BFE46C")
#   cat(" \n \n")
#
#
#   ## plot limits by date
#   plot(pp$Date, pp$DiffuseFraction_kd,
#        ylab = "Not Diffuse fraction", xlab = "SZA", ylim = ylim,
#        cex = .1)
#   title(paste("#3", ay))
#
#   points(pp[!is.na(get(flagname_UPP)), DiffuseFraction_kd, Date],
#          cex = .2, col = "red")
#   points(pp[!is.na(get(flagname_LOW)), DiffuseFraction_kd, Date],
#          cex = .2, col = "cyan")
#   points(pp[!is.na(get(flagname_OBS)), DiffuseFraction_kd, Date],
#          cex = .3, col = "#BFE46C")
#   cat(" \n \n")
#
#
#   ## plot limits by Azimuth
#   plot(pp$Azimuth, pp$DiffuseFraction_kd,
#        ylim = ylim,
#        ylab = "Not Diffuse fraction", xlab = "Azimuth",
#        cex = .1)
#   title(paste("#3", ay))
#
#   points(pp[!is.na(get(flagname_UPP)), DiffuseFraction_kd, Azimuth],
#          cex = .2, col = "red")
#   points(pp[!is.na(get(flagname_LOW)), DiffuseFraction_kd, Azimuth],
#          cex = .2, col = "cyan")
#   points(pp[!is.na(get(flagname_OBS)), DiffuseFraction_kd, Azimuth],
#          cex = .3, col = "#BFE46C")
#   cat(" \n \n")
#
#
#   ## plot by sky position
#   plot(pp$Azimuth, pp$Elevat,
#        ylab = "Elevation", xlab = "Azimuth",
#        cex = .1)
#   title(paste("#3", ay))
#
#   points(pp[!is.na(get(flagname_UPP)), Elevat, Azimuth],
#          cex = .2, col = "red")
#   points(pp[!is.na(get(flagname_LOW)), Elevat, Azimuth],
#          cex = .3, col = "cyan")
#   points(pp[!is.na(get(flagname_OBS)), Elevat, Azimuth],
#          cex = .2, col = "#BFE46C")
#   cat(" \n \n")
#
# }
#
# if (DO_PLOTS) {
#
#   if (!interactive()) {
#     pdf(paste0("~/BBand_LAP/REPORTS/REPORTS/QCRad_V9_F", testN, ".pdf"))
#   }
#
#   tmp <- BB |> filter(!is.na(QCv9_03_upp_flag)     |
#                         !is.na(QCv9_03_low_flag) |
#                         !is.na(QCv9_03_obs_flag)) |> collect() |> as.data.table()
#
#   for (ad in sort(unique(c(as.Date(tmp$Date))))) {
#
#     pp <- data.table(
#       BB |> filter(as.Date(Date) == as.Date(ad) &
#                      Elevat > QS$sun_elev_min)   |>
#         collect()
#     )
#
#     layout(matrix(c(1, 2), 2, 1, byrow = TRUE))
#     par(mar = c(2,4,2,1))
#
#     plot(pp$Date, pp$DiffuseFraction_kd, "l",
#          col = "cyan", ylab = "Not Diffuse Fraction", xlab = "")
#
#     abline(h = QS$dif_rati_pr1, col = "red")
#     abline(h = QS$dif_rati_pr2, col = "red",     lty = 2)
#     abline(h = QS$dif_rati_po1, col = "blue")
#     abline(h = QS$dif_rati_po2, col = "blue",    lty = 2)
#     abline(h = QS$dif_rati_min, col = "#BFE46C", lty = 2)
#
#
#     title(paste("#3", as.Date(ad, origin = "1970-01-01")))
#
#     par(mar = c(2, 4, 1, 1))
#     ylim <- range(pp$GLB_strict, pp$DIR_strict, na.rm = T)
#     plot( pp$Date, pp$GLB_strict, "l",
#           ylim = ylim, col = "green", ylab = "", xlab = "")
#     lines(pp$Date, pp$DIR_strict, col = "blue" )
#
#     points(pp[!is.na(QCv9_03_upp_flag), Date],
#            pp[!is.na(QCv9_03_upp_flag), DIR_strict],
#            ylim = ylim, col = "red")
#     points(pp[!is.na(QCv9_03_upp_flag), Date],
#            pp[!is.na(QCv9_03_upp_flag), GLB_strict],
#            ylim = ylim, col = "red")
#     points(pp[!is.na(QCv9_03_low_flag), Date],
#            pp[!is.na(QCv9_03_low_flag), DIR_strict],
#            ylim = ylim, col = "magenta")
#     points(pp[!is.na(QCv9_03_low_flag), Date],
#            pp[!is.na(QCv9_03_low_flag), GLB_strict],
#            ylim = ylim, col = "magenta")
#     points(pp[!is.na(QCv9_03_obs_flag), Date],
#            pp[!is.na(QCv9_03_obs_flag), DIR_strict],
#            ylim = ylim, col = "#BFE46C")
#     points(pp[!is.na(QCv9_03_obs_flag), Date],
#            pp[!is.na(QCv9_03_obs_flag), GLB_strict],
#            ylim = ylim, col = "#BFE46C")
#
#     ## reset layout
#     layout(1)
#   }
# }
# rm(list = ls(pattern = "flagname_.*"))
# dummy <- gc()
# if (!interactive()) dummy <- dev.off()
# #+ echo=F, include=T




## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
