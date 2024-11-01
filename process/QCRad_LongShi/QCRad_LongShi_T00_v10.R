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

#' **QCRad T00**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#' Create some basic radiometric data
#'  - DIR_strict
#'  - GLB_strict
#'  - HOR_strict
#'  - DIFF_strict
#'  - DiffuseFraction_kd
#'  - Transmittance_GLB
#'
#+ echo=F, include=T

#+ echo=F, include=F
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
Script.Name  <- "~/BBand_LAP/process/QCRad_LongShi/QCRad_LongShi_T00_v10.R"
Script.ID    <- "Q0"
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
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)

##  Variables  -----------------------------------------------------------------
if (file.exists(parameter_fl)) {
  QS <<- readRDS(parameter_fl)
} else {
  QS <<- list()
}

QS$sun_elev_min <- 0  ## Drop ALL radiation data when sun is below this point

##  Create strict radiation data  ----------------------------------------------
if (Sys.info()["nodename"] == "sagan") {
  cat(paste("\n0. Create radiometric variables", "\n\n"))

  ##  Open dataset  ------------------------------------------------------------
  con <- dbConnect(duckdb(dbdir = DB_DUCK))

  DT <- tbl(con, "LAP") |>
    filter(Elevat > QS$sun_elev_min)  ## sun is up

  ## __ GHI  -------------------------------------------------------------------
  make_null_column(con, "LAP", "GLB_strict") ## Always create empty column

  ##  Prepare strict global irradiance
  ADD <- tbl(con, "LAP") |>
    filter(Elevat > QS$sun_elev_min)    |>  ## sun is up
    filter(!is.na(CM21_sig))            |>  ## valid measurements
    filter(is.na(cm21_bad_data_flag))   |>  ## not bad data
    filter(cm21_sig_limit_flag == 0)    |>  ## in acceptable values range
    select(Date, GLB_wpsm)              |>
    mutate(

      GLB_strict = case_when(
        GLB_wpsm <  0 ~ 0,                    ## Negative values to zero
        GLB_wpsm >= 0 ~ GLB_wpsm              ## All other selected values as is

      ))
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")

  ## __ DNI  -------------------------------------------------------------------
  make_null_column(con, "LAP", "DIR_strict") ## Always create empty column

  ##  Prepare strict direct radiation
  ADD <- tbl(con, "LAP") |>
    filter(Elevat > QS$sun_elev_min)  |>  ## sun is up
    filter(!is.na(CHP1_sig))          |>  ## valid measurements
    filter(is.na(chp1_bad_data_flag)) |>  ## not bad data
    filter(chp1_sig_limit_flag == 0)  |>  ## acceptable values range
    filter(Async_tracker_flag != T)   |>  ## not in an async
    select(Date, DIR_wpsm, SZA)       |>
    mutate(

      DIR_strict = case_when(
        DIR_wpsm <  0 ~ 0,                ## Negative values to zero
        DIR_wpsm >= 0 ~ DIR_wpsm          ## All other selected values as is

      ))
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")

  ## __  HOR  ------------------------------------------------------------------
  make_null_column(con, "LAP", "HOR_strict") ## Always create empty column

  ##  Prepare strict direct on horizontal plane radiation
  ADD <- tbl(con, "LAP") |>
    filter(Elevat > QS$sun_elev_min) |>
    filter(!is.na(DIR_strict))       |>
    select(Date, DIR_strict, SZA)    |>
    mutate(

      HOR_strict = DIR_strict * cos(SZA * pi / 180)

    )
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")

  ## __  DIFF  -----------------------------------------------------------------
  ##  DHI = GHI – DNI cos(z)
  make_null_column(con, "LAP", "DIFF_strict") ## Always create empty column

  ##  Prepare strict diffuse radiation
  ADD <- tbl(con, "LAP") |>
    filter(Elevat > QS$sun_elev_min)     |>
    filter(!is.na(GLB_strict))           |>
    filter(!is.na(HOR_strict))           |>
    select(Date, GLB_strict, HOR_strict) |>
    mutate(

      DIFF_strict = GLB_strict - HOR_strict

    ) |>
    mutate(

      DIFF_strict = case_when(
        DIFF_strict <  0 ~ NA,               ## diffuse only positive
        DIFF_strict >= 0 ~ DIFF_strict

      )
    )
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")

  ## __ Diffuse fraction  ------------------------------------------------------
  make_null_column(con, "LAP", "DiffuseFraction_kd") ## Always create empty column

  ##  Prepare strict diffuse fraction
  ADD <- tbl(con, "LAP") |>
    filter(Elevat > QS$sun_elev_min)      |>
    filter(!is.na(DIFF_strict))           |>
    filter(!is.na(GLB_strict))            |>
    filter(GLB_strict > 0)                |> ## don't use zero global
    filter(DIFF_strict < GLB_strict)      |> ## diffuse < global
    select(Date, DIFF_strict, GLB_strict) |>
    mutate(

      DiffuseFraction_kd = DIFF_strict / GLB_strict

    )
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")

  ## __ Transmittance  ---------------------------------------------------------
  ## ClearnessIndex_kt -> Transmittance_GLB rename to proper
  ## or Solar insolation ratio, Solar insolation factor
  make_null_column(con, "LAP", "Transmittance_GLB") ## Always create empty column

  ##  Prepare strict transmittance
  ADD <- tbl(con, "LAP") |>
    filter(Elevat > 0)                     |>  ## can compute only when sun is up
    filter(!is.na(GLB_strict))             |>
    filter(GLB_strict >= 0)                |>  ## only for positive global
    filter(!is.na(TSI_TOA))                |>
    select(Date, GLB_strict, SZA, TSI_TOA) |>
    mutate(

      Transmittance_GLB = case_when(
        GLB_strict / (cos(SZA * pi / 180) * TSI_TOA) >  9000 ~ 9000,
        GLB_strict / (cos(SZA * pi / 180) * TSI_TOA) <= 9000 ~ GLB_strict / (cos(SZA * pi / 180) * TSI_TOA)

      )
    )
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")
}

##  Store filters parameters  --------------------------------------------------
saveRDS(object = QS,
        file   = parameter_fl)

##  Clean exit  ----------------------------------------------------------------
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
