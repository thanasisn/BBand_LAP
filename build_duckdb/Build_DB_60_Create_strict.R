# /* #!/opt/R/4.2.3/bin/Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
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
#'  - Transmittance_DIR
#'
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.cap   = " - empty caption - ")
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
Script.Name  <- "~/BBand_LAP/build_duckdb/Build_DB_60_Create_strict.R"
Script.ID    <- "60"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)


##  Create strict radiation data  ----------------------------------------------
if (Sys.info()["nodename"] == "sagan") {

  cat(Script.ID, ":", "Create radiometric variables \n\n")
  status_msg(ScriptName = Script.Name,
             msg        = c("Create radiometric variables"))

  ##  Open dataset  ------------------------------------------------------------
  con <- dbConnect(duckdb(dbdir = DB_BROAD))

  DT <- tbl(con, "LAP") |>
    filter(Elevat > Sun_elev_MIN)  ## sun is up

  ## __ GHI  -------------------------------------------------------------------
  make_null_column(con, "LAP", "GLB_strict") ## Always create empty column

  ##  Prepare strict global irradiance
  ADD <- tbl(con, "LAP") |>
    filter(Elevat > Sun_elev_MIN)                       |> ## sun is up
    filter(!is.na(CM21_sig))                            |> ## valid measurements
    filter(cm21_bad_data_flag  %in% c("pass", "empty")) |> ## not bad data
    filter(cm21_sig_limit_flag %in% c("pass", "empty")) |> ## in acceptable values range
    select(Date, GLB_wpsm)                              |>
    mutate(

      GLB_strict = case_when(

        GLB_wpsm <  0 ~        0,  ## Negative values set to zero
        GLB_wpsm >= 0 ~ GLB_wpsm   ## All other selected values as is

      ))
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")
  status_msg(ScriptName = Script.Name,
             msg        = c("Created GLB_strict"))


  ## __ DNI  -------------------------------------------------------------------
  make_null_column(con, "LAP", "DIR_strict") ## Always create empty column

  ##  Prepare strict direct radiation
  ADD <- tbl(con, "LAP") |>
    filter(Elevat > Sun_elev_MIN)                       |> ## sun is up
    filter(!is.na(CHP1_sig))                            |> ## valid measurements
    filter(chp1_bad_data_flag  %in% c("pass", "empty")) |> ## not bad data
    filter(chp1_sig_limit_flag %in% c("pass", "empty")) |> ## acceptable values range
    filter(Async_tracker_flag != T)                     |> ## not in an async
    select(Date, DIR_wpsm, SZA)                         |>
    mutate(

      DIR_strict = case_when(

        DIR_wpsm <  0 ~        0,   ## Negative values set to zero
        DIR_wpsm >= 0 ~ DIR_wpsm    ## All other selected values as is

      ))
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")
  status_msg(ScriptName = Script.Name,
             msg        = c("Created DIR_strict"))

  ## __  HOR  ------------------------------------------------------------------
  make_null_column(con, "LAP", "HOR_strict") ## Always create empty column

  ##  Prepare strict direct on horizontal plane radiation
  ADD <- tbl(con, "LAP") |>
    filter(Elevat > Sun_elev_MIN) |>
    filter(!is.na(DIR_strict))    |>
    select(Date, DIR_strict, SZA) |>
    mutate(

      HOR_strict = DIR_strict * cos(SZA * pi / 180)

    )
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")
  status_msg(ScriptName = Script.Name,
             msg        = c("Created HOR_strict"))

  ## __  DIFF  -----------------------------------------------------------------
  ##  DHI = GHI – DNI cos(z)
  make_null_column(con, "LAP", "DIFF_strict") ## Always create empty column

  ##  Prepare strict diffuse radiation
  ADD <- tbl(con, "LAP")                 |>
    filter(Elevat > Sun_elev_MIN)        |>  ## sun is up
    filter(!is.na(GLB_strict))           |>  ## have global
    filter(!is.na(HOR_strict))           |>  ## have direct
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
  status_msg(ScriptName = Script.Name,
             msg        = c("Created DIFF_strict"))

  ## __ Diffuse fraction  ------------------------------------------------------
  make_null_column(con, "LAP", "DiffuseFraction_kd") ## Always create empty column

  ##  Prepare strict diffuse fraction
  ADD <- tbl(con, "LAP") |>
    filter(Elevat > Sun_elev_MIN)         |>
    filter(!is.na(DIFF_strict))           |> ## have diffuse
    filter(!is.na(GLB_strict))            |> ## have global
    filter(GLB_strict > 0)                |> ## don't use zero global
    filter(DIFF_strict < GLB_strict)      |> ## diffuse < global
    select(Date, DIFF_strict, GLB_strict) |>
    mutate(

      DiffuseFraction_kd = DIFF_strict / GLB_strict

    )
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")
  status_msg(ScriptName = Script.Name,
             msg        = c("Created DiffuseFraction_kd"))


  ## __  Global Transmittance  -------------------------------------------------
  ## was in the old implementation ClearnessIndex_kt
  ## or Solar insolation ratio, Solar insolation factor
  make_null_column(con, "LAP", "Transmittance_GLB") ## Always create empty column

  ##  Prepare strict global transmittance
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
  status_msg(ScriptName = Script.Name,
             msg        = c("Created Transmittance_GLB"))


  ## TODO test
  ## __  Direct Transmittance  -------------------------------------------------

  ##  Prepare strict direct transmittance
  ADD <- tbl(con, "LAP") |>
    filter(Elevat > 0)                     |>  ## can compute only when sun is up
    filter(!is.na(DIR_strict))             |>
    filter(DIR_strict >= 0)                |>  ## only for positive direct
    filter(!is.na(TSI_TOA))                |>
    select(Date, DIR_strict, SZA, TSI_TOA) |>
    mutate(

      Transmittance_DIR = case_when(

        DIR_strict / TSI_TOA >  9000 ~ 9000,
        DIR_strict / TSI_TOA <= 9000 ~ DIR_strict / TSI_TOA

      )
    )
  ##  Write data in the data base
  res <- update_table(con, ADD, "LAP", "Date")
  status_msg(ScriptName = Script.Name,
             msg        = c("Created Transmittance_DIR"))

  cat("\nTODO do some test plots\n\n")
}



##  Clean exit  ----------------------------------------------------------------
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
