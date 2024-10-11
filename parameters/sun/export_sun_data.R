#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Export some sun data for other uses
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )

stop("wait for paeper end")

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/sun/export_sun_data.R"
Script.ID   <- "0B"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
  sink(file = paste0("~/BBand_LAP/REPORTS/LOGs/",    basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)

cat("\n Initialize params DB and/or import Sun data\n\n")

export_hours <- 30
solstices_fl <- "~/DATA/SUN/LAP_solstices"
sunsets_fl   <- "~/DATA/SUN/LAP_sunrises_sunsets"
daylength_fl <- "~/DATA/SUN/LAP_daylength"
pysolar_file <- "~/DATA/SUN/Pysolar_LAP.Rds"
astropy_file <- "~/DATA_RAW/SUN/Astropy_LAP.Rds"

##  Open dataset  --------------------------------------------------------------
sun <- dbConnect(duckdb(dbdir = DB_LAP,  read_only = TRUE))

##  Choose Astropy  ------------------------------------------------------------
SUN <- tbl(sun, "params") |>
  filter(!is.na(AsPy_Elevation) & Date >= DB_start_date) |>
  select(Date, AsPy_Azimuth, AsPy_Elevation, AsPy_Dist)  |>
  rename(Azimuth          = "AsPy_Azimuth")              |>
  rename(Sun_Dist_Astropy = "AsPy_Dist")                 |>
  rename(Elevat           = "AsPy_Elevation")            |>
  mutate(
    SZA     = 90 - Elevat,
    preNoon = case_when(
      Azimuth <= 180 ~ TRUE,
      Azimuth >  180 ~ FALSE
    )
  )

##  Detect Solstices in sun data  ----------------------------------------------
if (!file.exists(paste0(solstices_fl, ".Rds")) |
    file.mtime(paste0(solstices_fl, ".Rds")) < Sys.time() - export_hours * 3600) {

  Solstices <- SUN                              |>
    group_by(year = year(Date))                 |>
    filter(Elevat == max(Elevat, na.rm = TRUE)) |>
    select(-preNoon)                            |>
    collect()                                   |>
    arrange(Date)                               |>
    data.table()

  write_RDS(Solstices, paste0(solstices_fl, ".Rds"), notes = Script.Name)
  write.csv(Solstices, paste0(solstices_fl, ".csv"), quote = F)
}

##  Detect sunsets, sunrises  --------------------------------------------------
if (!file.exists(paste0(sunsets_fl, ".Rds")) |
    file.mtime(paste0(sunsets_fl, ".Rds")) < Sys.time() - 30 * 3600) {

  Sunsets <- SUN |>
    filter(Elevat > 0) |>
    group_by(Day = as.Date(Date), preNoon) |>
    filter(Elevat == min(Elevat, na.rm = T)) |>
    mutate(Sun = case_when(
      preNoon == TRUE  ~ "Sunrise",
      preNoon == FALSE ~ "Sunset"
    )) |>
    collect() |>
    arrange(Date) |>
    data.table() |>
    select(-preNoon)

  write_RDS(Sunsets, paste0(sunsets_fl, ".Rds"), notes = Script.Name)
  write.csv(Sunsets, paste0(sunsets_fl, ".csv"), quote = F)
}

##  Compute daylength  ---------------------------------------------------------
if (!file.exists(paste0(daylength_fl, ".Rds")) |
    file.mtime(paste0(daylength_fl, ".Rds")) < Sys.time() - 30 * 3600) {

  Daylengths <- Sunsets[, .(Daylength = diff(as.numeric(range(Date))) / 60),
                        by = Day]

  write_RDS(Daylengths, paste0(daylength_fl, ".Rds"), notes = Script.Name)
  write.csv(Daylengths, paste0(daylength_fl, ".csv"), quote = F)
}


##  Export legacy  -------------------------------------------------------------
## These exports will be removed in the future

## __ Export pysolar for old and forgotten processes  --------------------------
if (!file.exists(pysolar_file) |
    file.mtime(pysolar_file) < Sys.time() - 30 * 3600) {

  pysolar <- tbl(sun, "params") |>
    filter(!is.na(PySo_Azimuth))                   |>
    filter(!is.na(PySo_Elevation))                 |>
    filter(Date >= as.POSIXct("1993-01-01 00:00")) |>
    select(Date, PySo_Azimuth, PySo_Elevation)     |>
    rename(V1 = Date)                              |>
    rename(V2 = PySo_Azimuth)                      |>
    rename(V3 = PySo_Elevation)                    |>
    collect()                                      |>
    arrange(V1)                                    |>
    data.table()

  write_RDS(pysolar, pysolar_file, notes = Script.Name)
  rm(pysolar); gc()
}

## __ Export Astropy for old and forgotten processes  --------------------------
if (!file.exists(astropy_file) |
    file.mtime(pysolar_file) < Sys.time() - 30 * 3600) {

  astropy <- tbl(sun, "params") |>
    filter(!is.na(AsPy_Azimuth))                          |>
    filter(!is.na(AsPy_Elevation))                        |>
    filter(Date >= as.POSIXct("1993-01-01 00:00"))        |>
    select(Date, AsPy_Azimuth, AsPy_Elevation, AsPy_Dist) |>
    rename(Azimuth          = "AsPy_Azimuth")             |>
    rename(Dist             = "AsPy_Dist")                |>
    rename(Elevation        = "AsPy_Elevation")           |>
    collect()                                             |>
    arrange(Date)                                         |>
    data.table()

  write_RDS(astropy, astropy_file, notes = Script.Name)
  rm(astropy); gc()
}


## clean exit
dbDisconnect(sun, shutdown = TRUE); rm("sun"); closeAllConnections()


## __ Test SZAs matching  ------------------------------------------------------
# SUN <- tbl(sun, "params")
# LAP <- tbl(con, "LAP")
#
# SUN <- SUN |>
#   filter(as.Date(Date) > "1993-04-01") |>
#   select(LAP_SZA_start, LAP_SZA_middle, Date)
# LAP <- LAP |>
#   filter(as.Date(Date) > "1993-04-01") |>
#   select(lap_sza, Date)
#
# test <-
#   left_join(SUN, LAP, by = "Date", copy = T) |>
#   collect() |> data.table()
#
# A <- test[, .(sum(abs(lap_sza - LAP_SZA_start), na.rm = T),
#          sum(abs(lap_sza - LAP_SZA_start), na.rm = T)/mean(lap_sza, na.rm = T)),  by = year(Date)]
# B <- test[, .(sum(abs(lap_sza - LAP_SZA_middle), na.rm = T),
#          sum(abs(lap_sza - LAP_SZA_middle), na.rm = T)/mean(lap_sza, na.rm = T)), by = year(Date)]
#
# ylim = range(A$V2, B$V2, na.rm = T)
# plot(  A$year, A$V2, ylim = ylim)
# points(B$year, B$V2, col = "red")
#
# ylim = range(A$V1, B$V1, na.rm = T)
# plot(  A$year, A$V1, ylim = ylim)
# points(B$year, B$V1, col = "red")
#
# summary(test[, lap_sza /  LAP_SZA_start])
# summary(test[, lap_sza /  LAP_SZA_middle])
#
# lm(test$lap_sza ~ test$LAP_SZA_middle)
# lm(test$lap_sza ~ test$LAP_SZA_start)
#
# cor(test$lap_sza, test$LAP_SZA_start, use = "complete.obs")
# cor(test$lap_sza, test$LAP_SZA_middle, use = "complete.obs")


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
