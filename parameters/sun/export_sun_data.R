#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Use full queries of Sun data
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
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

solstices_fl <- "~/DATA/SUN/LAP_solstices"
sunsets_fl   <- "~/DATA/SUN/LAP_sunrises_sunsets"
daylength_fl <- "~/DATA/SUN/LAP_daylength"

##  Open dataset  --------------------------------------------------------------
sun <- dbConnect(duckdb(dbdir = DB_LAP, read_only = TRUE))

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
Solstices <- SUN                              |>
  group_by(year = year(Date))                 |>
  filter(Elevat == max(Elevat, na.rm = TRUE)) |>
  select(-preNoon)                            |>
  collect()                                   |>
  arrange(Date)                               |>
  data.table()

if (!file.exists(paste0(solstices_fl, ".Rds")) |
    file.mtime(paste0(solstices_fl, ".Rds")) < Sys.time() - 30 * 3600) {
  saveRDS(  Solstices, paste0(solstices_fl, ".Rds"))
  write.csv(Solstices, paste0(solstices_fl, ".csv"), quote = F)
}

##  Detect sunsets, sunrises  --------------------------------------------------
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

if (!file.exists(paste0(sunsets_fl, ".Rds")) |
    file.mtime(paste0(sunsets_fl, ".Rds")) < Sys.time() - 30 * 3600) {
  saveRDS(  Sunsets, paste0(sunsets_fl, ".Rds"))
  write.csv(Sunsets, paste0(sunsets_fl, ".csv"), quote = F)
}

##  Compute daylength  ---------------------------------------------------------
Daylengths <- Sunsets[, .(Daylength = diff(as.numeric(range(Date))) / 60),
                      by = Day]

if (!file.exists(paste0(daylength_fl, ".Rds")) |
    file.mtime(paste0(daylength_fl, ".Rds")) < Sys.time() - 30 * 3600) {
  saveRDS(  Daylengths, paste0(daylength_fl, ".Rds"))
  write.csv(Daylengths, paste0(daylength_fl, ".csv"), quote = F)
}


##  Export legacy  -------------------------------------------------------------

## __ Export pysolar for old and forgotten processes  ---------------------------
pysolar_file = "~/DATA/SUN/Pysolar_LAP.Rds"
pysolar <- tbl(sun, "params") |>
  filter(!is.na(PySo_Azimuth))                   |>
  filter(!is.na(PySo_Elevation))                 |>
  filter(Date >= as.POSIXct("1993-01-01 00:00")) |>
  select(Date, PySo_Azimuth, PySo_Elevation)     |>
  rename(V1 = Date)                              |>
  rename(V2 = PySo_Azimuth)                      |>
  rename(V3 = PySo_Elevation)                    |>
  arrange(V1)                                    |>
  collect()                                      |>
  data.table()

write_RDS(pysolar, pysolar_file)
rm(pysolar); gc()





stop()


## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
