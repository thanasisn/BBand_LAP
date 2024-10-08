#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Use full querries of Sun data
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
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
Script.Name <- "~/BBand_LAP/parameters/sun/sun_data_queries.R"
Script.ID   <- "1A"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
  sink(file = paste0("~/BBand_LAP/REPORTS/LOGs/",    basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)

cat("\n Initialize params DB and/or import Sun data\n\n")

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_DUCK, read_only = TRUE))

## Get data from the main database
SUN <- tbl(con, "LAP") |>
  select(Date,
         Azimuth,
         Elevat,
         SZA,
         year,
         Day,
         preNoon,
         doy)



# sun <- dbConnect(duckdb(dbdir = DB_LAP, read_only = TRUE))
#
# ##  Relly on Astropy data
# SUN <- tbl(sun, "params") |>
#   filter(!is.na(AsPy_Elevation) & Date >= DB_start_date) |>
#   select(Date, AsPy_Azimuth, AsPy_Elevation, AsPy_Dist)  |>
#   rename(Azimuth          = "AsPy_Azimuth")              |>
#   rename(Sun_Dist_Astropy = "AsPy_Dist")                 |>
#   rename(Elevat           = "AsPy_Elevation")




Solstices <- SUN |>
  group_by(year(Date))                        |>
  filter(Elevat == max(Elevat, na.rm = TRUE)) |>
  select(-preNoon)                            |>
  collect()                                   |>
  data.table()

# Eleva > 0 & min(elevat ) & by preNoon



Sunsets <- SUN |>
  filter(Elevat > 0) |>
  group_by(Day, preNoon) |>
  filter(Elevat == min(Elevat, na.rm = T)) |>
  mutate(Sun = case_when(
    preNoon == TRUE  ~ "Sunrise",
    preNoon == FALSE ~ "Sunset"
  )) |> collect() |> data.table()


Sunsets <- Sunsets[, ] |> select(-year, -preNoon)


Daylengths <- Sunsets[, .(Daylength = diff(as.numeric(range(Date))) / 60), by = Day]

plot(Daylengths)
summary(Daylength)



## TODO find some daily values
## - length of daylight
## - sun set and sun rise
## - sun angle limits
## - solstice


lap_vars <- tbl(con, "LAP") |> colnames() |> length()
lap_cols <- tbl(con, "LAP") |> tally() |> pull()

meta_vars <- tbl(con, "META") |> colnames() |> length()
meta_cols <- tbl(con, "META") |> tally() |> pull()


values <- lap_vars * lap_cols + meta_vars * meta_cols


"Bytes/Value" <-
round(file.size(DB_DUCK) / (as.double(lap_cols) * as.double(lap_vars) + as.double(meta_cols) * as.double(meta_vars)), 5 )



stop()











## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
