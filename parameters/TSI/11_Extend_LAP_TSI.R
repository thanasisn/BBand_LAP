#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Populates:
#'  - TSI from NOAA
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

## __ Set environment  ---------------------------------------------------------
# closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/TSI/11_Extend_LAP_TSI.R"
Script.ID   <- "0B"

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
library(RNetCDF,    warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2,    warn.conflicts = FALSE, quietly = TRUE)

cat("\n Create TSI data for LAP\n\n")

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_TSI))
sun <- dbConnect(duckdb(dbdir = DB_LAP, read_only = TRUE))

NOAA <- tbl(con, "TSI_NOAA") |>
  # filter(!prelimi)           |>
  filter(!is.na(TSI))        |>
  mutate(Day = as.Date(Time))

STIS <- tbl(con, "TSI_TSIS") |>
  filter(!is.na(TSI)) |>
  mutate(Day = as.Date(Time))

LAP  <- tbl(con, "LAP_TSI")


##  Find adjustment  ------------------------------------------------------------
noaarange <- NOAA |> summarise(min = min(Day, na.rm = T), max = max(Day, na.rm = T)) |> collect() |> data.table()
stisrange <- STIS |> summarise(min = min(Day, na.rm = T), max = max(Day, na.rm = T)) |> collect() |> data.table()

commonmin <- stisrange$min
commonmax <- noaarange$max

get_mean <- . %>%
  filter(Day >= commonmin & Day <= commonmax) %>%
  group_by(Day) %>%
  summarise(TSI = mean(TSI, na.rm = T))

mnoaa <- NOAA |> get_mean()
mstis <- STIS |> get_mean()

offsets <- full_join(mnoaa, mstis, by = "Day") |>
  mutate(diff = TSI.x - TSI.y)      |>
  summarise(
    meandiff = mean(diff, na.rm = T),
    mediandiff = median(diff, na.rm = T)
  ) |> collect() |> data.table()

## Create adjusted values
STIS <- tbl(con, "TSI_TSIS") |>
  filter(!is.na(TSI))        |>
  select(Time, TSI) |> collect() |> data.table()
STIS[, TSI := TSI + offsets$meandiff]
STIS[, Source  := "TSIS_RAW"]
STIS[, Updated := Sys.time()]

## function to interpolate TSIS
tsi_fun <- approxfun(
  x      = STIS$Time,
  y      = STIS$TSI,
  method = "linear",
  rule   = 1,
  ties   = mean
)

## just
STIS <- STIS[Time > commonmax, ]

ggplot() +
  geom_point(data = mstis, aes(x = Day, y = TSI), color = "blue", size = 0.5) +
  geom_point(data = mnoaa, aes(x = Day, y = TSI), color = "black") +
  geom_point(data = mstis, aes(x = Day, y = TSI + offsets$meandiff), color = "green", size = 0.5) +
  geom_point(data = STIS,  aes(x = as.Date(Time), y = TSI), color = "orange")

## Fill with TSI DATA  ---------------------------------------------------------
#'
#'  Insert raw NOAA values to the main table
#'
#+ echo=T

## ADD row values for LAP
RAW <- STIS |> rename(Date = "Time")

## Add raw values
update_table(con, RAW, "LAP_TSI", "Date")


## Fill raw with interpolation  ------------------------------------------------
#'
#'  Fill NOAA TSI with interpolated values.
#'
#'  Create a function than can fill any date
#'
#+ echo=T
some <- LAP |>
  filter(is.na(TSI) & Date > commonmin) |>
  select(Date) |> collect() |> data.table()
some[, TSI     := tsi_fun(Date)]
some[, Source  := "TSIS_INTERP"]
some <- some[!is.na(TSI)]

## write only when needed
if (nrow(some) > 0) {
  cat(paste(Script.ID, ":",
            "Interpolate TSI for"), "\n")
  res <- update_table(con, some, "LAP_TSI", "Date")
}


#'
#'  Create values of TSI at TOA and LAP
#'
#+ echo=T

## Fill TOA and LAP ground
make_new_column(con = con, table = "LAP_TSI", "TSI_TOA")
make_new_column(con = con, table = "LAP_TSI", "TSI_LAP")


## to fill
some <- LAP |>
  filter(Date > DB_start_date)            |>
  filter(!is.na(TSI))                     |>
  filter(is.na(TSI_TOA) | is.na(TSI_LAP)) |>
  select(Date, TSI)

SUN <- tbl(sun, "params") |>
  filter(!is.na(AsPy_Elevation) & Date >= DB_start_date) |>
  rename(Sun_Dist_Astropy = "AsPy_Dist")       |>
  rename(Elevat           = "AsPy_Elevation")  |>
  mutate(SZA              = 90 - Elevat)       |>
  select(Date, Sun_Dist_Astropy, SZA)

ADD <- left_join(some, SUN, copy = T) |>
  mutate(
    TSI_TOA = TSI / Sun_Dist_Astropy^2,  ## TSI on LAP TOA
    TSI_LAP = TSI_TOA * cos(SZA*pi/180)  ## TSI on LAP ground
  ) |>
  select(Date, TSI_TOA, TSI_LAP) |>
  filter(!is.na(TSI_TOA) & !is.na(TSI_LAP))

## write only when needed
if (ADD |> tally() |> pull() > 0) {
  cat(paste(Script.ID, ":",
            "TOA and Ground TSI"), "\n")
  res <- update_table(con, ADD, "LAP_TSI", "Date")
}



# test <- NEW |> filter(year(Date) == 1993) |> collect() |> data.table()
#
# plot(test$Date, test$TSI)
# points(test[Source == "NOAA_RAW", TSI, Date], col = "red")
#
# plot(test$Date, test$TSI_TOA)
# plot(test$Date, test$TSI_LAP)

if (interactive()) {stop("dont close")}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con)

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
