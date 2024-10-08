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
sun <- dbConnect(duckdb(dbdir = DB_LAP, read_only = TRUE))

##  Rely on sun data only
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


##  Detect Solstices in sun data
Solstices <- SUN |>
  group_by(year = year(Date))                        |>
  filter(Elevat == max(Elevat, na.rm = TRUE)) |>
  select(-preNoon)                            |>
  collect()                                   |>
  data.table()

##  Detect sunsets, sunrises
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


Daylengths <- Sunsets[, .(Daylength = diff(as.numeric(range(Date))) / 60), by = Day]

plot(Daylengths)
summary(Daylengths)





duckdb_stats <- function(db_file) {
  con <- dbConnect(duckdb(dbdir = db_file, read_only = TRUE))
  db_stats <- data.table()
  for (atbl in  dbListTables(con)) {
    ## count nas
    fillness <- tbl(con, atbl) |>
      summarise_all(
        ~ sum(case_match(!is.na(.x),
                         TRUE  ~1L,
                         FALSE ~0L))
      ) |> collect() |> data.table()
    ## compute
    fillness <- data.table(
      Table    = atbl,
      Variable = names(fillness),
      Non_na   = as.vector(fillness |> t()),
      N        = tbl(con, atbl) |> tally() |> pull()
    )
    fillness[, missing  := N - Non_na]
    fillness[, fill_pc  := round(100 * (N - Non_na) / N, 5) ]
    fillness[, empty_pc := round(100 * (1 - (N - Non_na) / N), 5) ]
    db_stats <- rbind(db_stats, fillness)
  }
  db_sums <- db_stats[, .(Values = sum(Non_na),
                          Total  = sum(N)), by = Table]
  ## bytes per value
  data_density <- db_sums[, file.size(db_file) / sum(Values)]

  dbDisconnect(con, shutdown = TRUE)

  return(
    list(data_base    = db_file,
         base_name    = basename(db_file),
         db_stats     = db_stats,
         db_sums      = db_sums,
         file_sise    = file.size(db_file),
         file_sise_h  = fs::as_fs_bytes(file.size(db_file)),
         data_density = data_density)
  )
}

duckdb_stats(DB_DUCK)
duckdb_stats(DB_LAP)





stop()











## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
