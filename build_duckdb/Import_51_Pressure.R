#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Add atmospheric pressure data in DB from preexisting data
#'
#' Populates:
#'
#' - `Pressure`
#' - `Pressure_source`
#'
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
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
Script.Name <- "~/BBand_LAP/build_duckdb/Import_51_Pressure.R"
Script.ID   <- "51"

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

cat("\n Import  Pressure  data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_DUCK))

##  Load all Pressure data  ----------------------------------------------------
PRESSURE <- data.table(readRDS(COMP_PRES))

if (second(PRESSURE$Date[1]) == 0) {
    ## move to centre
    PRESSURE[ , Date := Date + 30]
}
names(PRESSURE)[names(PRESSURE) == "pressure"] <- "Pressure"
names(PRESSURE)[names(PRESSURE) == "Source"  ] <- "Pressure_source"

## clean some duplicate value
PRESSURE[duplicated(PRESSURE$Date) & Pressure_source == "iama_corrected" ]
test <- PRESSURE[duplicated(PRESSURE$Date) | duplicated(PRESSURE$Date, fromLast = TRUE)]
if (!nrow(test) == 0) {warning("Pressure data should be cleaner\n")}
PRESSURE <- PRESSURE[!duplicated(PRESSURE$Date)]

##  Update Pressure data in DB  -----------------------------------------------------
PRESSURE <- right_join(PRESSURE,
                  tbl(con, "LAP") |>
                    select(Date)  |>
                    collect(),
                  by = "Date")

if (nrow(PRESSURE) > 0) {
  cat(Script.ID, ": ", nrow(PRESSURE), "rows of pressure data to add\n")

  ## FIXME this will not update with changed pressure data
  update_table(con      = con,
               new_data = PRESSURE,
               table    = "LAP",
               matchvar = "Date")

} else {
  cat(Script.ID, ": ", "No new pressure data to add\n")
}

tbl(con, "LAP") |> group_by(Pressure_source) |> tally()

## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

