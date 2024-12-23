#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Add TSI data in DB from pre-existent data
#' [`github.com/thanasisn/TSI`](`https://github.com/thanasisn/TSI`)
#'
#' Populates:
#'
#' - `Sun_Dist_Astropy`
#' - `TSI_TOA`
#' - `TSI_1au`
#' - `TSI_source`
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_50_Import_TSI.R"
Script.ID   <- "50"

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

cat("\n Import  TSI  data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_DUCK))

##  Load all TSI data  ---------------------------------------------------------
if (!file.exists(COMP_TSI)) { stop("Missing TSI file:", COMP_TSI) }
TSI <- readRDS(COMP_TSI)
TSI <- TSI[!is.na(TSIextEARTH_comb)]
names(TSI)[names(TSI) == "sun_dist"        ] <- "Sun_Dist_Astropy"
names(TSI)[names(TSI) == "TSIextEARTH_comb"] <- "TSI_TOA"
names(TSI)[names(TSI) == "tsi_1au_comb"    ] <- "TSI_1au"
names(TSI)[names(TSI) == "Source"          ] <- "TSI_source"
TSI$measur_error_comb <- NULL
dummy <- gc()

## TODO check TSI accuracy digits!!
cat(Script.ID, ": ", "TODO Check TSI accuracy digits!\n")

## Remove all TSI data from database in order to update all
remove_column(con, "LAP", "TSI_TOA")
remove_column(con, "LAP", "TSI_1au")
remove_column(con, "LAP", "TSI_source")


##  Update TSI data in DB  -----------------------------------------------------
TSI <- right_join(TSI,
                  tbl(con, "LAP")       |>
                    filter(Elevat > -1) |> ## We don't use TSI below horizon
                    select(Date)        |>
                    collect(),
                  by = "Date")
TSI <- TSI[!is.na(TSI_source)]

##  Create categorical column
categories <- unique(c("empty", TSI$TSI_source))
make_categorical_column("TSI_source", categories, con, "LAP")

##  Add all TSI data to the database  ------------------------------------------
if (nrow(TSI) > 0) {
  cat(Script.ID, ": ", nrow(TSI), "rows of TSI data to add\n")

  update_table(con      = con,
               new_data = TSI,
               table    = "LAP",
               matchvar = "Date")

} else {
  cat(Script.ID, ": ", "No new TSI data to add\n")
}

tbl(con, "LAP") |> group_by(TSI_source) |> tally()

## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
