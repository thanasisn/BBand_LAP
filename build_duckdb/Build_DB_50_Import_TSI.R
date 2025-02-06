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
#' - `TSI_LAP`
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
knitr::opts_chunk$set(fig.cap   = " empty caption ")
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_50_Import_TSI.R"
Script.ID   <- "50"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
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
con <- dbConnect(duckdb(dbdir = DB_BROAD))
tsi <- dbConnect(duckdb(dbdir = DB_TSI, read_only = TRUE))

##  Load all TSI data  ---------------------------------------------------------
TSI <- tbl(tsi, "LAP_TSI") |>
  rename(TSI_source = Source)

## TODO check TSI accuracy digits!!
cat(Script.ID, ": ", "TODO Check TSI accuracy digits!\n")

## Remove all TSI data from database in order to update all
remove_column(con, "LAP", "TSI_TOA")
remove_column(con, "LAP", "TSI_LAP")
remove_column(con, "LAP", "TSI_source")

##  Update TSI data in DB  -----------------------------------------------------
TSI <- right_join(TSI,
                  tbl(con, "LAP")       |>
                    filter(Elevat > -1) |> ## We don't use TSI below horizon
                    select(Date)        |>
                    collect(),
                  by   = "Date",
                  copy = TRUE) |>
  filter(!is.na(TSI_source)) |>
  select(-Updated)

##  Create categorical column  -------------------------------------------------
categories <- unique(c("empty",
                       TSI |> select(TSI_source) |> distinct() |> pull()))
make_categorical_column("TSI_source", categories, con, "LAP")

##  Add all TSI data to the database  ------------------------------------------
if (TSI |> tally() |> pull() > 0) {
  cat(Script.ID, ": ", TSI |> tally() |> pull(), "rows of TSI data to add\n")
  status_msg(ScriptName = Script.Name, msg = c(TSI |> tally() |> pull(), "rows of TSI data to add"))

  res <- update_table(con      = con,
                      new_data = TSI,
                      table    = "LAP",
                      matchvar = "Date",
                      quiet    = TRUE)

} else {
  cat(Script.ID, ": ", "No new TSI data to add\n")
  status_msg(ScriptName = Script.Name, msg = c("No new TSI data to add"))
}

pander::pander(tbl(con, "LAP") |> group_by(TSI_source) |> tally() |> collect())

## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

#+ results="asis", echo=FALSE
goodbye()
