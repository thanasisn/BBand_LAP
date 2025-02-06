# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Add atmospheric pressure data in DB from pre-existing data
#'
#' Populates:
#'
#' - `Pressure`
#' - `Pressure_source`
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'

#+ include=F
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_51_Import_Pressure.R"
Script.ID   <- "51"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)

cat("\n Import  Pressure  data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_BROAD))

##  Load all Pressure data  ----------------------------------------------------
PRESSURE <- data.table(readRDS(COMP_PRES))

if (second(PRESSURE$Date[1]) == 0) {
    ## move to centre
    PRESSURE[ , Date := Date + 30]
}
names(PRESSURE)[names(PRESSURE) == "pressure"] <- "Pressure"
names(PRESSURE)[names(PRESSURE) == "Source"  ] <- "Pressure_source"

## __ Clean some duplicate values  --------------------
PRESSURE[duplicated(PRESSURE$Date) & Pressure_source == "iama_corrected" ]
test <- PRESSURE[duplicated(PRESSURE$Date) | duplicated(PRESSURE$Date, fromLast = TRUE)]
if (!nrow(test) == 0) {warning("Pressure data should be cleaner\n")}
PRESSURE <- PRESSURE[!duplicated(PRESSURE$Date)]


## __ Get data to update  ------------------------------------------------------
PRESSURE <- right_join(PRESSURE,
                  tbl(con, "LAP") |>
                    select(Date)  |>
                    collect(),
                  by = "Date")
PRESSURE <- PRESSURE[!is.na(Pressure_source)]

## __ Create categorical column  -----------------------------------------------
categories <- unique(c("empty", PRESSURE$Pressure_source))
make_categorical_column("Pressure_source", categories, con, "LAP")

##  Add pressure data to database  ---------------------------------------------
if (nrow(PRESSURE) > 0) {
  cat(Script.ID, ": ", nrow(PRESSURE), "rows of pressure data to add\n")
  status_msg(ScriptName = Script.Name,
             msg        = c(nrow(PRESSURE), "rows of pressure data to add"))


  ## FIXME this will not update changed pressure data
  update_table(con      = con,
               new_data = PRESSURE,
               table    = "LAP",
               matchvar = "Date")

} else {
  cat(Script.ID, ": ", "No new pressure data to add\n")
  status_msg(ScriptName = Script.Name,
             msg        = c("No new pressure data to add"))
}

tbl(con, "LAP") |> group_by(Pressure_source) |> tally()


#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
goodbye()
