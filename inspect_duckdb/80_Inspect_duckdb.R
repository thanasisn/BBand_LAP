# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Inspect duckdb "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#'
#' link-citations:  yes
#' colorlinks:      yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        4
#'     fig_width:        8
#'     fig_height:       5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' params:
#'   CLEAN: TRUE
#'
#' ---

#'
#'  **SIG**
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
Script.Name <- "~/BBand_LAP/inspect_duckdb/80_Inspect_duckdb.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/duck/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/LOGs/duck/",    basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
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
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )


#'
#' ##  Statistics of the databases
#'
#+ echo=F
duckdb_stats <- function(db_file) {
  con <- dbConnect(duckdb(dbdir = db_file, read_only = TRUE))
  db_stats <- data.table()
  for (atbl in  dbListTables(con)) {
    ## count nas
    fillness <- tbl(con, atbl) |>
      summarise_all(
        ~ sum(case_match(!is.na(.x),
                         TRUE  ~1L,
                         FALSE ~0L),
              na.rm = TRUE)
      ) |> collect() |> data.table()
    ## compute
    fillness <- data.table(
      Table    = atbl,
      Variable = names(fillness),
      Non_na   = as.vector(fillness |> t()),
      N        = tbl(con, atbl) |> tally() |> pull()
    )
    fillness[, missing  := N - Non_na]
    fillness[, empty_pc  := round(100 * (N - Non_na) / N, 5) ]
    fillness[, fill_pc := round(100 * (1 - (N - Non_na) / N), 5) ]
    db_stats <- rbind(db_stats, fillness)
  }
  db_sums <- db_stats[, .(Values = sum(Non_na),
                          Total  = sum(N)), by = Table]
  ## bytes per value
  data_density <- db_sums[, file.size(db_file) / sum(Values)]

  dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

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

#+ include=T, echo=T
duckdb_stats(DB_DUCK)

duckdb_stats(DB_LAP)



k#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

