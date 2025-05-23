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
knitr::opts_chunk$set(fig.cap   = " empty caption ")
knitr::opts_chunk$set(fig.pos   = "!ht"   )
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
Script.Name <- "~/BBand_LAP/inspect_duckdb/Duckdb_save_stats.R"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/duck/", basename(sub("\\.R$", ".pdf", Script.Name))))
  # sink(file = paste0("~/BBand_LAP/REPORTS/LOGs/duck/",    basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
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
library(cloc,       warn.conflicts = FALSE, quietly = TRUE)
# install.packages("hrbrmstr/cloc")
# remotes::install_github("hrbrmstr/cloc")

overview_data <- "~/BBand_LAP/SIDE_DATA/Data_size_duckdb.Rds"

#'
#' ##  Statistics of the databases
#'
#+ echo=F
duckdb_stats <- function(db_file) {
  con <- dbConnect(duckdb(dbdir = db_file, read_only = TRUE))
  db_stats <- data.table()
  for (atbl in  dbListTables(con)) {
    ## count NAs
    fillness <- tbl(con, atbl) |>
      summarise_all(
        ~ sum(case_match(!is.na(.x),
                         TRUE  ~1L,
                         FALSE ~0L),
              na.rm = TRUE)
      ) |> collect() |> data.table()

    ## number of vars?
    # Nvar <- tbl(con, atbl) |> colnames() |> length()

    ## compute
    fillness <- data.table(
      Table    = atbl,
      Variable = names(fillness),
      Non_na   = as.vector(fillness |> t()),
      N        = tbl(con, atbl) |> tally() |> pull()
    )
    fillness[, missing  := N - Non_na]
    fillness[, empty_pc := round(100 * (N - Non_na) / N,       5) ]
    fillness[, fill_pc  := round(100 * (1 - (N - Non_na) / N), 5) ]
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

## Init storage
if (file.exists(overview_data)) {
  gather <- readRDS(overview_data)
} else {
  gather <- list()
}

##  SUN location info  ---------------------------------------------------------
res      <- duckdb_stats(DB_LAP)
res$host <- Sys.info()["nodename"]
res$date <- Sys.time()
gather   <- c(gather, list(res))


##  TSI info  ------------------------------------------------------------------
res      <- duckdb_stats(DB_TSI)
res$host <- Sys.info()["nodename"]
res$date <- Sys.time()
gather   <- c(gather, list(res))


##  Broadband info  ------------------------------------------------------------
res      <- duckdb_stats(DB_BROAD)
res$host <- Sys.info()["nodename"]
res$date <- Sys.time()
gather   <- c(gather, list(res))


##  Deduplicate data  ----------------------------------------------------------
databases <- unique(sapply(gather, "[[", "base_name"))
for (adb in databases) {
  lls <- sapply(gather, "[[", "base_name") == adb
  dt  <- data.table(date = as.POSIXct(sapply(gather[lls], "[[", "date"), origin = origin))
  dt[, day := as.Date(date)]
  dt[, base_name := adb]
  setorder(dt, date)
  ## choose to remove
  dt  <- dt[duplicated(dt$day, fromLast = TRUE)]
  res <- sapply(gather, "[[", "base_name") == adb &
    sapply(gather, "[[", "date") %in% dt$date
  ## drop data
  gather <- gather[!res]
}


## Save data
saveRDS(gather, overview_data)
rm(overview_data)




##  Source code statistics  ----------------------------------------------------
## override previous data file
overview_data <- "~/BBand_LAP/SIDE_DATA/Source_code_stats.Rds"
codemetrics   <- data.frame()

dir_list <- c(list.dirs("~/BBand_LAP", recursive = FALSE), path.expand("~/BBand_LAP"))
dir_list <- grep(".Rproj.user|.git|REPORTS|RESOURCES|PARAMS|renv|SIDE_DATA", dir_list, invert = TRUE, value = TRUE)

for (adir in dir_list) {
  codestats   <- data.table(cloc(adir))
  codestats[, Date := Sys.Date()]
  codestats   <- codestats[!language %in% c("HTML", "SUM", "TeX", "Markdown", "JSON", "Rmd")]
  codemetrics <- rbind(codemetrics, codestats)
}

## Gather results
if (!file.exists(overview_data)) {
  saveRDS(codemetrics, overview_data)
} else {
  DATA <- readRDS(overview_data)

  DATA <- rows_upsert(DATA,
                      codemetrics,
                      by = c("Date", "source", "language")) |>
    distinct()
  saveRDS(DATA, overview_data)
}
rm(overview_data)




#+ results="asis", echo=FALSE
goodbye()
