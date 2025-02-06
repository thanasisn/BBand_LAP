#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Radiation processing for LAP"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#'
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
#'   html_document:
#'     toc:        true
#'     fig_width:  9
#'     fig_height: 4
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     keep_md:          no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        4
#'     fig_width:        8
#'     fig_height:       5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---
#+ include=F

#'
#' # Reads CM-21 signal from `[0-9]*06.LAP$``
#'
#' Populates:
#'  - CM21_sig
#'  - CM21_sig_sd
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_02_cm21.R"
Script.ID   <- "02"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_CM21.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)


#+ include=T, echo=F, results="asis"
cat("\n Import  CM-21  data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_BROAD))

##  Get CM-21 files  -----------------------------------------------------------
inp_filelist <- list.files(path        = SIRENA_GLB,
                           recursive   = TRUE,
                           pattern     = "[0-9]*06.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE)
cat("\n**Found:", length(inp_filelist), "CM-21 files from Sirena**\n")
## just in case, there are nested folders with more lap files in Sirena
inp_filelist <- grep("OLD", inp_filelist, ignore.case = T, invert = T, value = T)

inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, cm21_basename := basename(fullname)]
stopifnot( all(duplicated(sub("\\..*", "", inp_filelist$cm21_basename))) == FALSE)

inp_filelist$Day <- as.Date(parse_date_time(
  sub("06\\..*", "", inp_filelist$cm21_basename),
  "dmy"))
setorder(inp_filelist, Day)
cat("\n**Found:",paste(nrow(inp_filelist), "CM-21 files**\n"))

## check files have unique days
stopifnot(!any(duplicated(inp_filelist$Day)))

## keep only files which correspond to existing dates
inp_filelist <- right_join(inp_filelist,
                           tbl(con, "LAP") |>
                             select(Day)   |>
                             distinct()    |>
                             collect(),
                           by = "Day") |>
  filter(!is.na(cm21_basename))

## add only files not in the metadata
if (dbExistsTable(con, "META") &
    any(dbListFields(con, "META") %in% "cm21_basename")) {
  inp_filelist <- anti_join(inp_filelist,
                            tbl(con, "META") |>
                              filter(!is.na(cm21_basename)) |>
                              select(Day) |>
                              collect(),
                            by = "Day") |>
    filter(!is.na(cm21_basename))
}
setorder(inp_filelist, Day)

cat("\n**Parse:", paste(nrow(inp_filelist), "CM-21 files**\n\n"))

## create categorical column for signal
categories <- c("empty",                 ## default NA
                "pass",                  ## signal is normal
                "Abnormal LOW signal",   ## too low signal
                "Abnormal HIGH signal")  ## too high signal

make_categorical_column("cm21_sig_limit_flag", categories, con, "LAP")

##  Import CM-21 files  --------------------------------------------------------
if (nrow(inp_filelist) > 0) {
  for (ll in 1:nrow(inp_filelist)) {
    ff <- inp_filelist[ll, ]

    cat(Script.ID, ": ",
        basename(ff$fullname),
        paste(ff$Day),
        ll, "/", nrow(inp_filelist))

    ## __ Check data base is ready for import  ---------------------------------
    if (tbl(con, "LAP") |> filter(Day == ff$Day) |> tally() |> pull() != 1440) {
      cat("Data base not ready to import", paste(ff$Day), "\n")
      next()
    }

    ## __ Read CM-21 LAP file  ----------------------------------------------
    suppressWarnings(rm(D_minutes))
    D_minutes <- seq(from       = as.POSIXct(paste(as_date(ff$Day), "00:00:30"), tz = ""),
                     length.out = 1440,
                     by         = "min")

    lap    <- fread(ff$fullname, na.strings = "-9")
    lap$V1 <- as.numeric(lap$V1)
    lap$V2 <- as.numeric(lap$V2)
    stopifnot(is.numeric(lap$V1))
    stopifnot(is.numeric(lap$V2))
    stopifnot(dim(lap)[1] == 1440)
    lap[V1 < -8, V1 := NA]
    lap[V2 < -8, V2 := NA]
    cat(" r")

    day_data <- data.table(Date        = D_minutes,      # Date of the data point
                           CM21_sig    = lap$V1,         # Raw value for CM-21
                           CM21_sig_sd = lap$V2)         # Raw SD value for CM-21

    ## try to fix dates
    day_data[, Date := round_date(Date, unit = "second")]

    ## flag signal limits
    day_data[,
             cm21_sig_limit_flag := "pass"]

    day_data[CM21_sig < cm21_signal_lower_limit(Date),
             cm21_sig_limit_flag := "Abnormal LOW signal"]

    day_data[CM21_sig > cm21_signal_upper_limit(Date),
             cm21_sig_limit_flag := "Abnormal HIGH signal"]

    ## enforce data base numeric scheme
    day_data[CM21_sig > 9000, CM21_sig := 9000]

    ## meta data for file
    file_meta <- data.table(Day           = ff$Day,
                            cm21_basename = basename(ff$fullname),
                            cm21_mtime    = file.mtime(ff$fullname),
                            cm21_parsed   = Sys.time(),
                            cm21_md5sum   = as.vector(md5sum(ff$fullname)))
    cat(" p")

    ## Add data and metadata
    {
      res <- update_table(con      = con,
                          new_data = day_data,
                          table    = "LAP",
                          matchvar = "Date",
                          quiet    = TRUE)
      ## Add metadata
      if (!dbExistsTable(con, "META")) {
        ## Create new table
        cat("\n Initialize table 'META' \n\n")
        dbWriteTable(con, "META", file_meta)
      }
      ## Append new data
      res <- update_table(con      = con,
                          new_data = file_meta,
                          table    = "META",
                          matchvar = "Day",
                          quiet    = TRUE)
      cat(" w\n")
    }
  }
} else {
  cat(Script.ID, ": ", "No new files to add\n\n")
}

##  Checks  --------------------------------------------------------------------

## __ Check matching days  -----------------------------------------------------
A <- tbl(con, "LAP")  |> filter(!is.na(CM21_sig))      |> distinct(Day) |> pull()
B <- tbl(con, "META") |> filter(!is.na(cm21_basename)) |> distinct(Day) |> pull()

## Signal missing from meta data
test_A <- A[!A %in% B]
AA <- tbl(con, "LAP")  |> filter(Day %in% test_A) |> collect() |> data.table()
BB <- tbl(con, "META") |> filter(Day %in% test_A) |> collect() |> data.table()
stopifnot(nrow(AA) == 0)
stopifnot(nrow(BB) == 0)

## Ignore meta data files without any data
test_B <- B[!B %in% A]
CC <- tbl(con, "LAP")  |> filter(Day %in% test_B) |> collect() |> data.table()
DD <- tbl(con, "META") |> filter(Day %in% test_B) |> collect() |> data.table()
days_to_ignore <- CC[is.na(CM21_sig), unique(Day) ]
stopifnot(DD[!Day %in% days_to_ignore, .N] == 0)

##  Do some inspection  --------------------------------------------------------
if (interactive()) {

  fs::file_size(DB_BROAD)

  tbl(con, "LAP")  |> colnames()
  tbl(con, "META") |> colnames()

  tbl(con, "LAP")  |> filter(!is.na(CM21_sig)) |> glimpse()

  tbl(con, "LAP")  |> filter(!is.na(CM21_sig)) |> tally()
}

#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
goodbye()
