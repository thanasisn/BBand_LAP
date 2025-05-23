# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Read INCLINED CM-21 signal from `[0-9]*01.LAP$`
#'
#' Populates:
#'  - CM21INC_sig
#'  - CM21INC_sig_sd
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_06_cm21inclined.R"
Script.ID   <- "06"

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
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)

cat("\n Import  INCLINED CM-21  data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_BROAD))

##  Get CM-21 files  -----------------------------------------------------------
inp_filelist <- list.files(path        = SIRENA_INC,
                           recursive   = TRUE,
                           pattern     = "[0-9]*01.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE)
cat("\n**Found:", length(inp_filelist), "INCLINED CM-21 files from Sirena**\n")
## just in case, there are nested folders with more lap files in Sirens
inp_filelist <- grep("OLD", inp_filelist, ignore.case = T, invert = T, value = T)
inp_filelist <- grep("Horizontal", inp_filelist, ignore.case = T, invert = T, value = T)

inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, cm21inc_basename := basename(fullname)]
stopifnot(all(duplicated(sub("\\..*", "", inp_filelist$cm21inc_basename))) == FALSE)

inp_filelist$Day <- as.Date(parse_date_time(
    sub("01\\..*", "", inp_filelist$cm21inc_basename),
    "dmy"))
setorder(inp_filelist, Day)
cat("\n**Found:", paste(nrow(inp_filelist), "INCLINED CM-21 files**\n"))

## check files have unique days
stopifnot(!any(duplicated(inp_filelist$Day)))

## keep only files which correspond to existing dates
inp_filelist <- right_join(inp_filelist,
                           tbl(con, "LAP") |>
                             select(Day)   |>
                             distinct()    |>
                             collect(),
                           by = "Day") |>
  filter(!is.na(cm21inc_basename))

## add only files not in the metadata
if (dbExistsTable(con, "META") &
    any(dbListFields(con, "META") %in% "cm21inc_basename")) {
  inp_filelist <- anti_join(inp_filelist,
                            tbl(con, "META") |>
                              filter(!is.na(cm21inc_basename)) |>
                              select(Day) |>
                              collect(),
                            by = "Day") |>
    filter(!is.na(cm21inc_basename))
}
setorder(inp_filelist, Day)

cat("\n**Parse:", paste(nrow(inp_filelist), "INCLINED CM-21 files**\n\n"))

##  Import INCLINED CM-21 files  -----------------------------------------------
if (nrow(inp_filelist) > 0) {
  for (ll in 1:nrow(inp_filelist)) {
    ff <- inp_filelist[ll, ]

    cat(Script.ID, ": ",
        basename(ff$fullname),
        paste(ff$Day),
        ll,"/",nrow(inp_filelist))

    ## __ Check data base is ready for import  ---------------------------------
    if (tbl(con, "LAP") |> filter(Day == ff$Day) |> tally() |> pull() != 1440) {
      cat("Data base not ready to import", paste(ff$Day), "\n")
      next()
    }

    ## __ Read INCLINED CM-21 LAP file  ----------------------------------------
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

    day_data <- data.table(Date           = D_minutes,      # Date of the data point
                           CM21INC_sig    = lap$V1,         # Raw value for INCLINED CM21
                           CM21INC_sig_sd = lap$V2)         # Raw SD value for INCLINED CM21

    file_meta <- data.table(Day                = ff$Day,
                            cm21inc_basename   = basename(ff$fullname),
                            cm21inc_mtime      = file.mtime(ff$fullname),
                            cm21inc_parsed     = Sys.time(),
                            cm21inc_md5sum     = as.vector(md5sum(ff$fullname)))

    ## try to fix dates
    day_data[, Date := round_date(Date, unit = "second")]

    ## TODO when have more data about the instrument function
    # ## normal signal flag
    # day_data[, cm21inc_sig_limit_flag := 0L ]
    #
    # ## "Abnormal LOW signal"
    # day_data[CM21inc_sig < cm21inc_signal_lower_limit(Date),
    #          cm21inc_sig_limit_flag := 1L ]
    #
    # ## "Abnormal HIGH signal"
    # day_data[CM21INC_sig > cm21inc_signal_upper_limit(Date),
    #          cm21inc_sig_limit_flag := 2L ]

    ## meta data for file
    file_meta <- data.table(Day              = ff$Day,
                            cm21inc_basename = basename(ff$fullname),
                            cm21inc_mtime    = file.mtime(ff$fullname),
                            cm21inc_parsed   = Sys.time(),
                            cm21inc_md5sum   = as.vector(md5sum(ff$fullname)))
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
A <- tbl(con, "LAP")  |> filter(!is.na(CM21INC_sig))      |> distinct(Day) |> pull()
B <- tbl(con, "META") |> filter(!is.na(cm21inc_basename)) |> distinct(Day) |> pull()

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
days_to_ignore <- CC[is.na(CM21INC_sig), unique(Day) ]
stopifnot(DD[!Day %in% days_to_ignore, .N] == 0)

##  Do some inspection  --------------------------------------------------------
if (interactive()) {

  fs::file_size(DB_BROAD)

  tbl(con, "LAP")  |> colnames()
  tbl(con, "META") |> colnames()

  tbl(con, "LAP")  |> filter(!is.na(CM21INC_sig)) |> glimpse()

  tbl(con, "LAP")  |> filter(!is.na(CM21INC_sig)) |> tally()

  # dd <- tbl(con, "META") |> collect() |> data.table()
}


#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
goodbye()
