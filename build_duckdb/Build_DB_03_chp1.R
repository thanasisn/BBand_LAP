#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Read CHP-1 signal from `[0-9]*03.LAP$`
#'
#' Populates:
#'  - CHP1_sig
#'  - CHP1_sig_sd
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_03_chp1.R"
Script.ID   <- "03"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)

cat("\n Import  CHP-1  data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_BROAD))

##  Get CHP-1 files  -----------------------------------------------------------
inp_filelist <- list.files(path        = SIRENA_DIR,
                           recursive   = TRUE,
                           pattern     = "[0-9]*03.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:",paste(length(inp_filelist), "CHP-1 files from Sirena**\n"))
## just in case, there are nested folders with more lap files in Sirena
inp_filelist <- grep("OLD", inp_filelist, ignore.case = T, invert = T, value = T)

inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, chp1_basename := basename(fullname)]
stopifnot( all(duplicated(sub("\\..*", "", inp_filelist$chp1_basename))) == FALSE)

inp_filelist$Day <- as.Date(parse_date_time(
  sub("03\\..*", "", inp_filelist$chp1_basename),
  "dmy"))
setorder(inp_filelist, Day)
cat("\n**Found:",paste(nrow(inp_filelist), "CHP-1 files**\n"))

## check files have unique days
stopifnot(!any(duplicated(inp_filelist$Day)))

## keep only files which correspond to existing dates
inp_filelist <- right_join(inp_filelist,
                           tbl(con, "LAP") |>
                             select(Day)   |>
                             distinct()    |>
                             collect(),
                           by = "Day") |>
  filter(!is.na(chp1_basename))

## add only files not in the metadata
if (dbExistsTable(con, "META") &
    any(dbListFields(con, "META") %in% "chp1_basename")) {
  inp_filelist <- anti_join(inp_filelist,
                            tbl(con, "META") |>
                              filter(!is.na(chp1_basename)) |>
                              select(Day) |>
                              collect(),
                            by = "Day") |>
    filter(!is.na(chp1_basename))
}
setorder(inp_filelist, Day)

cat("\n**Parse:",paste(nrow(inp_filelist), "CHP-1 files**\n\n"))

## create categorical column for signal
categories <- c("empty",                 ## default NA
                "pass",                  ## signal is normal
                "Abnormal LOW signal",   ## too low signal
                "Abnormal HIGH signal")  ## too high signal

make_categorical_column("chp1_sig_limit_flag", categories, con, "LAP")

##  Import CHP-1 files  --------------------------------------------------------
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

    ## __ Read CHP-1 LAP file  ----------------------------------------------
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
                           CHP1_sig    = lap$V1,         # Raw value for CHP1
                           CHP1_sig_sd = lap$V2)         # Raw SD value for CHP1

    ## try to fix dates
    day_data[, Date := round_date(Date, unit = "second")]

    ## flag signal limits
    day_data[,
             chp1_sig_limit_flag := "pass"]

    day_data[CHP1_sig < chp1_signal_lower_limit(Date),
             chp1_sig_limit_flag := "Abnormal LOW signal"]

    day_data[CHP1_sig > chp1_signal_upper_limit(Date),
             chp1_sig_limit_flag := "Abnormal HIGH signal" ]

    ## Over 9000!! enforce data base numeric scheme
    day_data[CHP1_sig > 9000, CHP1_sig := 9000]

    ## meta data for file
    file_meta <- data.table(Day           = ff$Day,
                            chp1_basename = basename(ff$fullname),
                            chp1_mtime    = file.mtime(ff$fullname),
                            chp1_parsed   = Sys.time(),
                            chp1_md5sum   = as.vector(md5sum(ff$fullname)))
    cat(" p")

    ## Add data and metadata
    {
      update_table(con      = con,
                   new_data = day_data,
                   table    = "LAP",
                   matchvar = "Date")
      ## Add metadata
      if (!dbExistsTable(con, "META")) {
        ## Create new table
        cat("\n Initialize table 'META' \n\n")
        dbWriteTable(con, "META", file_meta)
      }
      ## Append new data
      update_table(con      = con,
                   new_data = file_meta,
                   table    = "META",
                   matchvar = "Day")
      cat(" w\n")
    }
  }
} else {
  cat(Script.ID, ": ", "No new files to add\n\n")
}

##  Checks  --------------------------------------------------------------------

## __ Check matching days  -----------------------------------------------------
A <- tbl(con, "LAP")  |> filter(!is.na(CHP1_sig))      |> distinct(Day) |> pull()
B <- tbl(con, "META") |> filter(!is.na(chp1_basename)) |> distinct(Day) |> pull()

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
days_to_ignore <- CC[is.na(CHP1_sig), unique(Day) ]
stopifnot(DD[!Day %in% days_to_ignore, .N] == 0)

##  Do some inspection  --------------------------------------------------------
if (interactive()) {

  fs::file_size(DB_BROAD)

  tbl(con, "LAP")  |> colnames()
  tbl(con, "META") |> colnames()

  tbl(con, "LAP")  |> filter(!is.na(CHP1_sig)) |> glimpse()

  tbl(con, "LAP")  |> filter(!is.na(CHP1_sig)) |> tally()
}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
