#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Reads Global radiation from Sirena TOT files into the database
#'
#'  - Reads raw resistance data
#'  - Converts resistance to temperature
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
knitr::opts_chunk$set(fig.cap   = " - empty caption - ")
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_16_cm21_TOT.R"
Script.ID   <- "16"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)

cat("\n Import  CM-21  TOT  data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_BROAD))

##  Get TOT CM-21 files  ------------------------------------------------------
inp_filelist <- list.files(path        = SIRENA_TOT,
                           recursive   = TRUE,
                           pattern     = "[0-9]*\\TOT.*.dat",
                           ignore.case = TRUE,
                           full.names  = TRUE)
cat("\n**Found:", length(inp_filelist), "TOT CM-21 files from Sirena**\n")
## just in case, there are nested folders with more lap files in Sirens
inp_filelist <- grep("OLD", inp_filelist, ignore.case = T, invert = T, value = T)

inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, tot_cm21_basename := basename(fullname)]
stopifnot( all(duplicated(sub("\\..*", "", inp_filelist$tot_cm21_basename))) == FALSE )

inp_filelist$Day <- as.Date(parse_date_time(
    sub("\\.dat", "", sub("TOT", "", inp_filelist$tot_cm21_basename, ignore.case = T), ignore.case = T),
    "jy"))
setorder(inp_filelist, Day)
cat("\n**Found:",paste(nrow(inp_filelist), "TOT CM-21 files**\n"))

## check files have unique days
stopifnot(!any(duplicated(inp_filelist$Day)))

## keep only files which correspond to existing dates
inp_filelist <- right_join(inp_filelist,
                           tbl(con, "LAP") |>
                             select(Day)   |>
                             distinct()    |>
                             collect(),
                           by = "Day") |>
  filter(!is.na(tot_cm21_basename))

## add only files not in the metadata
if (dbExistsTable(con, "META") &
    any(dbListFields(con, "META") %in% "tot_cm21_basename")) {
  inp_filelist <- anti_join(inp_filelist,
                            tbl(con, "META") |>
                              filter(!is.na(tot_cm21_basename)) |>
                              select(Day) |>
                              collect(),
                            by = "Day") |>
    filter(!is.na(tot_cm21_basename))
}
setorder(inp_filelist, Day)

cat("\n**Parse:",paste(nrow(inp_filelist), "TOT CM-21 files**\n\n"))

##  Import TOT CM-21 files  ----------------------------------------------------
if (nrow(inp_filelist) > 0) {
  for (ll in 1:nrow(inp_filelist)) {
    ff <- inp_filelist[ll, ]

    cat(Script.ID, ": ",
        basename(ff$fullname),
        paste(ff$Day),
        ll,"/",nrow(inp_filelist), "\n")

    ## __ Check data base is ready for import  ---------------------------------
    if (tbl(con, "LAP") |> filter(Day == ff$Day) |> tally() |> pull() != 1440) {
      cat("Data base not ready to import", paste(ff$Day), "\n")
      next()
    }

    ## __  Read TOT file  ----------------------------------------------
    day_data <- fread(ff$fullname, na.strings = "-9")

    if (dim(day_data)[1] != 1440) {
      cat("** Can not parse file: ", basename(ff$fullname), "**\n")
      warning("Can not parse file: ", basename(ff$fullname))
      next()
    }

    dateess   <- paste(ff$Day, day_data$TIME_UT %/% 1, round((day_data$TIME_UT %% 1) * 60))
    ## use the 30 second times stamp
    day_data$Date <- as.POSIXct(strptime(dateess, "%F %H %M")) - 30
    day_data[, TIME_UT := NULL]
    names(day_data)[names(day_data) == "SZA"]     <- "lap_sza"
    names(day_data)[names(day_data) == "[W.m-2]"] <- "tot_glb"
    names(day_data)[names(day_data) == "st.dev"]  <- "tot_glb_sd"
    day_data[tot_glb    < -8, tot_glb    := NA]
    day_data[tot_glb_sd < -8, tot_glb_sd := NA]

    stopifnot(is.numeric(day_data$tot_glb))
    stopifnot(is.numeric(day_data$tot_glb_sd))
    stopifnot(dim(day_data)[1] == 1440)

    ## get metadata
    file_meta <- data.table(Day                 = ff$Day,
                            tot_cm21_basename   = basename(ff$fullname),
                            tot_cm21_mtime      = file.mtime(ff$fullname),
                            tot_cm21_parsed     = Sys.time(),
                            tot_cm21_md5sum     = as.vector(md5sum(ff$fullname)))

    ## try to fix dates
    day_data[, Date := round_date(Date, unit = "second")]

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
    }
  }
} else {
  cat(Script.ID, ": ", "No new files to add\n\n")
}

##  Checks  --------------------------------------------------------------------

## __ Check matching days  -----------------------------------------------------
A <- tbl(con, "LAP")  |> filter(!is.na(tot_glb))      |> distinct(Day) |> pull()
B <- tbl(con, "META") |> filter(!is.na(tot_cm21_basename)) |> distinct(Day) |> pull()

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

  tbl(con, "LAP")  |> filter(!is.na(tot_glb)) |> glimpse()

  tbl(con, "LAP")  |> filter(!is.na(tot_glb)) |> tally()

  # dd <- tbl(con, "META") |> collect() |> data.table()
}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
