#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Reads CM-21 signal from `[0-9]*06.LAP$``
#'
#' Populates:
#'  - CM21_sig
#'  - CM21_sig_sd
#'
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#'
#+ echo=F, include=T


#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"  )
knitr::opts_chunk$set(fig.align  = "center")
knitr::opts_chunk$set(fig.pos    = '!h'    )


## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_02_cm21.R"
Script.ID   <- "02"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
source("~/BBand_LAP/functions/Functions_CM21.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)


cat("\n Import  CM-21  data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_DUCK))




##  Get CM-21 files  -----------------------------------------------------------
inp_filelist <- list.files(path        = SIRENA_GLB,
                           recursive   = TRUE,
                           pattern     = "[0-9]*06.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:",paste(length(inp_filelist), "CM-21 files from Sirena**\n"))
## just in case, there are nested folders with more lap files in Sirens
inp_filelist <- grep("OLD", inp_filelist, ignore.case = T, invert = T, value = T )

inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, cm21_basename := basename(fullname)]
stopifnot( all(duplicated(sub("\\..*", "", inp_filelist$cm21_basename))) == FALSE )

inp_filelist$Day <- as.Date(parse_date_time(
  sub("06\\..*", "", inp_filelist$cm21_basename),
  "dmy"))
setorder(inp_filelist, Day)
cat("\n**Found:",paste(nrow(inp_filelist), "CM-21 files**\n"))

# inp_filelist <- inp_filelist[!inp_filelist$cm21_basename %in% BB_meta$cm21_basename]
# inp_filelist <- inp_filelist[inp_filelist$day %in% BB_meta$day]
# cat("\n**Parse:",paste(nrow(inp_filelist), "CM-21 files**\n\n"))

### FIXME test
# inp_filelist <- inp_filelist[Day > "2023-01-01" & Day < "2024-01-01"]
# inp_filelist <- inp_filelist[Day > "2023-01-01"]



## keep only files which correspond to existing dates
inp_filelist <- right_join(inp_filelist,
                           tbl(con, "LAP") |>
                             select(Day)   |>
                             distinct()    |>
                             collect(),
                           by = "Day") |>
  filter(!is.na(cm21_basename))



## add only files not in the metadata
if (dbExistsTable(con, "META")) {
  inp_filelist <- anti_join(inp_filelist,
                            tbl(con, "META") |>
                              filter(!is.na(cm21_basename)) |>
                              select(Day) |>
                              collect(),
                            by = "Day") |>
    filter(!is.na(cm21_basename))
}


## parse all files
if (nrow(inp_filelist) > 0) {
  for (ll in 1:nrow(inp_filelist)) {
    ff <- inp_filelist[ll, ]

    cat(Script.ID, ": ",
        basename(ff$fullname),
        paste(ff$Day),
        ll,"/",nrow(inp_filelist), "\n")

    ## prepare input file data
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

    day_data <- data.table(Date        = D_minutes,      # Date of the data point
                           CM21_sig    = lap$V1,         # Raw value for CM21
                           CM21_sig_sd = lap$V2)         # Raw SD value for CM21

    ## try to fix dates
    day_data[, Date := round_date(Date, unit = "second")]

    ## normal signal flag
    day_data[, cm21_sig_limit_flag := 0L ]

    ## "Abnormal LOW signal"
    day_data[CM21_sig < cm21_signal_lower_limit(Date),
             cm21_sig_limit_flag := 1L ]

    ## "Abnormal HIGH signal"
    day_data[CM21_sig > cm21_signal_upper_limit(Date),
             cm21_sig_limit_flag := 2L ]

    # ## use epoch only
    # day_data$Epoch <- as.integer(day_data$Date)
    # day_data$Date  <- NULL

    ## meta data for file
    file_meta <- data.table(Day           = ff$Day,
                            cm21_basename = basename(ff$fullname),
                            cm21_mtime    = file.mtime(ff$fullname),
                            cm21_parsed   = Sys.time(),
                            cm21_md5sum   = as.vector(md5sum(ff$fullname)))

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
        # db_create_index(con, "META", columns = "Day", unique = TRUE)
      }

      ## Append new data
      res <- update_table(con      = con,
                   new_data = file_meta,
                   table    = "META",
                   matchvar = "Day")

      stopifnot(res |> tally() |> pull() == 1)
      stopifnot(!is.na(res |> select(Day) |> pull()))

      # ## Append new data
      # upsert_table(con      = con,
      #              new_data = file_meta,
      #              table    = "META",
      #              matchvar = "Day")

      # res <- rows_update(x         = tbl(con, "META"),
      #                    y         = file_meta,
      #                    by        = "Day",
      #                    unmatched = "ignore",
      #                    in_place  = TRUE,
      #                    copy      = TRUE)
    }
  }
} else {
  cat(Script.ID, ": ", "No new files to add\n\n")
}

## clean exit
dbDisconnect(con); rm(con); closeAllConnections()

##  Checks  --------------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_DUCK))

## all days should match
stopifnot(
  setequal(
    tbl(con, "LAP")  |> filter(!is.na(CM21_sig))      |> distinct(Day) |> pull(),
    tbl(con, "META") |> filter(!is.na(cm21_basename)) |> distinct(Day) |> pull()
  )
)


A <- tbl(con, "LAP")  |> filter(!is.na(CM21_sig))      |> distinct(Day) |> pull()
B <- tbl(con, "META") |> filter(!is.na(cm21_basename)) |> distinct(Day) |> pull()


test <- A[!A %in% B]
B[!B %in% A]

tbl(con, "LAP")  |> filter(Day %in% test)
tbl(con, "META") |> filter(Day %in% test)




## clean exit
dbDisconnect(con); rm(con); closeAllConnections()


if (FALSE) {

  fs::file_size(DB_DUCK)

  con   <- dbConnect(duckdb(dbdir = DB_DUCK))

  tbl(con, "LAP")  |> colnames()
  tbl(con, "META") |> colnames()

  tbl(con, "LAP")  |> filter(!is.na(CM21_sig)) |> glimpse()

  tbl(con, "LAP")  |> filter(!is.na(CM21_sig)) |> tally()

  # dd <- tbl(con, "META") |> collect() |> data.table()
}


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
