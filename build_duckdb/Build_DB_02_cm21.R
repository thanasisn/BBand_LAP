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
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
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
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_02_cm21.R"
Script.ID   <- "02"
# renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")

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
inp_filelist <- inp_filelist[Day > "2023-01-01" & Day < "2024-01-01"]



## only new files in the date range
if (dbExistsTable(con, "META")) {
  inp_filelist <- anti_join(inp_filelist,
                            tbl(con, "META") |>
                              select(Day) |>
                              filter(!is.na(Day)) |>
                              collect(),
                            by = "Day")
}



update_table <- function(con,  new_data, table, matchvar) {
  ## detect data types
  tt1 <- data.table(names = colnames(tbl(con, table)),
                    types = tbl(con, table) |> head(1) |> collect() |> sapply(class))
  dd1 <- data.table(names = colnames(new_data),
                    types = new_data |> head(1) |> collect() |> sapply(class))

  if (!all(dd1$names %in% tt1$names)) {
    ## get new variables
    new_vars <- dd1[!names %in% tt1$names, ]
    cat("New", new_vars$names)

    for (i in 1:nrow(new_vars)) {

      ## translate data types to duckdb
      ctype <- switch(paste0(unlist(new_vars$types[i]), collapse = ""),
                      POSIXctPOSIXt = "datetime",
                      unlist(new_vars$types[i]))

      cat("\nNEW VAR:", paste(new_vars[i, ]), "\n")

      ## create new columns with a query
      qq <- paste("ALTER TABLE", table,
                  "ADD COLUMN",  new_vars$names[i], ctype, "DEFAULT null")
      dbSendQuery(con, qq)
    }
  }

  rows_update(x = tbl(con, table),
              y = new_data,
              by   = matchvar,
              unmatched = "ignore",
              in_place = TRUE,
              copy = TRUE)
}



insert_table <- function(con,  new_data, table, matchvar) {
  ## detect data types
  tt1 <- data.table(names = colnames(tbl(con, table)),
                    types = tbl(con, table) |> head(1) |> collect() |> sapply(class))
  dd1 <- data.table(names = colnames(new_data),
                    types = new_data |> head(1) |> collect() |> sapply(class))

  if (!all(dd1$names %in% tt1$names)) {
    ## get new variables
    new_vars <- dd1[!names %in% tt1$names, ]
    cat("New", new_vars$names)

    for (i in 1:nrow(new_vars)) {

      ## translate data types to duckdb
      ctype <- switch(paste0(unlist(new_vars$types[i]), collapse = ""),
                      POSIXctPOSIXt = "datetime",
                      unlist(new_vars$types[i]))

      cat("\nNEW VAR:", paste(new_vars[i, ]), "\n")

      ## create new columns with a query
      qq <- paste("ALTER TABLE", table,
                  "ADD COLUMN",  new_vars$names[i], ctype, "DEFAULT null")
      dbSendQuery(con, qq)
    }
  }

  rows_insert(x  = tbl(con, table),
              y  = new_data,
              by = matchvar,
              conflict = "ignore",
              in_place = TRUE,
              copy     = TRUE)
}



if (nrow(inp_filelist) > 0) {
  for (ll in 1:nrow(inp_filelist)) {
    ff <- inp_filelist[ll, ]

    cat(Script.ID, ": ", basename(ff$fullname), ff$Day, ll,"/",nrow(inp_filelist),"\n")

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

    day_data <- data.table(Date        = as.POSIXct(D_minutes),      # Date of the data point
                           CM21_sig    = lap$V1,         # Raw value for CM21
                           CM21_sig_sd = lap$V2)         # Raw SD value for CM21
    day_data$Epoch <- as.integer(day_data$Date)
    day_data$Date <- NULL

    ## meta data for file
    file_meta <- data.table(Day             = ff$Day,
                            cm21_basename   = basename(ff$fullname),
                            cm21_mtime      = file.mtime(ff$fullname),
                            cm21_parsed     = Sys.time(),
                            cm21_md5sum     = as.vector(md5sum(ff$fullname)))

    ## Add data
    update_table(con      = con,
                 new_data = day_data,
                 table    = "LAP",
                 matchvar = "Epoch")

    ## Add metadata
    if (!dbExistsTable(con, "META")) {
      ## Create new table
      cat("\n Initialize table 'META' \n\n")
      dbWriteTable(con, "META", file_meta)
      db_create_index(con, "META", columns = "Day", unique = TRUE)
    } else {
      ## Append new data
      insert_table(con      = con,
                   new_data = file_meta,
                   table    = "META",
                   matchvar = "Day")
    }


  }
} else {
  cat(Script.ID, ": ", "No new files to add\n\n")
}


dbDisconnect(con)


stop()





con   <- dbConnect(duckdb(dbdir = DB_DUCK))


tbl(con, "LAP") |> colnames()
tbl(con, "META") |> colnames()


tbl(con, "LAP")  |> filter(!is.na(CM21_sig)) |> glimpse()

tbl(con, "LAP")  |> filter(!is.na(CM21_sig)) |> tally()
tbl(con, "LAP")  |> filter(!is.na(CM21_sig)) |> distinct(Day) |> tally()

tbl(con, "META") |> tally()






# myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
