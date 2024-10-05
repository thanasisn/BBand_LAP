#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Reads Global radiation from sirena TOT files into the database
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
knitr::opts_chunk$set(fig.pos   = '!h'    )

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_16_cm21_TOT.R"
Script.ID   <- "16"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
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
con   <- dbConnect(duckdb(dbdir = DB_DUCK))

##  Get TOT CM-21 files  ------------------------------------------------------
inp_filelist <- list.files(path        = SIRENA_TOT,
                           recursive   = TRUE,
                           pattern     = "[0-9]*\\TOT.*.dat",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:",paste(length(inp_filelist), "TOT CM-21 files from Sirena**\n"))
## just in case, there are nested folders with more lap files in Sirens
inp_filelist <- grep("OLD", inp_filelist, ignore.case = T, invert = T, value = T )

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

cat("\n**Parse:",paste(nrow(inp_filelist), "TOT CM-21 files**\n\n"))

##  Import TOT CM-21 files  ----------------------------------------------------
if (nrow(inp_filelist) > 0) {
  for (ll in 1:nrow(inp_filelist)) {
    ff <- inp_filelist[ll, ]

    cat(Script.ID, ": ",
        basename(ff$fullname),
        paste(ff$Day),
        ll,"/",nrow(inp_filelist), "\n")





    stop()

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








for (YYYY in unique(year(inp_filelist$day))) {
    subyear <- inp_filelist[year(day) == YYYY]
    ## months to do
    for (mm in subyear[, unique(month(day))]) {

        ##  read this month set files
        gathermeta <- data.table()
        for (ad in submonth$day) {
            ss <- submonth[day == ad]

            ## __  Read TOT file  ----------------------------------------------
            temp <- fread(ss$fullname, na.strings = "-9")

            if (dim(temp)[1] != 1440) {
                cat("** Can not parse file: ", basename(ss$fullname), "**\n")
                warning("Can not parse file: ", basename(ss$fullname))
                next()
            }

            dateess   <- paste(ss$day, temp$TIME_UT %/% 1, round((temp$TIME_UT %% 1) * 60))
            ## use the 30 second times stamp
            temp$Date <- as.POSIXct(strptime(dateess, "%F %H %M")) - 30
            temp[, TIME_UT := NULL]
            ## rename columns
            names(temp)[names(temp) == "SZA"]     <- "lap_sza"
            names(temp)[names(temp) == "[W.m-2]"] <- "tot_glb"
            names(temp)[names(temp) == "st.dev"]  <- "tot_glb_sd"
            ## deal with NAs
            temp[tot_glb    < -8, tot_glb    := NA]
            temp[tot_glb_sd < -8, tot_glb_sd := NA]

            stopifnot(is.numeric(temp$tot_glb))
            stopifnot(is.numeric(temp$tot_glb_sd))
            stopifnot(dim(temp)[1] == 1440)

            temp[, year  := as.integer(year(Date))]
            temp[, month := as.integer(month(Date))]

            ## get metadata
            file_meta <- data.table(day                 = as_date(ad),
                                    tot_cm21_basename   = basename(ss$fullname),
                                    tot_cm21_mtime      = file.mtime(ss$fullname),
                                    tot_cm21_parsed     = Sys.time(),
                                    tot_cm21_md5sum     = as.vector(md5sum(ss$fullname)))

            # gather <- rows_patch(gather, day_data, by = "Date")
            # gather     <- rows_update(gather, day_data, by = "Date")
            gather     <- rows_upsert(gather, temp, by = "Date")
            gathermeta <- rbind(gathermeta, file_meta)
            rm(temp, file_meta, ss )
        }

        if (nrow(gathermeta) == 0) {
            cat("\nNo new files to import\n\n")
            next()
        }

        BB_meta <- rows_update(BB_meta, gathermeta, by = "day")
        # BBdaily <- rows_patch(BBdaily, gathermeta, by = "day", unmatched = "ignore")

        setorder(gather, Date)

    }
}






myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

