#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Reads temperature data for CHP-1 into the database
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_05_chp1_TMP.R"
Script.ID   <- "05"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/",  basename(sub("\\.R$", ".pdf", Script.Name))))
  sink(file = paste0("~/BBand_LAP/REPORTS/LOGs/duck/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
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

cat("\n Import  CHP-1 temperature data\n\n")

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_DUCK))

##  Get tracker sync files  ----------------------------------------------------
inp_filelist <- list.files(path        = CHPTMP_DIR,
                           recursive   = TRUE,
                           pattern     = "sun_tracker_.*.therm$",,
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:", length(inp_filelist), "CHP-1 temperature files**\n")

inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, chp1_temp_basename := basename(fullname)]
stopifnot( all(duplicated(sub("\\..*", "", inp_filelist$chp1_temp_basename))) == FALSE )

inp_filelist$Day <- as.Date(parse_date_time(
    sub("\\.therm", "", sub("sun_tracker_", "", inp_filelist$chp1_temp_basename)),
    "Ymd"))
setorder(inp_filelist, Day)
cat("\n**Found:",paste(nrow(inp_filelist), "CHP-1 temperature files**\n"))

## check files have unique days
stopifnot(!any(duplicated(inp_filelist$Day)))

## keep only files which correspond to existing dates
inp_filelist <- right_join(inp_filelist,
                           tbl(con, "LAP") |>
                             select(Day)   |>
                             distinct()    |>
                             collect(),
                           by = "Day") |>
  filter(!is.na(chp1_temp_basename))

## add only files not in the metadata
if (dbExistsTable(con, "META") &
    any(dbListFields(con, "META") %in% "chp1_temp_basename")) {
  inp_filelist <- anti_join(inp_filelist,
                            tbl(con, "META") |>
                              filter(!is.na(chp1_temp_basename)) |>
                              select(Day) |>
                              collect(),
                            by = "Day") |>
    filter(!is.na(chp1_temp_basename))
}
setorder(inp_filelist, Day)

## ignore current and future dates
inp_filelist <- inp_filelist[ Day < as.Date(Sys.Date())]
cat("\n**Parse:", nrow(inp_filelist), "CHP-1 temperature files**\n\n")

## create categorical column for CHP-1 temperature
categories <- c("empty",                      ## default NA
                "pass",                       ## signal is normal
                "Abnormal LOW temperature",   ## too low temperature
                "Abnormal HIGH temperature",  ## too high temperature
                "Abnormal temperature SD")    ## too high temperature SD

make_categorical_column("chp1_bad_temp_flag", categories, con, "LAP")

##  Import CHP-1 temperature  --------------------------------------------------------
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

    ## __  Read CHP-1 temperature file  ----------------------------------------
    temp_temp    <- read.table(ff$fullname, sep = "\t", as.is = TRUE)
    temp_temp$V1 <- as.POSIXct(temp_temp$V1)
    temp_temp$V1 <- as.POSIXct(format(temp_temp$V1, format = "%F %R"))
    temp_temp$V1 <- temp_temp$V1 + 30
    temp_temp$V3[temp_temp$V3 == 0] <- NA

    temp_temp    <- data.table(temp_temp)
    temp_temp    <- temp_temp[, .(V2 = mean(V2, na.rm = T),
                                  V3 = mean(V3, na.rm = T)), by = V1]
    temp_temp[V2 < 0, V2 := NA]
    temp_temp[V2 < 0, V3 := NA]
    cat(" r")

    day_data <- data.frame(Date                = temp_temp$V1,
                           chp1_kohm_therm     = temp_temp$V2                        / 1000,
                           chp1_kohm_SD_therm  = temp_temp$V3                        / 1000,
                           chp1_kohm_meas_ERR  = Protek_506_R_error(   temp_temp$V2) / 1000,
                           chp1_temperature    = CHP_thermistor_R_to_T(temp_temp$V2),
                           chp1_temperature_SD = CHP_thermistor_ResUnc_to_TempUnc(temp_temp$V2, temp_temp$V3))
    day_data$chp1_temp_UNC <- CHP_thermistor_ResUnc_to_TempUnc(temp_temp$V2, day_data$chp1_kohm_meas_ERR * 1000)

    ## get file metadata
    file_meta <- data.table(Day                = ff$Day,
                            chp1_temp_basename = basename(ff$fullname),
                            chp1_temp_mtime    = file.mtime(ff$fullname),
                            chp1_temp_parsed   = Sys.time(),
                            chp1_temp_md5sum   = as.vector(md5sum(ff$fullname)))

    ## _ CHP-1 flag temperature physical limits  -------------------------------

    ## NAN to NA
    day_data <- day_data |>
      mutate_if(is.numeric, function(x) ifelse(is.nan(x), NA, x)) |> data.table()

    ## try to fix dates
    day_data[, Date := round_date(Date, unit = "second")]

    ## flag temperature limits
    day_data[,
             chp1_bad_temp_flag := "pass"]

    day_data[chp1_temperature < CHP1_TEMP_MIN,
             chp1_bad_temp_flag := "Abnormal LOW temperature"]

    day_data[chp1_temperature > CHP1_TEMP_MAX,
             chp1_bad_temp_flag := "Abnormal HIGH temperature"]

    day_data[chp1_temperature_SD > CHP1_TEMP_STD_LIM,
             chp1_bad_temp_flag := "Abnormal temperature SD"]

    ## Over 9000!! enforce data base numeric scheme
    day_data[chp1_kohm_therm > 9000, chp1_kohm_therm := 9000]
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

if (interactive()) {

  tbl(con, "LAP")  |> glimpse()
  tbl(con, "META") |> glimpse()

  tbl(con, "LAP") |>
    group_by(Async_tracker_flag) |>
    tally()

  tbl(con, "LAP") |>
    group_by(Async_step_count) |>
    tally()

  fs::file_size(DB_DUCK)

  tbl(con, "LAP")  |> colnames()
  tbl(con, "META") |> colnames()

  tbl(con, "LAP")  |> filter(!is.na(CM21_sig)) |> glimpse()

  tbl(con, "LAP")  |> filter(!is.na(CM21_sig)) |> tally()

  # dd <- tbl(con, "META") |> collect() |> data.table()
}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()


tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
