# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Compute or construct dark signal offset for CM-21.
#'
#' Fills:
#' - `CM21_sig_wo_dark` Global radiation signal without dark signal
#' - `GLB_wpsm`         Global radiation
#' - `GLB_SD_wpsm`      Global radiation standard radiation
#'
#' On the second pass will replace 'MISSING' dark with 'CONSTRUCTED' dark.
#'
#' TODO
#' - print dark type on graphs from metadata
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_42_cm21_dark_radiat.R"
Script.ID   <- "42"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_dark_calculation.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")
source("~/BBand_LAP/functions/Functions_CM21.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_BROAD))

##  Create a dummy column if not existing
if (Sys.info()["nodename"] == Main.Host) {
  make_new_column(con, "META", "cm21_dark_flag", "character")
}

##  Create a dark construct!  --------------------------------------------------
## create construct if are available data
vddays <- tbl(con, "META") |> filter(!is.na(cm21_dark_flag)) |> tally() |> pull()
if (vddays > 100) {
  test <- tbl(con, "META") |>
    select(Day,
           cm21_dark_flag,
           cm21_dark_Eve_med,
           cm21_dark_Mor_med,
           cm21_Daily_dark) |>
    filter(!is.na(cm21_Daily_dark))  |>
    collect() |> data.table()
  ## will use mean Daily dark
  cm21EVEdark   <- approxfun(test$Day, test$cm21_dark_Eve_med)
  cm21MORdark   <- approxfun(test$Day, test$cm21_dark_Mor_med)
  cm21DAILYdark <- approxfun(test$Day, test$cm21_Daily_dark  )

  ## get dark missing days
  missingdays <- tbl(con, "META") |>
    filter(!is.na(cm21_basename)) |>
    # filter(is.na(cm21_dark_flag) | cm21_dark_flag %in% c("MISSING", "CONSTRUCTED")) |> ## consider it to see more
    filter(cm21_dark_flag %in% c("MISSING", "CONSTRUCTED")) |>                           ## consider it to see less (original)
    select(Day) |> collect() |> data.table()
  ## should we use missing only?
  ## Create missing dark
  construct <- data.table(
    Date = missingdays,
    DARK = cm21DAILYdark(missingdays$Day)
  )
  plot(test$Day, test$cm21_Daily_dark,
       main = "Constructed Dark values for CM-21")
  points(construct$Date, construct$DARK, col = "red")
}

##  Dark and radiation calculations  -------------------------------------------
## days to loop
dayslist <- tbl(con, "META") |>
  filter(is.na(cm21_dark_flag) & !is.na(cm21_basename)) |>
  select(Day) |> pull()

cc <- 0
for (ad in sort(dayslist)) {
  ad <- as.Date(ad, origin = origin)
  cc <- cc + 1
  cat(Script.ID, ":", paste(ad), cc, "/",length(dayslist))
  status_msg(ScriptName = Script.Name, msg = c(paste(ad), cc, length(dayslist)))

  ## use only valid data for dark calculation
  ## !!!! We need all Elevation values for the day for the dark functions to work !!!
  daydata <-
    tbl(con, "LAP")                                     |>
    filter(Day == ad)                                   |> ## this day only
    ## NO!! filter(!is.na(CM21_sig))                      |> ## valid measurements
    filter(cm21_bad_data_flag  %in% c("pass", "empty")) |> ## not bad data
    filter(cm21_sig_limit_flag %in% c("pass", "empty")) |> ## acceptable values range
    collect() |> data.table()

  ## Ignore bad and missing data
  if (nrow(daydata) == 0) {
    cat("      >>  No useful CM-21 data on this day  <<\n")
    next()
  }
  if (any(is.na(daydata$Elevat))) {
    cat("      >>  The day is not initialized:", paste(ad), " <<\n")
    next()
  }

  ## __ Compute dark values for day  -------------------------------------------
  dark_day <- dark_calculations_2(
    dates      = daydata$Date,
    values     = daydata$CM21_sig,
    elevatio   = daydata$Eleva,
    nightlimit = DARK_ELEV,
    dstretch   = DSTRETCH
  )

  # plot(daydata$Date, daydata$CM21_sig)
  # abline(v = dark_day$dark_Mor_sta, col = 1)
  # abline(v = dark_day$dark_Mor_end, col = 2)
  # abline(v = dark_day$dark_Eve_sta, col = 3)
  # abline(v = dark_day$dark_Eve_end, col = 4)

  ## __ Resolve problematic dark calculations  ---------------------------------

  if (all(is.na(daydata$CM21_sig))) {
    ## data to compute dark are missing
    dark_flag              <- "NO SIGNAL DATA"
    todays_dark_correction <- as.numeric(NA)
    missingdark            <- as.numeric(NA)
  ## data to compute dark is missing
  } else if ( !((!is.na(dark_day$dark_Mor_med) & dark_day$dark_Mor_cnt >= DCOUNTLIM) |
                (!is.na(dark_day$dark_Eve_med) & dark_day$dark_Eve_cnt >= DCOUNTLIM))) {
    todays_dark_correction <- as.numeric(NA)
    dark_flag              <- "MISSING"
    missingdark            <- as.numeric(NA)

    ## get dark from pre-computed file
    if (exists("construct")) {
      ## can not find date
      if (!ad %in% construct$Date) {
        todays_dark_correction <- as.numeric(NA)
        dark_flag              <- "MISSING"
        missingdark            <- as.numeric(NA)
      } else {
        ## get data from recomputed dark database
        todays_dark_correction <- construct[Date == ad, DARK]
        dark_flag              <- "CONSTRUCTED"
      }
    } else {
      cat("Need to construct dark:", format(as.Date(ad)),"\n")
    }
  } else {
    ## __ Dark Correction function for non missing  ----------------------------
    dark_generator <- dark_function_2(dark_day    = dark_day,
                                      DCOUNTLIM   = DCOUNTLIM,
                                      type        = "median",
                                      missingdark = missingdark)
    ## Create dark signal for every minute
    todays_dark_correction <- dark_generator(daydata$Date)
    dark_flag              <- "COMPUTED"
  }

  ## __ Apply dark correction for the day  -------------------------------------
  daydata[, CM21_sig_wo_dark := CM21_sig - todays_dark_correction]

  ## __ Convert signal to radiation  -------------------------------------------
  daydata[, GLB_wpsm    := CM21_sig_wo_dark * cm21factor(Date)]
  daydata[, GLB_SD_wpsm := CM21_sig_sd      * cm21factor(Date)]
  cat(" p")

  ## __ Day stats  -------------------------------------------------------------
  names(dark_day) <- paste0("cm21_", names(dark_day))
  meta_day <- data.frame(Day                = as.Date(ad),
                         cm21_Daily_dark    = mean(todays_dark_correction, na.rm = T),
                         cm21_dark_flag     = dark_flag,
                         dark_day,
                         cm21_dark_computed = Sys.time()
  ) |> mutate_if(is.numeric, function(x) ifelse(is.nan(x), NA, x))

  ## __  Store data in the database  -------------------------------------------
  if (Sys.info()["nodename"] == Main.Host) {

    res <- update_table(con      = con,
                        new_data = daydata,
                        table    = "LAP",
                        matchvar = "Date",
                        quiet    = TRUE)
    cat(" w")
    res <- update_table(con      = con,
                        new_data = meta_day,
                        table    = "META",
                        matchvar = "Day",
                        quiet    = TRUE)
    cat(" w")
  }
  cat("\n")
}

tbl(con, "META") |> group_by(cm21_dark_flag) |> tally()


# tbl(con, "LAP") |> filter(!is.na(cm21_bad_data_flag)) |> glimpse()
# tbl(con, "LAP") |> filter(cm21_sig_limit_flag == "pass")
#
# tbl(con, "META") |>
#   filter(is.na(cm21_dark_flag) & !is.na(cm21_basename))


#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
goodbye()
