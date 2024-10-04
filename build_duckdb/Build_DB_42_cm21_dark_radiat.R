#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Compute or construct dark signal offset for CM-21.
#'
#' Fills:
#' - `CM21_sig_wo_dark` when it's appropriate
#'
#' On the second run will replace 'MISSING' dark with 'CONSTRUCTED' dark.
#'
#' TODO
#' - print dark type on graphs from metadata
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_42_cm21_dark_radiat.R"
Script.ID   <- "42"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_dark_calculation.R")
source("~/BBand_LAP/functions/Functions_CM21.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

# library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_DUCK))

##  Create a dummy column if not existing
make_new_column(con, "META", "cm21_dark_flag", "character")

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
    DARK = cm21DAILYdark(missingdays)
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
for (ad in dayslist) {
  ad <- as.Date(ad, origin = origin)
  cc <- cc + 1
  cat(Script.ID, ":", paste(ad), cc, "/",length(dayslist), "\n")

  ## use only valid data for dark calculation
  daydata <-
    tbl(con, "LAP")                   |>
    filter(Day == ad)                 |>  ## this day only
    filter(!is.na(CM21_sig))          |>  ## valid measurements
    filter(is.na(cm21_bad_data_flag)) |>  ## not bad data
    filter(cm21_sig_limit_flag == 0)  |>  ## acceptable values range
    collect() |>
    data.table()

  ## Ignore bad and missing data
  if (nrow(daydata) == 0) {
    cat("     No usefull CM-21 data in this file\n\n")
    next()
  }
  if (any(is.na(daydata$Elevat))) {
    cat("     The day is not initialized:", paste(ad),"\n")
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

  ## __ Resolve problematic dark calculations  ---------------------------------
  ## no data to use
  if (all(is.na(daydata$CM21_sig))) {
    dark_flag              <- "NO SIGNAL DATA"
    todays_dark_correction <- as.numeric(NA)
    missingdark            <- as.numeric(NA)
    ## data to compute dark are missing
  } else if ( !((!is.na(dark_day$dark_Mor_med) & dark_day$dark_Mor_cnt >= DCOUNTLIM) |
                (!is.na(dark_day$dark_Eve_med) & dark_day$dark_Eve_cnt >= DCOUNTLIM))) {
    todays_dark_correction <- as.numeric(NA)
    dark_flag              <- "MISSING"
    missingdark            <- as.numeric(NA)

    ## get dark from pre-computed file
    if (exists("construct")) {
      ## can not find date
      if (!aday %in% construct$Date) {
        todays_dark_correction <- as.numeric(NA)
        dark_flag              <- "MISSING"
        missingdark            <- as.numeric(NA)
      } else {
        ## get data from recomputed dark database
        todays_dark_correction <- construct[Date == aday, DARK]
        dark_flag              <- "CONSTRUCTED"
      }
    } else {
      cat("Need to constract dark:", format(as.Date(aday)),"\n")
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

  ## __ Day stats  -------------------------------------------------------------
  names(dark_day) <- paste0("cm21_", names(dark_day))
  meta_day <- data.frame(Day                = as.Date(ad),
                         cm21_Daily_dark    = mean(todays_dark_correction, na.rm = T),
                         cm21_dark_flag     = dark_flag,
                         dark_day,
                         cm21_dark_computed = Sys.time()
  )

  ## __  Store data in the database  -------------------------------------------
  update_table(con      = con,
               new_data = daydata,
               table    = "LAP",
               matchvar = "Date")
  update_table(con      = con,
               new_data = meta_day,
               table    = "META",
               matchvar = "Day")

}

tbl(con, "META") |> group_by(cm21_dark_flag) |> tally()



# tbl(con, "META") |> glimpse()
# tbl(con, "LAP")  |> glimpse()
#
# tbl(con, "LAP") |> filter(!is.na(cm21_bad_data_flag)) |> glimpse()
# tbl(con, "LAP") |> filter(cm21_sig_limit_flag == 0)
#
# tbl(con, "META") |>
#   filter(is.na(cm21_dark_flag) & !is.na(cm21_basename))


## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
