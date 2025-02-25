# /* !/usr/bin/env Rscript */
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
#' Read *.stp and *.snc  files
#'
#' This also initializes dataset and meta data for tracker.
#'
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_chp1_tracker_DB.R"
Script.ID   <- "TR"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
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


#+ include=T, echo=F, results="asis"
cat("\n Initialize DB and import  Tracker steps files\n\n")

##  Open dataset  --------------------------------------------------------------
## eventually could move tracker table in the main duckdb for efficiency
con   <- dbConnect(duckdb(dbdir = DB_TRACKER))

if (!dbExistsTable(con, "TRACKER_Steps")) {
  dbExecute(con, "CREATE TABLE TRACKER_Steps (Date TIMESTAMP)")
  ##  Create non default columns
  make_new_column(con      = con,
                  table    = "TRACKER_Steps",
                  acolname = "Step_feq_Azim",
                  acoltype = "DECIMAL(18, 13)")
  make_new_column(con      = con,
                  table    = "TRACKER_Steps",
                  acolname = "Step_feq_Elev",
                  acoltype = "DECIMAL(18, 13)")
}

if (!dbExistsTable(con, "TRACKER_META_Steps")) {
  dbExecute(con, "CREATE TABLE TRACKER_META_Steps (Day DATE)")
}

if (!dbExistsTable(con, "TRACKER_Async")) {
  dbExecute(con, "CREATE TABLE TRACKER_Async (Date TIMESTAMP)")
}

if (!dbExistsTable(con, "TRACKER_META_Async")) {
  dbExecute(con, "CREATE TABLE TRACKER_META_Async (Day DATE)")
}



## STEP files import -----------------------------------------------------------

## _ Get Tracker steps files  --------------------------------------------------
inp_filelist <- list.files(path       = trSTEP_DIR,
                           pattern    = "sun_tracker_.*.stp",
                           recursive  = TRUE,
                           full.names = TRUE)
inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, basename := basename(fullname)]

inp_filelist$Day <- as.Date(
  strptime(
    sub("\\.stp", "",
        sub("sun_tracker_", "",
            inp_filelist$basename)),
    format = "%F"))

cat("\n**Found:", paste(nrow(inp_filelist), "Tracker steps files**\n"))

## add only files not in the metadata
if (dbExistsTable(con, "TRACKER_META_Steps") &
    any(dbListFields(con, "TRACKER_META_Steps") %in% "chp1_Steps_basename")) {
  inp_filelist <- anti_join(inp_filelist,
                            tbl(con, "TRACKER_META_Steps") |>
                              filter(!is.na(chp1_Steps_basename)) |>
                              select(Day) |>
                              collect(),
                            by = "Day") |>
    filter(!is.na(basename))
}

setorder(inp_filelist, Day)

## ignore current and future dates
inp_filelist <- inp_filelist[Day < as.Date(Sys.Date())]
cat("\n**Parse:",paste(nrow(inp_filelist), "Tracker steps files**\n\n"))


## _ Init db import data  ------------------------------------------------------
if (nrow(inp_filelist) > 0) {
  for (ll in 1:nrow(inp_filelist)) {
    ff <- inp_filelist[ll, ]

    cat(Script.ID, ": Step ",
        basename(ff$fullname),
        paste(ff$Day),
        ll, "/", nrow(inp_filelist))

    ## _ Read sync data file  ----------------------------------------------
    step_temp <- fread(ff$fullname, na.strings = "None")
    names(step_temp)[names(step_temp) == "V1"] <- "Date"
    names(step_temp)[names(step_temp) == "V2"] <- "Axis"
    names(step_temp)[names(step_temp) == "V3"] <- "Num"
    names(step_temp)[names(step_temp) == "V4"] <- "Step"
    names(step_temp)[names(step_temp) == "V5"] <- "Sun"
    names(step_temp)[names(step_temp) == "V6"] <- "StepsTaken"
    names(step_temp)[names(step_temp) == "V7"] <- "Tracker"
    cat(" r")

    ## reshape data
    step_temp$Num <- NULL
    step_temp[Axis == "a", Axis := "Azim"]
    step_temp[Axis == "z", Axis := "Elev"]
    dt_azim      <- step_temp[Axis == "Azim"]
    dt_elev      <- step_temp[Axis == "Elev"]
    dt_azim$Axis <- NULL
    dt_elev$Axis <- NULL
    dt_azim[, Step_feq := c(NA, diff(Date))]
    dt_elev[, Step_feq := c(NA, diff(Date))]

    wecare <- grep("Date|Taken", names(dt_azim),
                   value = TRUE, ignore.case = TRUE, invert = TRUE)

    wecare <- grep("Date", names(dt_azim),
                   value = TRUE, ignore.case = TRUE, invert = TRUE)

    for (av in wecare) {
      names(dt_azim)[names(dt_azim) == av] <- paste0(av, "_Azim")
      names(dt_elev)[names(dt_elev) == av] <- paste0(av, "_Elev")
    }

    step_temp <- merge(dt_azim, dt_elev, all = TRUE)
    # step_temp[, Tracker_event := "Step"]
    ## date matching observations
    step_temp[, Date30 := 30 + as.POSIXct((as.numeric(Date) %/% 60) * 60, origin = "1970-01-01")]
    cat(" p")

    ## _ Get metadata for steps file  --------------------------------------
    step_meta <- data.table(Day                 = step_temp[, unique(as.Date(Date))],
                            chp1_Steps_basename = basename(ff$fullname),
                            chp1_Steps_mtime    = file.mtime(ff$fullname),
                            chp1_Steps_parsed   = Sys.time(),
                            chp1_Steps_md5sum   = as.vector(md5sum(ff$fullname)))
    cat(" m")

    ## Add data and metadata
    {
      res <- insert_table(con      = con,
                          new_data = step_temp,
                          table    = "TRACKER_Steps",
                          matchvar = "Date",
                          quiet    = TRUE)

      res <- insert_table(con      = con,
                          new_data = step_meta,
                          table    = "TRACKER_META_Steps",
                          matchvar = "Day",
                          quiet    = TRUE)
    }

    cat(" w\n")
  }
}
# rm(step_meta, step_temp)



## ASYNC files import ----------------------------------------------------------

## _ Get tracker Async files  ---------------------------------------------------
inp_filelist <- list.files(path        = trSYNC_DIR,
                           recursive   = TRUE,
                           pattern     = "sun_tracker_.*.snc$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, chp1_Async_basename := basename(fullname)]

inp_filelist$Day <- as.Date(
  parse_date_time(
    sub("\\.snc", "",
        sub("sun_tracker_", "", inp_filelist$chp1_Async_basename)),
    "Ymd"))

cat("\n**Found:", paste(nrow(inp_filelist), "tracker sync files**\n"))

stopifnot(all(duplicated(sub("\\..*", "", inp_filelist$chp1_Async_basename))) == FALSE)

## add only files not in the metadata
if (dbExistsTable(con, "TRACKER_META_Async") &
    any(dbListFields(con, "TRACKER_META_Async") %in% "chp1_Async_basename")) {
  inp_filelist <- anti_join(inp_filelist,
                            tbl(con, "TRACKER_META_Async") |>
                              filter(!is.na(chp1_Async_basename)) |>
                              select(Day) |>
                              collect(),
                            by = "Day") |>
    filter(!is.na(basename))
}

setorder(inp_filelist, Day)

## ignore current and future dates
inp_filelist <- inp_filelist[Day < as.Date(Sys.Date())]
cat("\n**Parse:",paste(nrow(inp_filelist), "Tracker steps files**\n\n"))

stop("check steps")

## _ Import Async files  -------------------------------------------------------
if (nrow(inp_filelist) > 0) {
  for (ll in 1:nrow(inp_filelist)) {
    ss <- inp_filelist[ll, ]

    cat(Script.ID, ": Async ",
        basename(ss$fullname),
        paste(ss$Day),
        ll, "/", nrow(inp_filelist))

    ## _ Read tracker sync file  -------------------------------------------
    sync_temp    <- fread(ss$fullname, na.strings = "None")
    ## get dates from file
    sync_temp$V1 <- as.POSIXct(sync_temp$V1)

    names(sync_temp)[names(sync_temp) == "V1"] <- "Date"
    names(sync_temp)[names(sync_temp) == "V2"] <- "Axis"
    names(sync_temp)[names(sync_temp) == "V3"] <- "Num"
    names(sync_temp)[names(sync_temp) == "V4"] <- "Step_Should_1" ## can find specs
    names(sync_temp)[names(sync_temp) == "V5"] <- "Step_Should_2"
    names(sync_temp)[names(sync_temp) == "V6"] <- "Axis_step"
    names(sync_temp)[names(sync_temp) == "V7"] <- "Axis_freq"
    names(sync_temp)[names(sync_temp) == "V8"] <- "Tracker_freq"
    cat(" r")

    ## reshape data
    sync_temp$Num <- NULL
    sync_temp[Axis == "a", Axis := "Azim"]
    sync_temp[Axis == "z", Axis := "Elev"]
    dt_azim      <- sync_temp[Axis == "Azim"]
    dt_elev      <- sync_temp[Axis == "Elev"]
    dt_azim$Axis <- NULL
    dt_elev$Axis <- NULL

    wecare <- grep("Date|Tracker_freq|Step_diff", names(dt_azim),
                   value = TRUE, ignore.case = TRUE, invert = TRUE)

    for (av in wecare) {
      names(dt_azim)[names(dt_azim) == av] <- paste0(av, "_Azim")
      names(dt_elev)[names(dt_elev) == av] <- paste0(av, "_Elev")
    }

    sync_temp <- rbind(dt_azim, dt_elev, fill = TRUE)
    # sync_temp[, Tracker_event := "Async"]

    stop("date30")
    cat(" p")


    ## _ Get async file metadata  ----------------------------------------------
    sync_meta <- data.table(Day                 = sync_temp[, unique(as.Date(Date))],
                            chp1_Async_basename = basename(ss$fullname),
                            chp1_Async_mtime    = file.mtime(ss$fullname),
                            chp1_Async_parsed   = Sys.time(),
                            chp1_Async_md5sum   = as.vector(md5sum(ss$fullname)))

    ## Add data and metadata
    {
      res <- insert_table(con      = con,
                          new_data = sync_temp,
                          table    = "TRACKER_Async",
                          matchvar = "Date",
                          quiet    = TRUE)

      res <- insert_table(con      = con,
                          new_data = sync_meta,
                          table    = "TRACKER_META_Async",
                          matchvar = "Day",
                          quiet    = TRUE)

    }
    cat(" w\n")
  }
}



##  Inspect data  --------------------------------------------------------------
source("~/CODE/FUNCTIONS/R/data.R")

SS <- tbl(con, "TRACKER_Steps")


datesr <- SS |> summarise(
  stdate = min(Date, na.rm = TRUE),
  eddate = max(Date, na.rm = TRUE)
) |> collect()

stdate <- datesr$stdate - 120
eddate <- datesr$eddate + 120


stop()

broad  <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))
tbl(broad, "LAP") |>
  filter(Date > stdate & Date < eddate) |>
  select(Date, Elevat, Azimuth)

BB <- data.table(open_dataset(DB_DIR)                      |>
                   filter(Date > stdate & Date < eddate) |>
                   select(Date, Elevat, Azimuth)         |> collect())


## Check minutes and steps taken

SS <- SS |> filter(Tracker_event== "Step")

SS |> mutate(
  Date30 := 30 + as.POSIXct((as.numeric(Date) %/% 60) * 60)
)

ST[, Date30 := 30 + as.POSIXct((as.numeric(Date) %/% 60) * 60, origin = "1970-01-01") ]


steps <- ST[, .(NStepAzim = sum(is.numeric(StepsTaken_Azim), na.rm = TRUE),
                NStepElev = sum(is.numeric(StepsTaken_Elev), na.rm = TRUE),
                .N), by = Date30 ]

ST[ is.numeric(StepsTaken_Azim) , .N, by = Date30 ]


table(steps$NStepAzim)
table(steps$NStepElev)
table(steps$N)

table(steps)

gc()


hist(SS[, Sun_Azim - Tracker_Azim ])
# plot(ST[, Sun_Azim - Tracker_Azim, Date ])

hist(SS[, Sun_Elev - Tracker_Elev ])
# plot(ST[, Sun_Elev - Tracker_Elev, Date ])

hist(SS[, Step_feq_Elev ], breaks = 100)
hist(SS[, Step_feq_Azim ], breaks = 100)

hist(SS[, Tracker_freq], breaks = 100)

hist(SS[, Axis_step_Azim], breaks = 100)
hist(SS[, Axis_step_Elev], breaks = 100)
hist(SS[, Axis_freq_Elev], breaks = 100)
hist(SS[, Axis_freq_Azim], breaks = 100)

hist(SS[, Tracker_freq], breaks = 100)


hist(SS[, Step_Should_Azim],   breaks = 100)
hist(SS[, Step_Response_Azim], breaks = 100)
hist(SS[, Step_Should_Elev],   breaks = 100)
hist(SS[, Step_Response_Elev], breaks = 100)

hist(SS[, Step_Should_Azim - Step_Response_Azim],   breaks = 100)
hist(SS[, Step_Should_Elev - Step_Response_Elev],   breaks = 100)


ASBB <- merge(SS, BB, by = "Date", all = TRUE)

# TEST
ASBB <- ASBB[ year(Date) == 2023, ]
ASBB$sync <- TRUE

ASBB[                , Step_Diff := Step_Should_Azim - Step_Response_Azim]
ASBB[is.na(Step_Diff), Step_Diff := Step_Should_Elev - Step_Response_Elev]

ASBB[                , Freq_Axis := Axis_freq_Elev]
ASBB[is.na(Freq_Axis), Freq_Axis := Axis_freq_Azim]


days <- unique(ASBB[!is.na(Tracker_event), as.Date(Date)])



for (ad in days) {
  temp <- ASBB[as.Date(Date) == ad]


  freq <- temp[!is.na(Freq_Axis) & Freq_Axis > 0 , Freq_Axis, Date ]
  freq[, SDate := Date - Freq_Axis]

  for (ii in nrow(freq)){
    ASBB[ Date <= freq[ii]$Date & Date >= freq[ii]$SDate, sync := FALSE]
  }



  stop()
}

hist(ASBB$Tracker_freq)
hist(ASBB$Freq_Axis)

test <- ASBB[ Freq_Axis > Tracker_freq]
test <- rm.cols.NA.DT(test)


ASBB <- ASBB[Tracker_event == "Step"]
ASBB <- rm.cols.NA.DT(ASBB)


table(ASBB$sync)

## ASYNC
## - No step data for the minute
## - Go back from async cases and chech data

## Tracker_freq: time of each loop in tracker
## Axis_freq:    time of each movement

stop()

## Find async cases to really remove!

ss <- data.table(open_dataset(DB_Steps_DIR) |> filter(year == 2023) |> collect())


ss[Tracker_event == "Async", as.Date(Date)]

test <- ss[as.Date(Date)=="2020-03-19"]

test <- ss[year(Date)==2023]


hist(test[,Sun_Azim - Tracker_Azim])
plot(test[,Sun_Azim - Tracker_Azim, Date])
plot(test[,Sun_Azim - Tracker_Azim, Sun_Elev])

hist(test[,Sun_Elev - Tracker_Elev])
plot(test[,Sun_Elev - Tracker_Elev, Date])
plot(test[,Sun_Elev - Tracker_Elev, Sun_Elev])

hist(test[,Step_Should_Azim - Step_Response_Azim], breaks = 100)
hist(test[,Step_Should_Elev - Step_Response_Elev], breaks = 100)

plot(test[,Step_Should_Azim - Step_Response_Azim, Date])
plot(test[,Step_Should_Elev - Step_Response_Elev, Date])

plot(test[, Step_Azim, Date])
plot(test[, Step_Elev, Date])

# plot(test[, StepsTaken, Date])

plot(test[, Axis_freq_Azim, Date])
plot(test[, Axis_freq_Elev, Date])
plot(test[, Tracker_freq, Date])





#+ results="asis", echo=FALSE
goodbye()
