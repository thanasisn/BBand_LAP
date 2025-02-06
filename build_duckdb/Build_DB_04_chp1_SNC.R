#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Reads tracker synchronization data form `sun_tracker_.*.snc$`
#'
#' Populates:
#'  - Async_tracker_flag
#'
#' There are more data but are parsed here
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_04_chp1_SNC.R"
Script.ID   <- "04"

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

cat("\n Import  CHP-1 tracker ASYNC data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_BROAD))

##  Get tracker sync files  ----------------------------------------------------
inp_filelist <- list.files(path        = trSYNC_DIR,
                           recursive   = TRUE,
                           pattern     = "sun_tracker_.*.snc$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:", length(inp_filelist), "tracker sync files**\n")

inp_filelist <- data.table(fullname = inp_filelist)
inp_filelist[, chp1_sync_basename := basename(fullname)]
stopifnot(all(duplicated(sub("\\..*", "", inp_filelist$chp1_sync_basename))) == FALSE)

inp_filelist$Day <- as.Date(
  sub("sun_tracker_", "",
      sub("\\.snc", "",
          inp_filelist$chp1_sync_basename
          )
      )
  )
setorder(inp_filelist, Day)
cat("\n**Found:", nrow(inp_filelist), "tracker sync files**\n")

## check files have unique days
stopifnot(!any(duplicated(inp_filelist$Day)))

## keep only files which correspond to existing dates
inp_filelist <- right_join(inp_filelist,
                           tbl(con, "LAP") |>
                             select(Day)   |>
                             distinct()    |>
                             collect(),
                           by = "Day") |>
  filter(!is.na(chp1_sync_basename))

## add only files not in the metadata
if (dbExistsTable(con, "META") &
    any(dbListFields(con, "META") %in% "chp1_sync_basename")) {
  inp_filelist <- anti_join(inp_filelist,
                            tbl(con, "META") |>
                              filter(!is.na(chp1_sync_basename)) |>
                              select(Day) |>
                              collect(),
                            by = "Day") |>
    filter(!is.na(chp1_sync_basename))
}
setorder(inp_filelist, Day)

## ignore current and future dates
inp_filelist <- inp_filelist[Day < as.Date(Sys.Date())]
cat("\n**Parse:", nrow(inp_filelist), "tracker sync files**\n\n")

##  Import tracker sync  --------------------------------------------------------
if (nrow(inp_filelist) > 0) {
  for (ll in 1:nrow(inp_filelist)) {
    ff <- inp_filelist[ll, ]

    cat(Script.ID, ": ",
        basename(ff$fullname),
        paste(ff$Day),
        ll,"/",nrow(inp_filelist))

    async    <- rep(FALSE, 1440)  # The snc file exist, so start with all not async
    asyncstp <- rep(NA,    1440)  # Async magnitude (steps missed)

    ## __ Check data base is ready for import  ---------------------------------
    if (tbl(con, "LAP") |> filter(Day == ff$Day) |> tally() |> pull() != 1440) {
      cat("Data base not ready to import", paste(ff$Day), "\n")
      next()
    }

    ## Recreate time stamp for all minutes of day starting from zero!!!
    D_minutes <- seq(from       = as.POSIXct(paste(as_date(ff$Day), "00:00:00 UTC")),
                     length.out = 1440,
                     by         = "min")

    ## __  Read tracker sync file  ---------------------------------------------
    ## TODO we should use step files as more reliable to detect async events!!!
    syc_temp    <- read.table(ff$fullname, sep = "\t", as.is = TRUE, na.strings = "None")
    ## get dates from file
    syc_temp$V1 <- as.POSIXct(syc_temp$V1)
    ## round to start of each minute
    async_minu  <- as.POSIXct(format(syc_temp$V1,format = "%F %R"))  ## async end
    ## get minutes with async
    uniq_async  <- unique(async_minu)
    ## async time distance
    syc_temp$timeDist <- apply(syc_temp[, c('V7', 'V8')], MARGIN = 1, FUN = max, na.rm = T)
    cat(" r")

    for (amin in uniq_async) {
      min_ind <- async_minu == amin
      stepgo  <- syc_temp$V4[min_ind]
      stepis  <- syc_temp$V5[min_ind]
      stepout <- suppressWarnings(max(abs( stepgo - stepis ), na.rm = TRUE))
      if (is.finite(stepout)) {
        # Async magnitude (count steps missed)
        asyncstp[ which(D_minutes == amin) ] <- stepout
      }
    }

    ## set async from time back
    syc_temp$async_start <- syc_temp$V1 - syc_temp$timeDist
    syc_temp$async_start <- as.POSIXct(format(syc_temp$async_start, format = "%F %R"))
    syc_temp$async_end   <- as.POSIXct(format(syc_temp$V1,          format = "%F %R"))
    ## create vector of asyncs
    for (ik in 1:nrow(syc_temp)) {
      async[ which( D_minutes <= syc_temp$async_end[   ik ] &
                      D_minutes >= syc_temp$async_start[ ik ]  ) ] <- TRUE   ## !!
    }

    ## Move dates to the centre of each minute, as the rest of DB
    D_minutes <- D_minutes + 30

    day_data <- data.frame(Date               = D_minutes,
                           Async_tracker_flag = async,
                           Async_step_count   = asyncstp)

    ## get file metadata
    file_meta <- data.table(Day                = as_date(ff$Day),
                            chp1_sync_basename = basename(ff$fullname),
                            chp1_sync_mtime    = file.mtime(ff$fullname),
                            chp1_sync_parsed   = Sys.time(),
                            chp1_sync_md5sum   = as.vector(md5sum(ff$fullname)))
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

  fs::file_size(DB_BROAD)

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
