#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
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
#'
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
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
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)

cat("\n Import  CHP-1 temperature data\n\n")

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_DUCK))

##  Get tracker sync files  --------------------------------------------------------
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

## keep only files which correspond to existing dates
inp_filelist <- right_join(inp_filelist,
                           tbl(con, "LAP") |>
                             select(Day)   |>
                             distinct()    |>
                             collect(),
                           by = "Day") |>
  filter(!is.na(chp1_temp_basename))






## ignore current and future dates
inp_filelist <- inp_filelist[ day < as.Date(Sys.Date())]
cat("\n**Parse:", nrow(inp_filelist), "CHP-1 temperature files**\n\n")

## parse all files
if (nrow(inp_filelist) > 0) {
  for (ll in 1:nrow(inp_filelist)) {
    ff <- inp_filelist[ll, ]

    cat(Script.ID, ": ",
        basename(ff$fullname),
        paste(ff$Day),
        ll,"/",nrow(inp_filelist), "\n")


    stop()
    async    <- rep(FALSE, 1440)  # The snc file exist, so start with all not async
    asyncstp <- rep(NA,    1440)  # Async magnitude (steps missed)

    ## Recreate time stamp for all minutes of day starting from zero!!!
    D_minutes <- seq(from       = as.POSIXct(paste(as_date(ff$Day), "00:00:00 UTC")),
                     length.out = 1440,
                     by         = "min")

    ## __  Read tracker sync file  -------------------------------------
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
      update_table(con      = con,
                   new_data = file_meta,
                   table    = "META",
                   matchvar = "Day")

    }
  }
} else {
  cat(Script.ID, ": ", "No new files to add\n\n")
}











stop()

##  Import CHP-1 files  ------------------------------------------------------
for (YYYY in unique(year(inp_filelist$day))) {
    subyear <- inp_filelist[year(day) == YYYY]
    ## months to do
    for (mm in subyear[, unique(month(day))]) {

        ##  read this month set files
        gathermeta <- data.table()
        for (ad in submonth$day) {
            ss <- submonth[day == ad]

            ## __  Read CHP-1 temperature file  --------------------------------
            temp_temp    <- read.table(ss$fullname, sep = "\t", as.is = TRUE)
            temp_temp$V1 <- as.POSIXct( temp_temp$V1 )
            temp_temp$V1 <- as.POSIXct( format( temp_temp$V1, format = "%F %R" ) )
            temp_temp$V1 <- temp_temp$V1 + 30
            temp_temp$V3[temp_temp$V3 == 0] <- NA

            temp_temp    <- data.table(temp_temp)
            temp_temp    <- temp_temp[, .( V2 = mean(V2, na.rm = T),
                                           V3 = mean(V3, na.rm = T) ), by = V1 ]

            temp_temp[V2 < 0, V2 := NA]
            temp_temp[V2 < 0, V3 := NA]

            day_data <- data.frame(Date                = temp_temp$V1,
                                   year                = year(temp_temp$V1),
                                   month               = month(temp_temp$V1),
                                   chp1_R_therm        = temp_temp$V2,
                                   chp1_R_SD_therm     = temp_temp$V3,
                                   chp1_R_meas_ERR     = Protek_506_R_error(    temp_temp$V2),
                                   chp1_temperature    = CHP_thermistor_R_to_T( temp_temp$V2),
                                   chp1_temperature_SD = CHP_thermistor_ResUnc_to_TempUnc(temp_temp$V2, temp_temp$V3))
            day_data$chp1_temp_UNC <- CHP_thermistor_ResUnc_to_TempUnc(temp_temp$V2, day_data$chp1_R_meas_ERR)

            ## get file metadata
            file_meta <- data.table(day                = as_date(ad),
                                    chp1_temp_basename = basename(ss$fullname),
                                    chp1_temp_mtime    = file.mtime(ss$fullname),
                                    chp1_temp_parsed   = Sys.time(),
                                    chp1_temp_md5sum   = as.vector(md5sum(ss$fullname)))

            # gather <- rows_patch(gather, day_data, by = "Date")
            gather     <- rows_upsert(gather, day_data, by = "Date")
            gathermeta <- rbind(gathermeta, file_meta)
            rm(day_data, file_meta, ss)
            rm(temp_temp)
        }

        BB_meta <- rows_update(BB_meta, gathermeta, by = "day")
        # BBdaily <- rows_patch(BBdaily, gathermeta, by = "day", unmatched = "ignore")

        setorder(gather, Date)

        ## store this month / set data
        write_parquet(gather,  partfile)
        cat("05 Save: ", partfile, "\n")
        write_parquet(BB_meta, DB_META_fl)
        rm(gather, gathermeta, submonth)
    }
    rm(subyear)
}
rm(inp_filelist)




myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

