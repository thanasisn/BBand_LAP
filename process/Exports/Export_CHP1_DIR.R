# /* #!/opt/R/4.2.3/bin/Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "CHP-1 export DNI data for Sirena."
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
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
#'     fig_height: 5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---
#+ include=F

#'
#' **DNI export**
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
Script.Name <- "~/BBand_LAP/process/Exports/Export_CHP1_DIR.R"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)


## data export folder
DATOUT     <- "~/DATA/CHP1_LAP.DIR/"
OTHEREXPOR <- "~/ZHOST/"

MIN_YEAR  <- 2016
MAX_YEAR  <- 2024
yearstodo <- MIN_YEAR:MAX_YEAR

tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y"))

#+ include=TRUE, echo=T, results="asis", messages=F
##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_BROAD))
sun <- dbConnect(duckdb(dbdir = DB_LAP, read_only = TRUE))

LAP <- tbl(con, "LAP")
SUN <- tbl(sun, "params")


for (YYYY in yearstodo) {

  pdf(file = paste0(DATOUT, "/", "CurrentDataPlots_", YYYY, ".pdf"))

  year_data <- LAP         |>
    filter(year == YYYY)   |>
    select(
      Date,
      # SZA,
      Day,
      lap_sza,
      DIR_wpsm,
      DIR_SD_wpsm,
      Async_tracker_flag,
      cm21_sig_limit_flag,
      cm21_bad_data_flag
    )                      |>
    mutate(
      ## burn bad radiation data
      DIR_wpsm := if_else(
        Async_tracker_flag == TRUE  |
          !cm21_sig_limit_flag %in% c("pass", "empty") |
          !cm21_bad_data_flag  %in% c("pass", "empty")  ,
        true    = NA,
        false   = DIR_wpsm,
        missing = NA
      ),
      ## burn bad radiation SD data
      DIR_SD_wpsm := if_else(
        Async_tracker_flag == TRUE  |
          !cm21_sig_limit_flag %in% c("pass", "empty") |
          !cm21_bad_data_flag  %in% c("pass", "empty")  ,
        true    = NA,
        false   = DIR_SD_wpsm,
        missing = NA
      )
    )             |>
    arrange(Date) |>
    collect()     |>
    data.table()

  # year_data[Async_tracker_flag == T]
  # year_data[!cm21_sig_limit_flag %in% c("pass", "empty")]
  # year_data[!cm21_bad_data_flag %in% c("pass", "empty")]

  year_data$Async_tracker_flag  <- NULL
  year_data$cm21_sig_limit_flag <- NULL
  year_data$cm21_bad_data_flag  <- NULL

  ##  Simple export data for others  -------------------------------------------
  # write_dat(object  = year_data,
  #           file    = paste0(OTHEREXPOR, "/", "CHP1_DIR_", YYYY),
  #           contact = "<natsisphysicist@gmail.com>")

  ## keep only days with data
  test      <- year_data[, all(is.na(DIR_wpsm)), by = Day]
  year_data <- year_data[!Day %in% test[V1 == T, Day]]

  ## fill missing SZA form SUN
  miszsa <- year_data[is.na(lap_sza), Date]
  LSZA <- SUN |>
    filter(Date %in% miszsa)    |>
    select(Date, LAP_SZA_start) |>
    rename(lap_sza = LAP_SZA_start) |>
    collect() |>
    data.table()
  year_data <- rows_patch(year_data, LSZA)

  ## all SZA are here
  stopifnot(year_data[is.na(lap_sza), .N] == 0)

  ## all minutes are here
  stopifnot(all(year_data[, .N, by = Day]$N == 1440))

  ## prepare data to export
  year_data$DIR_wpsm[   is.na(year_data$DIR_wpsm)   ] <- -9L
  year_data$DIR_SD_wpsm[is.na(year_data$DIR_SD_wpsm)] <- -9L

  ## create receiving folder
  yearfolder <- paste0(DATOUT, "/", YYYY)
  dir.create(yearfolder, showWarnings = F)

  ## Export each day ---------------------------------------------------------
  alldays <- sort(unique(year_data$Day))

  for (dd in alldays) {
    dateD <- as.Date(dd, origin = "1970-01-01")

    if (interactive()) cat(sprintf("%s   \r", dateD))
    status_msg(ScriptName = Script.Name,
               msg = c(format(dateD)))

    dayselec <- year_data$Day == dateD
    if (sum(dayselec) != 1440 ) { stop("Day don't have 1440 minutes!!") }

    filename <- paste0(DATOUT, "/", strftime(dateD, format = "%Y/DIR%3j%y.DAT"))
    oneday   <- year_data[dayselec, ]
    TIME_UT  <- as.numeric((oneday$Date - as.POSIXct( dateD ) + 30) / 3600)
    doy      <- yday(dateD)

    output <- data.frame(TIME_UT = round(TIME_UT,            digits = 4),
                         SZA     = round(oneday$lap_sza,      digits = 2),
                         Wm2     = round(oneday$DIR_wpsm,    digits = 3),
                         st.dev  = round(oneday$DIR_SD_wpsm, digits = 3))

    ## custom file header
    cat(" TIME_UT    SZA    [W.m-2]   st.dev\r\n",
        file = filename)
    ## format and write data
    write.table(format(output,
                       digits    = 3,
                       width     = 8,
                       row.names = FALSE,
                       scietific = FALSE,
                       nsmall    = 2),
                file      = filename,
                append    = TRUE,
                quote     = FALSE,
                col.names = FALSE,
                row.names = FALSE,
                eol = "\r\n")

    ## make sure has consistent line endings
    # system(paste("unix2dos ", filename),
    #        ignore.stdout = TRUE)

    ## create a plot of the output data
    par(mar = c(3, 3, 1, 1))
    par(mgp = c(1.5, .5, 0))

    ## set NAs for plotting
    output[output <= -9] <- NA

    if (!all(is.na(output$Wm2))) {

      plot(output$TIME_UT, output$Wm2, "l",
           lwd  = 1.5, col = "green",
           ylab = expression(Watt/m^2),
           xlab = "Hour (UTC)")
      title(main = paste(dateD), cex.main = .8)

      points( output$TIME_UT, output$st.dev,
              pch = 18, cex = .4, col = "blue")

      legend("topright", bty = "n",
             legend = c("Direct Normal Irradiance",
                        "Standard deviation")     ,
             lty = c( 1, NA),
             pch = c(NA, 18),
             col = c("green", "blue"),  cex = 0.8)
      legend("topleft", tag, bty = "n", cex = 0.8)

      ## by sza
      plot(output$SZA, output$Wm2, "l",
           lwd  = 1.5, col = "green",
           ylab = expression(Watt/m^2),
           xlab = "SZA")
      title(main = paste(dateD), cex.main = .8)

      points(output$SZA, output$st.dev,
             pch = 18, cex = .4 , col = "blue")

      legend("topright", bty = "n",
             legend = c("Direct Normal Irradiance",
                        "Standard deviation")     ,
             lty = c( 1, NA),
             pch = c(NA, 18),
             col = c("green", "blue"),  cex = 0.8)
      legend("topleft", tag, bty = "n", cex = 0.8)

    }## if data
    rm(output)
  }## alldays
  dev.off()
}

#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)
dbDisconnect(sun, shutdown = TRUE); rm(sun)

#+ results="asis", echo=FALSE
goodbye()
