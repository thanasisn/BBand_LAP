#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Daily raw CHP-1 data **SIG** "
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
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        4
#'     fig_width:        8
#'     fig_height:       5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#'
#'  **CHP-1 SIG PLOT**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
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
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_duckdb/10_Plot_daily_CHP1_sig.R"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/duck/", basename(sub("\\.R$", ".pdf", Script.Name))))
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
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

## __  Variables  --------------------------------------------------------------
OUT_FOLDER <- "~/BBand_LAP/REPORTS/DAILY/CHP1_signal/"
OUT_PREFIX <- "CHP1_signal_"
dir.create(OUT_FOLDER, showWarnings = FALSE, recursive = TRUE)
tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))

## __ Execution control  -------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))

## when dark was computed for each year
metalist <- tbl(con, "META")        |>
  filter(!is.na(chp1_basename))     |>
  select("Day", chp1_dark_computed) |>
  collect() |> data.table()

metalist[, year := year(Day)]
metalist <- metalist[, .(updated = max(chp1_dark_computed, na.rm = TRUE)), by = year ]

## when each plot file was produced
plotfiles <- data.table(path = list.files(path    = OUT_FOLDER,
                                          pattern = OUT_PREFIX,
                                          full.names  = TRUE,
                                          ignore.case = TRUE))
plotfiles[, mtime := file.mtime(path)]
plotfiles[, year  := as.numeric(sub(OUT_PREFIX, "",
                                    sub("\\.pdf", "",
                                        basename(path))))]

## find what needs update
selected    <- merge(metalist, plotfiles, all = TRUE)
years_to_do <- selected[is.na(path) | updated > mtime, year ]

# TEST
# years_to_do <- 2022

for (YYYY in sort(years_to_do)) {
  ## load data for year
  META <- tbl(con, "META") |>
    filter(year(Day) == YYYY) |>
    collect() |> data.table()

  DT <- tbl(con, "LAP") |>
    filter(year == YYYY)
  year_data <- DT |>
    select(Date, Elevat,
           CHP1_sig,
           CHP1_sig_sd,
           chp1_bad_data_flag,
           Async_tracker_flag,
           CHP1_sig_wo_dark) |>
    collect() |> data.table()

  cat(YYYY, "rows:", nrow(year_data), "\n")
  ## days with data
  daystodo <- year_data[!is.na(CHP1_sig), unique(as.Date(Date))]
  daystodo <- sort(daystodo)

  if (!interactive()) {
    pdffile <- paste0(OUT_FOLDER, "/", OUT_PREFIX, YYYY, ".pdf")
    cat("Ploting:", pdffile, "\n")
    pdf(file = pdffile)
  }

  for (aday in sort(daystodo)) {
    dd   <- year_data[as.Date(Date) == aday]
    aday <- as.Date(aday, origin = "1970-01-01")
    setorder(dd, Date)
    status_msg(ScriptName = Script.Name, msg = c(YYYY, paste(aday), length(daystodo)))

    layout(matrix(c(1,2,3,3,3,3), 6, 1, byrow = TRUE))

    ## Night signal
    par("mar" = c(0,4,2,1))

    if  (all(is.na(dd[Elevat < 0, CHP1_sig]))) {
      plot.new()
    } else {
      plot(dd[Elevat < 0, Date], dd[Elevat < 0, CHP1_sig],
           ylim = range(dd[Elevat < 0, .(CHP1_sig, CHP1_sig_wo_dark)], na.rm = TRUE),
           pch = 19,  cex = 0.5, col = "cyan",
           xaxt = "n",
           xlab = "", ylab = "Night [V]")
      points(dd[Elevat < 0, Date], dd[Elevat < 0, CHP1_sig_wo_dark],
             pch = 19,  cex = 0.5, col = "blue")
      abline(h = 0, col = "grey")
    }

    title(paste0("CHP-1  doy: ", yday(aday), "  ",
                 aday, "  dark ",
                 tolower(META[Day == aday, chp1_dark_flag])))

    ## Signal SD
    par("mar" = c(0,4,0,1))
    plot(dd$Date, dd$CHP1_sig_sd,
         ylim = range(c(0, dd$CHP1_sig_sd), na.rm = T),
         pch  = 19,  cex = 0.5, col = "red",
         xaxt = "n", xlab = "", ylab = "Signal SD [V]")
    abline(h = 0, col = "grey", lty = 2)

    ## Signal
    par("mar" = c(3,4,0,1))
    plot(dd$Date, dd$CHP1_sig, type = "l",
         ylim = range(dd[, .(CHP1_sig, CHP1_sig_wo_dark)], na.rm = TRUE),
         lwd  = 1.5,
         pch  = 19,  cex = 0.5, col = "cyan",
         xlab = "", ylab = "Signal [V]")
    lines(dd$Date, dd$CHP1_sig_wo_dark,
          col = "blue",)
    abline(h = 0, col = "grey", lty = 2)

    ## Plot bad data
    points(dd[Async_tracker_flag == TRUE, CHP1_sig, Date],
           col = "magenta", cex = 1.2)

    points(dd[!chp1_bad_data_flag %in% c("empty", "pass"),
              CHP1_sig,
              Date],
           col = "black", cex = 1.2, pch = 0)

    ## Decorations
    text(dd$Date[1], max(dd$CHP1_sig, dd$CHP1_sig_wo_dark, na.rm = TRUE),
         labels = tag, pos = 4, cex = 0.9)

    legend("topright", pch = 19, bty = "n",
           legend = c(
             "Signal",
             "Signal dark corrected",
             "Signal SD",
             "Tracker Async",
             "Excluded bad data"),
           col = c("cyan",
                   "blue",
                   "red",
                   "magenta",
                   "black")
    )
  }
  dev.off()
}

## TODO add temperature?

## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

