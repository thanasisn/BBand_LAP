#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Daily CHP-1 radiation data **L1** "
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
#'  **DHI L1 PLOT**
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
knitr::opts_chunk$set(fig.pos   = '!h'    )

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_duckdb/30_Plot_daily_CHP1_L1.R"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/duck/", basename(sub("\\.R$", ".pdf", Script.Name))))
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

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )


## __  Variables  --------------------------------------------------------------
OUT_FOLDER <- "~/BBand_LAP/REPORTS/DAILY/CHP1_DIR_L1/"
OUT_PREFIX <- "CHP1_direct_L1_"
dir.create(OUT_FOLDER, showWarnings = FALSE, recursive = TRUE)
tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))
cex <- 0.6

## __ Open database  -----------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_DUCK, read_only = TRUE))
DT  <- tbl(con, "LAP")
MT  <- tbl(con, "META")

## __ Execution control  -------------------------------------------------------
TEST <- TRUE
TEST <- FALSE

metalist <- MT |>
  mutate(year               = year(Day),
         chp1_dark_computed = as.Date(chp1_dark_computed)) |>
  filter(!is.na(chp1_basename))                            |>
  select(chp1_dark_computed, year)                         |>
  group_by(year)                                           |>
  summarise(updated = max(chp1_dark_computed, na.rm = T))  |>
  collect() |> data.table()


plotfiles <- data.table(path = list.files(path        = OUT_FOLDER,
                                          pattern     = OUT_PREFIX,
                                          full.names  = TRUE,
                                          ignore.case = TRUE))
plotfiles$mtime <- file.mtime(plotfiles$path)
plotfiles$year  <- as.numeric(
    sub(OUT_PREFIX, "", sub("\\.pdf", "", basename(plotfiles$path))))

## find year to do
selected    <- merge(metalist, plotfiles, all = TRUE)
years_to_do <- selected[is.na(path) | updated > mtime, year ]


## TEST
if (TEST) {
  years_to_do <- 2016
}


for (YYYY in sort(years_to_do)) {
  ## load data for year
  year_data <- DT |> filter(year == YYYY) |> collect() |> data.table()
  cat(YYYY, "rows:", nrow(year_data), "\n")

  ## days with data
  daystodo <- year_data[!is.na(DIR_SD_wpsm), unique(as.Date(Date))]
  daystodo <- sort(daystodo)
  ## signal limit for all year
  # ylim <- range(year_data[, .(CHP1_sig, CHP1_sig_wo_dark)], na.rm = TRUE)

  if (!interactive()) {
    pdffile <- paste0(OUT_FOLDER, "/", OUT_PREFIX, YYYY, ".pdf")
    cat("Ploting:", pdffile, "\n")
    pdf(file = pdffile)
  }

  for (aday in sort(daystodo)) {
    dd   <- year_data[as.Date(Date) == aday]
    setorder(dd, Date)
    aday <- as.Date(aday, origin = "1970-01-01")

    layout(matrix(c(1,2,2,2,2), 5, 1, byrow = TRUE))

    ## Direct SD
    par("mar" = c(0, 4.5, 2, 1))
    plot(
      dd$Date, dd$DIR_SD_wpsm,
      ylim = range(c(0, dd$DIR_SD_wpsm), na.rm = T),
      pch  = 19,
      cex  = cex,
      col = "red",
      xaxt = "n", xlab = "",
      ylab = expression(paste("SD [", Watt/m^2, "]"))
    )
    abline(h = 0, col = "grey", lty = 2)

    title(paste0("Direct Irradiance  doy: ", yday(aday), "  ", aday))

    ## Radiation
    par("mar" = c(3, 4.5, 0, 1))
    ylim <- range(dd[, .(DIR_wpsm, HOR_strict)], na.rm = TRUE)
    plot(
      dd$Date, dd$HOR_strict, type = "l",
      ylim = ylim,
      lwd  = 1.5,
      pch  = 19,
      cex  = cex,
      col  = "cyan",
      ylab = expression(paste("Direct irradiance [", Watt/m^2, "]"))
    )
    lines(dd$Date, dd$DIR_wpsm,
          col = "blue",)
    abline(h = 0, col = "grey", lty = 2)

    ## Temperature correction
    if (!all(is.na(dd$DIR_wpsm_temp_cor))) {
      par(new = T)
      plot(dd$Date, dd$DIR_wpsm_temp_cor, type = "l",
           xaxt = "n", yaxt = "n",
           lty = 3, col = "darkgrey")
    }

    ## Decorations
    text(dd$Date[1], max(dd[, .(DIR_wpsm, HOR_strict)], na.rm = TRUE),
         labels = tag, pos = 4, cex = .9)

    legend("topright", bty = "n",
           lty = c( 1,  1, NA,  3),
           pch = c(NA, NA, 19, NA),
           legend = c(
             "Direct on horizontal",
             "Direct beam",
             "Signal SD",
             "Direct beam temp. cor."),
           col = c("cyan",
                   "blue",
                   "red",
                   "darkgrey")
    )
  }
  dev.off()
}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

