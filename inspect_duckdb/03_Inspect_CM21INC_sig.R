# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Inspect raw CM-21 INCLINED data **SIG** "
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
#' params:
#'   CLEAN: TRUE
#'
#' ---

#'
#'  **CM-21 Inclined SIG**
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
knitr::opts_chunk$set(fig.cap   = " - empty caption - ")
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
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_duckdb/03_Inspect_CM21INC_sig.R"

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
OutliersPlot <- 4
CLEAN        <- TRUE
# CLEAN        <- FALSE

## __ Execution control  -------------------------------------------------------
## When knitting
if (exists("params")) {
  # params <- list(CLEAN = CLEAN)
  CLEAN <- params$CLEAN
}
## When running
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  if (any(args == "CLEAN")) { CLEAN <- TRUE  }
  if (any(args == "DIRTY")) { CLEAN <- FALSE }
  cat("Arguments", paste(args),"\n")
}

cat(paste("\n**CLEAN:", CLEAN, "**\n"))

##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_DUCK, read_only = TRUE))

## years in the data base
datayears <- tbl(con, "LAP")  |>
  filter(!is.na(CM21INC_sig)) |>
  select(year)                |>
  distinct() |> pull() |> sort()

## TODO compare output files with parsed dates from meta
years_to_do <- datayears

# TEST
#years_to_do <- 1995

#'
#' ## Intro
#'
#' Produce yearly plots for **CM-21 INCLINED**.
#'
#' Shows only **raw data** aspects.
#'
#' It can use flags to show 'CLEAN'/'DIRTY' data.
#'
#' For 'CLEAN' data, it removes from view:
#'
#' - Bad recordings ranges `cm21INC_bad_data_flag`
#' - Physical recording limits `cm21INC_signal_lower_limit()` and `cm21INC_signal_upper_limit()`
#'
#' Mark outliers for signal and SD with:
#'
#' **mean(variable) -/+ `r OutliersPlot` * sd(variable)**
#'
#' This is just a report it doesn't alter the data.
#'

#+ include=TRUE, echo=FALSE, results="asis"
for (YYYY in sort(years_to_do)) {
  days_of_year <- seq.Date(as.Date(paste0(YYYY, "-01-01")),
                           as.Date(paste0(YYYY, "-12-31")), by = "day")
  ## don't go to the future
  days_of_year <- days_of_year[days_of_year <= Sys.Date()]

  cat("\n\n\\FloatBarrier\n\n")
  cat("\\newpage\n\n")
  cat("\n## Year:", YYYY, "\n\n")

  ## load data for year
  ## FIXME this draws all data in memory, could use only database
  DT <- tbl(con, "LAP") |> filter(year == YYYY)
  year_data <- DT |>
    select(
      Date, SZA, Day, Elevat, Azimuth,
      contains("cm21inc", ignore.case = T)
    ) |>
    collect() |> data.table()
  setorder(year_data, Date)

  ## No Recording limits have been set
  # year_data[, sig_lowlim := cm21INC_signal_lower_limit(Date)]
  # year_data[, sig_upplim := cm21INC_signal_upper_limit(Date)]
  year_data[, sig_lowlim := -20]
  year_data[, sig_upplim := +60]

  ## Choose what to plot (data.table slicing dong work)
  # if (CLEAN) {
  #     year_data[!is.na(CM21INC_sig), .N]
  #     cat("\nRemove bad data regions\n")
  #     cat(year_data[!is.na(cm21INC_bad_data_flag), .N], year_data[!is.na(CM21INC_sig), .N], "\n\n")
  #     year_data$CM21INC_sig   [!is.na(year_data$cm21INC_bad_data_flag)] <- NA
  #     year_data$CM21INC_sig_sd[!is.na(year_data$cm21INC_bad_data_flag)] <- NA
  #
  #     cat("\nRemove data above physical limits\n")
  #     cat(year_data[CM21INC_sig > sig_upplim, .N], year_data[!is.na(CM21INC_sig), .N], "\n\n")
  #     year_data$CM21INC_sig   [year_data$CM21INC_sig > year_data$sig_upplim] <- NA
  #     year_data$CM21INC_sig_sd[year_data$CM21INC_sig > year_data$sig_upplim] <- NA
  #
  #     cat("\nRemove data below physical limits\n")
  #     cat(year_data[CM21INC_sig < sig_lowlim, .N], year_data[!is.na(CM21INC_sig), .N], "\n\n")
  #     year_data$CM21INC_sig   [year_data$CM21INC_sig < year_data$sig_lowlim] <- NA
  #     year_data$CM21INC_sig_sd[year_data$CM21INC_sig < year_data$sig_lowlim] <- NA
  # }

  ## Missing days
  dwod <- year_data[is.na(CM21INC_sig), .N == 1440, by = Day]
  empty_days <- dwod[V1 == T, Day]
  cat(paste0("\n**", length(empty_days), " days without any CM-21 data**\n\n"))
  cat(format(empty_days), " ")
  cat("\n\n")

  ## Get outliers limits
  suppressWarnings({
    ## Try to find outliers
    yearlims <- data.table()
    for (an in grep("CM21INC_sig", names(year_data), value = TRUE)) {
      daily <- year_data[, .(dmin = min(get(an), na.rm = T),
                             dmax = max(get(an), na.rm = T)), by = as.Date(Date)]
      low <- daily[!is.infinite(dmin), mean(dmin, na.rm = T) - OutliersPlot * sd(dmin, na.rm = T)]
      upe <- daily[!is.infinite(dmax), mean(dmax, na.rm = T) + OutliersPlot * sd(dmax, na.rm = T)]
      yearlims <- rbind(yearlims, data.table(an = an,low = low, upe = upe))
    }
  })

  cat("\n\n### Proposed outliers limits \n")
  cat('\n\n\\footnotesize\n\n')
  cat(pander(yearlims))
  cat('\n\n\\normalsize\n\n')

  cat("\n**Days with outliers:**\n\n")
  cat(format(
    year_data[CM21INC_sig > yearlims[an == "CM21INC_sig", upe], unique(as.Date(Date))]
  ))
  cat("\n\n")
  cat(format(
    year_data[CM21INC_sig < yearlims[ an == "CM21INC_sig", low], unique(as.Date(Date))]
  ))
  cat("\n\n")
  cat(format(
    year_data[CM21INC_sig_sd > yearlims[ an == "CM21INC_sig_sd", upe], unique(as.Date(Date))]
  ))
  cat("\n\n")
  cat(format(
    year_data[CM21INC_sig_sd < yearlims[ an == "CM21INC_sig_sd", low], unique(as.Date(Date))]
  ))
  cat("\n\n")

  cat("\n**Days hitting physical limit:**\n\n")
  cat(format(
    year_data[CM21INC_sig > sig_upplim, unique(as.Date(Date))]
  ))
  cat("\n\n")
  cat(format(
    year_data[CM21INC_sig < sig_lowlim, unique(as.Date(Date))]
  ))

  cat('\n\n\\footnotesize\n\n')
  cat(pander(summary(year_data[, .(Date, SZA, CM21INC_sig, CM21INC_sig_sd)])))
  cat('\n\n\\normalsize\n\n')


  hist(year_data$CM21INC_sig,
       breaks = 50,
       main   = paste("INCLINED CM-21 signal ", YYYY))
  abline(v = yearlims[ an == "CM21INC_sig", low], lty = 3, col = "red")
  abline(v = yearlims[ an == "CM21INC_sig", upe], lty = 3, col = "red")
  cat('\n\n')


  hist(year_data$CM21INC_sig_sd,
       breaks = 50,
       main   = paste("INCLINED CM-21 signal SD", YYYY))
  abline(v = yearlims[ an == "CM21INC_sig_sd", low], lty = 3, col = "red")
  abline(v = yearlims[ an == "CM21INC_sig_sd", upe], lty = 3, col = "red")
  cat('\n\n')


  plot(year_data$Elevat, year_data$CM21INC_sig,
       pch  = 19,
       cex  = .1,
       main = paste("INCLINED CM-21 signal ", YYYY),
       xlab = "Elevation",
       ylab = "INCLINED CM-21 signal")
  points(year_data$Elevat, year_data$sig_lowlim, pch = ".", col = "red")
  points(year_data$Elevat, year_data$sig_upplim, pch = ".", col = "red")
  cat('\n\n')


  plot(year_data$Date, year_data$CM21INC_sig,
       pch  = 19,
       cex  = .1,
       main = paste("INCLINED CM-21 signal ", YYYY ),
       xlab = "",
       ylab = "INCLINED CM-21 signal" )
  points(year_data$Date, year_data$sig_lowlim, pch = ".", col = "red")
  points(year_data$Date, year_data$sig_upplim, pch = ".", col = "red")
  abline(h = yearlims[ an == "CM21INC_sig", low], lty = 3, col = "red")
  abline(h = yearlims[ an == "CM21INC_sig", upe], lty = 3, col = "red")
  cat('\n\n')

  plot(year_data$Elevat, year_data$CM21INC_sig_sd,
       pch  = 19,
       cex  = .1,
       main = paste("INCLINED CM-21 signal SD", YYYY),
       xlab = "Elevation",
       ylab = "INCLINED CM-21 signal SD")
  abline(h = yearlims[ an == "CM21INC_sig_sd", low], lty = 3, col = "red")
  abline(h = yearlims[ an == "CM21INC_sig_sd", upe], lty = 3, col = "red")
  cat('\n\n')

  all    <- cumsum(tidyr::replace_na(year_data$CM21INC_sig, 0))
  pos    <- year_data[ CM21INC_sig > 0 ]
  pos$V1 <- cumsum(tidyr::replace_na(pos$CM21INC_sig, 0))
  neg    <- year_data[ CM21INC_sig < 0 ]
  neg$V1 <- cumsum(tidyr::replace_na(neg$CM21INC_sig, 0))
  xlim   <- range(year_data$Date)
  plot(year_data$Date, all,
       type = "l",
       xlim = xlim,
       ylab = "",
       yaxt = "n", xlab = "",
       main = paste("Cum Sum of CM-21 INCLINED signal ",  YYYY))
  par(new = TRUE)
  plot(pos$Date, pos$V1,
       xlim = xlim,
       col = "blue", type = "l",
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  par(new = TRUE)
  plot(neg$Date, neg$V1,
       xlim = xlim,
       col = "red", type = "l",
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  legend("left", legend = c("Positive signal",
                            "Negative signal",
                            "All signal"),
         lty = 1, bty = "n", cex = 0.8,
         col = c("blue", "red", "black"))
  cat('\n\n')

  all    <- cumsum(tidyr::replace_na(year_data$CM21INC_sig_sd, 0))
  pos    <- year_data[ CM21INC_sig > 0 ]
  pos$V1 <- cumsum(tidyr::replace_na(pos$CM21INC_sig_sd, 0))
  neg    <- year_data[ CM21INC_sig < 0 ]
  neg$V1 <- cumsum(tidyr::replace_na(neg$CM21INC_sig_sd, 0))
  xlim   <- range(year_data$Date)
  plot(year_data$Date, all,
       type = "l",
       xlim = xlim,
       ylab = "",
       yaxt = "n", xlab = "",
       main = paste("Cum Sum of CM-21 INCLINED sd ",  YYYY))
  par(new = TRUE)
  plot(pos$Date, pos$V1,
       xlim = xlim,
       col = "blue", type = "l",
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  par(new = TRUE)
  plot(neg$Date, neg$V1,
       xlim = xlim,
       col = "red", type = "l",
       ylab = "", yaxt = "n", xlab = "", xaxt = "n")
  legend("left", legend = c("Positive signal",
                            "Negative signal",
                            "All signal"),
         lty = 1, bty = "n", cex = 0.8,
         col = c("red", "blue", "black"))
  cat('\n\n')

  month_vec <- strftime( year_data$Date, format = "%m")
  dd        <- aggregate(year_data[, .(CM21INC_sig, CM21INC_sig_sd, Elevat, Azimuth)],
                         list(month_vec), FUN = summary, digits = 6 )

  boxplot(year_data$CM21INC_sig ~ month_vec )
  title(main = paste("CM21 INC signal by month", YYYY))
  cat('\n\n')

  boxplot(year_data$CM21INC_sig_sd ~ month_vec)
  title(main = paste("CM21 INC sd by month", YYYY))
  cat('\n\n')

  # boxplot(year_data$Elevat ~ month_vec )
  # title(main = paste("Elevation by month", YYYY) )
  # cat('\n\n')

  # boxplot(year_data$Azimuth ~ month_vec )
  # title(main = paste("Azimuth by month", YYYY) )
  # cat('\n\n')



  ## __ Plots of exceptions for investigation  -------------------------------

  # ## ____ 2005 ---------------------------------------------------------------
  # if (YYYY == 2005) {
  #     cat("\n### Year:", YYYY, " exceptions \n\n")
  #
  #     part <- year_data[Date > as.POSIXct("2005-11-15") &
  #                       Date < as.POSIXct("2005-12-31") ]
  #     ylim <- range(-1, 2, part$sig_lowlim, part$sig_upplim)
  #
  #     plot(  part$Date, part$CM21INC_sig,  pch = ".", ylim = ylim,
  #            xlab = "", ylab = "CM-21 INCLINED signal")
  #     points(part$Date, part$sig_lowlim, pch = ".", col = "red")
  #     points(part$Date, part$sig_upplim, pch = ".", col = "red")
  #     ## plot config changes
  #     abline(v = signal_physical_limits$Date, lty = 3)
  #     ## Plot existing sirena data
  #     points(part$Date, part$tot_glb / cm21factor(part$Date), pch = ".", col = "cyan")
  # }

}


#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
