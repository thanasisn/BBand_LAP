# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Inspect CHP-1 radiation data **DNI/DHI L1** "
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
#'  **CHP-1 DNI L1**
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
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_duckdb/20_Inspect_CHP1_sig_snc_temp.R"

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
TEST  <- FALSE
# TEST  <- TRUE

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))
DT  <- tbl(con, "LAP")

DT <- DT |>
  select(
    Date, SZA, Elevat, year, Azimuth, preNoon,
    contains(c("DIR", "HOR", "chp1")))



DT |> colnames()



## years in the data base
datayears <- DT |>
  filter(!is.na(DIR_wpsm)) |>
  select(year) |>
  distinct()   |>
  pull()

## TODO compare output files with parsed dates from meta
years_to_do <- datayears

if (TEST) {
    years_to_do <- 2023
}

#'
#' ## Intro
#'
#' Produce yearly plots for **CHP-1**.
#'
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
  year_data <- DT |> filter(year == YYYY) |> collect() |> data.table()

  ## do some cleaning for displaying
  year_data[!chp1_bad_temp_flag %in% c("empty", "pass"), chp1_temperature    := NA]
  year_data[!chp1_bad_temp_flag %in% c("empty", "pass"), chp1_temperature_SD := NA]

  ## _ Check for night time extreme values -----------------------------------
  #'
  #' Problems in the night signal especially bellow DARK_ELEV can be spotted
  #' and corrected with exclusions to protect the dark signal calculation.
  #'

  ## __ Dynamic outliers limits ----
  OutliersUP   <- 3.5
  OutliersDOWN <- 4.5


  ## _ Dark data Global ------------------------------------------------------
  av  <- "DIR_wpsm"
  ppD <- data.table(year_data[Elevat < DARK_ELEV, get(av), Date])
  pp  <- ppD[ , .(dmin = min(V1, na.rm = T),
                  dmax = max(V1, na.rm = T)),
              by = as.Date(Date)]
  low <- pp[!is.infinite(dmin), mean(dmin, na.rm = T) - OutliersDOWN * sd(dmin, na.rm = T)]
  upe <- pp[!is.infinite(dmax), mean(dmax, na.rm = T) + OutliersUP   * sd(dmax, na.rm = T)]
  pplims <- data.table(av = av,low = low, upe = upe)

  plot(
    ppD,
    pch  = 19,
    cex  = .1,
    main = paste(YYYY, av, ", Elevat <", DARK_ELEV, "°"),
    xlab = "",
    ylab = expression(paste("Direct Irradiance [", Watt/m^2, "]"))
  )
  abline(h = pplims$low, col = "red", lty = 3)
  abline(h = pplims$upe, col = "red", lty = 3)
  cat('\n\n')

  offend <- ppD[ V1 > pplims$upe | V1 < pplims$low ]

  if (nrow(offend) > 0) {
    cat("\n### Dark outlier days\n\n")
    cat(pander(
      offend[, .(Max = max(V1), Min = min(V1), N = .N), by = as.Date(Date)]
    ))
    cat('\n\n')
  }

  ## _ Dark data Global SD ---------------------------------------------------
  av  <- "DIR_SD_wpsm"
  ppD <- data.table(year_data[Elevat < DARK_ELEV, get(av), Date])
  pp  <- ppD[ , .(dmin = min(V1, na.rm = T),
                  dmax = max(V1, na.rm = T)),
              by = as.Date(Date)]
  low <- pp[!is.infinite(dmin), mean(dmin, na.rm = T) - OutliersDOWN * sd(dmin, na.rm = T)]
  upe <- pp[!is.infinite(dmax), mean(dmax, na.rm = T) + OutliersUP   * sd(dmax, na.rm = T)]
  pplims <- data.table(av = av,low = low, upe = upe)

  plot(
    ppD,
    pch  = 19,
    cex  = .1,
    main = paste(YYYY, av, ", Elevat <", DARK_ELEV, "°"),
    xlab = "",
    ylab = expression(paste("Direct Irradiance [", Watt/m^2, "]"))
  )
  abline(h = pplims$low, col = "red", lty = 3)
  abline(h = pplims$upe, col = "red", lty = 3)
  cat('\n\n')

  offend <- ppD[ V1 > pplims$upe | V1 < pplims$low ]

  if (nrow(offend) > 0) {
    cat("\n### Dark SD outlier days\n\n")
    cat(pander(
      offend[, .(Max = max(V1), Min = min(V1), N = .N), by = as.Date(Date)]
    ))
    cat('\n\n')
  }

  ## _ Night data Global -----------------------------------------------------
  av  <- "DIR_wpsm"
  ppD <- data.table(year_data[Elevat < 0, get(av), Date])
  pp  <- ppD[ , .(dmin = min(V1, na.rm = T),
                  dmax = max(V1, na.rm = T)),
              by = as.Date(Date)]
  low <- pp[!is.infinite(dmin), mean(dmin, na.rm = T) - OutliersDOWN * sd(dmin, na.rm = T)]
  upe <- pp[!is.infinite(dmax), mean(dmax, na.rm = T) + OutliersUP   * sd(dmax, na.rm = T)]
  pplims <- data.table(av = av,low = low, upe = upe)

  plot(
    ppD,
    pch  = 19,
    cex  = .1,
    main = paste(YYYY, av, ", Elevat <", 0, "°"),
    xlab = "",
    ylab = expression(paste("Direct Irradiance [", Watt/m^2, "]"))
  )
  abline(h = pplims$low, col = "red", lty = 3)
  abline(h = pplims$upe, col = "red", lty = 3)
  cat('\n\n')

  offend <- ppD[ V1 > pplims$upe | V1 < pplims$low ]

  if (nrow(offend) > 0) {
    cat("\n### Night outlier days\n\n")
    cat(pander(
      offend[, .(Max = max(V1), Min = min(V1), N = .N), by = as.Date(Date)]
    ))
    cat('\n\n')
  }


  ## _ Night data Global SD --------------------------------------------------
  av  <- "DIR_SD_wpsm"
  ppD <- data.table(year_data[Elevat < 0, get(av), Date])
  pp  <- ppD[ , .(dmin = min(V1, na.rm = T),
                  dmax = max(V1, na.rm = T)),
              by = as.Date(Date)]
  low <- pp[!is.infinite(dmin), mean(dmin, na.rm = T) - OutliersDOWN * sd(dmin, na.rm = T)]
  upe <- pp[!is.infinite(dmax), mean(dmax, na.rm = T) + OutliersUP   * sd(dmax, na.rm = T)]
  pplims <- data.table(av = av,low = low, upe = upe)

  plot(
    ppD,
    pch  = 19,
    cex  = .1,
    main = paste(YYYY, av, ", Elevat <", 0, "°"),
    xlab = "",
    ylab = expression(paste("Direct Irradiance [", Watt/m^2, "]"))
  )
  abline(h = pplims$low, col = "red", lty = 3)
  abline(h = pplims$upe, col = "red", lty = 3)
  cat('\n\n')

  offend <- ppD[ V1 > pplims$upe | V1 < pplims$low ]

  if (nrow(offend) > 0) {
    cat("\n### Night SD outlier days\n\n")
    cat(pander(
      offend[, .(Max = max(V1), Min = min(V1), N = .N), by = as.Date(Date)]
    ))
    cat('\n\n')
  }


  ## _ Distribution of direct and SD -----------------------------------------
  wattlimit <- 50
  hist(year_data[ DIR_wpsm > wattlimit, DIR_wpsm],
       main = paste(YYYY, "Direct  >", wattlimit, "[Watt/m^2]"),
       breaks = 100 , las = 1, probability = T, xlab = "[Watt/m^2]")
  lines(density(year_data$DIR_wpsm, na.rm = T), col = "orange", lwd = 3)
  cat('\n\n')

  hist(year_data$DIR_SD_wpsm,
       main = paste(YYYY, "Direct SD"),
       breaks = 100 , las = 1, probability = T, xlab = "[Watt/m^2]")
  lines(density(year_data$DIR_SD_wpsm, na.rm = T), col = "orange", lwd = 3)
  cat('\n\n')


  ## Scatter points by sun position ------------------------------------------
  plot(
    year_data$Elevat,
    year_data$DIR_wpsm,
    pch  = 19,
    cex  = .1,
    main = paste("Direct Beam ", YYYY),
    xlab = "Elevation [°]",
    ylab = expression(paste("Direct Irradiance [", Watt/m^2, "]"))
  )
  cat('\n\n')

  plot(
    year_data$Azimuth,
    year_data$DIR_wpsm,
    pch  = 19,
    cex  = .1,
    main = paste("Direct Beam ", YYYY),
    xlab = "Azimuth [°]",
    ylab = expression(paste("Direct Irradiance [", Watt/m^2, "]"))
  )
  cat('\n\n')


  ## Scatter points by date --------------------------------------------------
  plot(
    year_data[, DIR_wpsm, Date],
    pch  = 19,
    cex  = .1,
    main = paste("Direct Beam ", YYYY),
    xlab = "",
    ylab = expression(paste("Direct Irradiance [", Watt/m^2, "]"))
  )
  cat('\n\n')

  plot(
    year_data[, HOR_strict, Azimuth],
    pch  = 19,
    cex  = .1,
    main = paste("Direct Horizontal strict ", YYYY),
    xlab = "Azimuth [°]",
    ylab = expression(paste("Direct Irradiance [", Watt/m^2, "]"))
  )
  cat('\n\n')


  ## _ Scatter points by date ------------------------------------------------
  plot(
    year_data[, HOR_strict, Date],
    pch  = 19,
    cex  = .1,
    main = paste("Direct Horizontal strict", YYYY),
    xlab = "",
    ylab = expression(paste("Direct Irradiance [", Watt/m^2, "]"))
  )
  cat('\n\n')


  ## _ Scatter points by time of day -----------------------------------------
  plot(
    year_data[preNoon == TRUE & !is.na(HOR_strict), HOR_strict, Elevat],
    pch  = 19,
    cex  = .1,
    col  = "blue",
    main = paste("Direct Horizontal strict", YYYY),
    xlab = "Elevation [°]",
    ylab = expression(paste("Direct Irradiance [", Watt/m^2, "]"))
  )
  points(
    year_data[preNoon == FALSE & !is.na(HOR_strict), HOR_strict, Elevat],
    pch = 19,
    cex = 0.1,
    col = "green")
  legend("topleft",
         legend = c("Before noon", "After noon"),
         col    = c("blue",        "green"),
         pch    = 19, bty = "n")
  cat('\n\n')


  minelevet <- -1
  xlim <- range(year_data$Elevat, na.rm = TRUE)
  gap  <- 1
  xlim[2] <- xlim[2] + abs(diff(range(year_data[Elevat > minelevet, Elevat], na.rm = TRUE))) + gap
  xlim[1] <- minelevet

  plot(
    year_data[preNoon == TRUE & Elevat > minelevet, HOR_strict, Elevat],
    xlim = xlim,
    pch  = 19,
    cex  = .05,
    col  = "blue",
    main = paste("DHI morning/evening balance", YYYY),
    xaxt = "n",
    xlab = "Sun Elevation",
    ylab = expression(W %.% m^-2))

  points(
    year_data[preNoon == FALSE & Elevat > minelevet,
              HOR_strict,
              -Elevat + gap + abs(diff(range(year_data$Elevat))) ],
    pch = 19,
    cex = 0.05,
    col = "green")
  cat('\n\n')


  ## _ Box plots by week -----------------------------------------------------
  year_data[ , weekn := week(Date)]

  boxplot(year_data[Elevat > 0, HOR_strict] ~ year_data[Elevat > 0, weekn],
          xlab = "Week", ylab = "[Watt/m^2]")
  title(main = paste(YYYY, "DHI (Elevation > 0)"))
  cat('\n\n')

  # boxplot(year_data[Elevat > 0, HOR_SD_wpsm] ~ year_data[Elevat > 0, weekn],
  #         xlab = "Week", ylab = "[Watt/m^2]")
  # title(main = paste(YYYY, "DHI SD (Elevation > 0)"))
  # cat('\n\n')

  boxplot(year_data[, CHP1_sig - CHP1_sig_wo_dark] ~ year_data[, weekn],
          xlab = "Week", ylab = "[V]")
  title(main = paste(YYYY, "Dark correction"))
  cat('\n\n')

  cat('\n\n\\footnotesize\n\n')
  cat(pander(summary(year_data[, .(Date, SZA, DIR_wpsm, DIR_SD_wpsm, HOR_strict, chp1_temperature)])))
  cat('\n\n\\normalsize\n\n')


  ## Temperature data --------------------------------------------------------
  if (year_data[!is.na(chp1_temperature), .N] > 0) {
    cat("\n\n\\FloatBarrier\n\n")
    cat("\n## Temperature data:", YYYY, "\n\n")

    hist(year_data$chp1_temperature,
         breaks = 50,
         main   = paste("CHP1 temperature ",  YYYY))
    cat('\n\n')

    hist(year_data$chp1_temperature_SD,
         breaks = 50,
         main   = paste("CHP1 temperature SD", YYYY))
    cat('\n\n')

    plot(year_data$Date, year_data$chp1_temperature,
         pch  = 19,
         cex  = .5,
         main = paste("CHP1 temperature ", YYYY),
         xlab = "",
         ylab = "CHP1 temperature [C]" )
    cat('\n\n')

    plot(year_data$Elevat, year_data$chp1_temperature_SD,
         pch  = 19,
         cex  = .5,
         main = paste("CHP1 temperature SD", YYYY),
         xlab = "Elevation [°]",
         ylab = "CHP1 temperature SD [C°]")
    cat('\n\n')
  }
}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
