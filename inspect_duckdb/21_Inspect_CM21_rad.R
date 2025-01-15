# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Inspect CM-21 radiation data **GHI L1** "
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
#'  **CM-21 GHI L1**
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
Script.Name <- "~/BBand_LAP/inspect_duckdb/21_Inspect_CM21_rad.R"

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
con <- dbConnect(duckdb(dbdir = DB_DUCK, read_only = TRUE))
DT  <- tbl(con, "LAP")

DT <- DT |>
  select(
    Date, SZA, Elevat, year, Azimuth, preNoon,
    contains(c("GLB", "cm21")))


##  Overall statistics  --------------------------------------------------------

## variables for stats
vars <- c("GLB_wpsm", "GLB_SD_wpsm")


#'
#' ## Stats on whole data set.
#'
#'
#+ include=T, echo=F

## histogram of GHI SD
hist(
  DT |> select("GLB_SD_wpsm") |> collect() |> pull(),
  breaks = 100,
  xlab   =  expression(W %.% m^-2),
  main   = "Distribution of GHI SD"
)

## histogram of GHI
hist(
  DT |> select("GLB_wpsm") |> collect() |> pull(),
  breaks = 100,
  xlab   =  expression(W %.% m^-2),
  main   = "Distribution of GHI"
)


## extreme SD to check
SD_outliers <- DT |> filter(GLB_SD_wpsm > 400) |> select(Date) |> collect()
pander(SD_outliers)

SD_negative <- DT |> filter(GLB_SD_wpsm < 0) |> select(Date) |> collect()
pander(SD_negative)


## extreme values to check
GHI_negative <- DT |> filter(GLB_wpsm < -20) |> select(Date) |> collect()
pander(GHI_negative)



## get daily ranges
extreme <- DT |>
  filter(!is.na(GLB_wpsm))      |>
  group_by(Day = as.Date(Date)) |>
  summarise(
    across(
      all_of(vars),
      list(max = ~ max(., na.rm = TRUE),
           min = ~ min(., na.rm = TRUE)))) |>
  collect() |> data.table()


plot(extreme[, GLB_wpsm_max, Day])
plot(extreme[, GLB_wpsm_min, Day])

plot(extreme[, GLB_SD_wpsm_max, Day])
plot(extreme[, GLB_SD_wpsm_min, Day])

pander(
  head(extreme[order(extreme$GLB_SD_wpsm_min, decreasing = T)], 30)
)



##  Yearly plots  --------------------------------------------------------------

## years in the data base
datayears <- DT |>
  filter(!is.na(GLB_wpsm)) |>
  select(year) |>
  distinct()   |> pull() |> sort()



## TODO compare output files with parsed dates from meta
years_to_do <- datayears

# TEST
if (TEST) {
  years_to_do <- 1995
}

#'
#' ## Intro
#'
#' Produce yearly plots for **CM-21**.
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

  ## _ Check for night time extreme values -----------------------------------
  #'
  #' Problems in the night signal especially bellow DARK_ELEV can be spotted
  #' and corrected with exclusions to protect the dark signal calculation.
  #'

  ## __ Dynamic outliers limits --------------------------------------------
  OutliersUP   <- 3.5
  OutliersDOWN <- 4.5

  ## _ Dark data Global ------------------------------------------------------
  av  <- "GLB_wpsm"
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
    ylab = expression(paste("Global Irradiance [", Watt/m^2, "]"))
  )
  abline(h = pplims$low, col = "red", lty = 3)
  abline(h = pplims$upe, col = "red", lty = 3)
  cat('\n\n')

  offend <- ppD[ V1 > pplims$upe | V1 < pplims$low ]

  if (nrow(offend) > 0) {
    cat("\n### Dark outlier days\n\n")
    cat(pander(
      offend[, .(Max = max(V1), Min = min(V1), N = .N), by = as.Date(Date) ]
    ))
    cat('\n\n')
  }

  ## _ Dark data Global SD ---------------------------------------------------
  av  <- "GLB_SD_wpsm"
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
    ylab = expression(paste("Global Irradiance [", Watt/m^2, "]"))
  )
  abline(h = pplims$low, col = "red", lty = 3)
  abline(h = pplims$upe, col = "red", lty = 3)
  cat('\n\n')

  offend <- ppD[ V1 > pplims$upe | V1 < pplims$low ]

  if (nrow(offend) > 0) {
    cat("\n### Dark SD outlier days\n\n")
    cat(pander(
      offend[, .(Max = max(V1), Min = min(V1), N = .N), by = as.Date(Date) ]
    ))
    cat('\n\n')
  }

  ## _ Night data Global -----------------------------------------------------
  av  <- "GLB_wpsm"
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
    ylab = expression(paste("Global Irradiance [", Watt/m^2, "]"))
  )
  abline(h = pplims$low, col = "red", lty = 3)
  abline(h = pplims$upe, col = "red", lty = 3)
  cat('\n\n')

  offend <- ppD[ V1 > pplims$upe | V1 < pplims$low ]

  if (nrow(offend) > 0) {
    cat("\n### Night outlier days\n\n")
    cat(pander(
      offend[, .(Max = max(V1), Min = min(V1), N = .N), by = as.Date(Date) ]
    ))
    cat('\n\n')
  }


  ## _ Night data Global SD --------------------------------------------------
  av  <- "GLB_SD_wpsm"
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
    ylab = expression(paste("Global Irradiance [", Watt/m^2, "]"))
  )
  abline(h = pplims$low, col = "red", lty = 3)
  abline(h = pplims$upe, col = "red", lty = 3)
  cat('\n\n')

  offend <- ppD[ V1 > pplims$upe | V1 < pplims$low ]

  if (nrow(offend) > 0) {
    cat("\n### Night SD outlier days\n\n")
    cat(pander(
      offend[, .(Max = max(V1), Min = min(V1), N = .N), by = as.Date(Date) ]
    ))
    cat('\n\n')
  }


  ## _ Distribution of Global and SD -----------------------------------------
  hist(year_data[Elevat < DARK_ELEV, GLB_wpsm],
       main = paste(YYYY, "GHI Elevat <", DARK_ELEV, "°"),
       breaks = 100 , las = 1, probability = T, xlab = "Watt/m^2")
  abline(v = CM21_MAXnightLIM, col = "red", lty = 3)
  abline(v = CM21_MINnightLIM, col = "red", lty = 3)
  cat('\n\n')


  ## _ Plots of SD -----------------------------------------------------------
  plot(
    year_data[Elevat > 0, GLB_SD_wpsm, Date],
    pch  = 19,
    cex  = .1,
    main = paste(YYYY, "GHI SD, Elevat >", 0, "°"),
    xlab = "",
    ylab = expression(paste("Global Irradiance [", Watt/m^2, "]"))
  )
  cat('\n\n')


  ## _ Distribution of Global and SD -----------------------------------------
  wattlimit <- 50
  hist(
    year_data[GLB_wpsm > wattlimit, GLB_wpsm],
    main = paste(YYYY, "GHI >", wattlimit, "[Watt/m^2]"),
    breaks = 100,
    las = 1,
    probability = T,
    xlab = expression(paste("Global Irradiance [", Watt/m^2, "]"))
  )
  lines(density(year_data$GLB_wpsm, na.rm = T), col = "orange", lwd = 3)
  cat('\n\n')

  hist(
    year_data$GLB_SD_wpsm,
    main = paste(YYYY, "GHI SD"),
    breaks = 100,
    las = 1,
    probability = T,
    xlab = expression(paste("Global Irradiance [", Watt/m^2, "]"))
  )
  lines(density(year_data$GLB_SD_wpsm, na.rm = T), col = "orange", lwd = 3)
  cat('\n\n')


  ## _ Scatter points by sun position ----------------------------------------
  plot(
    year_data[, GLB_wpsm, Elevat],
    pch  = 19,
    cex  = .1,
    main = paste("Global radiation ", YYYY),
    xlab = "Elevation [°]",
    ylab = expression(paste("Global Irradiance [", Watt/m^2, "]"))
  )
  cat('\n\n')

  plot(
    year_data[, GLB_wpsm, Azimuth],
    pch  = 19,
    cex  = .1,
    main = paste("Global radiation ", YYYY),
    xlab = "Azimuth [°]",
    ylab = expression(paste("Global Irradiance [", Watt/m^2, "]"))
  )
  cat('\n\n')


  ## _ Scatter points by date ------------------------------------------------
  plot(
    year_data[, GLB_wpsm, Date],
    pch  = 19,
    cex  = .1,
    main = paste("Global radiation ", YYYY),
    xlab = "",
    ylab = expression(paste("Global Irradiance [", Watt/m^2, "]"))
  )
  cat('\n\n')


  ## _ Scatter points by time of day -----------------------------------------
  plot(
    year_data[preNoon == TRUE, GLB_wpsm, Elevat],
    pch  = 19,
    cex  = .1,
    col  = "blue",
    main = paste("Global ", YYYY),
    xlab = "Elevation [°]",
    ylab = expression(paste("Global Irradiance [", Watt/m^2, "]"))
  )
  points(
    year_data[preNoon == TRUE, GLB_wpsm, Elevat],
    pch = 19,
    cex = 0.1,
    col = "green"
  )
  legend("topleft",
         legend = c("Before noon", "After noon"),
         col    = c("blue",        "green"),
         pch    = 19, bty = "n")
  cat(" \n \n")


  minelevet <- -1
  xlim      <- range(year_data$Elevat, na.rm = TRUE)
  gap       <- 1
  xlim[2] <- xlim[2] + abs(diff(range(year_data[Elevat > minelevet, Elevat], na.rm = TRUE))) + gap
  xlim[1] <- minelevet

  plot(
    year_data[preNoon == TRUE & Elevat > minelevet,
              GLB_wpsm, Elevat],
    xlim = xlim,
    pch  = 19,
    cex  = .05,
    col  = "blue",
    main = paste("GHI morning/evening balance", YYYY),
    xaxt = "n",
    xlab = "Sun Elevation",
    ylab = expression(W %.% m^-2)
  )

  points(-year_data[preNoon == FALSE & Elevat > minelevet, Elevat] + gap + abs(diff(range(year_data$Elevat, na.rm = TRUE))),
         year_data[preNoon == FALSE & Elevat > minelevet, GLB_wpsm],
         pch = 19,
         cex = 0.05,
         col = "green")
  cat(" \n \n")

  plot(year_data[preNoon == TRUE & Elevat > minelevet, Elevat],
       year_data[preNoon == TRUE & Elevat > minelevet, GLB_wpsm],
       xlim = xlim,
       pch  = 19,
       cex  = .05,
       col  = "green",
       main = paste("GHI morning/evening balance", YYYY),
       xaxt = "n",
       xlab = "Sun Elevation",
       ylab = expression(W %.% m^-2))

  points(year_data[preNoon == FALSE & Elevat > minelevet,
                   GLB_wpsm,
                   -Elevat + gap + abs(diff(range(year_data$Elevat))) ],
         pch = 19,
         cex = 0.05,
         col = "blue")
  cat(" \n \n")



  ## _ Box plots by week -----------------------------------------------------
  year_data[ , weekn := week(Date)]

  boxplot(year_data[Elevat > 0, GLB_wpsm] ~ year_data[Elevat > 0, weekn],
          xlab = "Week", ylab = "[Watt/m^2]")
  title(main = paste(YYYY, "GHI (Elevation > 0)"))
  cat('\n\n')

  boxplot(year_data[Elevat > 0, GLB_SD_wpsm] ~ year_data[Elevat > 0, weekn],
          xlab = "Week", ylab = "[Watt/m^2]")
  title(main = paste(YYYY, "GHI SD (Elevation > 0)"))
  cat('\n\n')

  boxplot(year_data[, CM21_sig - CM21_sig_wo_dark] ~ year_data[, weekn],
          xlab = "Week", ylab = "[V]")
  title(main = paste(YYYY, "Dark correction"))
  cat('\n\n')


  cat('\n\n\\footnotesize\n\n')
  cat(pander(summary(year_data[, .(Date, SZA, GLB_wpsm, GLB_SD_wpsm)])))
  cat('\n\n\\normalsize\n\n')

}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
