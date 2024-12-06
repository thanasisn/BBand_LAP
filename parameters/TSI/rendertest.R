#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Populates:
#'  - TSI from NOAA
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#+ include=F

## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )

## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/TSI/11_Extend_LAP_TSI.R"
Script.ID   <- "0B"

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
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2,    warn.conflicts = FALSE, quietly = TRUE)

#+ include=T, echo=F, results="asis"
cat("\n# Extend TSI data for LAP with TSIS\n\n")

##  Open dataset  --------------------------------------------------------------
iris <- data.table(iris)

##  Find adjustments  ----------------------------------------------------------
#' ## Find adjustments
#'
#' Use the common period to find an offset adjustment.
#'
#' Use NOAA as base and bring TSIS near NOAA
#'

summary(iris)



## Fill with TSI DATA  ---------------------------------------------------------
#'
#'  Insert raw TSIS values to the main table
#'
#+ echo=T

print("hello")

## Fill raw with interpolation  ------------------------------------------------
#'
#'  Fill TSIS TSI with interpolated values.
#'
#'  Create a function than can fill any date
#'
#'  Assume old TSIS data do not need update,
#'  otherwise should select old TSIS and replace them all
#'
#+ echo=F

print("hello echo F")


## __ Calculate other TSI values for LAP  --------------------------------------
#'
#'  Create values of TSI at TOA and LAP for all TSIs at 1 au.
#'
#+ include=T

print("hello include true")

## __ Calculate other TSI values for LAP  --------------------------------------
#'
#'  Create values of TSI at TOA and LAP for all TSIs at 1 au.
#'
#+ include=F

print("hello include false")


##  Plot composite TSI
iris |>
  ggplot() +
  geom_point(
    aes(x = Sepal.Length,
        y = Sepal.Width,
        color = Species),
    size = 0.5)




tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Runjjj.log", append = TRUE)
