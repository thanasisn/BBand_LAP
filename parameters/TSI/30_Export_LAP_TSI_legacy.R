#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = "!ht"   )

## __ Set environment  ---------------------------------------------------------
# closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/TSI/30_Export_LAP_TSI_legacy.R"
Script.ID   <- "0B"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)

cat("\n Extend TSI data for LAP with TSIS\n\n")

#'
#' Data file: /home/athan/DATA/SUN/TSI_COMPOSITE.Rds
#'
#' This was used in some articles as input.
#'

# File structure:
#
# $ Date             : POSIXct, format: "1993-01-01 00:00:30" "1993-01-01 00:01:30" ...
# $ sun_dist         : num  0.983 0.983 0.983 0.983 0.983 ...
# $ TSIextEARTH_comb : num  NA NA NA NA NA NA NA NA NA NA ...
# $ measur_error_comb: num  NA NA NA NA NA NA NA NA NA NA ...
# $ tsi_1au_comb     : num  NA NA NA NA NA NA NA NA NA NA ...
# $ Source           : chr  "TSIS_adjusted" "TSIS_adjusted" "TSIS_adjusted"

##  Check if need to run  -----------------------------------------------------

if (!file.exists(COMP_TSI_legacy) |
     file.mtime(DB_TSI) > file.mtime(COMP_TSI_legacy)) {

  cat("\nHave to export\n")

  ##  Open dataset  --------------------------------------------------------------
  con <- dbConnect(duckdb(dbdir = DB_TSI, read_only = TRUE))
  sun <- dbConnect(duckdb(dbdir = DB_LAP, read_only = TRUE))

  SUN <- tbl(sun, "params")                                |>
    filter(!is.na(AsPy_Elevation) & Date >= DB_start_date) |>
    rename(sun_dist = "AsPy_Dist")                         |>
    select(Date, sun_dist)

  LAP  <- tbl(con, "LAP_TSI")            |>
    filter(!is.na(TSI_TOA))              |>
    rename(tsi_1au_comb     = "TSI",
           TSIextEARTH_comb = "TSI_TOA") |>
    select(Date, TSIextEARTH_comb, tsi_1au_comb, Source)

  ## Read all in memory
  EXP <- left_join(LAP, SUN, copy = T) |>
    arrange(Date) |> collect() |> data.table()

  ## Store to disk
  write_RDS(object = EXP, file = COMP_TSI_legacy)

  ## clean exit
  dbDisconnect(con, shutdown = TRUE); rm(con)
  dbDisconnect(sun, shutdown = TRUE); rm(sun)
} else {
  cat("\nDo not have to export new Rds\n\n")
}

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
