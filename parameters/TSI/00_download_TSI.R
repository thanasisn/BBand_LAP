#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Download TSI from:
#' -  NOAA
#' -  TSIS get and parse
#' -  SORCE get and parse
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#+ include=F

## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
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
Script.Name <- "~/BBand_LAP/parameters/TSI/00_download_TSI.R"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
library(data.table)

#+ include=T, echo=F, results="asis"
cat("\n ## GET TSI DATA\n\n")

## __ NOAA  --------------------------------------------------------------------
#'
#' Get data from NOAA.
#'
if (Sys.info()["nodename"] == "sagan") {
  command <- paste("wget --timeout=66 -N -r -np -nd -nH -A .nc -P", DEST_NOAA, FROM_NOAA)
  cat(command, "\n\n")
  system(command)
}

## __ TSIS  --------------------------------------------------------------------
#'
#' Get data from TSIS and read it.
#'
if (Sys.info()["nodename"] == "sagan") {
  command <- paste0("curl \"", FROM_TSIS, "\" > ", DEST_TSIS)
  cat(command, "\n\n")
  system(command)
}

##  Parse data  ----------------------------------------------------------------
tsis_data <- fread(DEST_TSIS)
## fix names
names(tsis_data)[grep("time",names(tsis_data))]    <- "Date"
names(tsis_data)[grep(" \\(W/m\\^2\\)", names(tsis_data))] <-
  sub(" \\(W/m\\^2\\)", "", grep(" \\(W/m\\^2\\)", names(tsis_data), value = TRUE))
## ignore zeros
tsis_data <- tsis_data[ tsi_1au >= 1 ]
tsis_data[, provisional_flag := NULL ]
wecare <- grep("true_earth", names(tsis_data), value = TRUE, invert = TRUE)
setorder(tsis_data, Date)
## save data
tsis_data <- tsis_data[, ..wecare ]
saveRDS(object = tsis_data,
        file   = DATA_TSIS)


# ## NO NEW DATA
#
# ## __ SORCE  --------------------------------------------------------------------
# #'
# #' Get data from SORCE and read it.
# #'
# #+ echo=T
# ##  Get data  ------------------------------------------------------------------
# command <- paste0("curl \"", FROM_SORCE, "\" > ", DEST_SORCE)
# cat(command, "\n\n")
# system(command)
#
# ##  Parse data  ----------------------------------------------------------------
# sorce_data <- fread(DEST_SORCE)
# ## fix names
# names(sorce_data)[grep("time",names(sorce_data))]    <- "Date"
# names(sorce_data)[grep(" \\(W/m\\^2\\)", names(sorce_data))] <-
#   sub(" \\(W/m\\^2\\)", "", grep(" \\(W/m\\^2\\)", names(sorce_data), value = TRUE))
# ## ignore zeros
# sorce_data <- sorce_data[tsi_1au >= 1]
# wecare <- grep("true_earth", names(sorce_data), value = TRUE, invert = TRUE)
# setorder(sorce_data, Date)
# ## save data
# sorce_data <- sorce_data[, ..wecare ]
# saveRDS(object = sorce_data,
#         file   = DATA_SORCE)


#+ results="asis", echo=FALSE
tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
