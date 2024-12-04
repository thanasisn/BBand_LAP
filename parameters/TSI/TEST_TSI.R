#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Populates:
#'  - TSI from NOAA
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
knitr::opts_chunk$set(fig.pos   = '!h'    )

## __ Set environment  ---------------------------------------------------------
# closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/parameters/TSI/Create_LAP_TSI.R"
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
library(RNetCDF,    warn.conflicts = FALSE, quietly = TRUE)

cat("\n TEST TSI \n\n")

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_TSI, read_only = TRUE))

NEW_DT <- tbl(con, "LAP_TSI")
NEW_DT <- NEW_DT |> select(Date, TSI, TSI_TOA)

##  Load all TSI data  ---------------------------------------------------------
if (!file.exists(COMP_TSI)) { stop("Missing TSI file:", COMP_TSI) }
TSI <- readRDS(COMP_TSI)
TSI <- TSI[!is.na(TSIextEARTH_comb)]
names(TSI)[names(TSI) == "sun_dist"        ] <- "Sun_Dist_Astropy"
names(TSI)[names(TSI) == "TSIextEARTH_comb"] <- "TSI_TOA"
names(TSI)[names(TSI) == "tsi_1au_comb"    ] <- "TSI_1au"
names(TSI)[names(TSI) == "Source"          ] <- "TSI_source"
TSI$measur_error_comb <- NULL
TSI$Sun_Dist_Astropy <- NULL
dummy <- gc()


ayears <- unique(year(TSI$Date))

for (ay in ayears) {
  OLD <- TSI[year(Date)== ay,]
  NEW <- NEW_DT |> filter(year(Date)== ay) |> collect() |> data.table()

  test <- right_join(NEW, OLD, by = "Date")

  plot(test[, TSI_TOA.x/TSI_TOA.y, Date])
  title(paste(ay, "TOA"))

  plot(test[,       TSI/TSI_1au, Date])
  title(paste(ay, "1 AU"))


}





if (interactive()) {stop("dont close")}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con)

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
