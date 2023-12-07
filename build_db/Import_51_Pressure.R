#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Add atmospheric pressure data in DB from preexisting data
#'
#' Populates:
#'
#' - `Pressure`
#' - `Pressure_source`
#'
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
#'
#+ echo=F, include=T


#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"  )
knitr::opts_chunk$set(fig.align  = "center")
knitr::opts_chunk$set(fig.pos    = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_db/Import_51_Pressure.R"
renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)


##  Load all Pressure data  ----------------------------------------------------
if (!file.exists(COMP_PRES)) { stop("Missing Pressure file:", COMP_PRES) }
PRESSURE <- data.table(readRDS(COMP_PRES))

if (second(PRESSURE$Date[1]) == 0) {
    ## move to center
    PRESSURE[ , Date := Date + 30]
}
names(PRESSURE)[names(PRESSURE) == "pressure"] <- "Pressure"
names(PRESSURE)[names(PRESSURE) == "Source"  ] <- "Pressure_source"
# InitVariableBBDB("Pressure",        as.numeric(NA))
# InitVariableBBDB("Pressure_source", as.character(NA))

## clean some duplicate value
PRESSURE[duplicated(PRESSURE$Date) & Pressure_source == "iama_corrected" ]

test <- PRESSURE[duplicated(PRESSURE$Date) | duplicated(PRESSURE$Date, fromLast=TRUE)  ]


##  Find data set files to update  ---------------------------------------------

## list data base files
filelist <- data.table(
    names = list.files(DB_DIR,
                       pattern = "*.parquet",
                       recursive  = TRUE,
                       full.names = TRUE))
dd               <- dirname(filelist$names)
dd               <- tstrsplit(dd, "/")
filelist$flmonth <- as.numeric(unlist(dd[length(dd)]))
filelist$flyear  <- as.numeric(unlist(dd[length(dd)-1]))

## list data set files possible to touch
BB <- opendata()
wewantlist <- BB                         |>
    select(Date, Pressure)               |>
    filter(is.na(Pressure) &
           Date >= min(PRESSURE$Date))   |>
     mutate(month = month(Date),
            year  = year(Date))          |>
     select(month, year)                 |>
     unique() |> collect()

## list which data set to touch
wewantlist <- data.table(wewantlist)

## TODO get implementation from 50!
## touch these files only
filelist <- filelist[wewantlist, on = .(flmonth = month, flyear = year)]
rm(wewantlist, BB, dd)


test <- PRESSURE[duplicated(PRESSURE$Date) | duplicated(PRESSURE$Date, fromLast = TRUE)]
if (!nrow(test) == 0) {warning("Pressure data should be cleaner\n")}
PRESSURE <- PRESSURE[!duplicated(PRESSURE$Date)]


##  Update Pressure data in DB  -----------------------------------------------------
for (af in filelist$names) {
    datapart <- data.table(read_parquet(af))
    datapart[, month := as.integer(month(Date))]
    datapart[, year  := as.integer(year(Date)) ]
    cat("51 Load: ", af, "\n")

    ## update the whole data part on one go
    datapart <- rows_update(datapart, PRESSURE, by = "Date", unmatched = "ignore")

    ## store actual data
    writePARQUET(x = datapart, sink = af)
    cat("51 Save: ", af, "\n\n")
    ## clean
    rm(datapart)
}

myunlock(DB_lock)

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

