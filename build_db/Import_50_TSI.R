#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Add TSI data in DB from preexistence data
#' [`github.com/thanasisn/TSI`](`https://github.com/thanasisn/TSI`)
#'
#' Populates:
#'
#' - `Sun_Dist_Astropy`
#' - `TSI_TOA`
#' - `TSI_1au`
#' - `TSI_source`
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
Script.Name <- "~/BBand_LAP/build_db/Import_50_TSI.R"
renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)


##  Load all TSI data  ---------------------------------------------------------
if (!file.exists(COMP_TSI)) { stop("Missing TSI file:", COMP_TSI) }
TSI <- readRDS(COMP_TSI)
TSI <- TSI[!is.na(TSIextEARTH_comb)]
names(TSI)[names(TSI) == "sun_dist"        ] <- "Sun_Dist_Astropy"
names(TSI)[names(TSI) == "TSIextEARTH_comb"] <- "TSI_TOA"
names(TSI)[names(TSI) == "tsi_1au_comb"    ] <- "TSI_1au"
names(TSI)[names(TSI) == "Source"          ] <- "TSI_source"
TSI$measur_error_comb <- NULL
dummy <- gc()


# if (any(duplicated(TSI$Date))) {
#     stop("Duplicate dates in TSI")
# }


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

if (filelist[,sum(!is.na(names))] < 300) {
    stop("Not enough input files found!")
} else {
    cat("Files in the DB:", nrow(filelist), "\n")
}

## find data set files we need to touch
BB <- opendata()

wewantlist <- BB                             |>
    select(Date, TSI_source)                 |>
    filter(TSI_source == "TSIS_adjusted" |
           is.na(TSI_source))                |>
    mutate(month = month(Date),
           year  = year(Date))               |>
    select(TSI_source, month, year)          |>
    unique() |> collect()

## list available TSI data
tsilist <- TSI                       |>
    select(Date, TSI_source)         |>
    mutate(month = month(Date),
           year  = year(Date))       |>
    select(TSI_source, month, year)  |>
    unique()                         |>
    collect()

## list which data set to touch
tsilist    <- data.table(tsilist)
wewantlist <- data.table(wewantlist)
select     <- semi_join(tsilist, wewantlist, by = c("month", "year"))
# select     <- unique(tsilist[wewantlist, .(month, year), on = .(month, year)]) ## this breaks
cat("Months to touch:", nrow(select), "\n")

## touch these files only
filelist <- semi_join(filelist, select,
                      by = join_by(flmonth == month, flyear == year )) |>
    select(names)
# filelist <- filelist[select, on = .(flmonth = month, flyear = year)]
rm(select, wewantlist, tsilist, BB)
dummy <- gc()
cat("Files to do:    ", nrow(filelist), "\n")


##  Update TSI data in DB  -----------------------------------------------------
for (af in filelist$names) {
    datapart <- data.table(read_parquet(af))
    datapart[, month := as.integer(month(Date))]
    datapart[, year  := as.integer(year(Date)) ]
    cat("50 Load: ", af, "\n")

    ## update the whole data part on one go
    datapart <- rows_update(datapart, TSI, by = "Date", unmatched = "ignore")

    ## store actual data
    writePARQUET(x = datapart, sink = af)
    cat("50 Save: ", af, "\n\n")
    ## clean
    rm(datapart)
}


##  Show some info  ------------------------------------------------------------
BB <- opendata()

wehavelist <- BB                    |>
    select(Date, TSI_source)        |>
    mutate(month = month(Date),
           year  = year(Date))      |>
    select(TSI_source, month, year) |>
    unique() |> collect()

wehavelist <- data.table(wehavelist)
setorder(wehavelist, year, month)


cat("\n\n")
pander::pander(wehavelist)
cat("\n\n")


myunlock(DB_lock)


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

