#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Compute
#'
#' Fills with valid only data:
#'
#'
#+ include=T, echo=F

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/tttttttt.R"

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)


TEST_DB <- TRUE

if (TEST_DB) {
    ## copy data to temp
    tyear <- 2022
    system(paste( "cp -rv --update ", DB_HASH_fl, test_DB_HASH_fl))
    system(paste( "cp -rv --update ", DB_META_fl, test_DB_META_fl))
    system(paste0("rsync -avr ", DB_DIR, "/", tyear, "/ ", test_DB_DIR, "/", tyear))
    ## replace paths with test paths
    DB_DIR     <- test_DB_DIR
    DB_lock    <- test_DB_lock
    DB_META_fl <- test_DB_META_fl
    DB_HASH_fl <- test_DB_HASH_fl
}



##  Initialize meta data file  -------------------------------------------------
if (file.exists(DB_META_fl)) {
    BB_meta <- read_parquet(DB_META_fl)
    ## add more days
    BB_meta <- merge(BB_meta,
                     data.table(day = seq(max(BB_meta$day),
                                          Sys.Date(),
                                          by = "day")),
                     by = "day",
                     all = TRUE)
    stopifnot(sum(duplicated(BB_meta$day)) == 0)
} else {
    stop("NO METADATA FILE!!")
}


##  Find data set files to update  ---------------------------------------------

BB <- opendata()

BB |> select(Date, TSI_source) |> filter(TSI_source %in% c(NA, TSIS_adjusted))


stop()


##  Dark calculations on dataset  ----------------------------------------------

## list data base files
filelist <- data.table(
    names = list.files(DB_DIR,
                       pattern = "*.parquet",
                       recursive  = TRUE,
                       full.names = TRUE))
dd      <- dirname(filelist$names)
dd      <- tstrsplit(dd, "/")

filelist$flmonth <- as.numeric(unlist(dd[length(dd)]))
filelist$flyear  <- as.numeric(unlist(dd[length(dd)-1]))

## list data set files to touch
dark_to_do <- BB_meta[chp1_dark_flag %in% c(NA, "MISSING") & !is.na(chp1_basename) & chp1_sig_NAs != 1440]
cat("There are ", nrow(dark_to_do), "days with missing dark\n\n")
cat(format(dark_to_do$day), " ")
cat("\n")

todosets <- unique(rbind(
    dark_to_do[, .(month = month(day), year = year(day))]
))

## select what dataset files to touch
filelist <- filelist[todosets, on = .(flmonth = month, flyear = year)]





rm(todosets, dd)


TSI <- readRDS(COMP_TSI)
TSI <- TSI[ !is.na(TSIextEARTH_comb) ]
names(TSI)[names(TSI) == "sun_dist"         ] <- "Sun_Dist_Astropy"
names(TSI)[names(TSI) == "TSIextEARTH_comb" ] <- "TSI_TOA"
names(TSI)[names(TSI) == "tsi_1au_comb"     ] <- "TSI_1au"
names(TSI)[names(TSI) == "Source"           ] <- "TSI_source"
TSI$measur_error_comb <- NULL





## loop data base files computing black for CHP-1
for (af in filelist$names) {
    datapart <- data.table(read_parquet(af))
    datapart[, month := as.integer(month(Date))]
    datapart[, year  := as.integer(year(Date)) ]


    stop()


    ## use only valid data for dark
    data_use <- datapart[is.na(chp1_bad_data_flag) &
                         !is.na(CHP1_sig) &
                         is.na(CHP1_sig_wo_dark)]

    cat("Load: ", af, "\n")

    ## Ignore bad and missing data
    if (datapart[is.na(chp1_bad_data_flag) & !is.na(CHP1_sig), .N ] == 0) {
        cat("\nNo usefull CHP-1 data in this file\n\n")
        next()
    }


    # ## store actual data
    # write_parquet(x = datapart, sink = af)
    # write_parquet(BB_meta, DB_META_fl)
    # cat("Save: ", af, "\n\n")
    # ## clean
    # rm(datapart, meta_day)

}





# myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
