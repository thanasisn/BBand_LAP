#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Apply flags on data
#'
#'
#+ include=T, echo=F

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/settname.R"

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)
library(pander,     warn.conflicts = TRUE, quietly = TRUE)




## Create SZA ------------------------------------------------------------------
BB <- opendata()

## init SZA column
var <- "SZA"
if (!any(names(BB) == var)) {
    BB |> mutate("{var}" := 90 - Elevat) |> writedata()
}

BB |> dim()

## TODO test
## remove some data
pp <- BB |> filter(Date > as_datetime("2023-03-02 00:00:00")) |> mutate(SZA = NA)

## recreate table
pp <- pp |> to_duckdb()
CC <- BB |> to_duckdb()
CC <- rows_upsert(CC, pp, by = "Date")

CC |> to_arrow() |> writedata()




left_join(BB, pp, by = "Date")

rows_update(BB, pp)

df1 <- tibble(x = 1:3)
df2 <- tibble(x = c(2, 3, 3), y = c("a", "b", "c"))

out <- nest_join(df1, df2)
out
out$df2

|> collect()

as_datetime("2023-03-02 00:00:00")



stop()

# BB |> select(chp1_bad_data) %>% filter(!is.na(chp1_bad_data)) %>%  collect()
# BB |> select(cm21_bad_data_flag) %>% filter(!is.na(cm21_bad_data_flag)) %>%  collect()
# BB |> select(chp1_temp_bad_data) %>% filter(!is.na(chp1_temp_bad_data)) %>%  collect()


##  Create new column if not exist in the dataset  -----------------------------
var <- "SZA"
if (!any(names(BB) == var)) {
    cat("Create column  ", var ,"  in dataset\n")
    BB <- BB |> mutate(chp1_bad_data = as.character(NA)) |> compute()
    # BB |> writedata()
}
rm(BB)



##  Initialize meta data file  -------------------------------------------------
if (file.exists(DB_META_fl)) {
    BB_meta <- read_parquet(DB_META_fl)
    ## add more days
    BB_meta <- merge(BB_meta,
                     data.table(day = seq(max(BB_meta$day), Sys.Date(),
                                          by = "day")),
                     by = "day",
                     all = TRUE)
    stopifnot(sum(duplicated(BB_meta$day)) == 0)
    ## new columns
    var <- "chp1_bad_data_flagged"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.POSIXct(BB_meta[[var]])
    }
} else {
    stop("STAR A NEW DB!!")
}



## Flag exclusions file by file  -----------------------------------------------
## FIXME should find a better method through arrow dataset

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

## list data set to touch
todosets <- unique(rbind(
    BB_meta[is.na(chp1_bad_data_flagged),
            .(month = month(day), year = year(day))],
    BB_meta[is.na(cm21_bad_data_flagged),
            .(month = month(day), year = year(day))]
))

## select what to touch
filelist <- filelist[todosets, on = .(flmonth = month, flyear = year)]
rm(todosets, dd)


for (af in filelist$names) {
    datapart <- read_parquet(af)
    cat("Load: ", af, "\n")


}

## clean
rm(BB_meta)
rm(filelist)
rm(ranges_CHP1)
rm(ranges_CM21)






myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
