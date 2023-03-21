#!/usr/bin/env Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */


## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({funr::sys.script()},
                        error = function(e) {
                            cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("CHP1_resistance_to_tem")
                        })

source("~/BBand_LAP/DEFINITIONS.R")
source("~/CHP_1_DIR/Functions_CHP1.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)
on.exit(myunlock(DB_lock))

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)
library(pander)


## CHP-1 exclusions ------------------------------------------------------------

ranges_CHP1       <- read.table(CHP1_EXCLUDE,
                                sep          = ";",
                                colClasses   = "character",
                                strip.white  = TRUE,
                                header       = TRUE,
                                comment.char = "#" )
ranges_CHP1$From  <- as.POSIXct(strptime(ranges_CHP1$From,  format = "%F %H:%M", tz = "UTC"))
ranges_CHP1$Until <- as.POSIXct(strptime(ranges_CHP1$Until, format = "%F %H:%M", tz = "UTC"))

## check negative ranges
if (!all((ranges_CHP1$Until - ranges_CHP1$From) >= 1)) {
    pander(ranges_CHP1[ !ranges_CHP1$From < ranges_CHP1$Until, ])
    stop("Inverted ranges in ", CHP1_EXCLUDE, "!!!")
}
## capitalize
ranges_CHP1$Comment <- sub("(.)", "\\U\\1", ranges_CHP1$Comment, perl = TRUE)
ranges_CHP1$Comment[ranges_CHP1$Comment == ""] <- "NO DESCRIPTION"
## compute time span
ranges_CHP1$HourSpan <- as.numeric(ranges_CHP1$Until - ranges_CHP1$From) / 3600


#'
#' Check time ranges span in hours
#'
#+ include=T, echo=F

hist(ranges_CHP1$HourSpan)
cat('\n\n')

temp <- ranges_CHP1[ ranges_CHP1$HourSpan > 12 , ]
row.names(temp) <- NULL
pander( temp )

cat('\n\n')
pander(data.table(table(ranges_CHP1$Comment)))
cat('\n\n')





## todo
## - create a factor column
## - fill column with data
## - try to fill by year / month?
## - if works move column creation elsewhere


opendata <- function() {
    open_dataset(sources       = DB_DIR,
                 unify_schemas = TRUE,
                 hive_style    = FALSE,
                 partitioning  = c("year", "month"))
}

writedata <- function(.) {
    write_dataset(., path         = DB_DIR,
                  format       = "parquet",
                  partitioning = c("year", "month"),
                  hive_style   = FALSE)
}


BB <- opendata()
# BB %>% writedata()


## create new column if not exist
var <- "chp1_bad_data"
if (!any(names(BB) == var)) {
    BB %>%
        mutate(chp1_bad_data = as.factor(NA)) %>%
        writedata()
}

## touch only needed
yearstodo <- unique(year(c(ranges_CHP1$From, ranges_CHP1$Until)))
for (ay in yearstodo) {
    for (ami in 1:12) {
        ami = 4
        ## pull data to work
        datapart <- BB %>% filter(year == ay & month == ami) %>% compute()
        datapart <- as.data.table(datapart)

        for (i in 1:nrow(ranges_CHP1)) {
            lower <- ranges_CHP1$From[   i]
            upper <- ranges_CHP1$Until[  i]
            comme <- ranges_CHP1$Comment[i]
            ## mark bad regions of data
            datapart[Date >= lower & Date < upper, chp1_bad_data := comme]


            # tempex <- data.table(Date = seq(lower, upper - 60, by = "min"),
            #                      chp1_bad_data = as.factor(comme))
            # dd<-datapart %>%
            #     filter(Date >= lower & Date < upper & is.na(chp1_bad_data)) %>%
            #     mutate(chp1_bad_data = as.factor(comme), .keep = "all") %>% compute()
            # datapart <- datapart %>%
            #     mutate(chp1_bad_data = ifelse((datapart$Date >= lower & datapart$Date < upper),
            #                                    NA,
            #                                    comme) , .keep = "all") %>% collect()
            # datapart <- rows_patch(datapart, tempex, by = "Date", unmatched = "ignore")
            # datapart <- rows_update(datapart, tempex, by = "Date", unmatched = "ignore")
            # datapart <- rows_upsert(datapart, tempex, by = "Date")
            # datapart <- left_join(datapart, tempex, by = Date)
            # datapart %>%
            #     filter(Date >= lower & Date < upper) %>%
            #     mutate(chp1_bad_data = as.factor(comme)) %>%
            #     compute()
            #
            # datapart %>% filter(!is.na(chp1_bad_data)) %>% collect()
            # datapart %>% filter(Date >= lower & Date < upper) %>% collect()
        }


        as_arrow_table(datapart) %>% filter(!is.na(chp1_bad_data)) %>% collect()

        aaa<-BB %>% filter(!is.na(chp1_bad_data)) %>% collect()

        ## works but updates file unnecessary
        datapart <- as_arrow_table(datapart)
        datapart %>% writedata()


        # datapart %>% collect()
        # datapart %>% writedata()

        stop()
    }
}




stop()
for (i in 1:nrow(ranges_CHP1)) {
    lower <- ranges_CHP1$From[   i]
    upper <- ranges_CHP1$Until[  i]
    comme <- ranges_CHP1$Comment[i]


    tempex <- data.table(Date = seq(lower, upper - 60, by = "min"),
                         chp1_bad_data = comme)


    BB %>%
        filter(Date >= lower & Date < upper) %>%
        mutate(chp1_bad_data = as.factor(comme)) %>%
        collect()

    # BB %>%
    #     filter( Date >= lower & Date < upper ) %>% collect()
    #
    # BB %>% mutate(chp1_bad_data = replace(chp1_bad_data,
    #                                       Date >= lower & Date < upper & is.na(chp1_bad_data),
    #                                       comme))
    #
    BB %>% mutate(chp1_bad_data = if_else(Date >= lower && Date < upper && is.na(chp1_bad_data),
                                          "pass",
                                          comme) , .keep = "all") %>% compute()
    # stop()
    ## mark bad regions of data
    # rawdata[Date >= lower & Date < upper, Bad_ranges := comme]
}



BB %>% filter(!is.na(chp1_bad_data)) %>% collect()
BB %>% select(chp1_bad_data) %>% collect()
BB %>% select(Date) %>% collect()


BB %>% filter(Date >= lower) %>% collect()

yearstodo <- unique(year(c(ranges_CHP1$From, ranges_CHP1$Until)))













tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
