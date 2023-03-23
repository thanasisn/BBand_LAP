#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

dd <- ""

## __ Set environment  ---------------------------------------------------------
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/Mark_excluded_data_ranges.R"


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


## Load CHP-1 exclusions -------------------------------------------------------
chp1_exclude_mtime <- file.mtime(CHP1_EXCLUDE)
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
ranges_CHP1$HourSpan <- (as.numeric(ranges_CHP1$Until) - as.numeric(ranges_CHP1$From)) / 3600


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






## Load CM-21 exclusions -------------------------------------------------------
cm21_exclude_mtime <- file.mtime(CM21_EXCLUDE)
ranges_CM21       <- read.table(CM21_EXCLUDE,
                                sep          = ";",
                                colClasses   = "character",
                                strip.white  = TRUE,
                                header       = TRUE,
                                comment.char = "#" )
ranges_CM21$From  <- as.POSIXct(strptime(ranges_CM21$From,  format = "%F %H:%M", tz = "UTC"))
ranges_CM21$Until <- as.POSIXct(strptime(ranges_CM21$Until, format = "%F %H:%M", tz = "UTC"))

## check negative ranges
if (!all((ranges_CM21$Until - ranges_CM21$From) >= 1)) {
    pander(ranges_CM21[ !ranges_CM21$From < ranges_CM21$Until, ])
    stop("Inverted ranges in ", CM21_EXCLUDE, "!!!")
}
## capitalize
ranges_CM21$Comment <- sub("(.)", "\\U\\1", ranges_CM21$Comment, perl = TRUE)
ranges_CM21$Comment[ranges_CM21$Comment == ""] <- "NO DESCRIPTION"
## compute time span
ranges_CM21$HourSpan <- (as.numeric(ranges_CM21$Until) - as.numeric(ranges_CM21$From)) / 3600


#'
#' Check time ranges span in hours
#'
#+ include=T, echo=F

hist(ranges_CM21$HourSpan)
cat('\n\n')

temp <- ranges_CM21[ ranges_CM21$HourSpan > 12 , ]
row.names(temp) <- NULL
pander( temp )

cat('\n\n')
pander(data.table(table(ranges_CM21$Comment)))
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


BB |> glimpse()




##  Create new column if not exist in the dataset  -----------------------------
var <- "chp1_bad_data"
if (!any(names(BB) == var)) {
    cat("Create column  ", var ,"  in dataset\n")
    # BB %>%
    #     mutate(chp1_bad_data = as.character(NA)) %>%
    #     compute() %>%
    #     writedata()
    BB <- BB |> mutate(chp1_bad_data = as.character(NA)) |> compute()
    BB |> writedata()
}
var <- "cm21_bad_data"
if (!any(names(BB) == var)) {
    cat("Create column  ", var ,"  in dataset\n")
    # BB %>%
    #     mutate(cm21_bad_data = as.character(NA)) %>%
    #     compute() %>%
    #     writedata()
    BB <- BB |> mutate(cm21_bad_data = as.character(NA)) |> compute()
    BB |> writedata()
}



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
    var <- "cm21_bad_data_flagged"
    if (!any(names(BB_meta) == var)) {
        BB_meta[[var]] <- NA
        BB_meta[[var]] <- as.POSIXct(BB_meta[[var]])
    }
} else {
    stop("STAR A NEW DB!!")
}







## Flag exclusions by file  ----------------------------------------------------

filelist <- list.files(DB_DIR,
                       pattern = "*.parquet",
                       recursive  = TRUE,
                       full.names = TRUE)

dd      <- dirname(filelist)
dd      <- tstrsplit(dd, "/")

flmonth <- as.numeric(unlist(dd[length(dd)]))
flyear  <- as.numeric(unlist(dd[length(dd)-1]))


BB_meta$chp1_bad_data_flagged

BB_meta$cm21_bad_data_flagged

cm21_exclude_mtime

## compare times remove files form file list

stop()

for (af in filelist) {
    datapart <- read_parquet(af)

    ## flag data
    for (i in 1:nrow(ranges_CHP1)) {
        lower  <- ranges_CHP1$From[   i]
        upper  <- ranges_CHP1$Until[  i]
        comme  <- ranges_CHP1$Comment[i]
        tempex <- data.table(Date = seq(lower + 30, upper - 60 + 30, by = "min"),
                             chp1_bad_data = comme)

        ## mark bad regions of data
        datapart <- rows_update(datapart, tempex, by = "Date", unmatched = "ignore")

        # datapart[Date >= lower & Date < upper, chp1_bad_data := comme]
        rm(tempex)
    }

    for (i in 1:nrow(ranges_CM21)) {
        lower  <- ranges_CM21$From[   i]
        upper  <- ranges_CM21$Until[  i]
        comme  <- ranges_CM21$Comment[i]
        tempex <- data.table(Date = seq(lower + 30, upper - 60 + 30, by = "min"),
                             cm21_bad_data = comme)

        ## mark bad regions of data
        datapart <- rows_update(datapart, tempex, by = "Date", unmatched = "ignore")
        # datapart[Date >= lower & Date < upper, cm21_bad_data := comme]
        rm(tempex)
    }

    unique(datapart$chp1_bad_data)
    unique(datapart$cm21_bad_data)

    chg_days <- unique(as.Date(datapart$Date))

    ## flag metadata
    BB_meta[day %in% chg_days, cm21_bad_data_flagged := cm21_exclude_mtime]
    BB_meta[day %in% chg_days, chp1_bad_data_flagged := chp1_exclude_mtime]

    write_parquet(x = datapart, sink = af)
    write_parquet(BB_meta, DB_META_fl)

    rm(datapart)

    stop()
}






stop()  ###
###
## touch only needed
yearstodo <- unique(year(c(ranges_CHP1$From, ranges_CHP1$Until)))
# for (ay in yearstodo) {
#     for (ami in 1:12) {
#         ami = 5
#         ## pull data to work
#         datapart <- BB %>% filter(year == ay & month == ami) %>% compute()
#         datapart <- as.data.table(datapart)

        for (i in 1:nrow(ranges_CHP1)) {
            lower <- ranges_CHP1$From[   i]
            upper <- ranges_CHP1$Until[  i]
            comme <- ranges_CHP1$Comment[i]
            ## mark bad regions of data
            # datapart[Date >= lower & Date < upper, chp1_bad_data := comme]

            tempex <- data.table(Date = seq(lower, upper - 60, by = "min"))

            yearsvec  <- unique(year(tempex$Date))
            monthsvec <- unique(month(tempex$Date))


            # mutate(BB, chp1_bad_data = base::replace(chp1_bad_data,
            #                                          Date >= lower & Date < upper,
            #                                          comme))
            #
            # mutate(BB, chp1_bad_data = replace(chp1_bad_data,
            #                                      Date >= lower & Date < upper,
            #                                      comme))
            #

            # BB %>%
            # filter(year %in% yearsvec & month %in% monthsvec) %>%
            #     filter(Date >= lower & Date < upper) %>%
            #     filter(is.na(chp1_bad_data)) %>% collect()

            # filter(Date >= lower & Date < upper) %>%
            # filter(!is.na(chp1_bad_data)) %>%  collect()

           # &
           #             Date >= lower & Date < upper &
           #             is.na(chp1_bad_data)) %>%
           #   collect()

            ## update values only part
            # temp <- BB %>%
            #     filter(year %in% yearsvec & Date >= lower & Date < upper & is.na(chp1_bad_data)) %>%
            #     mutate(chp1_bad_data = comme, .keep = "all") %>%
            #     collect()
            #
            # temp <- to_duckdb(as_tibble(temp))
            #
            #
            # DB <- to_duckdb(BB)
            # CC <- rows_update(DB , temp, by = "Date", unmatched = "ignore" ) %>% compute() %>%
            #       to_arrow()

            stop()
            # right_join(BB, as_tibble(temp), by = join_by(Date), keep = T) %>% compute()
            # BB <- left_join(BB, as_tibble(temp), by = join_by(Date)) %>% compute()

            # DF <- mutate(DF, V2 = base::replace(V2, V2 < 4, 0L))
            # tempex <- data.table(Date = seq(lower, upper - 60, by = "min"),
            #                      chp1_bad_data = as.factor(comme))
            # datapart <- datapart %>%
            #     mutate(chp1_bad_data = ifelse((datapart$Date >= lower & datapart$Date < upper),
            #                                    NA,
            #                                    comme) , .keep = "all") %>% collect()
            #
            # stop()
        }


        # as_arrow_table(datapart) %>% filter(!is.na(chp1_bad_data)) %>% collect()

        # aaa<-BB %>% filter(!is.na(chp1_bad_data)) %>% collect()

        ## works but updates file unnecessary
        # datapart <- as_arrow_table(datapart)
        # datapart %>% writedata()

        # datapart %>% collect()
        # datapart %>% writedata()
#
#         stop()
#     }
# }

aaa <- BB %>% filter(!is.na(chp1_bad_data)) %>% collect()
bbb <- CC %>% filter(!is.na(chp1_bad_data)) %>% collect()


stop()
for (i in 1:nrow(ranges_CHP1)) {
    lower <- ranges_CHP1$From[   i]
    upper <- ranges_CHP1$Until[  i]
    comme <- ranges_CHP1$Comment[i]

    ## TODO check those shifts!!!!
    tempex <- data.table(Date = seq(lower + 30, upper - 60 + 30, by = "min"),
                         chp1_bad_data = comme)

    # stop()

    # rows_update(to_duckdb(BB), tempex, by = "Date")

    # BB <- left_join(BB, tempex, by = "Date") %>% compute()

    # BB %>%
    #     filter(Date >= lower & Date < upper) %>%
    #     mutate(chp1_bad_data = (comme)) %>%
    #     collect()
    #
    # BB %>%
    #     filter(Date >= lower & Date < upper) %>%
    #     mutate(chp1_bad_data = (comme)) %>%
    #     compute()


    # BB %>%
    #     filter( Date >= lower & Date < upper ) %>% collect()

    # BB %>% mutate(chp1_bad_data = replace(chp1_bad_data,
    #                                       Date >= lower & Date < upper ,
    #                                       comme))

    ## ~ work??
    BB <- BB %>% mutate(chp1_bad_data = ifelse(Date >= lower & Date < upper,
                                               chp1_bad_data,
                                               comme), .keep = "all") %>% compute()
    # stop()
    ## mark bad regions of data

}



BB %>% filter(is.na(chp1_bad_data)) %>% collect()
BB %>% select(chp1_bad_data) %>% unique() %>% collect()
BB %>% select(cm21_bad_data) %>% unique() %>% collect()

BB %>% select(Date) %>% collect()


BB %>% filter(Date >= lower) %>% collect()

BB |> glimpse()

aa <- data.table(as_tibble(BB))
aa[!is.na(chp1_bad_data)]

data.table(BB)

ss <- BB |> head(n = 10000)

BB %>% filter(is.na(month)) %>% collect()
BB %>% filter(is.na(year)) %>% collect()


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
