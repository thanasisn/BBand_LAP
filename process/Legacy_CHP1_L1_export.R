# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Inspect raw CHP-1 data **SIG** "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Inspect raw data from CHP1."
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#'
#' link-citations:  yes
#' colorlinks:      yes
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        4
#'     fig_width:        8
#'     fig_height:       5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' params:
#'   CLEAN: TRUE
#'
#' ---

#'
#'  **SIG**
#'
#' **Source code: [github.com/thanasisn/BBand_LAP](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [thanasisn.netlify.app/3-data_display/](https://thanasisn.netlify.app/3-data_display/)**
#'
#+ echo=F, include=T





#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""       )
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(fig.pos    = '!h'     )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/process/Legacy_CHP1_L0_export.R"

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")
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

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )



## __ Execution control  -------------------------------------------------------
COMPARE <- TRUE
COMPARE <- FALSE


## years in the data base
datayears <- opendata() |> filter(year >= 2016) |> select(year) |> unique() |> collect() |> pull() |> sort()

BB_meta  <- read_parquet(DB_META_fl)
BB       <- opendata()


# datayears <- 2023

editedyears <- as.vector(na.omit(unique(
    year(BB_meta$day)[year(BB_meta$day) >= year(BB_meta$chp1_parsed)]
)))



# G Date30        : POSIXct, format: "2023-01-01 00:00:30" "2023-01-01 00:01:30" ...
# G CHP1value     : num  -6.30e-05 2.90e-04 1.45e-04 -3.05e-05 -1.37e-04 ...
# G CHP1sd        : num  0.000948 0.001197 0.001005 0.001126 0.000864 ...
# G AsynStep      : int  NA NA NA NA NA NA NA NA NA NA ...
# G Async         : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# G Azimuth       : num  53.6 54 54.4 54.8 55.1 ...
# G Elevat        : num  -64.3 -64.1 -64 -63.8 -63.7 ...
# G CHP1temp      : num  NA NA NA NA NA NA NA NA NA NA ...
# G CHP1tempSD    : num  NA NA NA NA NA NA NA NA NA NA ...
# G CHP1tempUNC   : num  NA NA NA NA NA NA NA NA NA NA ...
# G Date          : POSIXct, format: "2023-01-01 00:00:00" "2023-01-01 00:01:00" ...
# G preNoon       : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
# $ wattDIR_tmp_cr: logi  NA NA NA NA NA NA ...
# $ wattHOR_tmp_cr: logi  NA NA NA NA NA NA ...
# $ chp1TempCF    : logi  NA NA NA NA NA NA ...
# $ wattDIR_unc_WT: logi  NA NA NA NA NA NA ...
# $ wattHOR_unc_WT: logi  NA NA NA NA NA NA ...
# $ SZA           : num  154 154 154 154 154 ...
# $ rel_Time      : num  NA NA NA NA NA NA NA NA NA NA ...
# $ rel_Elev      : num  NA NA NA NA NA NA NA NA NA NA ...
# $ DumDarkCHP1   : num  0.000431 0.000431 0.000431 0.000431 0.000431 ...
# $ wattDIR       : num  -0.1232 -0.0352 -0.0713 -0.1151 -0.1417 ...
# $ wattDIR_sds   : num  0.236 0.298 0.251 0.281 0.215 ...
# $ wattHOR       : num  0.111 0.0317 0.0641 0.1033 0.127 ...
# $ wattHOR_sds   : num  -0.213 -0.269 -0.225 -0.252 -0.193 ...
# $ wattDIR_unc_NT: num  0.0058 0.00166 0.00336 0.00542 0.00667 ...
# $ wattHOR_unc_NT: num  0.00522 0.00149 0.00302 0.00486 0.00598 ...
# $ Times         : POSIXct, format: "2023-04-06 00:00:30" "2023-04-06 00:01:30" ...





## export legacy files
for (YYYY in datayears) {
    ## legacy filename
    legacyout <- paste0("~/DATA/Broad_Band/Legacy_L1_CHP1_", YYYY, ".Rds")
    ## get data from DB
    year_data <- BB |>
        filter(year == YYYY) |>
        # select(c("Date", "CHP1_sig", "CHP1_sig_sd", "Async_step_count",
        #          "Async_tracker_flag", "Azimuth", "Elevat", "chp1_temperature",
        #          "chp1_temperature_SD", "chp1_temp_UNC",
        #          "chp1_bad_data_flag")) |>
        collect()
    year_data <- data.table(year_data)

    if (!file.exists(legacyout) |
        file.mtime(legacyout) < max(BB_meta$chp1_bad_data_flagged, na.rm = T) |
        YYYY %in% editedyears) {
        cat("Will export ", legacyout, "\n")
    } else {
        cat("SKIPPING ", legacyout, "\n")
        next()
    }

    wecare <- grep("TSI|Astropy|tot_|GLB_|cm21|CM21_",
                   names(year_data), value = TRUE, ignore.case = TRUE, invert = TRUE)
    year_data <- year_data[, ..wecare]


    ## Apply some filtering ----------------------------------------------------
    cat("\nRemove bad data regions\n")
    cat(year_data[!is.na(chp1_bad_data_flag), .N], year_data[!is.na(CHP1_sig), .N], "\n\n")
    year_data$CHP1_sig   [!is.na(year_data$chp1_bad_data_flag)] <- NA
    year_data$CHP1_sig_sd[!is.na(year_data$chp1_bad_data_flag)] <- NA

    cat("\nRemove tracker async cases\n")
    cat(year_data[Async_tracker_flag == TRUE, .N], year_data[!is.na(CHP1_sig), .N], "\n\n")
    year_data$CHP1_sig   [year_data$Async_tracker_flag == TRUE] <- NA
    year_data$CHP1_sig_sd[year_data$Async_tracker_flag == TRUE] <- NA

    cat("\nRemove bad temperatures\n")
    year_data[!is.na(chp1_bad_temp_flag), chp1_temperature    := NA]
    year_data[!is.na(chp1_bad_temp_flag), chp1_temperature_SD := NA]
    year_data[!is.na(chp1_bad_temp_flag), chp1_temp_UNC       := NA]

    stop()
    year_data$chp1_bad_data_flag <- NULL



    setorder(year_data, Date)

    ## Use the old names for output --------------------------------------------
    names(year_data)[names(year_data) == "Date"]                <- "Date30"
    names(year_data)[names(year_data) == "CHP1_sig"]            <- "CHP1value"
    names(year_data)[names(year_data) == "CHP1_sig_sd"]         <- "CHP1sd"
    names(year_data)[names(year_data) == "Async_step_count"]    <- "AsynStep"
    names(year_data)[names(year_data) == "Async_tracker_flag"]  <- "Async"
    names(year_data)[names(year_data) == "chp1_temperature"]    <- "CHP1temp"
    names(year_data)[names(year_data) == "chp1_temperature_SD"] <- "CHP1tempSD"
    names(year_data)[names(year_data) == "chp1_temp_UNC"]       <- "CHP1tempUNC"
    year_data$Date <- year_data$Date30 - 30

    year_data |> glimpse()
    ## Write data to old file format  ------------------------------------------
    year_data <- data.table(year_data)
    write_RDS(object = year_data,
              file   = legacyout)

}

## Old format of CHP1 L0
# 'data.frame':	383040 obs. of  11 variables:
# $ Date       : POSIXct, format: "2016-01-22 00:00:00" "2016-01-22 00:01:00"
# $ CHP1value  : num  NA NA NA NA NA NA NA NA NA NA ...
# $ CHP1sd     : num  NA NA NA NA NA NA NA NA NA NA ...
# $ AsynStep   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ Async      : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ Azimuth    : num  45.8 46.2 46.6 47 47.4 ...
# $ Elevat     : num  -63 -62.8 -62.7 -62.6 -62.4 ...
# $ CHP1temp   : num  NA NA NA NA NA NA NA NA NA NA ...
# $ CHP1tempSD : num  NA NA NA NA NA NA NA NA NA NA ...
# $ CHP1tempUNC: num  NA NA NA NA NA NA NA NA NA NA ...
# $ Date30     : POSIXct, format: "2016-01-22 00:00:30" "2016-01-22 00:01:30"





## Do a data comparison --------------------------------------------------------

## This part is to compare old data production with the new one.
## Will stop work when the new data production is implemented

#+ echo=F, include=T, results="asis"
if (COMPARE) {
    listlegacy <- list.files(path   = "~/DATA/Broad_Band/",
                             pattern = "Legacy_L0_CHP1_[0-9]{4}\\.Rds",
                             full.names = TRUE, ignore.case = TRUE)

    gather <- data.table()

    for (alf in listlegacy) {
        ## load new files
        legacy <- readRDS(alf)
        legacy$Azimuth     <- NULL
        legacy$Elevat      <- NULL
        legacy$Date        <- NULL
        legacy <- legacy[apply(legacy, MARGIN = 1, function(x) sum(is.na(x))) < ncol(legacy) - 1 ]

        ## load old files
        yyyy   <- unique(year(legacy$Date30))[1]
        baseDT <- data.table(readRDS(paste0("~/DATA/Broad_Band/LAP_CHP1_L0_",yyyy,".Rds")))
        baseDT$Azimuth     <- NULL
        baseDT$Elevat      <- NULL
        baseDT$Date        <- NULL

        baseDT[Async == TRUE, CHP1value := NA]
        baseDT[Async == TRUE, CHP1sd    := NA]

        baseDT <- baseDT[!(is.na(CHP1value)  &
                               is.na(CHP1sd)     &
                               is.na(CHP1temp)   &
                               is.na(CHP1tempSD) &
                               is.na(AsynStep)   &
                               is.na(CHP1tempUNC)) ]

        cat(paste("\n\n##", yyyy, "\n\n"))

        ## Drop some columns
        baseDT$CHP1temp    <- NULL
        legacy$CHP1temp    <- NULL
        baseDT$CHP1tempSD  <- NULL
        legacy$CHP1tempSD  <- NULL
        baseDT$CHP1tempUNC <- NULL
        legacy$CHP1tempUNC <- NULL

        legacy[Async == FALSE, Async := NA]
        baseDT[Async == FALSE, Async := NA]

        legacy$Date30 <- as.POSIXct(legacy$Date30, tz = "UTC")
        baseDT$Date30 <- as.POSIXct(baseDT$Date30, tz = "UTC")

        # baseDT <- baseDT[!is.na(CHP1value)]
        # legacy <- legacy[!is.na(CHP1value)]

        setorder(baseDT, Date30)
        setorder(legacy, Date30)

        ## merge two streams
        sss <- merge(baseDT, legacy, by = "Date30", all = T, suffixes = c(".old", ".new"))

        ## keep non empty
        sss <- sss[apply(sss, MARGIN = 1, function(x) sum(is.na(x))) < ncol(sss) - 1 ]

        vec <- sss[CHP1value.old == CHP1value.new]
        sss[vec, CHP1value.old := NA ]
        sss[vec, CHP1value.new := NA ]
        plot(  sss$Date30, sss$CHP1value.old, col = "red")
        points(sss$Date30, sss$CHP1value.new, col = "blue")

        vec <- sss[CHP1sd.old == CHP1sd.new]
        sss[vec, CHP1sd.old := NA ]
        sss[vec, CHP1sd.new := NA ]
        # plot(  sss$Date30, sss$CHP1sd.old, col = "red")
        # points(sss$Date30, sss$CHP1sd.new, col = "blue")

        vec <- sss[Async.old == Async.new]
        sss[vec, Async.old := NA ]
        sss[vec, Async.new := NA ]
        # plot(  sss$Date30, sss$Async.old, col = "red")
        # points(sss$Date30, sss$Async.new, col = "blue")

        vec <- sss[AsynStep.old == AsynStep.new]
        sss[vec, AsynStep.old := NA ]
        sss[vec, AsynStep.new := NA ]
        # plot(  sss$Date30, sss$AsynStep.old, col = "red")
        # points(sss$Date30, sss$AsynStep.new, col = "blue")

        # vec <- sss[CHP1temp.old == CHP1temp.new]
        # sss[vec, CHP1temp.old := NA ]
        # sss[vec, CHP1temp.new := NA ]
        # plot(  sss$Date30, sss$CHP1temp.old, col = "red")
        # points(sss$Date30, sss$CHP1temp.new, col = "blue")


        ## keep non empty
        sss <- sss[apply(sss, MARGIN = 1, function(x) sum(is.na(x))) < ncol(sss) - 1 ]

        cat("\n\n")
        cat(paste("\n\n###  Hmisc::describe ", yyyy, "\n\n"))
        cat("\n\n")

        cat("\n\n```\n")
        cat(print(Hmisc::describe(sss)), sep = "\n")
        cat("```\n\n")
        cat("\n\n")
        print(plot(Hmisc::describe(sss)))
        cat("\n\n")
        Hmisc::html(Hmisc::describe(sss))
        cat("\n\n")

        gather <- rbind(gather,sss, fill=T)

        cat("\n\n")
        cat(paste("\n\n###  compareDF ", yyyy, "\n\n"))
        cat("\n\n")

        aa <- compareDF::compare_df(legacy, baseDT,
                                    group_col = "Date30",
                                    tolerance = 0.00001)

        ## remove some data
        aa$comparison_table_ts2char <- aa$comparison_table_ts2char[
            apply(aa$comparison_table_ts2char, MARGIN = 1,
                  function(x) sum(is.na(x))) < ncol(aa$comparison_table_ts2char) - 2,
        ]

        ## remove results for clarity
        aa$comparison_table_diff_numbers <- NULL

        aa$comparison_df <- aa$comparison_df[
            apply(aa$comparison_df, MARGIN = 1,
                  function(x) sum(is.na(x))) < ncol(aa$comparison_df) - 2,
        ]

        aa$comparison_table_diff <- NULL

        aa$change_count <- NULL

        cat("\n\n")
        cat(pander(aa$change_summary),"\n")
        cat("\n\n")
        # cat(pander(aa$comparison_df),"\n")
        cat("\n\n")
        # cat(pander(aa$comparison_table_ts2char),"\n")
        cat("\n\n")



        cat(paste("\n\n###  arsenal::comparedf ", yyyy, "\n\n"))

        ss <- arsenal::comparedf(legacy, baseDT,
                                 by = "Date30",
                                 int.as.num = TRUE)

        ## remove a long table for display
        ss$frame.summary$unique[[1]] <- ss$frame.summary$unique[[1]][1]

        cat("\n\n")
        print(summary(ss))
        cat("\n\n")




        cat(paste("\n\n### NON common data summary ", yyyy, "\n\n"))

        cat("\n\n")
        cat(pander(summary(sss)))
        cat("\n\n")
    }

    cat(paste("\n\n##  All data summary \n\n"))

    pander(summary(gather))

    cat("\n\n```\n")
    cat(print(Hmisc::describe(gather)), sep = "\n")
    cat("```\n\n")
    cat("\n\n")
    print(plot(Hmisc::describe(gather)))
    cat("\n\n")
    Hmisc::html(Hmisc::describe(gather))
    cat("\n\n")
}



#' **END**
#+ include=T, echo=F
# myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
