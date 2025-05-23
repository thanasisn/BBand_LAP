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
#'     keep_md:          no
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
#'
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"  )
knitr::opts_chunk$set(fig.align  = "center")
knitr::opts_chunk$set(fig.cap   = " - empty caption - ")
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
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/process/Legacy_CHP1_L1_export.R"
renv::load("~/BBand_LAP")

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

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




## test
# datayears <- NULL


## export legacy files
for (YYYY in datayears) {
    ## legacy filename
    # legacyout <- paste0("~/DATA/Broad_Band/Legacy_L1_CHP1_", YYYY, ".Rds")
    legacyout <- paste0("~/DATA/Broad_Band/LAP_CHP1_L1_", YYYY, ".Rds")
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

    wecare <- grep("TSI|Astropy|tot_|GLB_|cm21|CM21_|_R_",
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
    names(year_data)[names(year_data) == "DIR_wpsm_temp_cor"]   <- "wattDIR_tmp_cr"
    names(year_data)[names(year_data) == "HOR_wpsm_temp_cor"]   <- "wattHOR_tmp_cr"
    names(year_data)[names(year_data) == "chp1_t_cor_factor"]   <- "chp1TempCF"
    names(year_data)[names(year_data) == "DIR_SD_wpsm"]         <- "wattDIR_sds"
    names(year_data)[names(year_data) == "DIR_wpsm"]            <- "wattDIR"
    names(year_data)[names(year_data) == "HOR_wpsm"]            <- "wattHOR"
    names(year_data)[names(year_data) == "HOR_SD_wpsm"]         <- "wattHOR_sds"
    year_data$DumDarkCHP1 <- year_data$CHP1value - year_data$CHP1_sig_wo_dark
    year_data$Date <- year_data$Date30 - 30

    year_data$CHP1_sig_wo_dark   <- NULL
    year_data$chp1_bad_data_flag <- NULL
    year_data$chp1_bad_temp_flag <- NULL
    year_data$month              <- NULL
    year_data$year               <- NULL
    year_data$doy                <- NULL
    year_data$lap_sza            <- NULL


    year_data |> glimpse()
    ## Write data to old file format  ------------------------------------------
    year_data <- data.table(year_data)
    write_RDS(object = year_data,
              file   = legacyout)

}


## Old format of CHP1 L1
# M DumDarkCHP1   : num  0.000431 0.000431 0.000431 0.000431 0.000431 ...
# G AsynStep      : int  NA NA NA NA NA NA NA NA NA NA ...
# G Async         : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# G Azimuth       : num  53.6 54 54.4 54.8 55.1 ...
# G CHP1sd        : num  0.000948 0.001197 0.001005 0.001126 0.000864 ...
# G CHP1temp      : num  NA NA NA NA NA NA NA NA NA NA ...
# G CHP1tempSD    : num  NA NA NA NA NA NA NA NA NA NA ...
# G CHP1tempUNC   : num  NA NA NA NA NA NA NA NA NA NA ...
# G CHP1value     : num  -6.30e-05 2.90e-04 1.45e-04 -3.05e-05 -1.37e-04 ...
# G Date          : POSIXct, format: "2023-01-01 00:00:00" "2023-01-01 00:01:00" ...
# G Date30        : POSIXct, format: "2023-01-01 00:00:30" "2023-01-01 00:01:30" ...
# G Elevat        : num  -64.3 -64.1 -64 -63.8 -63.7 ...
# G SZA           : num  154 154 154 154 154 ...
# G chp1TempCF    : logi  NA NA NA NA NA NA ...
# G preNoon       : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
# G wattDIR       : num  -0.1232 -0.0352 -0.0713 -0.1151 -0.1417 ...
# G wattDIR_sds   : num  0.236 0.298 0.251 0.281 0.215 ...
# G wattDIR_tmp_cr: logi  NA NA NA NA NA NA ...
# G wattHOR       : num  0.111 0.0317 0.0641 0.1033 0.127 ...
# G wattHOR_sds   : num  -0.213 -0.269 -0.225 -0.252 -0.193 ...
# G wattHOR_tmp_cr: logi  NA NA NA NA NA NA ...
# M Times         : POSIXct, format: "2023-04-06 00:00:30" "2023-04-06 00:01:30" ...
# M rel_Elev      : num  NA NA NA NA NA NA NA NA NA NA ...
# M rel_Time      : num  NA NA NA NA NA NA NA NA NA NA ...
# M wattDIR_unc_NT: num  0.0058 0.00166 0.00336 0.00542 0.00667 ...
# M wattDIR_unc_WT: logi  NA NA NA NA NA NA ...
# M wattHOR_unc_NT: num  0.00522 0.00149 0.00302 0.00486 0.00598 ...
# M wattHOR_unc_WT: logi  NA NA NA NA NA NA ...



## Do a data comparison --------------------------------------------------------

## This part is to compare old data production with the new one.
## Will stop work when the new data production is implemented

#+ echo=F, include=T, results="asis"
if (COMPARE) {
    listlegacy <- list.files(path   = "~/DATA/Broad_Band/",
                             pattern = "Legacy_L1_CHP1_[0-9]{4}\\.Rds",
                             full.names = TRUE, ignore.case = TRUE)

    ## gather remaiining
    gather <- data.table()

    for (alf in listlegacy) {

        ## load new files
        legacy <- readRDS(alf)
        yyyy   <- unique(year(legacy$Date30))[1]
        legacy <- legacy[!is.na(CHP1value),]
        legacy$Azimuth        <- NULL
        legacy$preNoon        <- NULL
        legacy$SZA            <- NULL
        legacy$DumDarkCHP1    <- NULL
        legacy$wattHOR        <- NULL
        legacy$wattDIR        <- NULL
        legacy$wattHOR_sds    <- NULL
        legacy$wattHOR_tmp_cr <- NULL
        legacy$Elevat         <- NULL
        legacy$Date           <- NULL
        legacy <- legacy[apply(legacy, MARGIN = 1, function(x) sum(is.na(x))) < ncol(legacy) - 1 ]
        legacy[is.na(CHP1value), Async    := NA]
        legacy[is.na(CHP1value), AsynStep := NA]

        ## load old files
        baseDT <- data.table(readRDS(paste0("~/DATA/Broad_Band/LAP_CHP1_L1_",yyyy,".Rds")))
        baseDT$Azimuth        <- NULL
        baseDT$preNoon        <- NULL
        baseDT$Elevat         <- NULL
        baseDT$SZA            <- NULL
        baseDT$wattHOR        <- NULL
        baseDT$wattDIR        <- NULL
        baseDT$wattHOR_sds    <- NULL
        baseDT$wattHOR_tmp_cr <- NULL
        baseDT$Date           <- NULL
        baseDT$wattDIR_unc_WT <- NULL
        baseDT$wattHOR_unc_WT <- NULL
        baseDT$wattDIR_unc_NT <- NULL
        baseDT$wattHOR_unc_NT <- NULL
        baseDT$DumDarkCHP1    <- NULL
        baseDT$rel_Time       <- NULL
        baseDT$rel_Elev       <- NULL
        baseDT$Times          <- NULL
        baseDT[is.na(CHP1value), Async    := NA]
        baseDT[is.na(CHP1value), AsynStep := NA]


        setequal(names(baseDT), names(legacy))

        cat(paste("\n\n##", yyyy, "\n\n"))

        setorder(baseDT, Date30)
        setorder(legacy, Date30)

        wecare <- names(legacy)
        wecare <- grep("Date", wecare, invert = TRUE, value = TRUE )

        ## merge two streams
        sss <- merge(baseDT, legacy,
                     by = "Date30", all = T, suffixes = c(".old", ".new"))

        for (av in wecare) {
            vold <- paste0(av,".old")
            nodl <- paste0(av,".new")

            vec <- sss[[vold]] == sss[[nodl]]
            sss[[vold]][vec] <- NA
            sss[[nodl]][vec] <- NA

            if (all(is.na(sss[[vold]])) & all(is.na(sss[[nodl]]))) {
                sss[[vold]] <- NULL
                sss[[nodl]] <- NULL
            }
            # if (!all(is.na(sss[[vold]]))) {
            #     plot(sss$Date30, sss[[vold]], col = "red",
            #          main = vold)
            # }
            # if (!all(is.na(sss[[nodl]]))) {
            #     par(new = T)
            #     plot(sss$Date30, sss[[nodl]], col = "blue",
            #          ylab = nodl)
            # }
        }


        wecare <- wecare[paste0(wecare,".old") %in% names(sss)]

        for (av in wecare) {
            vold <- paste0(av,".old")
            nodl <- paste0(av,".new")

            if (!is.numeric(sss[[vold]])) next()

            ## remove low diff data
            vec <- sss[[vold]]/sss[[nodl]]
            rat <- abs(vec) > 0.996

            sss[[vold]][rat] <- NA
            sss[[nodl]][rat] <- NA


            vec <- sss[[vold]]/sss[[nodl]]

            if (!all(is.na(vec))) {
                hist( vec, breaks = 100 )
                summary( vec )

                plot(sss[[vold]], sss[[nodl]],
                     xlab = vold, ylab = nodl)
            }

            differ <- 100 * abs( (sss[[vold]] - sss[[nodl]]) / sss[[vold]] )
            vec    <- differ <  0.1

            if (!all(is.na(differ))) {
                hist( differ, breaks = 100 )
                summary( differ )
            }

            sss[[vold]][vec] <- NA
            sss[[nodl]][vec] <- NA

            if (!all(is.na(sss[[vold]])) & !all(is.na(sss[[nodl]]))) {
                plot(sss[[vold]], sss[[nodl]],
                     xlab = vold, ylab = nodl)
            }

        }

        ## keep non empty
        sss <- sss[apply(sss, MARGIN = 1, function(x) sum(is.na(x))) < ncol(sss) - 1 ]

        source("~/CODE/FUNCTIONS/R/data.R")
        sss <- rm.cols.NA.DT(sss)

        stop()

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

        # cat("\n\n")
        # cat(paste("\n\n###  compareDF ", yyyy, "\n\n"))
        # cat("\n\n")
        #
        # aa <- compareDF::compare_df(legacy, baseDT,
        #                             group_col = "Date30",
        #                             tolerance = 0.00001)
        #
        # ## remove some data
        # aa$comparison_table_ts2char <- aa$comparison_table_ts2char[
        #     apply(aa$comparison_table_ts2char, MARGIN = 1,
        #           function(x) sum(is.na(x))) < ncol(aa$comparison_table_ts2char) - 2,
        # ]
        #
        # ## remove results for clarity
        # aa$comparison_table_diff_numbers <- NULL
        #
        # aa$comparison_df <- aa$comparison_df[
        #     apply(aa$comparison_df, MARGIN = 1,
        #           function(x) sum(is.na(x))) < ncol(aa$comparison_df) - 2,
        # ]
        #
        # aa$comparison_table_diff <- NULL
        #
        # aa$change_count <- NULL
        #
        # cat("\n\n")
        # cat(pander(aa$change_summary),"\n")
        # cat("\n\n")
        # cat(pander(aa$comparison_df),"\n")
        # cat("\n\n")
        # cat(pander(aa$comparison_table_ts2char),"\n")
        # cat("\n\n")



        # cat(paste("\n\n###  arsenal::comparedf ", yyyy, "\n\n"))
        #
        # ss <- arsenal::comparedf(legacy, baseDT,
        #                          by = "Date30",
        #                          int.as.num = TRUE)
        #
        # ## remove a long table for display
        # ss$frame.summary$unique[[1]] <- ss$frame.summary$unique[[1]][1]
        #
        # cat("\n\n")
        # print(summary(ss))
        # cat("\n\n")




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
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

