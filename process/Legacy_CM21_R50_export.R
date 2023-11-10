# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Inspect GHI CHP-1 data **L0** "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Inspect raw data from CM21."
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
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
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
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = "!h"    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/process/Legacy_CM21_R50_export.R"

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/BBand_LAP/functions/Functions_CM21.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)


if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
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
# COMPARE <- FALSE


## years in the data base
datayears <- opendata() |>  select(year) |> unique() |> collect() |> pull() |> sort()

BB_meta  <- read_parquet(DB_META_fl)
BB       <- opendata()

# test
datayears <- 2022

editedyears <- as.vector(na.omit(unique(
    year(BB_meta$day)[year(BB_meta$day) >= year(BB_meta$cm21_parsed)]
)))



## export legacy files
for (YYYY in datayears) {
    ## legacy filename
    legacyout <- paste0("~/DATA/Broad_Band/CM21_H_global/Legacy_LAP_CM21_H_L0_", YYYY, ".Rds")

    ## get relevant data from DB
    year_data <- BB |>
        filter(year == YYYY) |>
        select(c(
            "Date",
            "Azimuth",
            "Elevat",
            "GLB_wpsm",
            "GLB_SD_wpsm",
            "SZA",
            "preNoon",
            "cm21_bad_data_flag"
        )) |> collect()
    year_data <- data.table(year_data)

    if (!file.exists(legacyout) |
        file.mtime(legacyout) < max(BB_meta$cm21_bad_data_flagged, na.rm = TRUE) |
        YYYY %in% editedyears) {
        cat("Will export ", legacyout, "\n")
    } else {
        cat("SKIP export ", legacyout, "\n")
        next()
    }

    ## Apply some filtering ----------------------------------------------------
    cat("\nRemove bad data regions\n")
    cat(year_data[!is.na(cm21_bad_data_flag), .N], year_data[!is.na(GLB_wpsm), .N], "\n\n")
    year_data$GLB_wpsm   [!is.na(year_data$cm21_bad_data_flag)] <- NA
    year_data$GLB_SD_wpsm[!is.na(year_data$cm21_bad_data_flag)] <- NA

    setorder(year_data, Date)

    ## Use the old names for output --------------------------------------------
    names(year_data)[names(year_data) == "GLB_wpsm"]    <- "wattGLB"
    names(year_data)[names(year_data) == "GLB_SD_wpsm"] <- "wattGLB_SD"

    table(year_data$cm21_bad_data_flag)
    year_data$cm21_bad_data_flag <- NULL

    year_data |> glimpse()
    ## Write data to old file format  ------------------------------------------
    year_data <- data.table(year_data)
    write_RDS(object = year_data,
              file   = legacyout)
    rm(year_data)
}
dummy <- gc()

## Old format of CM21_H_L0
# $ Date      : POSIXct, format: "1993-04-12 00:00:30" "1993-04-12 00:01:30" ...
# $ Azimuth   : num  28.5 28.8 29.1 29.4 29.6 ...
# $ Elevat    : num  -36.5 -36.4 -36.3 -36.2 -36.1 ...
# $ QFlag_1   : logi  NA NA NA NA NA NA ...
# $ wattGLB   : num  NA NA NA NA NA NA NA NA NA NA ...
# $ wattGLB_SD: num  NA NA NA NA NA NA NA NA NA NA ...
# $ SZA       : num  126 126 126 126 126 ...
# $ QFlag_2   : Factor w/ 2 levels "NegativeGlobal",..: NA NA NA NA NA NA NA NA NA NA ...
# $ preNoon   : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...


stop()



## Do a data comparison --------------------------------------------------------

## This part is to compare old data production with the new one.
## Will stop work when the new data production is implemented

#+ echo=F, include=T, results="asis"
if (COMPARE) {
    listlegacy <- list.files(path   = "~/DATA/Broad_Band/CM21_H_global/",
                             pattern = "Legacy_LAP_CM21_H_L0_[0-9]{4}\\.Rds",
                             full.names = TRUE, ignore.case = TRUE)

    ## gather remaining data
    gather <- data.table()

    for (alf in listlegacy) {

        ## load new files
        legacy <- readRDS(alf)
        yyyy   <- unique(year(legacy$Date))[1]
        legacy <- legacy[!is.na(CM21value), ]
        legacy$Azimuth        <- NULL
        legacy$Elevat         <- NULL
        # legacy <- legacy[apply(legacy, MARGIN = 1, function(x) sum(is.na(x))) < ncol(legacy) - 1 ]

        ## load old files
        baseDT <- data.table(readRDS(paste0("~/DATA/Broad_Band/CM21_H_signal/LAP_CM21_H_S1_", yyyy, ".Rds")))
        baseDT$Azimuth  <- NULL
        baseDT$Elevat   <- NULL
        baseDT$QFlag_1  <- NULL


        setequal(names(baseDT), names(legacy))

        cat(paste("\n\n##", yyyy, "\n\n"))

        setorder(baseDT, Date)
        setorder(legacy, Date)

        wecare <- names(legacy)
        wecare <- grep("Date", wecare, invert = TRUE, value = TRUE )

        ## merge two streams
        sss <- merge(baseDT, legacy,
                     by = "Date", all = T, suffixes = c(".old", ".new"))

        for (av in wecare) {
            vold <- paste0(av,".old")
            nodl <- paste0(av,".new")

            ## remove identical values
            vec <- sss[[vold]] == sss[[nodl]]
            sss[[vold]][vec] <- NA
            sss[[nodl]][vec] <- NA
            ## remove empty columns
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
        ## remove empty rows
        sss <- sss[apply(sss, MARGIN = 1, function(x) sum(is.na(x))) < ncol(sss) - 1 ]

        wecare <- wecare[paste0(wecare, ".old") %in% names(sss)]

        ## compare remaining data
        for (av in wecare) {
            vold <- paste0(av, ".old")
            nodl <- paste0(av, ".new")



            if (!is.numeric(sss[[vold]])) next()

            ## remove low diff data
            vec <- sss[[vold]] / sss[[nodl]]
            rat <- abs(vec) > 0.996

            sss[[vold]][rat] <- NA
            sss[[nodl]][rat] <- NA


            vec <- sss[[vold]] / sss[[nodl]]

            if (!all(is.na(vec))) {
                hist(vec, breaks = 100)
                summary(vec)

                plot(sss[[vold]], sss[[nodl]],
                     xlab = vold, ylab = nodl)
                title(paste(vold, nodl))
            }

            differ <- 100 * abs(
                (sss[[vold]] - sss[[nodl]]) / sss[[vold]]
            )
            vec    <- differ <  0.1

            if (!all(is.na(differ))) {
                hist(differ, breaks = 100,
                     main = paste("Relative difference" ,vold, nodl))
                summary(differ)
            }

            sss[[vold]][vec] <- NA
            sss[[nodl]][vec] <- NA

            if (!all(is.na(sss[[vold]])) & !all(is.na(sss[[nodl]]))) {
                plot(sss[[vold]], sss[[nodl]],
                     xlab = vold, ylab = nodl)
                title(paste(vold, nodl))
            }

            if (!all(is.na(sss[[vold]]/sss[[nodl]]))) {
                plot(sss$Date, sss[[vold]]/sss[[nodl]],
                     xlab = vold, ylab = nodl)
                title(paste(vold, nodl))
            }
        }

        ## keep non empty
        sss <- sss[apply(sss, MARGIN = 1, function(x) sum(is.na(x))) < ncol(sss) - 1 ]

        source("~/CODE/FUNCTIONS/R/data.R")
        sss <- rm.cols.NA.DT(sss)


        cat(" \n \n")
        cat(paste("\n\n###  Hmisc::describe ", yyyy, "\n\n"))
        cat(" \n \n")

        cat("\n\n```\n")
        cat(print(Hmisc::describe(sss)), sep = "\n")
        cat("```\n\n")

        print(plot(Hmisc::describe(sss)))
        cat("\n\n")

        Hmisc::html(Hmisc::describe(sss))
        cat("\n\n")

        gather <- rbind(gather, sss, fill = TRUE)

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

        cat(" \n \n")
        cat(pander(summary(sss)))
        cat(" \n \n")
    }


    cat(paste("\n\n##  All data summary \n\n"))


    cat(pander(summary(gather)))

    cat("\n\n```\n")
    cat(print(Hmisc::describe(gather)), sep = "\n")
    cat("```\n\n")

    pander(print(Hmisc::describe(gather)))

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
