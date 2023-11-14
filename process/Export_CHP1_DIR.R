# /* #!/opt/R/4.2.3/bin/Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title: "CHP-1 export DNI data for Sirena."
#' author: "Natsis Athanasios"
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
#' - \usepackage{multicol}
#' - \setlength{\columnsep}{1cm}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     keep_md:          no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     fig_width:        7
#'     fig_height:       4.5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#' date: "`r format(Sys.time(), '%F')`"
#' params:
#'    ALL_YEARS: TRUE
#' ---

#+ echo=F, include=T

#+ echo=F, include=T
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "./BBand_LAP/process/Export_CHP1_DIR.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")

options(max.print = 1500)


## data export folder
DATOUT     <- "~/DATA/CHP1_LAP.DIR/"
OTHEREXPOR <- "~/ZHOST/"

MAX_DATE <- as.POSIXct("2023-04-01")
MIN_DATE <- as.POSIXct("2016-01-01")

tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))


## Sun position algorithm as the other broadband
zenangle <- function(YYYY,min,doy){
    as.numeric(
        system(
            paste("~/CM_21_GLB/BINARY/zenangle64 ", YYYY ,min, doy, " 40.634 -22.956"),
            intern = T)
    )
}

## Vectorize zenangle
vzen <- Vectorize(zenangle, "min")

## parallelize zenangle
library(foreach)
library(doMC)
registerDoMC()

pzen <- function(YYYY, min = 1:1440, doy) {
    foreach(min = min, .combine = 'c') %dopar% zenangle(YYYY = YYYY, min = min ,doy = doy)
}


BB <- opendata()

yearstodo <- as.vector(BB |> filter(!is.na(DIR_wpsm)) |> select(year) |> unique() |> collect())
yearstodo <- yearstodo$year

# yearstodo <- 2022

for (YYYY in yearstodo) {
    pdf(file = paste0(DATOUT, "/", "CurrentDataPlots_", YYYY, ".pdf"))

    year_data <- data.table(
        BB |>
            filter(year == YYYY) |>
            select(Date,
                   CHP1_sig,
                   DIR_wpsm,
                   DIR_SD_wpsm,
                   SZA,
                   lap_sza,
                   chp1_bad_data_flag,
                   Async_tracker_flag,
                   ) |>
            collect()
    )

    ## choose date range
    year_data <- year_data[Date < MAX_DATE & Date > MIN_DATE]

    ## Recording limits
    year_data[, sig_lowlim := chp1_signal_lower_limit(Date)]
    year_data[, sig_upplim := chp1_signal_upper_limit(Date)]

    ## Make sure we have used the proper flags
    ## This should not be needed
    year_data$DIR_wpsm   [!is.na(year_data$chp1_bad_data_flag)] <- NA
    year_data$DIR_SD_wpsm[!is.na(year_data$chp1_bad_data_flag)] <- NA
    year_data$DIR_wpsm   [year_data$Async_tracker_flag == TRUE] <- NA
    year_data$DIR_SD_wpsm[year_data$Async_tracker_flag == TRUE] <- NA
    year_data$DIR_wpsm   [year_data$CHP1_sig > year_data$sig_upplim] <- NA
    year_data$DIR_SD_wpsm[year_data$CHP1_sig > year_data$sig_upplim] <- NA
    year_data$DIR_wpsm   [year_data$CHP1_sig < year_data$sig_lowlim] <- NA
    year_data$DIR_SD_wpsm[year_data$CHP1_sig < year_data$sig_lowlim] <- NA

    year_data$chp1_bad_data_flag <- NULL
    year_data$Async_tracker_flag <- NULL
    year_data$sig_lowlim         <- NULL
    year_data$sig_upplim         <- NULL
    year_data$CHP1_sig           <- NULL

    ## drop empty
    year_data <- year_data[!is.na(DIR_wpsm)]

    ## Simple export data for others -------------------------------------------
    # myRtools::write_dat(object  = year_data,
    #                     file    = paste0(OTHEREXPOR, "/", "CHP1_DIR_", YYYY),
    #                     contact = "<natsisphysicist@gmail.com>")


    ## make sure we have all minutes
    allmin <- data.frame(
        Date = seq(as.POSIXct(paste0(min(as.Date(year_data$Date)), " 00:00:30 UTC")),
                   as.POSIXct(paste0(max(as.Date(year_data$Date)), " 23:59:30 UTC")),
                   by = "min"))
    year_data <- merge(year_data, allmin, all.y = TRUE)
    # year_data[, .N, by = as.Date(Date)]

    ## prepare data to export
    year_data$Day <- as.Date(year_data$Date)
    year_data$DIR_wpsm[   is.na(year_data$DIR_wpsm)   ] <- -9L
    year_data$DIR_SD_wpsm[is.na(year_data$DIR_SD_wpsm)] <- -9L
    year_data$SZA[        is.na(year_data$SZA)        ] <- -9L
    ## create receiving folder
    yearfolder <- paste0(DATOUT, "/", YYYY)
    dir.create(yearfolder, showWarnings = F)

    ## Export each day ---------------------------------------------------------
    alldays <- sort(unique(year_data$Day))

    for (dd in alldays) {
        dateD <- as.Date(dd, origin = "1970-01-01")
        cat(sprintf("%s   \r", dateD))

        dayselec <- year_data$Day == dateD
        if (sum(dayselec) != 1440 ) { stop("Day don't have 1440 minutes!!") }

        filename <- paste0(DATOUT, "/", strftime(dateD, format = "%Y/DIR%3j%y.DAT"))
        oneday   <- year_data[dayselec, ]
        TIME_UT  <- as.numeric((oneday$Date - as.POSIXct( dateD ) + 30) / 3600)
        doy      <- yday(dateD)

        ## calculate zenith angle as other instruments
        oneday$lapzen <- pzen(YYYY, 1:1440, doy)

        output <- data.frame(TIME_UT = round(TIME_UT,            digits = 4),
                             SZA     = round(oneday$lapzen,      digits = 2),
                             Wm2     = round(oneday$DIR_wpsm,    digits = 3),
                             st.dev  = round(oneday$DIR_SD_wpsm, digits = 3))

        ## custom file header
        cat(" TIME_UT    SZA    [W.m-2]   st.dev\r\n",
            file = filename)
        ## format and write data
        write.table(format(output,
                           digits    = 3,
                           width     = 8,
                           row.names = FALSE,
                           scietific = FALSE,
                           nsmall    = 2),
                    file      = filename,
                    append    = TRUE,
                    quote     = FALSE,
                    col.names = FALSE,
                    row.names = FALSE,
                    eol = "\r\n")
        ## make sure has consistent line endings
        system(paste("unix2dos ", filename),
               ignore.stdout = TRUE)

        ## create a plot of the output data
        par(mar = c(3,3,1,1))
        par(mgp = c(1.5,.5,0))

        output[output <= -9 ] <- NA

        if (!all(is.na(output$Wm2))) {

            ## test SZAs
            # plot(oneday$SZA, oneday$lapzen)

            plot(output$TIME_UT, output$Wm2, "l",
                 lwd  = 1.5, col = "green",
                 ylab = expression(Watt/m^2),
                 xlab = "Hour (UTC)")
            title(main = paste(dateD), cex.main = .8)

            points( output$TIME_UT, output$st.dev,
                    pch = 18, cex = .4, col = "blue")

            legend("topright", bty = "n",
                   legend = c("Direct Normal Irradiance",
                              "Standard deviation")     ,
                   lty = c( 1, NA),
                   pch = c(NA, 18),
                   col = c("green", "blue"),  cex = 0.8)
            legend("topleft", tag, bty = "n", cex = 0.8)

            ## by sza
            plot(output$SZA, output$Wm2, "l",
                 lwd  = 1.5, col = "green",
                 ylab = expression(Watt/m^2),
                 xlab = "SZA")
            title(main = paste(dateD), cex.main = .8)

            points(output$SZA, output$st.dev,
                   pch = 18, cex = .4 , col = "blue")

            legend("topright", bty = "n",
                   legend = c("Direct Normal Irradiance",
                              "Standard deviation")     ,
                   lty = c( 1, NA),
                   pch = c(NA, 18),
                   col = c("green", "blue"),  cex = 0.8)
            legend("topleft", tag, bty = "n", cex = 0.8)

        }## if data
        rm(output)
    }## alldays
    dev.off()
}



#' **END**
#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
