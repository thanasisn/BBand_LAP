# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "CM21 signal process. **S0 -> S1**"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Read signal data and write level 1 data.
#'                 Computes dark."
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
#'     fig_width:        6
#'     fig_height:       4
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#'
#' date: "`r format(Sys.time(), '%F')`"
#' params:
#'    ALL_YEARS: TRUE
#' ---

#'
#' **S0 -> S1**
#'
#' **Source code: [`github.com/thanasisn/CM_21_GLB`](https://github.com/thanasisn/CM_21_GLB)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display/2-cm21_global/`](https://thanasisn.netlify.app/3-data_display/2-cm21_global/)**
#'
#' Read **Signal 0** and compute dark level correction **Signal 1** data
#'
#' - Compute dark when it is feasible
#' - Apply dark correction
#' - Plot daily signal and dark correction on a pdf
#' - Keep dark statistics for use at next stages
#'
#+ echo=F, include=T

warning("Deprecated by ~/BBand_LAP/process/Legacy_CM21_R30_export.R")
stop("No need to run!")

####_  Document options _####

#+ echo=F, include=F
knitr::opts_chunk$set(comment   = ""      )
# knitr::opts_chunk$set(dev       = "pdf"   )
knitr::opts_chunk$set(dev       = "png"    )
knitr::opts_chunk$set(out.width = "100%"   )
knitr::opts_chunk$set(fig.align = "center" )
knitr::opts_chunk$set(fig.pos   = '!h'     )



#+ include=F, echo=F
####  Set environment  ####
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/CM_21_GLB/CM21_R30_Compute_dark.R"

if (!interactive()) {
    pdf( file = paste0("~/CM_21_GLB/RUNTIME/", basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("~/CM_21_GLB/RUNTIME/", basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
    filelock::lock(paste0("~/CM_21_GLB/LOGs/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}


## FIXME this is for pdf output
if (!interactive()) { options(warn = -1) }

#+ echo=F, include=F
####  External code  ####
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(pander,     quietly = TRUE, warn.conflicts = FALSE)
source("~/CM_21_GLB/Functions_dark_calculation.R")
source("~/CM_21_GLB/Functions_write_data.R")

####  Variables  ####
source("~/CM_21_GLB/DEFINITIONS.R")
panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )


####  Execution control  ####
## Default
ALL_YEARS <- FALSE
TEST      <- FALSE
# TEST      <- TRUE
# ALL_YEARS <- TRUE

## When running
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    if (!TEST | any(args == "NOTEST")) { TEST <- FALSE }
    if (any(args == "NOTALL"  )) { ALL_YEARS <- FALSE }
    if (any(args == "ALL"     )) { ALL_YEARS <- TRUE  }
    if (any(args == "ALLYEARS")) { ALL_YEARS <- TRUE  }
}
## When knitting
if (!exists("params")) {
    params <- list( ALL_YEARS = ALL_YEARS)
}
cat(paste("\n**ALL_YEARS:", ALL_YEARS, "**\n"))
cat(paste("\n**TEST     :", TEST,      "**\n"))

tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))

## Standard deviation filter (apply after other filters)
STD_ret_ap_for = 10   ## apply rule when there are enough data points
STD_relMAX     =  1   ## Standard deviation can not be > STD_relMAX * MAX(daily value)


## PATHS
missfiles  <- paste0(BASED, "LOGs/", basename(Script.Name),"_missingfilelist.dat" )
tmpfolder  <- paste0("/dev/shm/", sub(pattern = "\\..*", "" , basename(Script.Name)))
dailyplots <- paste0(BASED,"/REPORTS/", sub(pattern = "\\..*", "" , basename(Script.Name)), "_daily.pdf")
daylystat  <- paste0(dirname(GLOBAL_DIR), "/", sub(pattern = "\\..*", "", basename(Script.Name)), "_stats")


## . Get data input files ------------------------------------------------------
input_files <- list.files( path    = SIGNAL_DIR,
                           pattern = "LAP_CM21_H_S0_[0-9]{4}.Rds",
                           full.names = T )

input_years <- as.numeric(
    sub(".rds", "",
        sub(".*_S0_","",
            basename(input_files),),ignore.case = T))

## . Get storage files ---------------------------------------------------------
output_files <- list.files( path    = SIGNAL_DIR,
                            pattern = "LAP_CM21_H_S1_[0-9]{4}.Rds",
                            full.names = T )

## . Resolve years to do -------------------------------------------------------
if (!params$ALL_YEARS) {
    years_to_do <- c()
    for (ay in input_years) {
        inp <- grep(ay, input_files,  value = T)
        out <- grep(ay, output_files, value = T)
        if ( length(out) == 0 ) {
            ## do if not there
            years_to_do <- c(years_to_do,ay)
        } else {
            ## do if newer data
            if (file.mtime(inp) > file.mtime(out))
                years_to_do <- c(years_to_do,ay)
        }
        years_to_do <- sort(unique(years_to_do))
    }
} else {
    years_to_do <- sort(unique(input_years))
}

## TEST
# if (TEST) {
#     years_to_do <- 1993
# }


## Decide what to do
if (length(years_to_do) == 0 ) {
    stop("NO new data! NO need to parse!")
}
cat(c("\n**YEARS TO DO:", years_to_do, "**\n"))


## Keep record of dark signal
if (file.exists(DARKSTORE)) {
    darkDT <- readRDS(DARKSTORE)
} else {
    darkDT <- data.table()
}
## Read preprepared dark
if (file.exists(DARKCONST)) {
    construct <- readRDS(DARKCONST)
    construct <- construct[!is.na(DARK)]
}



#'
#' Calculate dark signal.
#'
#' Excludes calculation from daily data values where
#' signal standard deviation > `r STD_relMAX` * max(daily signal).
#'
#' Excludes calculation when there are less than `r STD_ret_ap_for`
#' valid data points for a day
#'
#+ include=T, echo=F


#+ include=TRUE, echo=F, results="asis"
panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )
for (yyyy in years_to_do) {

    #### Get raw data ####
    afile <- grep(yyyy, input_files,  value = T)

    ## create a new temp dir for plots
    unlink(tmpfolder, recursive = TRUE)
    dir.create(tmpfolder)
    pbcount <- 0

    #### Get raw data ####
    rawdata        <- readRDS(afile)
    rawdata$day    <- as.Date(rawdata$Date)
    NR_loaded      <- rawdata[ !is.na(CM21value), .N ]

    ##  Get only days with valid data
    daystodo <- rawdata[ , .(N = sum(!is.na(CM21value))), by = .(Days <- as.Date(Date)) ]
    daystodo <- daystodo[ N > 0, Days ]

    ##  Init yearly calculations
    globaldata    <- data.table()
    NR_extreme_SD <- 0

    cat("\\FloatBarrier\n\n")
    cat("\\newpage\n\n")
    cat("\n## Year:", yyyy, "\n\n")

    for (ddd in daystodo) {
        theday      <- as.POSIXct(as.Date(ddd, origin = "1970-01-01"))
        test        <- format(theday, format = "%d%m%y06")
        pbcount     <- pbcount + 1

        daymimutes  <- data.frame(
            Date = seq( as.POSIXct(paste(as.Date(theday), "00:00:30")),
                        as.POSIXct(paste(as.Date(theday), "23:59:30")), by = "min"  )
        )

        ## This is a workaround for BBand_LAP!
        if (!any(grepl("QFlag_1", names(rawdata)))) {
            rawdata$QFlag_1 <- NA
        }

        ## get all day
        wholeday    <- rawdata[ day == as.Date(theday) ]
        ## use only valid data for dark
        daydata     <- rawdata[ day == as.Date(theday) & is.na(QFlag_1) ]

        ## fill all minutes for nicer graphs
        daydata     <- merge(daydata, daymimutes, all = T)
        daydata$day <- as.Date(daydata$Date)


        ####  Filter Standard deviation extremes  ##############################
        ## SD can not be greater than the signal
        pre_count     <- daydata[ !is.na(CM21value), .N ]
        ## apply rule if there are enough data
        if (pre_count > STD_ret_ap_for) {
            daydata       <- daydata[ CM21sd < STD_relMAX * max(CM21value, na.rm = T) ]
            NR_extreme_SD <- NR_extreme_SD + pre_count - daydata[ !is.na(CM21value), .N ]
            if (nrow(daydata[ !is.na(CM21value) ]) < 1) {
                cat('\n')
                cat(paste(theday, "SKIP DAY: No data after extreme SD filtering!!"),"\n\n")
                next()
            }
        }
        ########################################################################



        ####    Calculate Dark signal   ########################################
        dark_day <- dark_calculations(dates      = daydata$Date,
                                      values     = daydata$CM21value,
                                      elevatio   = daydata$Eleva,
                                      nightlimit = DARK_ELEV,
                                      dstretch   = DSTRETCH)



        # if ( is.na(dark_day$Mmed) & is.na(dark_day$Emed) ) {
        if (! ((!is.na(dark_day$Mmed) & dark_day$Mcnt >= DCOUNTLIM) |
               (!is.na(dark_day$Emed) & dark_day$Ecnt >= DCOUNTLIM))) {
            # cat("Can not apply dark\n")
            todays_dark_correction <- NA
            dark_flag              <- "MISSING"
            missingdark            <- NA

            ## get dark from pre-computed file
            if (exists("construct")) {
                ## can not find date
                if (! theday %in% construct$Date) {
                    todays_dark_correction <- NA
                    dark_flag              <- "MISSING"
                    missingdark            <- NA
                } else {
                    ## get data from recomputed dark database
                    todays_dark_correction <- construct[ Date == theday, DARK]
                    dark_flag              <- "CONSTRUCTED"
                }
            }
        } else {
            ####    Dark Correction function   #################################
            dark_generator <- dark_function(dark_day    = dark_day,
                                            DCOUNTLIM   = DCOUNTLIM,
                                            type        = "median",
                                            adate       = theday ,
                                            test        = test,
                                            missfiles   = missfiles,
                                            missingdark = missingdark )

            ####    Create dark signal for correction    #######################
            todays_dark_correction <- dark_generator(daydata$Date)
            dark_flag              <- "COMPUTED"
        }

        ####    Apply dark correction    #######################################
        daydata[, CM21valueWdark := CM21value - todays_dark_correction ]

## Deprecated!! ##
#         ## plot to external pdf
#         pdf(file = paste0(tmpfolder,"/daily_", sprintf("%05d.pdf", pbcount)), )
#         if (any(grepl( "CM21valueWdark", names(daydata)))) {
#             ## plot night
#             somedata <- daydata[ Elevat < 1 & !is.na(CM21value) ]
#             if (nrow(somedata)>0 & any(!is.na(somedata$CM21valueWdark))) {
#                 ylim <- range(somedata$CM21valueWdark, somedata$CM21value)
#                 plot(  somedata$Date, somedata$CM21value,
#                        pch  = 19, cex = 0.5,
#                        ylab = "CHP1 Signal V", xlab = "", ylim = ylim)
#                 points(somedata$Date, somedata$CM21valueWdark,pch=19,cex=0.5, col = "blue")
#                 abline(h=0,col="orange")
#                 title(main = paste(test, format(daydata$Date[1], format = "%F"), dark_flag))
#                 text(somedata$Date[1], ylim[2], labels = tag, pos = 4, cex =.9)
#                 ## or plot day
#             } else {
#                 somedata <- daydata[ Elevat >=1 & !is.na(CM21value) ]
#                 if (nrow(somedata)>0 & any(!is.na(somedata$CM21valueWdark))) {
#                     ylim <- range(somedata$CM21valueWdark, somedata$CM21value)
#                     plot(  somedata$Date, somedata$CM21value,
#                            pch = 19, cex = 0.5,
#                            ylab = "CHP1 Signal V", xlab = "", ylim = ylim)
#                     points(somedata$Date, somedata$CM21valueWdark,
#                            pch = 19, cex = 0.5, col = "blue")
#                     abline(h=0,col="orange")
#                     title(main = paste(test, format(daydata$Date[1], format = "%F"), dark_flag))
#                     text(somedata$Date[1], ylim[2], labels = tag, pos = 4, cex =.9)
#                 } else {
#                     ## or plot empty for consistency
#                     plot.new()
#                     title(main = paste(test, format(daydata$Date[1], format = "%F"), "NOT PLOTABLE"))
#                 }
#             }
#         }
#         dev.off()
## Deprecated!! ##


        ####    Day stats    ###################################################
        day <- data.frame(Date      = theday,
                          NAs       = sum(is.na(daydata$CM21value)),
                          SunUP     = sum(      daydata$Eleva >= 0 ),
                          Dmean     = mean(     todays_dark_correction,na.rm = T),
                          sunMeas   = sum(      daydata$Eleva >= 0 &
                                                    !is.na(daydata$CM21value)),
                          dark_flag = dark_flag,
                          CalcDate  = Sys.time()
        )
        darkDT <- rbind( darkDT, cbind(day, dark_day), fill=T)



        ####    Get processed and unprocessed data    ##########################
        daydata    <- merge( daydata, wholeday,
                             by = intersect(names(daydata),names(wholeday)), all = T)

        globaldata <- rbind( globaldata, daydata, fill = TRUE )


        rm( theday, todays_dark_correction, dark_generator, day, daydata, dark_flag )

    } #END loop of days

    ## write all days with data
    globaldata$day        <- NULL
    globaldata$sig_lowlim <- NULL
    globaldata$sig_upplim <- NULL
    if (!TEST) {
        write_RDS(object = globaldata,
                  file   = paste0(SIGNAL_DIR,"/LAP_CM21_H_S1_",yyyy,".Rds") )
    }

    ## partial write most recent stats
    if (!TEST) {
        darkDT <- darkDT[ , .SD[which.max(CalcDate)], by = Date ]
        write_RDS(object = darkDT, file = DARKSTORE)
    }

## Deprecated!! ##
#     ## create pdf with all daily plots
#     system(paste0("pdftk ", tmpfolder, "/daily*.pdf cat output ",
#                   paste0(DAILYgrDIR,"CM21_dark_daily_",yyyy,".pdf")),
#            ignore.stderr = T )
## Deprecated!! ##


    cat(paste0("**",
               NR_loaded, "** non NA data points loaded\n\n" ))
    cat(paste0("**",
               NR_extreme_SD, "** points excluded from dark calculation due to extreme SD\n\n" ))


    cat('\\scriptsize\n\n')
    # cat('\\footnotesize\n')
    cat(pander( summary(globaldata[,!c("Date","Azimuth","QFlag_1")]) ))
    cat('\\normalsize\n\n')


    # hist(globaldata$CM21value, breaks = 50, main = paste("CM21 GHI ", yyyy ) )
    # cat('\n\n')

    # hist(globaldata$CM21valueWdark, breaks = 50, main = paste("CM21 SIG w Dark ", yyyy ) )
    # cat('\n\n')

    # hist(globaldata$CM21sd,    breaks = 50, main = paste("CM21 SIG SD", yyyy ) )
    # cat('\n\n')

    # plot(globaldata$Elevat, globaldata$CM21valueWdark, pch = 19, cex = .8,
    #      main = paste("CM21 SIG w Dark ", yyyy ),
    #      xlab = "Elevation",
    #      ylab = "CM21 GHI" )
    # cat('\n\n')

    # plot(globaldata$Elevat, globaldata$GLstd,    pch = 19, cex = .8,
    #      main = paste("CM21 GHI SD", yyyy ),
    #      xlab = "Elevation",
    #      ylab = "CM21 GHI Standard Deviations")
    # cat('\n\n')


    cat('\n\n')

    statist <- darkDT[ year(Date) == yyyy ]

    cat(paste("#### Summary of daily Dark\n\n"))

    hist(statist$sunMeas, main = paste("Sun measurements"    , yyyy), breaks = 50)
    hist(statist$Mavg,    main = paste("Morning Average Dark", yyyy), breaks = 50)
    hist(statist$Mmed,    main = paste("Morning Median Dark" , yyyy), breaks = 50)
    hist(statist$Mcnt,    main = paste("Morning count Dark"  , yyyy), breaks = 50)
    hist(statist$Eavg,    main = paste("Evening Average Dark", yyyy), breaks = 50)
    hist(statist$Emed,    main = paste("Evening Median Dark" , yyyy), breaks = 50)
    hist(statist$Ecnt,    main = paste("Evening count Dark"  , yyyy), breaks = 50)

    plot(statist$Date, statist$sunMeas, "p", pch = 16, cex = .5, main = paste("Sun measurements"    , yyyy), xlab = "" )
    plot(statist$Date, statist$SunUP,   "p", pch = 16, cex = .5, main = paste("Sun up measurements" , yyyy), xlab = "" )
    plot(statist$Date, statist$Mavg,    "p", pch = 16, cex = .5, main = paste("Morning Average Dark", yyyy), xlab = "" )
    plot(statist$Date, statist$Mmed,    "p", pch = 16, cex = .5, main = paste("Morning Median Dark" , yyyy), xlab = "" )
    plot(statist$Date, statist$Mcnt,    "p", pch = 16, cex = .5, main = paste("Morning count Dark"  , yyyy), xlab = "" )
    plot(statist$Date, statist$Eavg,    "p", pch = 16, cex = .5, main = paste("Evening Average Dark", yyyy), xlab = "" )
    plot(statist$Date, statist$Emed,    "p", pch = 16, cex = .5, main = paste("Evening Median Dark" , yyyy), xlab = "" )
    plot(statist$Date, statist$Ecnt,    "p", pch = 16, cex = .5, main = paste("Evening count Dark"  , yyyy), xlab = "" )

    cat(paste("#### Days with Evening dark data points count < 100\n"))
    cat('\n\n')
    cat(paste(unique(as.Date( statist$Date[ which(statist$Ecnt < 100 )]))),"\n\n")
    cat('\n\n')

    cat(paste("#### Days with Morning dark data points count < 50\n"))
    cat('\n\n')
    cat(paste(unique( as.Date( statist$Date[ which(statist$Mcnt < 50 ) ]))),"\n\n")
    cat('\n\n')

    cat(paste("#### Days with ( sun  measurements / sun up ) < 0.2\n"))
    cat('\n\n')
    cat(paste(unique( as.Date( statist$Date[ which(statist$sunMeas/statist$SunUP < .20 )]))),"\n\n")
    cat('\n\n')

    cat(paste("#### Day with the minimum morning median dark\n"))
    cat('\n\n')
    cat(paste(statist$Date[ which.min( statist$Mmed ) ]),"\n\n")
    cat('\n\n')

}
#'


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
if (interactive()) beepr::beep(7)
