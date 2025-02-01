# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Read raw CM21 data. **LAP -> SIG** "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Combine raw data from CM21 to yearly data sets."
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
#'     fig_width:        8
#'     fig_height:       5
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
#'  **LAP -> SIG**
#'
#' **Source code: [`github.com/thanasisn/CM_21_GLB`](https://github.com/thanasisn/CM_21_GLB)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display/2-cm21_global/`](https://thanasisn.netlify.app/3-data_display/2-cm21_global/)**
#'
#' Read **LAP files** with CM_21 signal data to **Signal** rds files
#'
#'   - Lists Sirena files
#'   - Lists Radmon files
#'   - Reads data from Sirena only
#'   - Adds sun position
#'   - NO FILTERING
#'   - Store as .Rds binaries files for further use
#'   - Works for a set date range
#'
#+ echo=F, include=T

warning("Deprecated by ./BBand_LAP/process/Legacy_CM21_R10_export.R")
stop("No need to run!")

####_  Document options _####

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""      )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"    )
knitr::opts_chunk$set(out.width  = "100%"   )
knitr::opts_chunk$set(fig.align  = "center" )
knitr::opts_chunk$set(fig.pos    = '!h'     )



#+ include=F, echo=F
####  Set environment  ####
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("CM21_R10_") })
if (!interactive()) {
    pdf( file = paste0("~/CM_21_GLB/RUNTIME/", basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("~/CM_21_GLB/RUNTIME/", basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
    filelock::lock(paste0("~/CM_21_GLB/LOGs/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}


#+ echo=F, include=F
####  External code  ####
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(pander,     quietly = TRUE, warn.conflicts = FALSE)
source("~/CM_21_GLB/Functions_write_data.R")
source("~/CM_21_GLB/Functions_CM21_factor.R")


####  Variables  ####
source("~/CM_21_GLB/DEFINITIONS.R")
panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )

OutliersPlot <- 4


## read existing TOT files form sirena
extra <- readRDS("~/DATA/Broad_Band/CM21_TOT.Rds")




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



#+ include=TRUE, echo=FALSE, results = 'asis'

## Source data files for import from storage
sirena_files <- list.files(path        = SIRENA_DIR,
                           recursive   = TRUE,
                           pattern     = "[0-9]*06.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:",paste(length(sirena_files), "files from Sirena**\n"))
## just in case, there are nested folders with more lap files in Sirens
sirena_files <- grep("OLD", sirena_files, ignore.case = T, invert = T, value = T )



#'
#+ include=TRUE, echo=FALSE

####  Read files of all yearsi ####

## TEST
# START_DAY  <- as.POSIXct("2004-01-01 00:00:00 UTC")
# END_DAY    <- as.POSIXct("2004-01-01 00:00:00 UTC")

## all allowed years
years_to_do <- format(seq(START_DAY, END_DAY, by = "year"), "%Y")




#'
#' Allowed years to do: `r years_to_do`
#'
#+ include=TRUE, echo=FALSE

####  Check for new data to parse  ####
if (!params$ALL_YEARS) {
    NEWDATA            <- FALSE
    sirena_files_dates <- file.mtime(sirena_files)
    storagefiles       <- list.files(SIGNAL_DIR, "LAP_CM21_H_SIG.*.rds",
                                     full.names  = TRUE,
                                     ignore.case = TRUE)
    last_storage_date  <- max(file.mtime(storagefiles))
    newfiles           <- sirena_files[sirena_files_dates > last_storage_date]

    ## check years stored
    storage_years <- as.numeric(
        sub(".rds", "",
            sub(".*_SIG_","",
                basename(storagefiles),),ignore.case = T))
    missing_years <- years_to_do[!years_to_do %in% storage_years]

    ## check new data
    new_to_do <- c()
    if (length(newfiles) > 0) {
        ## find years to do
        newyears <- unique(
            year(
                strptime(
                    sub("06\\.lap", "", basename(newfiles), ignore.case = T),
                    "%d%m%y")))
        new_to_do <- years_to_do[years_to_do %in% newyears]
        NEWDATA   <- TRUE
    }

    # missing_years <- 2015:2022

    ## Decide what to do
    if (TEST | length(missing_years) != 0 | NEWDATA) {
        years_to_do <- sort(unique(c(missing_years, new_to_do)))
    } else {
        stop("NO new data! NO need to parse!")
    }
}
#+ include=TRUE, echo=FALSE
cat(c("\n**YEARS TO DO:", years_to_do, "**\n"))


## TEST
if (TEST) {
    years_to_do <- 2022
    years_to_do <- c(1995, 2015, 2022)
    warning("Overriding years to do: ", years_to_do)
}






## Loop year to do -------------------------------------------------------------

#'
#' Years to do: `r years_to_do`
#'
#+ include=TRUE, echo=FALSE, results="asis"
for (YYYY in years_to_do) {
    yy           <- substr(YYYY, 3, 4)
    year_data    <- data.table()
    days_of_year <- seq.Date(as.Date(paste0(YYYY,"-01-01")),
                             as.Date(paste0(YYYY,"-12-31")), by = "day")

    cat("\n\n\\FloatBarrier\n\n")
    cat("\\newpage\n\n")
    cat("\n## Year:", YYYY, "\n\n" )

    missing_files <- c()
    for (aday in days_of_year) {
        aday  <- as.Date(aday, origin = "1970-01-01")
        sunfl <- paste0(SUN_FOLDER, "sun_path_", format(aday, "%F"), ".dat.gz")

        found <- grep( paste0( "/",YYYY,"/", format(aday, "%d%m%y06") ), sirena_files, ignore.case = T )
        ## check file names
        if (length(found) > 1) {
            stop("Found more file than we should") }
        if (length(found) == 0) {
            missing_files <- c(missing_files, paste0(YYYY,"/", format(aday, "%d%m%y06")))
            cat(paste0(YYYY,"/", format(aday, "%d%m%y06")), sep = "\n",
                file = MISSING_INP, append = T )
            next()
        }

        ## recreate time stamp for all minutes of day
        suppressWarnings(rm(D_minutes))
        D_minutes <- seq(from       = as.POSIXct(paste(aday,"00:00:30 UTC")),
                         length.out = 1440,
                         by         = "min" )

        ## __  Read LAP file  --------------------------------------------------
        lap <- fread(sirena_files[found], na.strings = "-9")
        lap$V1 <- as.numeric(lap$V1)
        lap$V2 <- as.numeric(lap$V2)
        stopifnot(is.numeric(lap$V1))
        stopifnot(is.numeric(lap$V2))
        stopifnot(dim(lap)[1] == 1440)
        lap[V1 < -8, V1 := NA]
        lap[V2 < -8, V2 := NA]


        ## __  Read SUN file  --------------------------------------------------
        if (!file.exists(sunfl)) stop(cat(paste("Missing:", sunfl, "\nRUN! Sun_vector_construction_cron.py\n")))
        sun_temp <- read.table(sunfl,
                               sep         = ";",
                               header      = TRUE,
                               na.strings  = "None",
                               strip.white = TRUE,
                               as.is       = TRUE)

        ##  Day data table to save
        day_data <- data.table(Date        = D_minutes,      # Date of the data point
                               CM21value   = lap$V1,         # Raw value for CM21
                               CM21sd      = lap$V2,         # Raw SD value for CM21
                               Azimuth     = sun_temp$AZIM,  # Azimuth sun angle
                               Elevat      = sun_temp$ELEV ) # Elevation sun angle

        ## __ Gather day data  -------------------------------------------------
        year_data <- rbind( year_data, day_data )
    }
    ## order data
    setorder(year_data,Date)

    ## check there are not duplicate dates read from raw
    testsanity <- year_data[, .N , by = as.Date(Date)]
    if (!all( testsanity$N == 1440 )) {
        cat("\n**There are days with not exactly 1440 minutes**\n\n")
        cat('\n\n')
        pander(testsanity[N != 1440])
        cat('\n\n')
    } else {
        cat("\n**All days have exactly 1440 minutes**\n\n")
    }

    ## print missing files
    cat("\n**Missing whole day files:**\n\n")
    cat(missing_files,sep = "\n\n")


    # #### . . Add all the minutes of the year
    # all_min   <- seq(as.POSIXct(paste0(YYYY,"-01-01 00:00:30")),
    #                  as.POSIXct(paste0(YYYY,"-12-31 23:59:30")), by = "mins")
    # all_min   <- data.frame(Date = all_min)
    # year_data <- merge(year_data, all_min, all = T)

    ## __ Add signal limits on plots -------------------------------------------
    year_data[ , sig_lowlim := cm21_signal_lower_limit(Date)]
    year_data[ , sig_upplim := cm21_signal_upper_limit(Date)]




    ####    Yearly Plots    ####################################################


    ## __ Do some plots for this year before filtering  -----------------------
    suppressWarnings({
        ## Try to find outliers
        yearlims <- data.table()
        for (an in grep("CM21", names(year_data), value = T)){
            daily <- year_data[ , .(dmin =  min(get(an),na.rm = T),
                                    dmax =  max(get(an),na.rm = T) ),
                                by = as.Date(Date)]
            low <- daily[ !is.infinite(dmin) , mean(dmin) - OutliersPlot * sd(dmin)]
            upe <- daily[ !is.infinite(dmax) , mean(dmax) + OutliersPlot * sd(dmax)]
            yearlims <- rbind(yearlims,  data.table(an = an,low = low, upe = upe))
        }
    })

    cat("\n\n### Proposed outliers limits \n")
    cat("\n\n")
    cat(pander(yearlims))
    cat("\n\n")


    # cat('\\scriptsize\n')
    # cat(pander( summary(year_data[,-c('Date','Azimuth')]) ))
    # cat('\\normalsize\n')

    cat('\n\n')

    hist(year_data$CM21value, breaks = 30, main = paste("CM-21 signal ",  YYYY))
    cat('\n\n')

    hist(year_data$CM21sd,    breaks = 30, main = paste("CM-21 signal SD",YYYY))
    cat('\n\n')

    ylim <- range(year_data$sig_lowlim,
                  year_data$sig_upplim,
                  year_data$CM21value,
                  na.rm = TRUE)

    plot(year_data$Elevat, year_data$CM21value, pch = 19, cex = .5,
         main = paste("CM-21 signal ", YYYY),
         xlab = "Elevation",
         ylab = "CM-21 signal",
         ylim = ylim)
    points(year_data$Elevat, year_data$sig_lowlim, pch = ".", col = "red")
    points(year_data$Elevat, year_data$sig_upplim, pch = ".", col = "red")
    cat('\n\n')


    plot(year_data$Date, year_data$CM21value, pch = 19, cex = .5,
         main = paste("CM-21 signal ", YYYY),
         xlab = "Elevation",
         ylab = "CM-21 signal",
         ylim = ylim)
    points(year_data$Date, year_data$sig_lowlim, pch = ".", col = "red")
    points(year_data$Date, year_data$sig_upplim, pch = ".", col = "red")
    ## plot config changes
    abline(v = signal_physical_limits$Date, lty = 3)
    cat('\n\n')


    all    <- cumsum(tidyr::replace_na(year_data$CM21value, 0))
    pos    <- year_data[ CM21value > 0 ]
    pos$V1 <- cumsum(tidyr::replace_na(pos$CM21value, 0))
    neg    <- year_data[ CM21value < 0 ]
    neg$V1 <- cumsum(tidyr::replace_na(neg$CM21value, 0))
    xlim   <- range(year_data$Date)
    plot(year_data$Date, all,
         type = "l",
         xlim = xlim,
         ylab = "",
         yaxt = "n", xlab = "",
         main = paste("Cum Sum of CM-21 signal ",  YYYY) )
    par(new = TRUE)
    plot(pos$Date, pos$V1,
         xlim = xlim,
         col = "blue", type = "l",
         ylab = "", yaxt = "n", xlab = "", xaxt = "n")
    par(new = TRUE)
    plot(neg$Date, neg$V1,
         xlim = xlim,
         col = "red", type = "l",
         ylab = "", yaxt = "n", xlab = "", xaxt = "n")
    cat('\n\n')


    all    <- cumsum(tidyr::replace_na(year_data$CM21sd, 0))
    pos    <- year_data[ CM21value > 0 ]
    pos$V1 <- cumsum(tidyr::replace_na(pos$CM21sd, 0))
    neg    <- year_data[ CM21value < 0 ]
    neg$V1 <- cumsum(tidyr::replace_na(neg$CM21sd, 0))
    xlim   <- range(year_data$Date)
    plot(year_data$Date, all,
         type = "l",
         xlim = xlim,
         ylab = "",
         yaxt = "n", xlab = "",
         main = paste("Cum Sum of CM-21 sd ",  YYYY) )
    par(new = TRUE)
    plot(pos$Date, pos$V1,
         xlim = xlim,
         col = "blue", type = "l",
         ylab = "", yaxt = "n", xlab = "", xaxt = "n")
    par(new = TRUE)
    plot(neg$Date, neg$V1,
         xlim = xlim,
         col = "red", type = "l",
         ylab = "", yaxt = "n", xlab = "", xaxt = "n")
    cat('\n\n')


    ## __ Plots of exceptions for investigation  -------------------------------

    ## ____ 1995 ---------------------------------------------------------------
    if (YYYY == 1995) {
        cat("\n### Year:", YYYY, " exceptions \n\n" )

        part <- year_data[ Date > as.POSIXct("1995-10-8") &
                           Date < as.POSIXct("1995-11-15") ]
        plot(part$Date, part$CM21value, pch = ".", ylim = c(-2,3))
        points(part$Date, part$sig_lowlim, pch = ".", col = "red")
        points(part$Date, part$sig_upplim, pch = ".", col = "red")
        ## plot config changes
        abline(v = signal_physical_limits$Date, lty = 3)

        testdata <- extra[ Date > as.POSIXct("1995-10-8") &
                           Date < as.POSIXct("1995-11-15") ]
        points(testdata$Date, testdata$WATTTOT / cm21factor(testdata$Date), pch = ".", col = "cyan")
        cat('\n\n')

        part <- year_data[ Date > as.POSIXct("1995-11-15") &
                           Date < as.POSIXct("1995-12-31") ]
        plot(part$Date, part$CM21value, pch = ".", ylim = c(-1,2))
        points(part$Date, part$sig_lowlim, pch = ".", col = "red")
        points(part$Date, part$sig_upplim, pch = ".", col = "red")
        ## plot config changes
        abline(v = signal_physical_limits$Date, lty = 3)

        testdata <- extra[ Date > as.POSIXct("1995-11-15") &
                           Date < as.POSIXct("1995-12-31") ]
        ## reverse sirena TOT to signal
        points(testdata$Date, testdata$WATTTOT / cm21factor(testdata$Date), pch = ".", col = "cyan")
        cat('\n\n')

    }


    ## ____ 1996 ---------------------------------------------------------------
    if (YYYY == 1996) {
        cat("\n### Year:", YYYY, " exceptions \n\n" )

        part <- year_data[ Date > as.POSIXct("1996-02-01") &
                           Date < as.POSIXct("1996-03-7") ]
        plot(part$Date, part$CM21value, pch = ".", ylim = c(-1,2))
        points(part$Date, part$sig_lowlim, pch = ".", col = "red")
        points(part$Date, part$sig_upplim, pch = ".", col = "red")
        abline(v=as.POSIXct("1996-2-08"))
        abline(v=as.POSIXct("1996-2-29 12:00"))

        testdata <- extra[Date > as.POSIXct("1996-02-01") &
                          Date < as.POSIXct("1996-03-7") ]
        ## reverse sirena TOT to signal
        points(testdata$Date, testdata$WATTTOT / cm21factor(testdata$Date), pch = ".", col = "cyan")

    }

    ## ____ 2004 ---------------------------------------------------------------
    if (YYYY == 2004) {
        cat("\n### Year:", YYYY, " exceptions \n\n" )
        cat("\n#### BEWARE!\n")
        cat("There is an unexpected +2.5V offset in the recording signal for
            2004-07-03 00:00 until 2004-07-22 00:00.
            We changed the allowed physical signal limits to compensate.
            Have to check dark calculation and the final output for problems.\n")

        part <- year_data[Date > as.POSIXct("2004-06-01") &
                          Date < as.POSIXct("2004-08-01") ]

        ylim <- range(-1,2, part$sig_lowlim, part$sig_upplim )

        plot(  part$Date, part$CM21value,  pch = ".", ylim = ylim)
        points(part$Date, part$sig_lowlim, pch = ".", col = "red")
        points(part$Date, part$sig_upplim, pch = ".", col = "red")

        ## plot config changes
        abline(v = signal_physical_limits$Date, lty = 3)

        testdata <- extra[Date > as.POSIXct("2004-06-01") &
                          Date < as.POSIXct("2004-08-01") ]
        ## Plot existing sirena data
        points(testdata$Date, testdata$WATTTOT / cm21factor(testdata$Date), pch = ".", col = "cyan")

    }

    ## ____ 2005 ---------------------------------------------------------------
    if (YYYY == 2005) {
        cat("\n### Year:", YYYY, " exceptions \n\n" )

        part <- year_data[Date > as.POSIXct("2005-11-15") &
                          Date < as.POSIXct("2005-12-31") ]

        ylim <- range(-1,2, part$sig_lowlim, part$sig_upplim )

        plot(  part$Date, part$CM21value,  pch = ".", ylim = ylim)
        points(part$Date, part$sig_lowlim, pch = ".", col = "red")
        points(part$Date, part$sig_upplim, pch = ".", col = "red")

        ## plot config changes
        abline(v = signal_physical_limits$Date, lty = 3)

        testdata <- extra[Date > as.POSIXct("2005-11-15") &
                          Date < as.POSIXct("2005-12-31") ]
        ## Plot existing sirena data
        points(testdata$Date, testdata$WATTTOT / cm21factor(testdata$Date), pch = ".", col = "cyan")
    }

    ## ____ 2015 ---------------------------------------------------------------
    if (YYYY == 2015) {
        cat("\n### Year:", YYYY, " exceptions \n\n" )

        part <- year_data[Date > as.POSIXct("2015-04-10") &
                          Date < as.POSIXct("2015-05-01") ]

        ylim <- range(-1,2, part$sig_lowlim, part$sig_upplim )

        plot(  part$Date, part$CM21value,  pch = ".", ylim = ylim)
        points(part$Date, part$sig_lowlim, pch = ".", col = "red")
        points(part$Date, part$sig_upplim, pch = ".", col = "red")

        ## plot config changes
        abline(v = signal_physical_limits$Date, lty = 3)

        testdata <- extra[Date > as.POSIXct("2015-04-10") &
                          Date < as.POSIXct("2015-05-01") ]
        ## Plot existing sirena data
        points(testdata$Date, testdata$WATTTOT / cm21factor(testdata$Date), pch = ".", col = "cyan")
    }


    ## __  More yearly plots  --------------------------------------------------

    plot(year_data$Elevat, year_data$CM21sd,    pch = 19, cex = .5,
         main = paste("CM-21 signal SD", YYYY ),
         xlab = "Elevation",
         ylab = "CM-21 signal Standard Deviations")
    abline( h = yearlims[ an == "CM21sd", low], col = "red")
    abline( h = yearlims[ an == "CM21sd", upe], col = "red")
    cat('\n\n')


    par(mar = c(2, 4, 2, 1))
    month_vec <- strftime( year_data$Date, format = "%m")
    dd        <- aggregate(year_data[,c("CM21value", "CM21sd", "Elevat", "Azimuth")],
                           list(month_vec), FUN = summary, digits = 6 )


    # cat("\n\n### CM 21 measurements, monthly aggregation\n")
    # cat("\n\n")
    # cat(pander(dd$CM21value))
    # cat("\n\n")
    #
    # cat("\n\n### CM 21 standard deviation, monthly aggregation\n")
    # cat(pander(dd$CM21sd))
    #
    # cat("\n\n### Sun Elevation\n")
    # cat(pander(dd$Elevat))
    #
    # cat("\n\n### Sun Azimuth\n")
    # cat(pander(dd$Azimuth))

    boxplot(year_data$CM21value ~ month_vec )
    title(main = paste("CM-21 value by month", YYYY))
    cat('\n\n')

    boxplot(year_data$CM21sd ~ month_vec )
    title(main = paste("CM-21 sd by month", YYYY))
    cat('\n\n')

    boxplot(year_data$Elevat ~ month_vec )
    title(main = paste("Elevation by month", YYYY))
    cat('\n\n')

    boxplot(year_data$Azimuth ~ month_vec )
    title(main = paste("Azimuth by month", YYYY))
    cat('\n\n')

    ## __ Save years signal data to file ---------------------------------------
    if (!TEST) {
        write_RDS(object = year_data,
                  file   = paste0(SIGNAL_DIR,"/LAP_CM21_H_SIG_",YYYY,".Rds")
        )
    }
}
## sort list of missing files
system(paste("sort -u -o ", MISSING_INP, MISSING_INP))


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
