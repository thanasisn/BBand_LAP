# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Read raw CHP1 data. **LAP -> SIG** "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Combine raw data from CHP1 to yearly data sets."
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
#' **Source code: [github.com/thanasisn/BBand_LAP](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [thanasisn.netlify.app/3-data_display/](https://thanasisn.netlify.app/3-data_display/)**
#'
#'
#+ echo=F, include=T


## __ Document options -------------

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""      )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"    )
knitr::opts_chunk$set(fig.align  = "center" )
# knitr::opts_chunk$set(fig.pos    = '!h'     )



#+ include=F, echo=F
## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("CHP1_R10_") })

source("~/BBand_LAP/DEFINITIONS.R")
source("~/CHP_1_DIR/Functions_CHP1.R")
source("~/BBand_LAP/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
mylock(DB_lock)


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

## __  Variables  -------------
OutliersPlot <- 4




####  Execution control  ####
ALL_YEARS <- FALSE
if (!exists("params")) {
    params <- list( ALL_YEARS = ALL_YEARS)
}


#+ include=TRUE, echo=FALSE, results = 'asis'

## Files for import

sirena_files <- list.files(path        = SIRENA_DIR,
                           recursive   = TRUE,
                           pattern     = "[0-9]*03.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:", paste(length(sirena_files), "files from Sirena**\n"))
## just in case, there are nested folders with more lap files in Sirens
sirena_files <- grep("OLD", sirena_files,
                     ignore.case = TRUE, invert = TRUE, value = TRUE )


radmon_files <- list.files(path        = RADMON_DIR,
                           recursive   = TRUE,
                           pattern     = "[0-9]*03.LAP$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:",paste(length(radmon_files), "files from Radmon**\n"))


####  Check files between Radmon and Sirena  ####

sir_names <- basename(sirena_files)
rad_names <- basename(radmon_files)

missing_from_sir <- rad_names[ ! rad_names %in% sir_names ]
if ( length(missing_from_sir) > 0 ) {
    warning(paste("\nThere are ", length(missing_from_sir) , " files on Radmon that are missing from Sirena\n"))
    cat(missing_from_sir,sep = "\n\n")
    cat("\n\n")
}
rm(rad_names, radmon_files)

#'
#+ include=TRUE, echo=FALSE

####  Read files of all years  ####

## all allowed years
years_to_do <- format(seq(START_DAY, END_DAY, by = "year"), "%Y" )

# years_to_do <- 2019

#'
#' Allowed years to do: `r years_to_do`
#'
#+ include=TRUE, echo=FALSE

####  Check for new data to parse  ####
if (!params$ALL_YEARS) {
    NEWDATA            <- FALSE
    sirena_files_dates <- file.mtime(sirena_files)
    storagefiles       <- list.files(SIGNAL_DIR, "LAP_CHP1_B_SIG.*.rds",
                                     full.names = T, ignore.case = T)
    last_storage_date  <- max(file.mtime(storagefiles))
    newfiles           <- sirena_files[sirena_files_dates > last_storage_date]

    ## check years stored
    storage_years <- as.numeric(
        sub(".rds", "",
            sub(".*_SIG_","",
                basename(storagefiles)), ignore.case = T))
    missing_years <- years_to_do[!years_to_do %in% storage_years]

    ## check new data
    new_to_do <- c()
    if (length(newfiles)>0) {
        ## find years to do
        newyears <- unique(
            year(
                strptime(
                    sub("03\\.lap","", basename(newfiles), ignore.case = T),
                    "%d%m%y")))
        new_to_do <- years_to_do[years_to_do %in% newyears]
        NEWDATA   <- TRUE
    }

    # missing_years <- 2015:2022

    ## decide what to do
    if (length(missing_years) != 0 | NEWDATA) {
        years_to_do <- sort(unique(c(missing_years,new_to_do)))
    } else {
        stop("NO new data! NO need to parse!")
    }
}

#'
#' Years to do: `r years_to_do`
#'
#+ include=TRUE, echo=FALSE, results="asis"

## loop all years
for (YYYY in years_to_do) {
    yy           <- substr(YYYY, 3, 4)
    year_data    <- data.table()
    days_of_year <- seq.Date(as.Date(paste0(YYYY, "-01-01")),
                             as.Date(paste0(YYYY, "-12-31")), by = "day")

    cat("\n\n\\FloatBarrier\n\n")
    cat("\\newpage\n\n")
    cat("\n## Year:", YYYY, "\n\n")

    missing_files <- c()
    for (aday in days_of_year) {
        aday  <- as.Date(aday, origin = "1970-01-01")
        sunfl <- paste0(SUN_FOLDER, "sun_path_", format(aday, "%F"), ".dat.gz")

        found <- grep(paste0("/",YYYY,"/", format(aday, "%d%m%y03") ), sirena_files, ignore.case = T )
        ## check file names
        if ( length(found) > 1 ) {
            stop("Found more file than we should") }
        if ( length(found) == 0 ) {
            missing_files <- c(missing_files, paste0(YYYY,"/", format(aday, "%d%m%y03")))
            cat(paste0(YYYY,"/", format(aday, "%d%m%y03")), sep = "\n",
                file = MISSING_INP, append = T )
            next()
        }

        ## recreate time stamp for all minutes of day
        suppressWarnings(rm(D_minutes))
        D_minutes <- seq(from       = as.POSIXct(paste(aday,"00:00:30 UTC")),
                         length.out = 1440,
                         by         = "min" )

        ####  Read LAP file  ####
        lap <- fread( sirena_files[found], na.strings = "-9" )
        lap[V1 < -8, V1 := NA]
        lap[V2 < -8, V2 := NA]

        stopifnot( dim(lap)[1] == 1440 )

        ####  Read SUN file  ####
        if (!file.exists(sunfl)) stop(cat(paste("Missing:", sunfl, "\nRUN! Sun_vector_construction_cron.py\n")))
        sun_temp <- read.table( sunfl,
                                sep         = ";",
                                header      = TRUE,
                                na.strings  = "None",
                                strip.white = TRUE,
                                as.is       = TRUE)

        ####  Day table to save  ####
        day_data <- data.table( Date        = D_minutes,      # Date of the data point
                                CHP1value   = lap$V1,         # Raw value for CHP1
                                CHP1sd      = lap$V2,         # Raw SD value for CHP1
                                Azimuth     = sun_temp$AZIM,  # Azimuth sun angle
                                Elevat      = sun_temp$ELEV ) # Elevation sun angle

        ####  Gather data  ####
        year_data <- rbind( year_data, day_data )
    }
    ## order data
    setorder(year_data,Date)

    ## check there are not duplicate dates read from raw
    testsanity <- year_data[, .N , by = as.Date(Date)]
    if (!all( testsanity$N == 1440 )) {
        cat("\n**There are days with not exactly 1440 minutes**\n\n")
        cat('\n\n')
        cat(pander(testsanity[ N != 1440]))
        cat('\n\n')
    } else {
        cat("\n**All days have exactly 1440 minutes**\n\n")
    }

    ## print missing files
    cat("\n**Missing whole day files:**\n\n")
    cat(missing_files,sep = "\n\n")


#     ## add signal limits on plots
#     year_data[ , sig_lowlim := signal_lower_limit(Date) ]
#     year_data[ , sig_upplim := signal_upper_limit(Date) ]


    ####    Yearly Plots    ####################################################


    ####  Do some plots for this year before filtering  ####
    suppressWarnings({
        ## Try to find outliers
        yearlims <- data.table()
        for (an in grep("CHP1",names(year_data),value = T)){
            daily <- year_data[ , .(dmin = min(get(an),na.rm = T),
                                    dmax = max(get(an),na.rm = T)), by = as.Date(Date) ]
            low <- daily[ !is.infinite(dmin), mean(dmin) - OutliersPlot * sd(dmin)]
            upe <- daily[ !is.infinite(dmax), mean(dmax) + OutliersPlot * sd(dmax)]
            yearlims <- rbind(yearlims, data.table(an = an,low = low, upe = upe))
        }
    })

    cat("\n\n### Proposed outliers limits \n")
    cat("\n\n")
    cat(pander(yearlims))
    cat("\n\n")


    cat('\n\n\\scriptsize\n\n')
    cat(pander( summary(year_data[,-c('Date','Azimuth')]) ))
    cat('\n\n\\normalsize\n\n')


    hist(year_data$CHP1value, breaks = 50, main = paste("CHP1 signal ",  YYYY ) )
    cat('\n\n')

    hist(year_data$CHP1sd,    breaks = 50, main = paste("CHP1 signal SD",YYYY ) )
    cat('\n\n')

    plot(year_data$Elevat, year_data$CHP1value, pch = 19, cex = .5,
         main = paste("CHP1 signal ", YYYY ),
         xlab = "Elevation",
         ylab = "CHP1 signal" )
    points(year_data$Elevat, year_data$sig_lowlim, pch = ".", col = "red")
    points(year_data$Elevat, year_data$sig_upplim, pch = ".", col = "red")
    cat('\n\n')


    plot(year_data$Date, year_data$CHP1value, pch = 19, cex = .5,
         main = paste("CHP1 signal ", YYYY ),
         xlab = "Elevation",
         ylab = "CHP1 signal" )
    points(year_data$Date, year_data$sig_lowlim, pch = ".", col = "red")
    points(year_data$Date, year_data$sig_upplim, pch = ".", col = "red")
    # abline(v=signal_physical_limits$Date)
    cat('\n\n')



    plot(year_data$Elevat, year_data$CHP1sd,    pch = 19, cex = .5,
         main = paste("CHP1 signal SD", YYYY ),
         xlab = "Elevation",
         ylab = "CHP1 signal Standard Deviations")
    abline( h = yearlims[ an == "CHP1sd", low], col = "red")
    abline( h = yearlims[ an == "CHP1sd", upe], col = "red")
    cat('\n\n')


    par(mar = c(2,4,2,1))
    month_vec <- strftime(  year_data$Date, format = "%m")
    dd        <- aggregate( year_data[,c("CHP1value", "CHP1sd", "Elevat", "Azimuth")],
                            list(month_vec), FUN = summary, digits = 6 )


    # cat("\n\n### CHP 1 measurements, monthly aggregation\n")
    # cat("\n\n")
    # cat(pander(dd$CHP1value))
    # cat("\n\n")
    #
    # cat("\n\n### CHP 1 standard deviation, monthly aggregation\n")
    # cat(pander(dd$CHP1sd))
    #
    # cat("\n\n### Sun Elevation\n")
    # cat(pander(dd$Elevat))
    #
    # cat("\n\n### Sun Azimuth\n")
    # cat(pander(dd$Azimuth))

    boxplot(year_data$CHP1value ~ month_vec )
    title(main = paste("CHP1value by month", YYYY) )
    cat('\n\n')

    boxplot(year_data$CHP1sd ~ month_vec )
    title(main = paste("CHP1sd by month", YYYY) )
    cat('\n\n')

    boxplot(year_data$Elevat ~ month_vec )
    title(main = paste("Elevation by month", YYYY) )
    cat('\n\n')

    boxplot(year_data$Azimuth ~ month_vec )
    title(main = paste("Azimuth by month", YYYY) )
    cat('\n\n')

    ####  Save signal data to file  ####
    write_RDS(object = year_data,
              file   = paste0(SIGNAL_DIR,"/LAP_CHP1_B_SIG_",YYYY,".Rds") )

}
## sort list of missing files
system(paste("sort -u -o ", MISSING_INP, MISSING_INP ))


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
