# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Read CHP1 temperature data. **-> TMP** "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Combine temperature data from CHP1 to yearly data sets."
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
#'     fig_width:        7.5
#'     fig_height:       4
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5

#' date: "`r format(Sys.time(), '%F')`"
#' params:
#'    ALL_YEARS: TRUE
#' ---

#'
#'  **-> SNC**
#'
#' **Source code: [github.com/thanasisn/CHP_1_DIR](https://github.com/thanasisn/CHP_1_DIR)**
#'
#' **Data display: [thanasisn.netlify.app/3-data_display/](https://thanasisn.netlify.app/3-data_display/)**
#'
#'
#+ echo=F, include=T


####_  Document options _####

#+ echo=F, include=F
knitr::opts_chunk$set(comment    = ""      )
# knitr::opts_chunk$set(dev        = "pdf"   )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"    )
knitr::opts_chunk$set(fig.align  = "center" )
# knitr::opts_chunk$set(fig.pos    = '!h'     )



#+ include=F, echo=F
####  Set environment  ####
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("CHP1_R12_") })
if(!interactive()) {
    pdf(  file = paste0("~/CHP_1_DIR/RUNTIME/", basename(sub("\\.R$",".pdf", Script.Name))))
    sink( file = paste0("~/CHP_1_DIR/RUNTIME/", basename(sub("\\.R$",".out", Script.Name))), split=TRUE)
    filelock::lock(paste0("~/CHP_1_DIR/LOGs/",  basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}


#+ echo=F, include=F
####  External code  ####
library(data.table, quietly = T, warn.conflicts = F)
library(pander,     quietly = T, warn.conflicts = F)
source("~/CHP_1_DIR/Functions_write_data.R")
source("~/CHP_1_DIR/Functions_CHP1.R")



####  Variables  ####
source("~/CHP_1_DIR/DEFINITIONS.R")
panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )



####  Execution control  ####
ALL_YEARS = FALSE
if (!exists("params")){
    params <- list( ALL_YEARS = ALL_YEARS)
}


#+ include=TRUE, echo=FALSE, results = 'asis'


## Files for import
therm_files <- list.files( path        = CHPTMP_DIR,
                           recursive   = TRUE,
                           pattern     = "sun_tracker_.*.therm$",
                           ignore.case = TRUE,
                           full.names  = TRUE )
cat("\n**Found:",paste(length(therm_files), "chp1 temperature files**\n"))



#'
#+ include=TRUE, echo=FALSE

####  Read files of all years  ####

## all allowed years
years_to_do <- format(seq(START_DAY, END_DAY, by = "year"), "%Y" )


#'
#' Allowed years to do: `r years_to_do`
#'
#+ include=TRUE, echo=FALSE

####  Check for new data to parse  ####
if (!params$ALL_YEARS) {
    NEWDATA            <- FALSE
    therm_files_dates   <- file.mtime(therm_files)
    storagefiles       <- list.files(SIGNAL_DIR, "LAP_CHP1_TEMP.*.rds",
                                     full.names = T, ignore.case = T)
    last_storage_date  <- max(file.mtime(storagefiles))
    newfiles           <- therm_files[therm_files_dates > last_storage_date]

    ## check years stored
    storage_years <- as.numeric(
        sub(".rds", "",
            sub(".*CHP1_TEMP_","",
                basename(storagefiles)),ignore.case = T))
    missing_years <- years_to_do[!years_to_do %in% storage_years]

    ## check new data
    new_to_do <- c()
    if (length(newfiles)>0) {
        ## find years to do
        newyears <- unique(
            year(
                strptime(
                    sub(".snc","",sub("sun_tracker_","", basename(newfiles), ignore.case = T)),
                    "%F")))

        new_to_do <- years_to_do[years_to_do %in% newyears]
        NEWDATA   <- TRUE
    }

    # missing_years <- 2016:2022

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
for ( YYYY in years_to_do ) {
    yy           <- substr(YYYY, 3,4)
    year_data    <- data.table()
    days_of_year <- seq.Date(as.Date(paste0(YYYY,"-01-01")),
                             as.Date(paste0(YYYY,"-12-31")), by = "day")

    cat("\n\n\\FloatBarrier\n\n")
    cat("\\newpage\n\n")
    cat("\n## Year:", YYYY, "\n\n" )

    missing_files <- c()
    for ( aday in days_of_year ) {
        aday     <- as.Date(aday, origin = "1970-01-01")
        asyncstp <- rep( NA,    1440 )
        async    <- rep( FALSE, 1440 )

        found    <- grep( paste0( aday ), therm_files, ignore.case = T, value = T )
        ## check file names
        if ( length(found) > 1 ) {
            stop("Found more file than we should") }
        if ( length(found) == 0 ) {
            missing_files <- c(missing_files, paste0(YYYY,"/", aday))
            cat(paste0(YYYY,"/", aday), sep = "\n", file = MISSING_TMP, append = T )
            next()
        }

        #### Thermistor data process ------------------------------------
        Temp        <- rep(NA, 1440)  # CHP1 temperature
        TempSD      <- rep(NA, 1440)  # Standard Deviation
        TempUNC     <- rep(NA, 1440)  # Measurement uncertainty
        RmeasuError <- rep(NA, 1440)  # Measurement uncertainty

        temp_temp    <- unique(read.table( found, sep = "\t", as.is = TRUE))
        temp_temp$V1 <- as.POSIXct( temp_temp$V1 )
        temp_temp$V1 <- as.POSIXct( format( temp_temp$V1, format = "%F %R" ) )
        temp_temp$V1 <- temp_temp$V1 + 30
        temp_temp$V3[ temp_temp$V3 == 0 ] <- NA

        temp_temp    <- data.table(temp_temp)
        temp_temp    <- temp_temp[, .( V2 = mean(V2, na.rm = T),
                                       V3 = mean(V3, na.rm = T) ), by = V1 ]

        day_data <- data.frame(Date         = temp_temp$V1,
                               CHP1RmeasERR = Protek_506_R_error(    temp_temp$V2),
                               CHP1temp     = CHP_thermistor_R_to_T( temp_temp$V2),
                               CHP1tempSD   = CHP_thermistor_ResUnc_to_TempUnc(temp_temp$V2, temp_temp$V3))
        day_data$CHP1tempUNC      <- CHP_thermistor_ResUnc_to_TempUnc(temp_temp$V2, day_data$CHP1RmeasERR )
        day_data$CHP1Resistance   <- temp_temp$V2
        day_data$CHP1ResistanceSD <- temp_temp$V3

        ####  Gather data  ####
        year_data <- rbind( year_data, day_data )
    }
    ## order data
    year_data <- data.table(year_data)
    if ( nrow(year_data) == 0) {
        cat("\n**There are no temperature data**\n\n")
        cat('\n\n')
        next()
    }
    year_data <- year_data[ CHP1Resistance > -800 ]
    setorder(year_data,Date)


    ## check there are not duplicate dates read from raw
    testsanity <- year_data[, .N , by = Date]
    if ( ! all( testsanity$N == 1 )) {
        cat("\n**There are duplicate records**\n\n")
        cat('\n\n')
        cat(pander(testsanity[ N != 1]))
        cat('\n\n')
    } else {
        cat("\n**There are no duplicate records**\n\n")
    }

    cat("\n\n")
    cat(pander(summary(year_data)))
    cat("\n\n")

    ## check negative resistance
    negativeN <- year_data[ CHP1Resistance < 0, .N ]
    if ( negativeN > 0 ){
        cat(paste0("\n**",negativeN," Negative resistance records removed**\n\n"))
        cat('\n\n')
    }
    year_data <- year_data[ CHP1Resistance > 0 ]

    ## check too low temperatures
    toocoldN <- year_data[ CHP1temp < CHP_TEMP_MIN, .N ]
    if ( toocoldN > 0 ) {
        cat(paste0("\n**",toocoldN," records removed with extremely low temp (",CHP_TEMP_MIN,")**\n\n"))
        cat('\n\n')
        cat("\n\n")
        cat(pander(year_data[CHP1temp < CHP_TEMP_MIN, .N, by = .(Date = as.Date(Date))] ))
        cat("\n\n")
    }
    year_data <- year_data[ CHP1temp > CHP_TEMP_MIN, ]

    ## check too high temperatures
    toohotN <- year_data[ CHP1temp > CHP_TEMP_MAX, .N ]
    if ( toohotN > 0 ) {
        cat(paste0("\n**",toohotN," records removed with extremely hight temp (",CHP_TEMP_MAX,")**\n\n"))
        cat('\n\n')
        cat("\n\n")
        cat(pander(year_data[CHP1temp > CHP_TEMP_MAX, .N, by = .(Date = as.Date(Date))] ))
        cat("\n\n")
    }
    year_data <- year_data[ CHP1temp < CHP_TEMP_MAX, ]

    ## check too high temperatures sd
    toosdN <- year_data[ CHP1tempSD > CHP_TEMP_STD_LIM, .N ]
    if ( toosdN > 0 ) {
        cat(paste0("\n**",toosdN," records removed with extremely hight temp (",CHP_TEMP_STD_LIM,")**\n\n"))
        cat('\n\n')
        cat("\n\n")
        cat(pander(year_data[CHP1tempSD > CHP_TEMP_STD_LIM, .N, by = .(Date = as.Date(Date))] ))
        cat("\n\n")
    }
    year_data <- year_data[ CHP1tempSD < CHP_TEMP_STD_LIM, ]


    hist(year_data[, CHP1Resistance ])
    cat("\n\n")

    hist(year_data[, CHP1temp ])
    cat("\n\n")

    hist(year_data[, CHP1tempSD ], breaks = 100)
    cat("\n\n")


    ####  Save signal data to file  ####
    if (nrow(year_data) > 0) {
        write_RDS(object = year_data,
                  file   = paste0(SIGNAL_DIR,"/LAP_CHP1_TEMP_",YYYY,".Rds") )
    }

}
## sort list of missing files
system(paste("sort -u -o ", MISSING_TMP, MISSING_TMP ))


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
