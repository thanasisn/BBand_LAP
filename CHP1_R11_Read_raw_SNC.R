# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Read tracker sync data. **-> SNC** "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Combine raw sync data from tracker to yearly data sets."
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
#'
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
                            return("CHP1_R10_") })
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



####  Variables  ####
source("~/CHP_1_DIR/DEFINITIONS.R")
panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )



####  Execution control  ####
ALL_YEARS <- FALSE
if (!exists("params")){
    params <- list( ALL_YEARS = ALL_YEARS)
}


#+ include=TRUE, echo=FALSE, results = 'asis'

## Files for import
sync_files <- list.files(path        = trSYNC_DIR,
                         recursive   = TRUE,
                         pattern     = "sun_tracker_.*.snc$",
                         ignore.case = TRUE,
                         full.names  = TRUE )
cat("\n**Found:",paste(length(sync_files), "tracker 'sync' files**\n"))



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
    NEWDATA           <- FALSE
    sync_files_dates  <- file.mtime(sync_files)
    storagefiles      <- list.files(SIGNAL_DIR, "LAP_CHP1_T_SNC.*.rds",
                                    full.names = T, ignore.case = T)
    last_storage_date <- max(file.mtime(storagefiles))
    newfiles          <- sync_files[sync_files_dates > last_storage_date]

    ## check years stored
    storage_years <- as.numeric(
        sub(".rds", "",
            sub(".*CHP1_T_SNC_","",
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

        found <- grep( paste0( aday ), sync_files, ignore.case = T, value = T )
        ## check file names
        if ( length(found) > 1 ) {
            stop("Found more file than we should") }
        if ( length(found) == 0 ) {
            missing_files <- c(missing_files, paste0(YYYY,"/", aday))
            cat(paste0(YYYY,"/", aday), sep = "\n", file = MISSING_SNC, append = T )
            next()
        }

        ## recreate time stamp for all minutes of day
        D_minutes    <- seq(from = as.POSIXct(paste(aday,"00:00:30 UTC")), length.out = 1440, by = "min" )

        ####TODO we should use step files as more reliable to detect async events!!!
        # stp_temp = read.table(stpfilename, sep = "\t", as.is = TRUE, na.strings = "None")

        syc_temp    <- read.table(found, sep = "\t", as.is = TRUE, na.strings = "None")
        syc_temp$V1 <- as.POSIXct(syc_temp$V1)
        async_minu  <- as.POSIXct(format(syc_temp$V1,format = "%F %R"))  ## async end
        uniq_async  <- unique(async_minu)

        ## async time distance
        syc_temp$timeDist <- apply(syc_temp[,c('V7','V8')], MARGIN = 1, FUN = max, na.rm = T)

        for (amin in uniq_async) {
            min_ind <- async_minu == amin
            stepgo  <- syc_temp$V4[ min_ind ]
            stepis  <- syc_temp$V5[ min_ind ]
            stepout <- suppressWarnings( max( abs( stepgo - stepis ), na.rm = TRUE ) )
            if (is.finite(stepout)) {
                asyncstp[ which( D_minutes == amin ) ] <- stepout
            }
        }

        ## set async from time back
        syc_temp$async_start <- syc_temp$V1 - syc_temp$timeDist
        syc_temp$async_start <- as.POSIXct(format(syc_temp$async_start, format = "%F %R"))
        syc_temp$async_end   <- as.POSIXct(format(syc_temp$V1,          format = "%F %R"))
        ## create vector of asyncs
        for (ik in 1:nrow(syc_temp)) {
            async[ which( D_minutes <= syc_temp$async_end[   ik ] &
                          D_minutes >= syc_temp$async_start[ ik ]  ) ] <- TRUE
        }

        day_data <- data.frame(Date = D_minutes,
                               Async = async)

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
    cat(paste(missing_files),sep = "\ \ \ \ ")
    cat("\n\n")

    cat("\n\n")
    cat(pander(summary(year_data)))
    cat("\n\n")


    count <- year_data[ , .(Asyncs = sum(Async)), by = .(Day = as.Date(Date))]

    plot(count$Day, count$Asyncs)
    cat("\n\n")

    hist(count$Asyncs)
    cat("\n\n")

    ####  Save signal data to file  ####
    write_RDS(object = year_data,
              file   = paste0(SIGNAL_DIR,"/LAP_CHP1_T_SNC_",YYYY,".Rds") )

}
## sort list of missing files
system(paste("sort -u -o ", MISSING_SNC, MISSING_SNC ))


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
