#!/usr/bin/env Rscript
#' Copyright (C) 2018 Athanasios Natsis <natsisphysicist@gmail.com>
#'
#' Read text files with CM_21 signal data
#'


####  Set environment  ####
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = funr::sys.script()


library(data.table)


####  . . Variables  ####
source("~/CM_21_GLB/DEFINITIONS.R")

source("~/CM_21_GLB/Functions_CM21_factor.R")

tag = paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))


####  Files for import  ####
## . get data input files ####
input_files <- list.files(path       = GLOBAL_DIR,
                          pattern    = "LAP_CM21_H_L1_[0-9]{4}.Rds",
                          full.names = TRUE)


input_files <- sort(input_files)



for (afile in input_files) {

    #### Get raw data ####
    ayear        <- readRDS(afile)
    NR_loaded    <- ayear[ !is.na(wattGLB), .N ]
    yyyy         <- year(ayear$Date[1])

    ## create all minutes
    allminutes <- seq( as.POSIXct( paste0(yyyy, "-01-01 00:00:30") ),
                       as.POSIXct( paste0(yyyy, "-12-31 23:59:30") ),
                       by = "mins" )

    ayear     <- merge( ayear,
                    data.frame(Date = allminutes),
                    by = "Date", all = T)

    ayear$day <- as.Date(ayear$Date)


    daystodo  <- unique( ayear$day )
stop()
    pdffile = paste0(REPORT_DIR, "Daily_GHI_", yyyy, ".pdf")
    pdf(pdffile)

    par(mar = c(4,4,2,1))
    par(mgp = c(2.2,1,0))

    for (ddd in daystodo) {

        aday    <- as.Date(ddd, origin = "1970-01-01")
        test    <- format( aday, format = "%d%m%y06" )

        ## get daily data
        daydata <- ayear[ day == as.Date(aday) ]

        ## Main data plot
        dddd = min(daydata$wattGLB, daydata$wattGLB_SD , na.rm = TRUE)
        uuuu = max(daydata$wattGLB, daydata$wattGLB_SD , na.rm = TRUE)
        if (dddd > -5  ) { dddd = 0  }
        if (uuuu < 190 ) { uuuu = 200}
        ylim = c(dddd , uuuu)

        plot(daydata$Date, daydata$wattGLB,
             "l", xlab = "UTC", ylab = expression(W / m^2),
             col  = "blue", lwd = 1.1, lty = 1, xaxt = "n", ylim = ylim, xaxs = "i" )
        abline(h = 0, col = "gray60")
        abline(v   = axis.POSIXct(1, at = pretty(daydata$Date, n = 12, min.n = 8 ), format = "%H:%M" ),
               col = "lightgray", lty = "dotted", lwd = par("lwd"))
        points(daydata$Date, daydata$wattGLB_SD, pch = ".", cex = 2, col = "red" )
        title( main = paste(test, format(daydata$Date[1] , format = "  %F")))
        text(daydata$Date[1], uuuu, labels = tag, pos = 4, cex =.7 )

    }
    dev.off()
}



paste("pdftk fdsf.pdf cat output fdsfss.pdf")


## END ##
tac = Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
