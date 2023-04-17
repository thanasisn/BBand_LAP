#!/usr/bin/env Rscript
#' Copyright (C) 2018 Athanasios Natsis <natsisphysicist@gmail.com>
#'

#'
#' Read prepared clean data (level 1) of CHP1
#' and output DIR files with direct beam irradiance for sirena repository
#'

closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n")
                            return("Undefined R script name!!") })
if(!interactive()) {
    pdf(file=sub("\\.R$",".pdf",Script.Name))
    sink(file=sub("\\.R$",".out",Script.Name,),split=TRUE)
}

options(max.print = 1500)


## data export folder
DATOUT = "/home/athan/DATA/CHP1_LAP.DIR/"
## data input folder
DATIN  = "/home/athan/DATA/Broad_Band"

YEARS  <- c(2016,2017,2018,2019,2020,2021,2022)


wecare <- c("SZA", "Date30", "wattDIR", "wattDIR_sds")
# wecare <- c("SZA", "Date30", "wattDIR", "wattDIR_sds", "Azimuth")
tag    <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))


## sun position algorithm as the other broadband
zenangle <- function(YYYY,min,doy){
    as.numeric(
        system(
            paste("/home/athan/CM_21_GLB/BINARY/zenangle64 ", YYYY ,min, doy, " 40.634 -22.956" ),
            intern = T)
    )
}

## vectorize zenangle
vzen <- Vectorize(zenangle, "min")

## parallelize zenangle
library(foreach)
library(doMC)
registerDoMC()

pzen <- function(YYYY, min = 1:1440, doy) {
    foreach(min = min, .combine = 'c') %dopar% zenangle(YYYY = YYYY, min = min ,doy = doy)
}





for (ayear in YEARS) {
    pdf(file = paste0(DATOUT,"/","CurrentDataPlots_",ayear,".pdf") )

    ## get data for a year
    filein <- list.files(path         = DATIN,
                         pattern      = paste0("LAP_CHP1_L1_", ayear, ".Rds"),
                         full.names   = TRUE,
                         include.dirs = FALSE )
    stopifnot(length(filein) == 1)

    data <- readRDS(filein)

    # ## for others
    # data <- data[!data$Async,]

    ## subset data to export
    data     <- data[ , wecare ]

    ## complete all days
    allmin <- seq( as.POSIXct(paste0(ayear, "-01-01 00:00:30 UTC")),
                   as.POSIXct(paste0(ayear, "-12-31 24:00:00 UTC")), by = "min" )
    allmin <- data.frame( Date30 = allmin )
    data   <- merge(data, allmin ,  all.y = TRUE )

    ## prepare data to export
    data$Day          <- as.Date(data$Date30)
    data$wattDIR[     is.na(data$wattDIR)     ] <- -9L
    data$wattDIR_sds[ is.na(data$wattDIR_sds) ] <- -9L
    data$SZA[         is.na(data$SZA)         ] <- -9L

    yearfolder <- paste0(DATOUT,"/",ayear)
    dir.create( yearfolder, showWarnings = F )


    # ## export for others
    # outdir <- "/home/athan/ZHOST/"
    # data$Date30 <- data$Date30 - 30
    # data$wattDIR[     data$wattDIR     <= -8     ] <- NA
    # data$wattDIR_sds[ data$wattDIR_sds <= -8 ] <- NA
    # data$SZA[         data$SZA  <= -8         ] <- NA
    #
    # data <- data[ !is.na(data$wattDIR), ]
    # data <- data[ data$Date < "2022-02-28 00:00", ]
    #
    # names(data)[names(data) == "Date30"] <- "Date"
    # data$Day <- NULL
    # data <- data[!is.na(data$wattDIR),]
    #
    # myRtools::write_dat(object = data,
    #                     file = sub( ".rds","", sub(DATIN, outdir, filein), ignore.case = T),
    #                     contact = "<natsisphysicist@gmail.com>")


    ## export each day
    alldays <- sort(unique(data$Day))

    for (dd in alldays) {
        dateD = as.Date(dd, origin = "1970-01-01")
        cat(sprintf( "%s   \r", dateD ))

        dayselec <- data$Day == dateD
        if (sum(dayselec) != 1440 ) { stop("Day don't have 1440 minutes!!") }

        filename <- paste0(DATOUT, "/", strftime(dateD, format = "%Y/DIR%3j%y.DAT"))

        oneday  <- data[ dayselec, ]

        TIME_UT <- as.numeric((oneday$Date30 - as.POSIXct( dateD ) + 30) / 3600)

        doy <- as.numeric(format( dateD, "%j" ))

        ## calculate zenith angle as other instruments
        lapzen <- pzen(ayear, 1:1440, doy )

        output <- data.frame( TIME_UT = round(TIME_UT,            digits = 4 ),
                              SZA     = round(lapzen,            digits = 2 ),
                              # SZA     = round(oneday$SZA,         digits = 2 ),
                              Wm2     = round(oneday$wattDIR,     digits = 3 ),
                              st.dev  = round(oneday$wattDIR_sds, digits = 3 ) )

        ## custom file header
        write(" TIME_UT    SZA    [W.m-2]   st.dev\r\n",
              file = filename)
        ## format and write data
        write.table(format(output,
                           digits    = 3,
                           width     = 8,
                           row.names = FALSE,
                           scietific = FALSE,
                           nsmall    = 2 ),
                    file      = filename,
                    append    = TRUE,
                    quote     = FALSE,
                    col.names = FALSE,
                    row.names = FALSE,
                    eol = "\r\n")
        ## make sure has consistent line endings
        system(paste("unix2dos ", filename))



        ## create a plot of the output data
        par(mar = c(3,3,1,1))
        par(mgp = c(1.5,.5,0))

        output[output <= -9 ] <- NA

        if (!all(is.na(output$Wm2))) {

            ## test SZAs
            # plot(oneday$SZA, lapzen)

            plot( output$TIME_UT, output$Wm2, "l",
                  lwd  = 1.5 , col = "green",
                  ylab = expression( Watt/m^2 ),
                  xlab = "Hour (UTC)")
            title(main = paste(dateD), cex.main = .8 )

            points( output$TIME_UT, output$st.dev,
                    pch = 18, cex = .4 , col = "blue")

            legend("topright", bty = "n",
                   legend = c( "Direct Normal Irradiance",
                               "Standard deviation")     ,
                   lty = c(  1,  NA),
                   pch = c( NA,  18),
                   col = c("green", "blue"),  cex = 0.8)
            legend("topleft", tag, bty = "n", cex = 0.8 )



            ## by sza
            plot( output$SZA, output$Wm2, "l",
                  lwd  = 1.5 , col = "green",
                  ylab = expression( Watt/m^2 ),
                  xlab = "SZA")
            title(main = paste(dateD), cex.main = .8 )

            points( output$SZA, output$st.dev,
                    pch = 18, cex = .4 , col = "blue")

            legend("topright", bty = "n",
                   legend = c( "Direct Normal Irradiance",
                               "Standard deviation")     ,
                   lty = c(  1,  NA),
                   pch = c( NA,  18),
                   col = c("green", "blue"),  cex = 0.8)
            legend("topleft", tag, bty = "n", cex = 0.8 )


        }## if data
        rm(output)
    }## alldays
    dev.off()
}## all years



cat(paste("Before upload to sirena check line endings"))
cat(paste("Run unix2dos"))


## info of script run
tac = Sys.time()
write(sprintf("%s %-10s %-10s %-50s %f",Sys.time(),Sys.info()["nodename"],Sys.info()["login"],Script.Name,difftime(tac,tic,units="mins")),"~/Aerosols/CM21datavalidation/run.log",append=T)
