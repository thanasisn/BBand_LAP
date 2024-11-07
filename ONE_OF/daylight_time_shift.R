#!/usr/bin/env Rscript
#' Copyright (C) 2019-2023 Athanasios Natsis <natsisphysicist@gmail.com>

#### Timeshift fix for Radmon source files

## __ Clear environment  -------------------------------------------------------
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = c("daylight_time_shift.R")

library(data.table)


## __ Notes  -------------------------------------------------------------------

#'
#' Fix erroneous timestamps due to computers daylight time configuration
#' This script shifts time of all channels by 1 hour
#' for the period of the problematic data
#'

channel = "3"
year    = "19"

for (channel in 0:7 ) {

    DATA_FILES = paste0("/home/athan/DATA_RAW/Raddata/",   channel)
    OUTPUT_DIR = paste0("/home/athan/ZHOST/Radddata/",     channel)
    KEEP_FILES = paste0("/home/athan/ZHOST/RadddataORIG/", channel)

    dir.create(OUTPUT_DIR,recursive = T, showWarnings = F)
    dir.create(KEEP_FILES,recursive = T, showWarnings = F)

    allfiles <- list.files(path         = DATA_FILES,
                           pattern      = paste0("*", year, "0", channel, ".LAP"),
                           full.names   = T,
                           ignore.case  = T,
                           recursive    = T,
                           include.dirs = F)

    if (length(allfiles) == 0 ) {
        cat(paste("No input files for channel:", channel))
        # next()
        stop()
    }

    gather <- data.table()
    ## load all data assuming the original date is correct
    for (af in allfiles) {
        cat(paste(af,"\n"))

        theday <- as.Date(strptime(basename(af), "%d%m%y" ))

        ## recreate time stamp for all minutes of day
        D_minutes  <- seq(from = as.POSIXct(paste(theday,"00:00:00 UTC")), length.out = 1440, by = "min" )

        temp <- fread(file = af, na.strings = "-9" )
        stopifnot(nrow(temp) == 1440)

        gather <- rbind( gather, cbind(Date = D_minutes, temp) )

    }

    setorder(gather, Date)

    ## will move one hour up!
    ## for 2019-03-31   2:00  -> 1:00
    ## ....
    ## for 2019-05-13   11:21 -> 10:21

    ## correct time
    data_before  <- gather[ Date <= "2019-03-31 00:59" ]
    data_after   <- gather[ Date >= "2019-05-13 10:21" ]
    ## wrong time
    data_correct <- gather[ Date >= "2019-03-31 02:00" & Date <= "2019-05-13 10:20"]

    ## shift time
    data_correct$Date <- data_correct$Date - 3600

    ## combine all data
    gather2 <- rbind(data_before, data_correct, data_after)

    ## inspect break points
    start <- gather[ Date >= "2019-03-31 00:00" & Date <= "2019-03-31 08:00" ]
    plot(start$Date, start$V1)

    end <- gather[ Date >= "2019-05-13 07:00" & Date <= "2019-05-13 13:00" ]
    plot(end$Date, end$V1)

    start <- gather2[ Date >= "2019-03-31 00:00" & Date <= "2019-03-31 07:00" ]
    plot(start$Date, start$V1)

    end <- gather2[ Date >= "2019-05-13 07:00" & Date <= "2019-05-13 13:00" ]
    plot(end$Date, end$V1)

    ## export only these dates
    export_days <- unique(as.Date(data_correct$Date))

    ## output template
    # 0.1520917  9.10187052951343E-04

    for (ad in export_days) {
        ## data
        temp   <- gather2[ as.Date(Date) == ad ]
        ## date of data
        theday <- as.Date(ad, origin = "1970-01-01")
        ## output file name
        outfile = paste0( OUTPUT_DIR, "/", format(theday, "%d%m%y"),"0",channel,".LAP" )

        ## recreate time stamp for all minutes of day
        D_minutes  <- seq(from = as.POSIXct(paste(theday,"00:00:00 UTC")),
                          length.out = 1440, by = "min" )
        D_minutes  <- data.table(Date = D_minutes)
        temp       <- merge(temp, D_minutes, by = "Date", all = T)

        ## write file in the proper format
        write.table(outfile,
                    na  = "-9",
                    x   = cbind(format(temp[, V1], justify = "left", digits =  6, nsmall = 8, scientific = F  )  ,
                                format(temp[, V2], justify = "left", digits = 15, scientific = T )),
                    quote     = F,
                    sep       = "  ",
                    row.names = F,
                    col.names = F,
                    eol       = "\r\n")

        ## keep original files intact
        keep_file = paste0(KEEP_FILES,"/",format(theday, "%d%m%y"),"0",channel,".LAP")
        orig_file = paste0(DATA_FILES,"/",format(theday, "%d%m%y"),"0",channel,".LAP")

        file.copy(orig_file, keep_file, overwrite = T )
    }
}

cat(paste("Change scientific notation from e -> E\n"))
system(paste0("sed -i 's/e/E/g' ", dirname(OUTPUT_DIR), "/**/*.LAP" ))

cat(paste("Change NA from NA -> -9\n"))
system(paste0("sed -i 's/NA/-9/g' ", dirname(OUTPUT_DIR), "/**/*.LAP" ))

cat(paste("DONE"))

