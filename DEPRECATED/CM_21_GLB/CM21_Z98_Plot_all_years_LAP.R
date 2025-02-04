#!/usr/bin/env Rscript
#' Copyright (C) 2018 Athanasios Natsis <natsisphysicist@gmail.com>
#'

## ffmpeg -framerate 15 -i Agregate%03d.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p yearr.mp4


#'
#' Plot global from TOT files exported
#'

####  Set environment  ####
closeAllConnections()
rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic = Sys.time()
Script.Name = funr::sys.script()
#~ if(!interactive()) {
#~     pdf(file=sub("\\.R$",".pdf",Script.Name))
#~     sink(file=sub("\\.R$",".out",Script.Name),split=TRUE)
#~ }



library(data.table)


####  . . Variables  ####
source("~/CM_21_GLB/DEFINITIONS.R")

## run twice !!

## for pdf only
CREATE_PDF <- TRUE

## for video
CREATE_PDF <- FALSE


tag = paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))

input_files <- list.files(TOT_EXPORT,
                          recursive = T,
                          include.dirs = F,
                          full.names = T)



#### .  . Export range  ####
start_year  <- year(EXPORT_START)
end_year    <- year(EXPORT_STOP) - 1
yearstodo   <- seq( start_year, end_year )


pdffile <- paste0(REPORT_DIR,"All_daily_", start_year,"-",end_year,".pdf")
mp4file <- paste0(REPORT_DIR,"All_daily_", start_year,"-",end_year,".mp4")

tempfolder <- "/dev/shm/temp_daily"
unlink(tempfolder, recursive = T, force = T)
dir.create(tempfolder)


## keep only input for desired output
input_files <- input_files[
    as.logical(
        rowSums(
            sapply(yearstodo, function(x) grepl(as.character(x), input_files ))
        )
    )]


## found manually for plot only
Gl_max <- 1420
Gl_min <- -11

dayofyears <- 366
# dayofyears = 20
videodura  <- 60
framerate  <- as.integer(dayofyears / videodura)

times <- hms::hms(hours = 1:24 )



if (CREATE_PDF) {
    pdf( pdffile , onefile = TRUE )
} else {
    png(paste0(tempfolder,"/Agregate%03d.png"),width = 900, height = 900, units = "px", pointsize = 14)
}


for (doy in 1:dayofyears) {
    cat(paste(doy,"\n"))

    this_day <- grep( sprintf("TOT%03d.*.DAT", doy), input_files, value = T )

    par(mar = c(4,4,2,1))
    par(mgp = c(2.2,1,0))


    plot(1, type = 'n',
         xlab = "UTC", ylab = expression( W/m^2),
         pch = 19, cex = .1,
         col  = "blue", lwd = 1.1, lty = 1, xaxt = "n", ylim = c(Gl_min, Gl_max), xlim = c(2*60,19*60) )

    abline(h = 0, col = "gray60")

    axis(1, at = (0:24) * 60, labels = paste(0:24) )

    abline(v   = (0:24) * 60 ,
           col = "lightgray", lty = "dotted", lwd = par("lwd"))

    ## sd under graph
    for (afil in this_day) {
        data <- fread(afil)
        data$st.dev[ data$"[W.m-2]" < -8 ] <- NA
        data$st.dev[ data$st.dev    < -8 ] <- NA
        points(data$st.dev, pch = "p", cex = .09, col = "red" )
    }


    # ## global with lines
    # for (afil in this_day) {
    #     data <- fread(afil)
    #     data$`[W.m-2]`[ "[W.m-2]" < -90 ] <- NA
    #     # points(data$`[W.m-2]`, pch = "p", cex = .09, col = "blue" )
    #     lines(data$`[W.m-2]`, lwd = 1.1, lty = 1, col = "blue")
    # }

    ## global with points
    for (afil in this_day) {
        data <- fread(afil)
        data$`[W.m-2]`[ data$"[W.m-2]" < -8 ] <- NA
        points(data$`[W.m-2]`, pch = ".", cex = 1, col = "blue")
    }



    title( main = paste(sprintf("%2d files",length(this_day)),
                        "TOT*.DAT  for  ",
                        format(as.Date(doy - 1, origin = "2016-01-01"), "%d %b" ),
                        "  ",
                        sprintf("(%03d)",doy)))

    text(120, Gl_max, labels = tag, pos = 4, cex =.8 )

}

dev.off()


if (!CREATE_PDF) {

    #     system(
    #         paste0("ffmpeg -y -framerate ", framerate, " -i ", paste0(tempfolder,"/Agregate%03d.png"),
    #                " -c:v libx264 -profile:v high  -pix_fmt yuv420p ",
    #                mp4file)
    #     )
    #
    # system(
    #     paste0("ffmpeg -y -framerate 15 -i ", paste0(tempfolder,"/Agregate%03d.png"),
    #            " -c:v libx264 -profile:v high  -pix_fmt yuv420p ",
    #            mp4file)
    # )

    system(
        paste0("ffmpeg -y -framerate 12 -i ", paste0(tempfolder,"/Agregate%03d.png"),
               " -c:v libx264 -profile:v high  -pix_fmt yuv420p ",
               mp4file)
    )
}


# ffmpeg -framerate 15 -i Agregate%03d.png -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p yearr.mp4



## END ##
tac = Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
