#!/usr/bin/env Rscript
# /* Copyright (C) 2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#### Tool to visual inspect the measurements of the instruments.

rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- tryCatch({ funr::sys.script() },
                        error = function(e) { cat(paste("\nUnresolved script name: ", e),"\n\n")
                            return("inspect_days_sirena_") })
sink(file   = paste0("~/BBand_LAP/LOGs/", basename(sub("\\.R$", ".log", Script.Name))),
     split  = TRUE,
     append = TRUE)

## TODO
## plot steps
## plot async
## plot sun


library(data.table, quietly = T, warn.conflicts = F)
library(optparse,   quietly = T, warn.conflicts = F)
library(plotly,     quietly = T, warn.conflicts = F)

source("~/CM_21_GLB/Functions_CM21_factor.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")

## excluded data mark
## if can not read skip files there me be formatting error!!
ranges_CHP1       <- read.table("~/Aerosols/source_R/PARAMS/Skip_ranges_CHP1.dat",
                                sep          = ";",
                                colClasses   = "character",
                                strip.white  = TRUE,
                                header       = TRUE,
                                comment.char = "#" )
ranges_CHP1$From  <- strptime(ranges_CHP1$From,  format = "%F %H:%M", tz = "UTC")
ranges_CHP1$Until <- strptime(ranges_CHP1$Until, format = "%F %H:%M", tz = "UTC")

## check negative ranges
if (!all((ranges_CHP1$Until - ranges_CHP1$From) >= 1)) {
    stop("Inverted ranges in Skip_ranges_CHP1.dat !!!!!!!")
}

ranges_CM21       <- read.table("~/Aerosols/source_R/PARAMS/Skip_ranges_CM21.dat",
                                sep          = ";",
                                colClasses   = "character",
                                strip.white  = TRUE,
                                header       = TRUE,
                                comment.char = "#" )
ranges_CM21$From  <- strptime(ranges_CM21$From,  format = "%F %H:%M", tz = "UTC")
ranges_CM21$Until <- strptime(ranges_CM21$Until, format = "%F %H:%M", tz = "UTC")

## check negative ranges
if (!all((ranges_CM21$Until - ranges_CM21$From) >= 1)) {
    stop("Inverted ranges in Skip_ranges_CM21.dat !!!!!!!")
}




## For qutebrowser ----
## Not so nice but works
# BROWSER_CMD <- "qutebrowser --backend webengine "
## --

## For Brave ----
## use a user-data-dir to avoid load my custom colors
BROWSER_CMD <- "brave-browser --window-size=1240,720 --user-data-dir=/tmp/bravetmp --incognito -app=file://"
# cat(paste("Will always do a killall",gsub(" .*", "", BROWSER_CMD), "!!"), "\n")
# system(paste( "killall", gsub(" .*", "", BROWSER_CMD)))
## --

## Data folder
# FOLDER <- "~/DATA_RAW/Raddata"
FOLDER <- "~/DATA_RAW/Bband"
SIRTOT <- "~/DATA/cm21_data_validation/AC21_lap.GLB_TOT/" # data on sirena
NEWTOT <- "~/DATA/cm21_data_validation/AC21_lap.GLB_NEW/" # exported by me


####   Get input    ############################################################
MINDATE <- as.Date("1993-01-01")
MAXDATE <- as.Date(Sys.Date())
MINSTEP <- 1
MAXSTEP <- 400
INITDAY <- paste0(year(Sys.Date()), "-01-01")
INISTEP <- 3


## TEST override start day
INITDAY <- "2023-03-15"
INISTEP <- 1

option_list <-  list(
    make_option(c("-d", "--day"),
                type    = "character",
                default = INITDAY,
                help    = paste0("Start day of ploting yyyy-mm-dd, [",MINDATE,", ",MAXDATE,"]"),
                metavar = "yyyy-mm-dd"),
    make_option(c("-s", "--step"),
                type    = "integer",
                default = INISTEP,
                help    = paste0("Step width in days, [",MINSTEP,", ",MAXSTEP,"]"),
                metavar = "integer")
)
opt_parser <- OptionParser(option_list = option_list)
args       <- parse_args(opt_parser)

STARTDAY <- args$day
STARTDAY <- as.Date(STARTDAY)
STEP     <- args$step

cat("Start day:", paste(STARTDAY),"\n")
cat("Step     :", STEP, "\n")

if (!(MINDATE <= STARTDAY & STARTDAY <= MAXDATE)) {
    stop(STARTDAY, " is invalid date")
}

if (!(MINSTEP <= STEP & STEP <= MAXSTEP)) {
    stop(STEP, " is invalid step")
}



####    Init    ################################################################

## global sigbal files
globalfiles <- list.files(path        = FOLDER,
                          recursive   = TRUE,
                          pattern     = "[0-9]*06.LAP$",
                          ignore.case = TRUE,
                          full.names  = TRUE)
## direct signal files
directfiles <- list.files(path        = FOLDER,
                          recursive   = TRUE,
                          pattern     = "[0-9]*03.LAP$",
                          ignore.case = TRUE,
                          full.names  = TRUE)
## current global files
totalfiles <- list.files(path        = SIRTOT,
                         recursive   = TRUE,
                         pattern     = "TOT[0-9]{5}.dat",
                         ignore.case = TRUE,
                         full.names  = TRUE)
## new global files
newtotfiles <- list.files(path        = NEWTOT,
                          recursive   = TRUE,
                          pattern     = "TOT[0-9]{5}.dat",
                          ignore.case = TRUE,
                          full.names  = TRUE)



if (STEP == 1) {
    MOVE <- 1
} else {
    MOVE <- STEP - 1
}

daystodo <- seq(STARTDAY, MAXDATE, by = MOVE)

# daystodo <- daystodo[1:3]

## loop plots
for (ap in daystodo) {
    toplot <- seq.Date( as.Date(ap, origin = "1970-01-01"), length.out = STEP, by = "day")
    gather <- data.table()

    ## loop days to plot with step
    for (ad in toplot) {
        theday    <- as.Date(ad, origin = "1970-01-01")
        strday    <- format(theday, "%d%m%y")
        cat("\n - - - - - - - - - - - - - - - - - - - \n")
        cat("Load:", format(theday, "%F"), "\n")

        ## get file names
        glbfile   <- grep( paste0(strday,"06.lap"), globalfiles, value = T, ignore.case = T )
        dirfile   <- grep( strday, directfiles, value = T )
        totfile   <- grep(format(theday, "%0j%y"), totalfiles, value = T)
        newfile   <- grep(format(theday, "%0j%y"), newtotfiles, value = T)

        ## test file names
        stopifnot(length(totfile) <= 1)
        stopifnot(length(glbfile) <= 1)
        stopifnot(length(dirfile) <= 1)
        stopifnot(length(newfile) <= 1)

        ## create time
        D_minutes <- seq(from       = as.POSIXct(paste(theday,"00:00:00 UTC")),
                         length.out = 1440,
                         by         = "min" )

        ## read global signal file
        if (length(glbfile) == 1) {
            glb <- fread(glbfile)
            names(glb) <- c("GLBsig", "GLBsd")
            ## clean missing
            glb[GLBsig < -8, GLBsig := as.numeric(NA)]
            glb[GLBsd  < -8, GLBsd  := as.numeric(NA)]
            ## burn extreme
            glb[GLBsig > 10, GLBsig := 10 ]
            glb[GLBsd  > 10, GLBsd  := 10 ]

        } else {
            cat(paste0("Missing global   : ", strday,"06.lap"),"\n")
            glb        <- data.table()
            glb$GLBsig <- rep(as.numeric(NA), 1440)
            glb$GLBsd  <- rep(as.numeric(NA), 1440)
        }
        ## read direct signal file
        if (length(dirfile) == 1) {
            dir <- fread(dirfile)
            names(dir) <- c("DIRsig", "DIRsd")
            ## clean missing
            dir[DIRsig < -8, DIRsig := as.numeric(NA)]
            dir[DIRsd  < -8, DIRsd  := as.numeric(NA)]
            ## burn extreme
            dir[DIRsig > 10, DIRsig := 10]
            dir[DIRsd  > 10, DIRsd  := 10]
        } else {
            cat(paste0("Missing direct   : ", strday,"03.lap"),"\n")
            dir        <- data.table()
            dir$DIRsig <- rep(as.numeric(NA), 1440)
            dir$DIRsd  <- rep(as.numeric(NA), 1440)
        }
        ## read existing total file
        if (length(totfile) == 1) {
            tot <- fread(totfile)
            names(tot) <- c("TIME_UT", "SZA", "TOTsig", "TOTsd")
            tot$SZA     <- NULL
            tot$TIME_UT <- NULL
            tot[ TOTsig == -9, TOTsig := NA ]
            tot[ TOTsd  == -9, TOTsd  := NA ]
        } else {
            cat(paste0("Missing TOTAL   : ", format(theday, "TOT%0j%y.DAT")),"\n")
            tot        <- data.table()
            tot$TOTsig <- rep(NA, 1440)
            tot$TOTsd  <- rep(NA, 1440)
        }
        ## read new total file
        if (length(newfile) == 1) {
            new <- fread(newfile)
            names(new) <- c("TIME_UT", "SZA", "NEWsig", "NEWsd")
            new$SZA     <- NULL
            new$TIME_UT <- NULL
            new[ NEWsig == -9, NEWsig := NA ]
            new[ NEWsd  == -9, NEWsd  := NA ]
        } else {
            cat(paste0("Missing new TOTAL: ", format(theday, "TOT%0j%y.DAT")),"\n")
            new        <- data.table()
            new$NEWsig <- rep(NA, 1440)
            new$NEWsd  <- rep(NA, 1440)
        }

        daydt  <- cbind(Date = D_minutes, glb, dir, tot, new)
        gather <- rbind(gather, daydt)
    }

    ## keep signal for debugging
    gather$GLBraw <- gather$GLBsig

    ## convert to radiation
    gather$GLBsig <- as.numeric(gather$GLBsig) * cm21factor(gather$Date)
    gather$GLBsd  <- gather$GLBsd  * cm21factor(gather$Date)
    gather$DIRsig <- gather$DIRsig * chp1factor(gather$Date)
    gather$DIRsd  <- gather$DIRsd  * chp1factor(gather$Date)

    ## find bad data marks
    bad_chp1 <- data.table()
    for (i in 1:nrow(ranges_CHP1)) {
        lower <- ranges_CHP1$From[   i]
        upper <- ranges_CHP1$Until[  i]
        comme <- ranges_CHP1$Comment[i]
        ## mark bad regions of data
        tmp <- gather[Date >= lower & Date < upper & !is.na(DIRsig) ]
        tmp$comment <- comme
        bad_chp1 <- rbind(bad_chp1, tmp)
    }

    bad_cm21 <- data.table()
    for (i in 1:nrow(ranges_CM21)) {
        lower <- ranges_CM21$From[   i]
        upper <- ranges_CM21$Until[  i]
        comme <- ranges_CM21$Comment[i]
        ## mark bad regions of data
        tmp <- gather[Date >= lower & Date < upper & !is.na(GLBsig) ]
        tmp$comment <- comme
        bad_cm21 <- rbind(bad_cm21, tmp)
    }


    ## Base Plot
    # xlim <- range(gather$Date)
    # ylim <- range(0, 300, gather$GLBsig, gather$DIRsig, gather$GLBsd, gather$DIRsd, na.rm = T)
    #
    # plot(NULL, xlab="", ylab="", xlim = xlim, ylim = ylim, xaxt = "n")
    # axis.POSIXct(1, gather$Date, format = "%F")
    # axis.POSIXct(1, at = seq(min(gather$Date), max(gather$Date), "2 hours"),
    #           labels = FALSE, tcl = -0.2)
    #
    # lines(gather$Date, gather$GLBsig, col = "green")
    # lines(gather$Date, gather$DIRsig, col = "blue")
    #
    # points(gather$Date, gather$GLBsd, col = "green", pch = 8, cex = 0.3)
    # points(gather$Date, gather$DIRsd, col = "blue" , pch = 8, cex = 0.3)
    #
    # if (nrow(bad_chp1) > 1 ) {
    #     points(bad_chp1$Date, bad_chp1$DIRsig, col = "red" )
    # }
    # if (nrow(bad_cm21) > 1 ) {
    #     points(bad_cm21$Date, bad_cm21$GLBsig, col = "red" )
    # }





    ## Plotly

    fig <- plot_ly()
    ## plot lines of radiation
    fig <- add_trace(fig, x = gather$Date, y = gather$NEWsig,
                     name = "my NEW Total",
                     line = list(color = "magenta"),
                     text = paste(format(gather$Date, "%F %R"),"\n","NEW:",round(gather$NEWsig,1)),
                     hoverinfo = 'text',
                     mode = "lines", type = "scatter")
    fig <- add_trace(fig, x = gather$Date, y = gather$DIRsig,
                     name = "Direct beam",
                     line = list(color = "blue"),
                     text = paste(format(gather$Date, "%F %R"),"\n","DBI:",round(gather$DIRsig,1)),
                     hoverinfo = 'text',
                     mode = "lines", type = "scatter")
    fig <- add_trace(fig, x = gather$Date, y = gather$GLBsig,
                     name = "Global on-the-fly",
                     line = list(color = "green"),
                     text = paste(format(gather$Date, "%F %R"),"\n","GHI:",round(gather$GLBsig,1)),
                     hoverinfo = 'text',
                     mode = "lines", type = "scatter")
    fig <- add_trace(fig, x = gather$Date, y = gather$TOTsig,
                     name = "Total (sirena)",
                     line = list(color = "cyan"),
                     text = paste(format(gather$Date, "%F %R"),"\n","TOT:",round(gather$TOTsig,1)),
                     hoverinfo = 'text',
                     mode = "lines", type = "scatter")
    ## plot standard deviation points
    fig <- add_trace(fig, x = gather$Date, y = gather$TOTsd,
                     name = "Total (sirena) SD",
                     marker = list(color = "cyan", symbol = "asterisk-open", size = 2),
                     text = paste("TOT SD:",round(gather$TOTsd,1)),
                     hoverinfo = 'text',
                     # showlegend = FALSE,
                     mode = 'markers', type = "scatter")
    fig <- add_trace(fig, x = gather$Date, y = gather$DIRsd,
                     name = "Direct beam SD",
                     marker = list(color = "blue", symbol = "asterisk-open", size = 2),
                     text = paste("DBI SD:",round(gather$DIRsd,1)),
                     hoverinfo = 'text',
                     # showlegend = FALSE,
                     mode = 'markers', type = "scatter")
    fig <- add_trace(fig, x = gather$Date, y = gather$GLBsd,
                     name = "Global SD",
                     marker = list(color = "green", symbol = "asterisk-open", size = 2),
                     # text = paste(format(gather$Date, "%F %R"),"\n",round(gather$GLBsd,1)),
                     text = paste("GHI SD:",round(gather$GLBsd,1)),
                     hoverinfo = 'text',
                     # showlegend = FALSE,
                     mode = 'markers', type = "scatter")

    ## plot excluded ranges
    if (nrow(bad_chp1) > 0 ) {
        fig <- add_trace(fig, x = bad_chp1$Date, y = bad_chp1$DIRsig,
                         name = "Excluded CHP1",
                         text   = bad_chp1$comment,
                         marker = list(color = "red", symbol = "square-open", size = 10),
                         mode = 'markers', type = "scatter")
    }
    if (nrow(bad_cm21) > 0 ) {
        fig <- add_trace(fig, x = bad_cm21$Date, y = bad_cm21$GLBsig,
                         name   = "Excluded CM21",
                         text   = bad_cm21$comment,
                         marker = list(color = "red", symbol = "square-open", size = 10),
                         mode   = 'markers', type = "scatter")
    }


    fig <- layout(fig, legend = list(x = 0.85, y = 0.95, bgcolor = 'rgba(75,75,75,0.3)'))
    # fig <- layout(fig, xaxis  = list(showcrossline = T))
    # fig <- layout(fig, hovermode = "x unified")
    fig <- layout(fig, hovermode = "x")

   #  fig
   #  print(fig)
   #  show (fig)
   #  stop()

    # Generate random file name
    temp <- paste(tempfile('plotly'), 'html', sep = '.')
    cat(paste(temp),"\n")

    # Save. Note, leaving selfcontained=TRUE created files that froze my browser
    htmlwidgets::saveWidget(fig, temp, selfcontained = FALSE)

    ## Launch with desired application

    ## must not have other plottly open
    # system(sprintf("brave-browser --incognito -app=file://%s ", temp),
    #        wait = TRUE)

    ## ugly
    # system(sprintf("qutebrowser --backend webengine %s", temp),
    #        wait = TRUE)

    ## slow
    # system(sprintf("firefox %s", temp),
    #        wait = TRUE)

    ## call the file in browser
    ## Brave need to have only one instance
    system(paste( "killall", gsub(" .*", "", BROWSER_CMD)))
    system(paste0(BROWSER_CMD, temp), wait = TRUE)
    file.remove(temp)

}





# table(round(gather$GLBsig/gather$TOTsig))
#
# testdd <- as.POSIXct("2004-07-03 10:00:00")
# dd <- gather[Date == testdd]
#
# cm21factor(testdd)
# dd$TOTsig / dd$GLBraw
# dd$GLBsig / dd$GLBraw
# dd$GLBsig / dd$TOTsig
