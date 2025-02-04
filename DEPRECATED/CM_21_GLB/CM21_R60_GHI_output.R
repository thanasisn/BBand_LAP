# /* Copyright (C) 2022 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Global from CM21. **L0 -> L1**"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Read signal and dark correction and convert to global radiation."
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
#' **S0 -> L1**
#'
#' **Source code: [`github.com/thanasisn/CM_21_GLB`](https://github.com/thanasisn/CM_21_GLB)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display/2-cm21_global/`](https://thanasisn.netlify.app/3-data_display/2-cm21_global/)**
#'
#'
#' - Drop marked data
#'
#+ echo=F, include=T

warning("Deprecated by ~/BBand_LAP/process/Legacy_CM21_R60_export.R")
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
                            return("CM21_R60_") })
if(!interactive()) {
    pdf( file = paste0("~/CM_21_GLB/RUNTIME/", basename(sub("\\.R$",".pdf", Script.Name))))
    sink(file = paste0("~/CM_21_GLB/RUNTIME/", basename(sub("\\.R$",".out", Script.Name))), split = TRUE)
    filelock::lock(paste0("~/CM_21_GLB/LOGs/", basename(sub("\\.R$",".lock", Script.Name))), timeout = 0)
}


## FIXME this is for pdf output
# options(warn=-1) ## hide warnings
# options(warn=2)  ## stop on warnings

#+ echo=F, include=T
####  External code  ####
library(data.table, quietly = TRUE, warn.conflicts = FALSE)
library(pander,     quietly = TRUE, warn.conflicts = FALSE)
source("~/CM_21_GLB/Functions_CM21_factor.R")
source("~/CM_21_GLB/Functions_write_data.R")


####  Variables  ####
source("~/CM_21_GLB/DEFINITIONS.R")
panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )


## temp folder for daily plots
tmpfolder <- paste0("/dev/shm/", sub(pattern = "\\..*", "", basename(Script.Name)))

elevlim   <- -5  ## elevation limit for scatter plots
wattlimit <- 50  ## radiation limit for histograms


####  Execution control  ####
## Default
ALL_YEARS <- FALSE
TEST      <- FALSE
# TEST      <- TRUE
# ALL_YEARS <- TRUE

## When running
args <- commandArgs( trailingOnly = TRUE )
if ( length(args) > 0 ) {
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





####  Get data input files  ####
input_files <- list.files( path    = GLOBAL_DIR,
                           pattern = "^LAP_CM21_H_L0_[0-9]{4}.Rds",
                           full.names = T )
input_years <- as.numeric(
    sub(".rds", "",
        sub(".*_L0_","",
            basename(input_files),),ignore.case = T))


####  Get storage files  ####
output_files <- list.files( path    = GLOBAL_DIR,
                            pattern = "LAP_CM21_H_L1_[0-9]{4}.Rds",
                            full.names = T )



if (!params$ALL_YEARS) {
    years_to_do <- c()
    for (ay in input_years) {
        inp <- grep(ay, input_files,  value = T)
        out <- grep(ay, output_files, value = T)
        if ( length(out) == 0 ) {
            ## do if not there
            years_to_do <- c(years_to_do, ay)
        } else {
            ## do if newer data
            if (file.mtime(inp) > file.mtime(out))
                years_to_do <- c(years_to_do, ay)
        }
        years_to_do <- sort(unique(years_to_do))
    }
} else {
    years_to_do <- sort(unique(input_years))
}



## TEST
if (TEST) {
    cat("\nTEST MODE IS ON!!\n\n")
    years_to_do <- 2022
    # years_to_do <- c(1995, 2015, 2022)
    warning("Overriding years to do: ", years_to_do)
}

## Decide what to do
if (length(years_to_do) == 0 ) {
    stop("NO new data! NO need to parse!")
}
cat(c("\n**YEARS TO DO:", years_to_do, "**\n"))




#'
#' ## CM21 export global radiation data
#'
#' **Set global values between 0 and `r MINglbSUNup` when sun is above `r SUN_ELEV`, to zero**
#'
#' **Drop nighttime data, when  Elevation < `r NIGHT_DROP`**
#'
#'
#+ include=T, echo=F





#+ include=TRUE, echo=F, results="asis"
for (yyyy in years_to_do) {

    cat("\n\n\\FloatBarrier\n\n")
    cat("\\newpage\n\n")
    cat("\n## Year:", yyyy, "\n\n" )

    ####  Get raw data
    afile    <- grep(yyyy, input_files, value = TRUE)
    rawdata  <- readRDS(afile)



    ## create a new temp dir for each year plots
    unlink(tmpfolder, recursive = TRUE)
    dir.create(tmpfolder)
    pbcount <- 0

    ####    Drop flagged records    ############################################
    for (qtag in unique(rawdata$QFlag_1[!is.na(rawdata$QFlag_1)])) {
        N <- rawdata[QFlag_1 == qtag, .N]

        cat(paste0("\n\n**",
                   N, " points droped due to ", qtag, " **\n\n"))
        rawdata <- rawdata[ ! QFlag_1 == qtag | is.na(QFlag_1) ]
    }


    for (qtag  in unique(rawdata$QFlag_2[!is.na(rawdata$QFlag_2)])) {
        N <- rawdata[ QFlag_2 == qtag, .N ]

        cat(paste0("\n\n**",
                   N, " points droped due to ",qtag," **\n\n"))
        rawdata <- rawdata[ ! QFlag_2 == qtag | is.na(QFlag_2) ]
    }


    # ####    Set near zero to zero    ###########################################
    # N <- rawdata[ Elevat >= SUN_ELEV & wattGLB < 0 & wattGLB > MINglbSUNup, .N ]
    # cat(paste0("\n\n**",
    #            N, " points with global near zero set to zero**\n\n"))
    # rawdata[ Elevat >= SUN_ELEV & wattGLB < 0 & wattGLB > MINglbSUNup,
    #          wattGLB := 0 ]


    # ####    Drop night data    #################################################
    # rawdata <- rawdata[ Elevat >= NIGHT_DROP ]
    # rawdata <- rawdata[ ! is.na(wattGLB) ]


    ####    Plot daily    ######################################################
    daystodo <- rawdata[ , .(N = sum(!is.na(wattGLB))), by = .(Days <- as.Date(Date)) ]
    daystodo <- daystodo[ N > 0, Days ]

    for (aday in sort(daystodo)) {
        theday      <- as.Date(aday, origin = "1970-01-01")
        test        <- format(theday, format = "%d%m%y06")
        daymimutes  <- data.frame(
            Date = seq( as.POSIXct(paste(as.Date(theday), "00:00:30")),
                        as.POSIXct(paste(as.Date(theday), "23:59:30")), by = "min")
        )
        daydata     <- rawdata[as.Date(Date) == as.Date(theday)]
        daydata     <- merge(daydata, daymimutes, all = T)
        pbcount     <- pbcount + 1


        ####    Normal plots    ################################################
        pdf(file = paste0(tmpfolder,"/daily_", sprintf("%05d.pdf", pbcount)), )
            # fix plot range
            dddd <- min(daydata$wattGLB,
                        daydata$wattGLB_SD, na.rm = TRUE)
            uuuu <- max(daydata$wattGLB,
                        daydata$wattGLB_SD, na.rm = TRUE)
            xlim <- range(daydata$Date)
            if (dddd > -5  ) {dddd = 0  }
            if (uuuu < 190 ) {uuuu = 300}
            ylim = c(dddd, uuuu)

            plot(1, type = "n",                         # Remove all elements of plot
                 xlab = "UTC", ylab = expression(W / m^2),
                 xaxt = "n", ylim = ylim, xlim = xlim, xaxs = "i")
            abline(h = 0, col = "gray60")

            lines(daydata$Date, daydata$wattGLB,
                 col = "green", lwd = 1.1, lty = 1)

            abline(v   = axis.POSIXct(1, at = pretty(daydata$Date, n = 12, min.n = 8 ), format = "%H:%M" ),
                   col = "lightgray", lty = "dotted", lwd = par("lwd"))
            points(daydata$Date, daydata$wattGLB_SD, pch = ".", cex = 2, col = "red" )
            title( main = paste(test, format(daydata$Date[1], format = "  %F")))
            text(daydata$Date[1], uuuu, labels = tag, pos = 4, cex =.8 )

            legend("topright", bty = "n",
                   legend = c("Global Irradiance with dark cor.",
                              "Standard Deviation"),
                   lty = c(1,NA), pch = c(NA,"."),
                   col = c("green", "red"), cex = 0.8,)
        dev.off()

    }##END loop of days


    ####    Save data for this year    #########################################
    rawdata[ , QFlag_1 := NULL ]
    rawdata[ , QFlag_2 := NULL ]
    write_RDS(object = rawdata,
              file   = paste0(GLOBAL_DIR,"/LAP_CM21_H_L1_", yyyy, ".Rds"))


    ## create pdf with all daily plots
    system(paste0("pdftk ", tmpfolder, "/daily*.pdf cat output ",
                  paste0(DAILYgrDIR,"Daily_GHI_L1_", yyyy, ".pdf")),
           ignore.stderr = T )



    ####    Yearly plots    ####################################################

    ##  Add time column (same date with original times)
    dummytimes    <- strftime(  rawdata$Date, format = "%H:%M:%S")
    rawdata$Times <- as.POSIXct(dummytimes,   format = "%H:%M:%S")


    par(mar = c(2,4,2,1))
    plot(rawdata$Times, rawdata$wattGLB,
         pch = ".", cex = 1.5, ylab = expression(W / m^2) , xlab = "",
         main = paste(yyyy, "Global" ))
    cat("\n\n")


    plot(rawdata$Date, rawdata$wattGLB, pch = 19, cex = 0.5,
         xlab = "", ylab = expression(W / m^2),
         main = paste("Global radiation", yyyy))
    cat("\n\n")


    plot(rawdata$Elevat, rawdata$wattGLB, pch = 19, cex = 0.5,
         xlab = "", ylab = expression(W / m^2),
         main = paste("Global radiation", yyyy))
    cat("\n\n")


    plot(rawdata$Azimuth, rawdata$wattGLB, pch = 19, cex = 0.5,
         xlab = "", ylab = expression(W / m^2),
         main = paste("Global radiation", yyyy))
    cat("\n\n")


    par(mar = c(4, 4, 2, 1))
    morning  <-   rawdata$preNoon & rawdata$Elevat > elevlim
    evening  <- ! rawdata$preNoon & rawdata$Elevat > elevlim

    plot(  rawdata$Elevat[morning], rawdata$wattGLB[morning], pch = ".", cex = 1.5, col = "blue",
           main = paste(yyyy, "Global " ), ylab = expression(W / m^2), xlab = expression("Elevation [°]"))
    points(rawdata$Elevat[evening], rawdata$wattGLB[evening], pch = ".", cex = 1.5,  col = "green")
    legend("topleft", legend = c("Before noon", "After noon"),
           bty="n" ,text.col = c("blue", "green"), cex = 1)
    cat("\n\n")



    all    <- cumsum(tidyr::replace_na(rawdata$wattGLB, 0))
    pos    <- rawdata[ wattGLB > 0 ]
    pos$V1 <- cumsum(tidyr::replace_na(pos$wattGLB, 0))
    neg    <- rawdata[ wattGLB < 0 ]
    neg$V1 <- cumsum(tidyr::replace_na(neg$wattGLB, 0))
    xlim   <- range(rawdata$Date)
    plot(rawdata$Date, all,
         type = "l",
         xlim = xlim,
         ylab = "",
         yaxt = "n", xlab = "",
         main = paste("Cum Sum of CM-21 signal ",  yyyy) )
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


    all    <- cumsum(tidyr::replace_na(rawdata$wattGLB_SD, 0))
    pos    <- rawdata[ wattGLB_SD > 0 ]
    pos$V1 <- cumsum(tidyr::replace_na(pos$wattGLB_SD, 0))
    neg    <- rawdata[ wattGLB_SD < 0 ]
    neg$V1 <- cumsum(tidyr::replace_na(neg$wattGLB_SD, 0))
    xlim   <- range(rawdata$Date)
    plot(rawdata$Date, all,
         type = "l",
         xlim = xlim,
         ylab = "",
         yaxt = "n", xlab = "",
         main = paste("Cum Sum of CM-21 sd ",  yyyy) )
    par(new = TRUE)
    plot(pos$Date, pos$V1,
         xlim = xlim,
         col = "blue", type = "l",
         ylab = "", yaxt = "n", xlab = "", xaxt = "n")
    if (nrow(neg)>0){
        par(new = TRUE)
        plot(neg$Date, neg$V1,
             xlim = xlim,
             col = "red", type = "l",
             ylab = "", yaxt = "n", xlab = "", xaxt = "n")
        cat('\n\n')
    }


    hist(rawdata$wattGLB[ rawdata$wattGLB > wattlimit],
         main = paste(yyyy, "Global ", "radiation > ",wattlimit),
         breaks = 40 , las=1, probability =  T, xlab = expression(W / m^2))
    lines(density(rawdata$wattGLB, na.rm = T) , col ="green" , lwd = 2)
    cat("\n\n")


    hist(rawdata$wattGLB_SD,
         main = paste(yyyy, "Global standard deviation "),
         breaks = 40 , las=1, probability =  T, xlab = expression(W / m^2))
    # lines(density(rawdata$wattGLB_SD, na.rm = T) , col ="green" , lwd = 2)
    cat("\n\n")


    par(mar = c(2,4,2,1))
    week_vec = strftime(  rawdata$Date , format = "%W")
    sunnupp = rawdata$Elevat >= 0


    boxplot(rawdata$wattGLB[sunnupp] ~ week_vec[sunnupp] )
    title(main = paste(yyyy, "Global irradiance (Elevation > 0)"))
    cat("\n\n")


    boxplot(rawdata$wattGLB_SD[sunnupp] ~ week_vec[sunnupp] )
    title(main = paste(yyyy, "Global standard deviation (Elevation > 0)"))
    cat("\n\n")


    # boxplot(rawdata[sunnupp, CM21value - CM21valueWdark  ] ~ week_vec[sunnupp],
    #         ylab = "")
    # title(main = paste(yyyy, "CM21 dark offset (Elevation > 0)"))
    # cat("\n\n")
}
#'
#+ include=T, echo=F


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
