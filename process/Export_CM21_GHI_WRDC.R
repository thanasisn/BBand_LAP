# /* #!/opt/R/4.2.3/bin/Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Export Global from CM21 to a WRDC data submission"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Export yearly submission of GHI in WRDC format and data preperation."
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
#'     keep_md:          no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        4
#'     fig_width:        8
#'     fig_height:       5
#'   html_document:
#'     toc:        true
#'     fig_width:  9
#'     fig_height: 4
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#'
#' **L1 -> WRDC**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
#+ echo=F, include=T

#+ echo=F, include=T
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/process/Export_CM21_GHI_WRDC.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(gdata,      warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")


# source("~/CM_21_GLB/Functions_write_data.R")



####  Variables  ####
panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )

tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))

EXPORT_DIR <- "~/DATA/Broad_Band/CM21_H_WRDC_exports/"
dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)


##  Set export range  ----------------------------------------------------------
yearstodo <- seq(2022, 2023)

cat("\n",
    "Will export: ", yearstodo,
    "\n\n")

## Load data base
BB <- opendata()


#+ include=TRUE, echo=F, results="asis"
for (yyyy in yearstodo) {

    cat("\n\\FloatBarrier\n")
    cat("\n\\newpage\n\n")
    cat("\n## Year:", yyyy, "\n\n" )


    ## _ Get data for the year -------------------------------------------------
    DATA <- data.table(BB |>
                           filter(
                               year == yyyy,               ## Select year
                               Elevat >=  0,               ## Drop night data
                               is.na(cm21_bad_data_flag),  ## Ignore bad data
                           ) |>
                           select(Date,
                                  Elevat,
                                  SZA,
                                  GLB_wpsm,
                                  GLB_SD_wpsm) |>
                           collect())


    ## _ Create all minutes  ---------------------------------------------------
    allminutes <- seq(as.POSIXct(paste0(yyyy, "-01-01 00:00:30")),
                      as.POSIXct(paste0(yyyy, "-12-31 23:59:30")),
                      by = "mins")
    allhours   <- seq(as.POSIXct(paste0(yyyy, "-01-01 00:00:30")),
                      as.POSIXct(paste0(yyyy, "-12-31 23:59:30")),
                      by = "hour")
    DATA <- merge(DATA,
                  data.frame(Date = allminutes),
                  by = "Date", all = T)
    DATA[, Day := as.Date(Date)]


    ## _ Remove negative values  -----------------------------------------------
    DATA[GLB_wpsm < 0, GLB_wpsm := 0]


    ## _ Quarters of hour aggregation  -----------------------------------------
    ##    Missing leap seconds on one minute data is OK
    DATAquarter <- DATA[, .(
        Dates      = min(Date),
        qGlobal    = mean(       GLB_wpsm,    na.rm = TRUE),
        qGlobalCNT = sum (!is.na(GLB_wpsm)),
        qGlobalSTD = sd  (       GLB_wpsm,    na.rm = TRUE),
        qElevaMEAN = mean(       Elevat,      na.rm = TRUE),
        qGLstd     = mean(       GLB_SD_wpsm, na.rm = TRUE),
        qGLstdCNT  = sum (!is.na(GLB_SD_wpsm)),
        qGLstdSTD  = sd  (       GLB_SD_wpsm, na.rm = TRUE)
    ),
    by = .(Quarter = as.numeric(Date) %/% (3600/4))]



    ## _ Hours from quarters  --------------------------------------------------
    DATAhour <- DATAquarter[, .(
        Dates      = min(Dates),
        hGlobal    = mean(qGlobal, na.rm = FALSE),   ## na.rm must be FALSE!
        hGlobalCNT = sum(!is.na(qGlobal))
    ),
    by = .(Hours = as.numeric(Dates) %/% 3600)]

    ## _ Prepare data format  --------------------------------------------------

    ##  Make Date stamp more conventional
    DATAhour[, Dates := Dates - 30]
    allhours <- allhours - 30

    ##  Check we don't want gaps in days
    stopifnot(all(DATAhour$Dates == allhours) == TRUE)

    ##  Output for all hours of the year
    Export <- DATAhour[, .(Dates, hGlobal)]
    setorder(Export, Dates)

    ##  WRDC don't want negative values
    Export[hGlobal < 0, hGlobal := 0]
    Export2 <- copy(Export)

    ##  Set NAs to -99 they are old school
    Export[is.na(hGlobal) | is.nan(hGlobal), hGlobal := -99L]

    ##  Create the format they like
    WRDCoutput <- data.frame(year   = year( Export$Dates ),
                             month  = month(Export$Dates ),
                             day    = day(  Export$Dates ),
                             time   = hour( Export$Dates ) + 0.5,
                             global =       Export$hGlobal)

    WRDCoutput2 <- data.frame(year   = year( Export2$Dates ),
                              month  = month(Export2$Dates ),
                              day    = day(  Export2$Dates ),
                              time   = hour( Export2$Dates ) + 0.5,
                              global =       Export2$hGlobal)


    ## _ Write WRDC submission file  -------------------------------------------
    wrdc_fl <- paste0(EXPORT_DIR, "/sumbit_to_WRDC_", yyyy, ".dat")


    ## _ Add headers
    cat("#Thessaloníki global radiation\r\n" ,
        file = wrdc_fl)
    cat("#year month day time(UTC)   global radiation (W/m2)\r\n" ,
        file = wrdc_fl, append = TRUE)

    ## _ Write output line by line
    for (i in 1:nrow(WRDCoutput)) {
        cat(
            sprintf( "%4d  %2d  %2d  %4.1f %10.4f\r\n",
                     WRDCoutput[i,1],
                     WRDCoutput[i,2],
                     WRDCoutput[i,3],
                     WRDCoutput[i,4],
                     WRDCoutput[i,5] ),
            file = wrdc_fl,
            append = TRUE )
    }

    ## replace -99.000 to -99
    system(paste("sed -i 's/   -99.0000/   -99/g' ", wrdc_fl))


    fwrite(WRDCoutput)



write.ftable()
write.fwf(WRDCoutput2, )

}




stop("FIXME")
# there are changes to previous steps that should be implement here probably




#'
#' ## Info
#'
#' Apply data aggregation and export data for submission to WRDC.
#'
#' We calculate the mean global radiation for every quarter of the hour using all available data and ignoring missing values.
#'
#' The mean hourly values are produced only for the cases where all four of the quarters of each hour are present in the data set.
#' If there is any missing quarterly value the hourly value is not exported.
#'
#'
#+ echo=F, include=T


#'
#' ## Aggregation method
#'
#' TODO: describe current method
#'
#' **WARNING: A different method may exist!!**
#'
#+ echo=F, include=T




#+ include=TRUE, echo=F, results="asis"
for (afile in input_files) {




    cat(paste("Data Exported to:", basename(wrdcfile),"\n"))

    panderOptions('table.alignment.default', 'right')
    panderOptions('table.split.table',        120   )


    cat('\\scriptsize\n')

    cat(pander( summary(hourlyoutput) ))

    cat('\\normalsize\n')

    cat('\n')


    plot(test$Dates, test$Global, ylab = "Hourly GHI", xlab = "", main = "Quarterly aggregated hourly GHI")

    hist(test$Global, xlab = "Hourly GHI", main = "Histogram of quarterly aggregated hourly GHI" )


} #END loop of years
#+ echo=F, include=T





# ooooo      <- read.table("output3.dat" )
# ooooo$date <- as.POSIXct(paste0(ooooo$V1,"-",ooooo$V2,"-",ooooo$V3," ",ooooo$V4-0.5,":00") )
#
# kkkk       <- read.table("~/Aerosols/CM21datavalidation/fwdatasubmissionthessaloniki/wrdc_lap_2017.dat")
# kkkk$date  <- as.POSIXct(paste0(kkkk$V1,"-",kkkk$V2,"-",kkkk$V3," ",kkkk$V4-0.5,":00") )
#
#
# (kkkk$V5[kkkk$V5<0 & kkkk$V5>-99])
#
# ayearquarter$day     = as.Date(ayearquarter$Dates)
#
# ## sequence of all days to try
# daystodo = unique( ayearquarter$day )
#
# #### PLOT NORMAL #########################
# totals  = length(daystodo)
# statist = data.frame()
# pbcount = 0
# stime = Sys.time()
# par( mar = c(4,4,3,1) )
# pdf( pdfgraphs, onefile = TRUE)
# for (ddd in daystodo) {
#
#     theday      = as.POSIXct( as.Date(ddd), origin = "1970-01-01")
#     test        = format( theday, format = "%d%m%y06" )
#     dayCMCF     = cm21factor(theday)
#
#     pbcount     = pbcount + 1
#     day         = data.frame()
#     dailyselect = ayearquarter$day == as.Date(theday)
#
#     daydata = ayearquarter[dailyselect,]
#
#     names(daydata) <-  c("Date30", "Global","qGlobalCNT", "qGlobalSTD", "qElevaMEAN", "GLstd", "qGLstdCNT", "qGLstdSTD", "day")
#
#
#
#         ## Main data plot
# dddd = min(daydata$Global, daydata$GLstd , na.rm = TRUE)
# uuuu = max(daydata$Global, daydata$GLstd , na.rm = TRUE)
# if (dddd > -5  ) { dddd = 0  }
# if (uuuu < 190 ) { uuuu = 200}
# ylim = c(dddd , uuuu)
#
# plot(daydata$Date30, daydata$Global,
#      "l", xlab = "UTC", ylab = "W/m^2",
#      col  = "blue", lwd = 1.1, lty = 1, xaxt = "n", ylim = ylim )
# abline(h = 0, col = "gray60")
# abline(v   = axis.POSIXct(1, at = pretty(daydata$Date30, n = 12, min.n = 8 ), format = "%H:%M" ),
#        col = "lightgray", lty = "dotted", lwd = par("lwd"))
# points(daydata$Date30, daydata$GLstd, pch = ".", cex = 2, col = "red" )
# title( main = paste(test, format(daydata$Date30[1] , format = "  %F")))
# text(daydata$Date30[1], uuuu, labels = tag, pos = 4, cex =.7 )
#
#
# }
# dev.off()
#
#
#
# suspecdates = suspecdates[is.element(suspecdates,as.POSIXct(daystodo))]
#
# totals  = length(suspecdates)
# statist = data.frame()
# pbcount = 0
# stime = Sys.time()
# par( mar = c(4,4,3,1) )
# pdf( suspects, onefile = TRUE)
# for (ddd in suspecdates) {
#
#     theday      = as.POSIXct( ddd, origin = "1970-01-01")
#     test        = format( theday, format = "%d%m%y06" )
#     dayCMCF     = cm21factor(theday)
#
#     pbcount     = pbcount + 1
#     day         = data.frame()
#     dailyselect = ayearquarter$day == as.Date(theday)
#
#     daydata = ayearquarter[dailyselect,]
#
#     names(daydata) <-  c("Date30", "Global","qGlobalCNT", "qGlobalSTD", "qElevaMEAN", "GLstd", "qGLstdCNT", "qGLstdSTD", "day")
#
#
#     ## Main data plot
# dddd = min(daydata$Global, daydata$GLstd , na.rm = TRUE)
# uuuu = max(daydata$Global, daydata$GLstd , na.rm = TRUE)
# if (dddd > -5  ) { dddd = 0  }
# if (uuuu < 190 ) { uuuu = 200}
# ylim = c(dddd , uuuu)
#
# plot(daydata$Date30, daydata$Global,
#      "l", xlab = "UTC", ylab = "W/m^2",
#      col  = "blue", lwd = 1.1, lty = 1, xaxt = "n", ylim = ylim )
# abline(h = 0, col = "gray60")
# abline(v   = axis.POSIXct(1, at = pretty(daydata$Date30, n = 12, min.n = 8 ), format = "%H:%M" ),
#        col = "lightgray", lty = "dotted", lwd = par("lwd"))
# points(daydata$Date30, daydata$GLstd, pch = ".", cex = 2, col = "red" )
# title( main = paste(test, format(daydata$Date30[1] , format = "  %F")))
# text(daydata$Date30[1], uuuu, labels = tag, pos = 4, cex =.7 )
#
# }
# dev.off()



#' **END**
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
