# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Check total radiation export for sirena"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
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
#'     toc_depth:        4
#'     fig_width:        8
#'     fig_height:       5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' params:
#'   CLEAN: TRUE
#'
#' ---

#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
#+ echo=F, include=T

## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = "!ht"   )
knitr::opts_chunk$set(echo      = FALSE   )

## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_db/60_Inspect_CM21_sig.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/R_myRtools/myRtools/R/write_.R")


library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)


##  Read and cache all tot exported TOT  ---------------------------------------
rdsfile    <- "~/DATA/cm21_data_validation/exported_tot_dat.Rds"
TOT_EXPORT <- "~/DATA/cm21_data_validation/AC21_lap.GLB_NEW_BB/"

##  List files
files <- list.files(TOT_EXPORT,
                    pattern     = "[0-9]*\\TOT.*.dat",
                    recursive   = T,
                    ignore.case = T,
                    full.names  = T)

##  Cache data, check if we have to read
if (!file.exists(rdsfile) |  max(file.mtime(files)) > file.mtime(rdsfile)) {
    DT <- data.table()
    for (af in files) {
        temp      <- fread(af)
        partdate  <- sub(".dat", "", sub(".*TOT","",af), ignore.case = T)
        date      <- as.POSIXct(strptime(partdate, "%j%y" ))
        temp$Date <- date
        temp$file <- af
        DT        <- rbind(DT, temp)
        cat(print(date),"\r")
    }
    cat("Cache data\n")
    writeDATA(DT, rdsfile)
} else {
    cat("Load cached data\n")
    DT <- readRDS(rdsfile)
}

##  Set NAs
DT[ `[W.m-2]` < -8, `[W.m-2]` := NA ]
DT[  st.dev   < -8,  st.dev   := NA ]


## export for validation of my process
## Do we still need that?
temp      <- copy(DT)
dateess   <- paste(temp$Date, temp$TIME_UT %/% 1, round((temp$TIME_UT %% 1) * 60) )
temp$Date <- as.POSIXct( strptime(dateess, "%F %H %M") )
temp[, file    := NULL]
temp[, TIME_UT := NULL]
names(temp)[names(temp) == "[W.m-2]"] <- "WATTTOT"
write_RDS(temp, "~/DATA/Broad_Band/CM21_TOT_2")
rm(temp)




#'
#' ## By year
#'
pander(
    DT[ , .(Files      = length(unique(file)),
            Datapoints = .N,
            Missing    = sum(is.na(`[W.m-2]`)),
            MissPerC   = round(100*sum(is.na(`[W.m-2]`))/.N, digits = 1 )),
        by = year(Date)]
)
#'
#' Unique files: `r length(unique(DT$file))`
#'
#' Unique days:  `r length(unique(DT$Date))`
#'

missing <- DT[, .(Missing = sum(is.na(`[W.m-2]`))), by = as.Date(Date)]
missing[ Missing == 0, Missing := NA]
plot(missing$as.Date, missing$Missing,
     pch = 19, cex = 0.5, main = "Missing data by day")
cat(" \n \n")

missing <- DT[, .(Missing = sum(is.na(`[W.m-2]`))), by = .(year(Date), month(Date)) ]
missing[, as.Date := as.Date(paste(year, month, "01"),"%Y %m %d")]
plot(missing$as.Date, missing$Missing,
     pch = 19, cex = 0.5, main = "Missing data by month")
cat(" \n \n")



#'
#' ## Data points by hour
#'
#+ echo=T, include=T
pander(
    table(DT$TIME_UT %/% 1)
)
#'
# hist( DT$TIME_UT %/% 1, breaks = 25)


#'
#' ## Data points by minute
#'
hist(round((DT$TIME_UT %% 1) * 60), breaks = -1:61)
cat(" \n \n")

#+ echo=F, include=F
# DT[ TIME_UT %/% 1 == 24, unique(file)]

count <- DT[, .N, by = file]
#' Do all files has 1440 records: `r all(count$N == 1440)`
pander(table(count$N))


## add nice date
dateess <- paste(DT$Date, DT$TIME_UT %/% 1, round((DT$TIME_UT %% 1) * 60))
DT$Date <- as.POSIXct(strptime(dateess, "%F %H %M"))
setorder(DT,Date)


#' ## Drop some data
#+ echo=T, include=T
DT <- DT[!is.na(`[W.m-2]`), ]
DT <- DT[`[W.m-2]` > -5   , ]
#'


#+ echo=F
#' ## Inspect all data
pander(summary(DT))
cat(" \n \n")
plot(DT$Date, DT$`[W.m-2]`)
cat(" \n \n")
hist(DT$`[W.m-2]`)
cat(" \n \n")


# ## apply exclusions this have been done for almost all data  me
# ranges       <- read.table( "~/Aerosols/source_R/PARAMETRICkip_date_ranges_CM21.txt",
#                             sep = ";",
#                             colClasses = "character",
#                             header = TRUE, comment.char = " )
# ranges$From  <- strptime(ranges$From,  format = "%F %H:%M",  = "UTC")
# ranges$Until <- strptime(ranges$Until, format = "%F %H:%M",  = "UTC")
#
# for ( i in 1:nrow(ranges) ) {
#     lower <- ranges$From[  i ]
#     upper <- ranges$Until[ i ]
#     ## select to remove
#     select  <- DT$Date >= lower & DT$Date <= upper
#     DT  <- DT[ ! select ]
#     rm(select)
# }


#' ## Monthly values
#+ echo=F
temp <- DT[, .(Mean   = mean(  `[W.m-2]`, na.rm = T),
               Max    = max(   `[W.m-2]`, na.rm = T),
               Min    = min(   `[W.m-2]`, na.rm = T),
               Median = median(`[W.m-2]`, na.rm = T)),
           by = .(year(Date), month(Date))]
temp$Date <- as.POSIXct(strptime( paste( temp$year, temp$month, "1"), "%Y %m %d" ))


plot(temp$Date, temp$Mean,   "l", main = "Monthly Mean")
cat(" \n \n")
plot(temp$Date, temp$Max,    "l", main = "Monthly Max")
cat(" \n \n")
# plot(temp$Date, temp$Min,    "l", main = "Monthly Min")
plot(temp$Date, temp$Median, "l", main = "Monthly Median")
cat(" \n \n")


#+ echo=F
DT$file    <- NULL
DT$TIME_UT <- NULL
names(DT)[names(DT) == "[W.m-2]"] <- "wattGLB"


perc    <- 0.99999
uplim   <- quantile(DT$wattGLB, na.rm = T, probs = perc)
datespp <- DT[wattGLB > uplim, unique(as.Date(Date)) ]


#'
#' ## Extreme values
#'
#+ echo=F, include=T, results='asis'
options(digits = 6)
cat(paste("There are", length(datespp),
    "days with more than", uplim,
    "watts, representing", (1 - perc) * 100,
    "% of the data.\n"))

#'
#' ## Plot all extreme days
#'
for (ad in datespp ) {
    pp <- DT[ as.Date(Date) == ad ]
    plot(  pp$Date, pp$wattGLB, "l", col = "green")
    points(pp$Date, pp$st.dev, col = "blue", pch=19, cex=.2)
    title(as.Date(ad, origin = "1970-01-01"))
    cat(" \n \n")
}

#'
#' ## Plot all extreme days in time series
#'
pp <- DT[as.Date(Date) %in% datespp]
plot(pp$Date, pp$wattGLB, "l", col = "green")
points(pp$Date, pp$st.dev, col = "blue", pch=19, cex=.2)
cat(" \n \n")



# writeDATA(data,
#           datfile,
#           contact = "natsisphysicist@gmail.com",
#           type = "dat")


#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
