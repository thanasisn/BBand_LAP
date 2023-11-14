# /* #!/opt/R/4.2.3/bin/Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title: "CM21 export GHI data for Sirena."
#' author: "Natsis Athanasios"
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
#' - \usepackage{multicol}
#' - \setlength{\columnsep}{1cm}
#'
#' output:
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     keep_md:          no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     fig_width:        7
#'     fig_height:       4.5
#'   html_document:
#'     toc:        true
#'     fig_width:  7.5
#'     fig_height: 5
#' date: "`r format(Sys.time(), '%F')`"
#' params:
#'    ALL_YEARS: TRUE
#' ---

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
Script.Name <- "~/BBand_LAP/process/Export_CM21_TOT.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")


##  Variables  -----------------------------------------------------------------
panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )

tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))


## compute SZA as other broadband
zenangle <- function(YYYY,min,doy){
    as.numeric(system(paste("~/CM_21_GLB/BINARY/zenangle64 ", YYYY ,min, doy, " 40.634 -22.956" ), intern = T))
}
vzen <- Vectorize(zenangle, "min")

##  Set export range  ----------------------------------------------------------
yearstodo <- seq(2022, year(Sys.time()))


#'
#' ## Info
#'
#' Export GHI for 'sirena' for the period `r min(yearstodo)` - `r max(yearstodo)`
#'
#' **Sun angles are calculated with other method than the rest of the broadband.**
#' This will change in the future.
#'
#' **We allow negative values of Global Radiation on export.**
#'
#' All missing values (NA) are set to "-9".
#'
#+ include=T, echo=F



#+ include=T, echo=F, results="asis"
cat("\n",
    "Will export: ", yearstodo,
    "\n\n")


## Load data base
BB <- opendata()


#+ include=TRUE, echo=F, results="asis"
for (yyyy in yearstodo) {

    cat('\n\\normalsize\n')
    cat("\n\\FloatBarrier\n")
    cat("\\newpage\n\n")
    cat("\n## Year:", yyyy, "\n\n")


    DATA <- data.table(BB |>
                           filter(year == yyyy) |>
                           select(
                               Date,
                               SZA,
                               lap_sza,
                               GLB_strict,
                               GLB_wpsm,
                               cm21_bad_data_flag,
                               GLB_SD_wpsm
                           ) |> collect()
                       )


    DATA[is.na(lap_sza)]



stop("DDDD")

}






#+ include=TRUE, echo=F, results="asis"
for (afile in input_files) {

    ## create all minutes
    allminutes <- seq( as.POSIXct(paste0(yyyy, "-01-01 00:00:30")),
                       as.POSIXct(paste0(yyyy, "-12-31 23:59:30")),
                       by = "mins" )

    ## be sure to have all minutes
    ayear <- merge( ayear,
                    data.frame(Date = allminutes),
                    by = "Date", all = T)

    ayear$day <- as.Date(ayear$Date)


    ## load SZA from first input
    temp_sun <- readRDS(list.files(path       = SIGNAL_DIR,
                                   pattern    = paste0("SIG_", yyyy,".Rds"),
                                   full.names = T)[1])
    temp_sun <- temp_sun[ , c("Date", "Azimuth", "Elevat") ]
    ## drop old sun positions
    ayear <- ayear[ , -c("Azimuth","Elevat")]
    ## replace with new sun positions
    ayear <- merge(ayear, temp_sun, by = "Date", all = T)



    ## convert NA to -9
    ayear$wattGLB[   is.na(ayear$wattGLB)    ] <- -9L
    ayear$wattGLB_SD[is.na(ayear$wattGLB_SD) ] <- -9L

    alldays <- sort(unique(ayear$day))

    ## create dir
    outputdir <- paste0(TOT_EXPORT, yyyy, "/")
    dir.create( outputdir, showWarnings = F )

    ## plot the whole year before output
    plot(ayear$Date, ayear$wattGLB,    main = paste(yyyy, "GLOBAL"))
    plot(ayear$Date, ayear$wattGLB_SD, main = paste(yyyy, "Global SD"))

    cat('\\begin{multicols}{3}')
    cat('\\scriptsize\n')


    ## export each day
    for (dd in alldays) {
        dateD <- as.Date(dd, origin = "1970-01-01")
        yyyy  <- year(dateD)
        doy   <- yday(dateD)

        aday <- ayear[ day == dateD ]
        if (nrow(aday) != 1440 ) {
            stop("Day do not have 1440 minutes!!")
        }

        ## this day output file
        filename <- paste0(outputdir, strftime(dateD, format = "TOT%3j%y.DAT"))

        ## may skip output if day has no global data
        if (all(aday$wattGLB == -9)) {
            cat("\\textbf{",paste0(dateD,": NO GHI DATA}\\\\\n"))
            # cat(paste(dateD,"\n"), file = missingday, append = T )
            next()
        }

        ## my definition of sza
        SZA     <- -(aday$Eleva - 90)

        if (any(is.na(SZA))) {
            stop("SZA should be always defined for TOT output files (", dateD,") \nHave to correct this!!")
        }

        ## format time like the others
        TIME_UT <- as.numeric((aday$Date - as.POSIXct( dateD ) + 30) / 3600)
        SZA[ is.na(SZA) ] <- -999L

        ## calculate zenith angles
        lapzen <- vzen(yyyy,1:1440,doy)
        if (any(is.na(lapzen))) {
            stop("SZA should be always defined for TOT output files (", dateD,") \nHave to correct this!!")
        }



        ## data to export
        output <- data.frame( TIME_UT = TIME_UT,
                              # SZA     = SZA,   ## my sza calculation pysolar
                              SZA     = lapzen,  ## sza similar to other broadband
                              Wm2     = round(aday$wattGLB,    digits = 3),
                              st.dev  = round(aday$wattGLB_SD, digits = 3) )


        ## custom header of the daily file
        cat(" TIME_UT    SZA    [W.m-2]   st.dev",
            file = filename,
            eol  = "\r\n")
        ## write formatted data to file
        write.table(format( output,
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

        # cat(paste("Written: ", basename(filename), "\r"))
        cat(paste0(dateD, ": ", basename(filename), " \\\\\n"))
    } #END of days

    cat('\\end{multicols}')
} #END of year loop


#' **END**
#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
