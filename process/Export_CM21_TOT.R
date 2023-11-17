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
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(doMC,       warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(foreach,    warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")


##  Variables  -----------------------------------------------------------------
panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )

tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))

TOT_EXPORT <- "~/DATA/cm21_data_validation/AC21_lap.GLB_NEW_BB/"

## Sun position algorithm as the other broadband
zenangle <- function(YYYY,min,doy){
    as.numeric(
        system(
            paste("~/CM_21_GLB/BINARY/zenangle64 ", YYYY ,min, doy, " 40.634 -22.956"),
            intern = T)
    )
}

## Vectorize zenangle
vzen <- Vectorize(zenangle, "min")

## Parallelize zenangle
registerDoMC()

pzen <- function(YYYY, min = 1:1440, doy) {
    foreach(min = min, .combine = 'c') %dopar% zenangle(YYYY = YYYY, min = min ,doy = doy)
}

##  Set export range  ----------------------------------------------------------
yearstodo <- seq(2016, year(Sys.time()))


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
                               doy,
                               lap_sza,
                               GLB_wpsm,
                               cm21_bad_data_flag,
                               GLB_SD_wpsm
                           ) |> collect()
                       )

    ## _ Drop bad data  --------------------------------------------------------
    DATA[!is.na(cm21_bad_data_flag), GLB_wpsm    := NA]
    DATA[ is.na(GLB_wpsm),           GLB_SD_wpsm := NA]
    DATA[, cm21_bad_data_flag := NULL]

    ## _ Random SZA check  -----------------------------------------------------
    ##   Check some days for SZA inconsistencies
    reldiff1 <- c()
    reldiff2 <- c()
    reldiff3 <- c()
    for (ad in sample(unique(DATA$doy), 10)) {
        ## Calculate lap_sza
        test_sza <- pzen(yyyy, 1:1440, ad)
        ## Compare stored to calculated lap sza
        reldiff1 <- c(reldiff1, 100 * (DATA[doy == ad, lap_sza] - test_sza) / test_sza)
        ## Compare my SZA to calculated
        reldiff2 <- c(reldiff2, 100 * (DATA[doy == ad, SZA] - test_sza)     / test_sza)
        ## Compare my SZA to stored
        reldiff3 <- c(reldiff3, 100 *  DATA[doy == ad, (SZA - lap_sza)      / lap_sza])
    }

    ##  Do some plots on big departures
    if (max(abs(reldiff1)) < 0.029) {
        plot(reldiff1, main = "Relat diff % lap_sza ~ zenangle")
        hist(reldiff1, main = "Relat diff % lap_sza ~ zenangle")
    }

    if (max(abs(reldiff2)) < 0.80) {
        plot(reldiff2, main = "Relat diff %  SZA ~ zenangle")
        hist(reldiff2, main = "Relat diff %  SZA ~ zenangle")
    }

    if (max(abs(reldiff3)) < 0.80) {
        plot(reldiff3, main = "Relat diff %  SZA ~ lap_angle")
        hist(reldiff3, main = "Relat diff %  SZA ~ lap_angle")
    }


    ## _ Fill missing lap_sza  -------------------------------------------------
    missza <- DATA[is.na(lap_sza), unique(doy)]
    for (ad in missza) {
        new_sza <- pzen(yyyy, 1:1440, ad)
        DATA[doy == ad, lap_sza := new_sza]
        cat("\nFilled LAP SZA for doy:", ad, "\n")
    }

    ## _ Convert NA to -9  -----------------------------------------------------
    DATA[is.na(GLB_wpsm),    GLB_wpsm    := -9L]
    DATA[is.na(GLB_SD_wpsm), GLB_SD_wpsm := -9L]
    setorder(DATA, Date)


    ## plot the whole year before output
    plot(DATA[, GLB_wpsm   , Date], main = paste(yyyy, "GLOBAL"))
    plot(DATA[, GLB_SD_wpsm, Date], main = paste(yyyy, "Global SD"))

    ## create output dir
    outputdir <- paste0(TOT_EXPORT, "/", yyyy, "/")
    dir.create(outputdir, showWarnings = FALSE, recursive = TRUE)


    cat('\\begin{multicols}{3}')
    cat('\\scriptsize\n')

    ## _ Export each day  ------------------------------------------------------
    for (dd in DATA[, unique(as.Date(Date))]) {
        dateD <- as.Date(dd, origin = "1970-01-01")
        yyyy  <- year(dateD)
        doy   <- yday(dateD)

        aday <- DATA[as.Date(Date) == dateD]
        if (nrow(aday) != 1440 ) {
            stop("Day does not have 1440 minutes!!")
        }

        ##  Output file
        filename <- paste0(outputdir, strftime(dateD, format = "TOT%3j%y.DAT"))

        ##  Skip output if day has no global data
        if (all(aday$GLB_wpsm == -9)) {
            cat("\\textbf{",paste0(dateD,": NO GHI DATA}\\\\\n"))
            next()
        }

        ##  Format time like the others
        TIME_UT <- as.numeric((aday$Date - as.POSIXct( dateD ) + 30) / 3600)

        ##  SZA if filed
        stopifnot(all(!is.na(aday$lap_sza)))

        ## _ Data to export  ---------------------------------------------------
        output <- data.frame(TIME_UT = TIME_UT,
                             SZA     = aday$lap_sza,  ## sza similar to other broadband
                             Wm2     = round(aday$GLB_wpsm,    digits = 3),
                             st.dev  = round(aday$GLB_SD_wpsm, digits = 3) )

        ## _ Custom header of the daily file
        cat(" TIME_UT    SZA    [W.m-2]   st.dev",
            file = filename,
            eol  = "\r\n")
        ## _ write formatted data to file
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

        cat(paste0(dateD, ": ", basename(filename), " \\\\\n"))

    } #END of days
    cat('\\end{multicols}')
} #END of years loop




#' **END**
#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
