# /* #!/opt/R/4.2.3/bin/Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Export Global from CM21 for WRDC data submission."
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Export yearly submission of GHI in WRDC format and data preparation."
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
#'
#' ---

#'
#' **L1 -> WRDC**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
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
#'
#' ## Aggregation method
#'
#' TODO: describe current method
#'
#' - Only positive global values
#'      - Negatives are set to zero
#' - Quarter of hours mean 'na.rm = TRUE'
#' - Hours from quarters mean 'na.rm = FALSE'
#'
#' **WARNING: A different official method may exist!!**
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
renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(gdata,      warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")


##  Variables  -----------------------------------------------------------------
panderOptions('table.alignment.default', 'right')
panderOptions('table.split.table',        120   )

tag <- paste0("Natsis Athanasios LAP AUTH ", strftime(Sys.time(), format = "%b %Y" ))

EXPORT_DIR <- "~/DATA/Broad_Band/CM21_H_WRDC_exports/"
dir.create(EXPORT_DIR, showWarnings = FALSE, recursive = TRUE)


##  Set export range  ----------------------------------------------------------
yearstodo <- seq(2023, year(Sys.time()))

#+ include=T, echo=F, results="asis"
cat("\n",
    "Will export: ", yearstodo,
    "\n\n")


## Load data base
BB <- opendata()


#+ include=TRUE, echo=F, results="asis"
for (yyyy in yearstodo) {

    cat("\n\\FloatBarrier\n")
    cat("\n\\newpage\n\n")
    cat("\n## Year:", yyyy, "\n\n")


    ## _ Get data for the year -------------------------------------------------
    DATA <- data.table(BB |>
                           filter(
                               year == yyyy,               ## Select year
                               Elevat >= -5,               ## Drop night but keep the whole quarter
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
    # Export[is.na(hGlobal) | is.nan(hGlobal), hGlobal := -99L]

    ##  Create the format they like
    # WRDCoutput <- data.frame(year   = year( Export$Dates ),
    #                          month  = month(Export$Dates ),
    #                          day    = day(  Export$Dates ),
    #                          time   = hour( Export$Dates ) + 0.5,
    #                          global =       Export$hGlobal)

    WRDCoutput2 <- data.frame(year   = year( Export2$Dates ),
                              month  = month(Export2$Dates ),
                              day    = day(  Export2$Dates ),
                              time   = hour( Export2$Dates ) + 0.5,
                              global =       Export2$hGlobal)


    ## _ Write WRDC submission file  -------------------------------------------
    # wrdc_fl <- paste0(EXPORT_DIR, "/sumbit_to_WRDC_", yyyy, ".dat")
    wrdc_fl2 <- paste0(EXPORT_DIR, "/sumbit_to_WRDC_", yyyy, ".dat")


    # ##  Add headers
    # cat("#Thessaloníki global radiation\r\n" ,
    #     file = wrdc_fl)
    # cat("#year month day time(UTC)   global radiation (W/m2)\r\n" ,
    #     file = wrdc_fl, append = TRUE)
    #
    # ##  Write output line by line
    # for (i in 1:nrow(WRDCoutput)) {
    #     cat(
    #         sprintf( "%4d  %2d  %2d  %4.1f %10.4f\r\n",
    #                  WRDCoutput[i,1],
    #                  WRDCoutput[i,2],
    #                  WRDCoutput[i,3],
    #                  WRDCoutput[i,4],
    #                  WRDCoutput[i,5] ),
    #         file = wrdc_fl,
    #         append = TRUE )
    # }
    #
    # ##  Replace -99.000 to -99
    # system(paste("sed -i 's/   -99.0000/   -99/g' ", wrdc_fl))


    ##  Add headers
    cat("#Thessaloníki global radiation\r\n" ,
        file = wrdc_fl2)
    cat("#year month day time(UTC)   global radiation (W/m2)\r\n" ,
        file = wrdc_fl2, append = TRUE)

    ##  Write all data at once
    write.fwf(WRDCoutput2,
              na       = "-99",
              sep      = "  ",
              digits   = 4,
              colnames = FALSE,
              append   = TRUE,
              file  = wrdc_fl2)

    cat(paste("\nData Exported to:", basename(wrdc_fl2),"\n"))



    ## _ Plots and Stats  ------------------------------------------------------
    panderOptions('table.alignment.default', 'right')
    panderOptions('table.split.table',        120   )

    cat('\n\\scriptsize\n\n')
    cat(pander(summary(WRDCoutput2)))
    cat('\n\\normalsize\n\n')


    plot(DATAquarter[, qGlobal, Dates],
         ylab = "GHI", xlab = "",
         main = "Aggregated GHI")

    ##  Use the exported file as input
    exfile <- fread(wrdc_fl2,
                    fill        = TRUE,
                    header      = TRUE,
                    skip        = 1,
                    check.names = TRUE,
                    na.strings  = "-99")
    exfile$Dates <- as.POSIXct(
        strptime(exfile[,
                        paste(X.year, month, day,
                              exfile$time.UTC. %/% 1, (exfile$time.UTC. %% 1) * 60)],
                 "%Y %m %d %H %M"))

    points(exfile[, global, Dates],
           col = "red")

    legend("topright",  pch = 1,
           legend = c("Quarterly", "Exported Hourly"),
           col    = c(1, "red")
    )
    cat(" \n \n")


    hist(DATAquarter[, qGlobal], breaks = 50,
         main = "Quarters", xlab = "Global quarter mean")
    cat(" \n \n")


    hist(DATAhour[, hGlobal], breaks = 50,
         main = "Hourly", xlab = "Global hour mean")
    cat(" \n \n")
}



#' **END**
#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
