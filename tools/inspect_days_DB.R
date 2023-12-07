#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#### Tool to visual inspect the measurements of the instruments.

rm(list = (ls()[ls() != ""]))
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_days_DB.R"
renv::load("~/BBand_LAP")

sink(file   = paste0("~/BBand_LAP/REPORTS/LOGs/", basename(sub("\\.R$", ".log", Script.Name))),
     split  = TRUE,
     append = TRUE)

## TODO
## plot steps
## plot async
## plot sun


library(arrow,      quietly = T, warn.conflicts = F)
library(data.table, quietly = T, warn.conflicts = F)
library(dplyr,      quietly = T, warn.conflicts = F)
library(optparse,   quietly = T, warn.conflicts = F)
library(plotly,     quietly = T, warn.conflicts = F)


source("~/CM_21_GLB/Functions_CM21_factor.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/BBand_LAP/DEFINITIONS.R")

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
## --



####   Get input    ############################################################
MINDATE <- as.Date("1993-01-01")
MAXDATE <- as.Date(Sys.Date())
MINSTEP <- 1
MAXSTEP <- 400
INITDAY <- paste0(year(Sys.Date()), "-01-01")
INISTEP <- 3

## TEST override start day
INITDAY <- "2023-08-22"
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

BB <- opendata()

if (STEP == 1) {
    MOVE <- 1
} else {
    MOVE <- STEP - 1
}

daystodo <- seq(STARTDAY, MAXDATE, by = MOVE)


## loop plots
for (ap in daystodo) {
    toplot <- seq.Date( as.Date(ap, origin = "1970-01-01"), length.out = STEP, by = "day")
    ## get all days from data base
    # gather <- data.table(BB |> filter(as.Date(Date) %in% toplot ) |> collect())

    ## faster query
    yearsq <- unique(year(toplot))
    monthq <- unique(month(toplot))
    gather <- data.table(open_dataset(sources       = DB_DIR,
                                      hive_style    = FALSE,
                                      partitioning  = c("year", "month")) |>
                             filter(year %in% yearsq & month %in% monthq) |>
                             filter(as.Date(Date) %in% toplot ) |>
                             collect())

    cat("\n - - - - - - - - - - - - - - - - - - - \n")
    cat("Load:", format(toplot, "%F"), "\n")

    # plot(gather$Date, gather$CM21_sig, type = "l", col = 2)
    # lines(gather$Date, gather$CM21_sig_wo_dark, type = "l", col = 3)
    #
    # plot(gather$Date, gather$CM21_sig * cm21factor(gather$Date), type = "l", col = 2)
    # lines(gather$Date, gather$CM21_sig_wo_dark * cm21factor(gather$Date), type = "l", col = 3)



    ## keep signal for debugging
    gather$GLBraw <- gather$CM21_sig_wo_dark

    ## convert to radiation
    gather$GLB_otf    <- gather$CM21_sig    * cm21factor(gather$Date)
    gather$GLB_sd_otf <- gather$CM21_sig_sd * cm21factor(gather$Date)

    gather$DIR_otf    <- gather$CHP1_sig    * chp1factor(gather$Date)
    gather$DIR_sd_otf <- gather$CHP1_sig_sd * chp1factor(gather$Date)


    ## find bad data marks from parametric files
    bad_chp1 <- data.table()
    for (i in 1:nrow(ranges_CHP1)) {
        lower <- ranges_CHP1$From[   i]
        upper <- ranges_CHP1$Until[  i]
        comme <- ranges_CHP1$Comment[i]
        ## mark bad regions of data
        tmp <- gather[Date >= lower & Date < upper & !is.na(DIR_otf) ]
        tmp$comment <- comme
        bad_chp1 <- rbind(bad_chp1, tmp)
    }

    bad_cm21 <- data.table()
    for (i in 1:nrow(ranges_CM21)) {
        lower <- ranges_CM21$From[   i]
        upper <- ranges_CM21$Until[  i]
        comme <- ranges_CM21$Comment[i]
        ## mark bad regions of data
        tmp <- gather[Date >= lower & Date < upper & !is.na(GLB_otf) ]
        tmp$comment <- comme
        bad_cm21 <- rbind(bad_cm21, tmp)
    }




    ## Plotly

    fig <- plot_ly()
    ## Direct with out dark on the fly
    fig <- add_trace(fig, x = gather$Date, y = gather$DIR_otf,
                     name = "Direct beam on-the-fly",
                     line = list(color = "blue"),
                     text = paste(format(gather$Date, "%F %R"),"\n","DBI F:",round(gather$DIR_otf,4)),
                     hoverinfo = 'text',
                     mode = "lines", type = "scatter")
    ## Direct final product
    fig <- add_trace(fig, x = gather$Date, y = gather$DIR_wpsm,
                     name = "Direct beam Clean",
                     line = list(color = "darkblue"),
                     text = paste(format(gather$Date, "%F %R"),"\n","DBI C:",round(gather$DIR_wpsm,4)),
                     hoverinfo = 'text',
                     mode = "lines", type = "scatter")

    ## Global with out dark on the fly
    fig <- add_trace(fig, x = gather$Date, y = gather$GLB_otf,
                     name = "Global on-the-fly",
                     line = list(color = "green"),
                     text = paste(format(gather$Date, "%F %R"),"\n","GHI F:",round(gather$GLB_otf,4)),
                     hoverinfo = 'text',
                     mode = "lines", type = "scatter")

    ## Global final product
    fig <- add_trace(fig, x = gather$Date, y = gather$GLB_wpsm,
                     name = "Global clean",
                     line = list(color = "darkgreen"),
                     text = paste(format(gather$Date, "%F %R"),"\n","GHI C:",round(gather$GLB_wpsm,4)),
                     hoverinfo = 'text',
                     mode = "lines", type = "scatter")

    ## Global from sirena
    fig <- add_trace(fig, x = gather$Date, y = gather$tot_glb,
                     name = "Global Sirena",
                     line = list(color = "lightgreen"),
                     text = paste(format(gather$Date, "%F %R"),"\n","GHI S:",round(gather$tot_glb,4)),
                     hoverinfo = 'text',
                     mode = "lines", type = "scatter")

    ## plot standard deviation points
    fig <- add_trace(fig, x = gather$Date, y = gather$DIR_sd_otf,
                     name = "Direct beam SD",
                     marker = list(color = "blue", symbol = "asterisk-open", size = 2),
                     text = paste("DBI SD:", round(gather$DIR_sd_otf,4)),
                     hoverinfo = 'text',
                     # showlegend = FALSE,
                     mode = 'markers', type = "scatter")
    fig <- add_trace(fig, x = gather$Date, y = gather$GLB_sd_otf,
                     name = "Global SD",
                     marker = list(color = "green", symbol = "asterisk-open", size = 2),
                     # text = paste(format(gather$Date, "%F %R"),"\n",round(gather$GLB_sd_otf,1)),
                     text = paste("GHI SD:",round(gather$GLB_sd_otf,4)),
                     hoverinfo = 'text',
                     # showlegend = FALSE,
                     mode = 'markers', type = "scatter")

    ## plot excluded ranges
    if (nrow(bad_chp1) > 0 ) {
        fig <- add_trace(fig, x = bad_chp1$Date, y = bad_chp1$DIR_otf,
                         name = "Excluded CHP1 otf",
                         text   = paste(bad_chp1$comment),
                         hoverinfo = 'text',
                         marker = list(color = "red", symbol = "square-open-dot", size = 10),
                         mode = 'markers', type = "scatter")
    }
    if (nrow(bad_cm21) > 0 ) {
        fig <- add_trace(fig, x = bad_cm21$Date, y = bad_cm21$GLB_otf,
                         name   = "Excluded CM21 otf",
                         text   = paste(bad_cm21$comment),
                         hoverinfo = 'text',
                         marker = list(color = "red", symbol = "square-open-dot", size = 10),
                         mode   = 'markers', type = "scatter")
    }

    fig <- add_trace(fig,
                     x = gather[Async_tracker_flag == TRUE, Date],
                     y = gather[Async_tracker_flag == TRUE, DIR_otf],
                     name   = "Tracker Async",
                     text   = paste("Async Steps: ",gather[Async_tracker_flag == TRUE, Async_step_count]),
                     hoverinfo = 'text',
                     marker = list(color = "magenta", symbol = "star-triangle-up-open-dot", size = 10),
                     mode   = 'markers', type = "scatter")

    fig <- add_trace(fig,
                     x = gather[!is.na(cm21_bad_data_flag), Date],
                     y = gather[!is.na(cm21_bad_data_flag), GLB_otf],
                     name   = "Excluded CM21 in DB",
                     text   = paste(gather[!is.na(cm21_bad_data_flag), cm21_bad_data_flag]),
                     hoverinfo = 'text',
                     marker = list(color = "red", symbol = "circle-x-opem", size = 10),
                     mode   = 'markers', type = "scatter")

    fig <- add_trace(fig,
                     x = gather[!is.na(chp1_bad_data_flag), Date],
                     y = gather[!is.na(chp1_bad_data_flag), DIR_otf],
                     name   = "Excluded CM21 in DB",
                     text   = paste(gather[!is.na(chp1_bad_data_flag), chp1_bad_data_flag]),
                     hoverinfo = 'text',
                     marker = list(color = "red", symbol = "circle-x-open", size = 10),
                     mode   = 'markers', type = "scatter")



    fig <- layout(fig, legend = list(x = 0.85, y = 0.95, bgcolor = 'rgba(75,75,75,0.3)'))
    # fig <- layout(fig, xaxis  = list(showcrossline = T))
    # fig <- layout(fig, hovermode = "x unified")
    fig <- layout(fig, hovermode = "x")


    # fig
    # print(fig)
    # show (fig)
    # stop()

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
