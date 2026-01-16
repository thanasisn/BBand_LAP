#!/usr/bin/env Rscript
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Simple plot of broadband signal"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' documentclass: article
#' classoption:   a4paper,oneside
#' fontsize:      10pt
#' geometry:      "left=0.5in,right=0.5in,top=0.5in,bottom=0.5in"
#'
#' header-includes:
#' - \usepackage{caption}
#' - \usepackage{placeins}
#' - \captionsetup{font=small}
#'
#' output:
#'   html_document:
#'     toc:              true
#'     keep_tex:         no
#'     keep_md:          no
#'   bookdown::pdf_document2:
#'     keep_tex:         no
#'     keep_md:          no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        4
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' ---

#'
#' This is a simple script to plot all channels.
#'
#' Uses as less external dependencies as possible.
#'
#' **It can not find and parse all available data.**
#'

## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/tools/Plot_broad_band.R"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
  sink(file = paste0("~/BBand_LAP/REPORTS/LOGs/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

## __ Load libraries  ----------------------------------------------------------
library(data.table)

## __ Variables  ---------------------------------------------------------------
START_YEAR <- year(Sys.Date()) - 1
# START_YEAR <- 1993

## Source files  on sirena
basedir   <- "/media/sirena_lapdata_ro/archive/Bband"
logdir    <- "~/BBand_LAP/REPORTS/LOGs/Broadbandsingal/"
outputdir <- "~/BBand_LAP/REPORTS/DAILY/BroadBandSignal/"

## use local copy
if (Sys.info()["nodename"] %in% c("tyler", "mumra")) {
  basedir   <- "~/DATA_RAW/Bband/"
}

dir.create(logdir,    showWarnings = F, recursive = T)
dir.create(outputdir, showWarnings = F, recursive = T)

if (!file.exists(basedir)) {
  stop("Input folder missing!!!\n")
}

COLS <- c("#f27979",  "#ffa640",   "#4c4700",  "#30bf30", "#3de6f2", "#5c33cc",  "#bbace6", "#e60099")

## do this channels
CHA <- c(         0,          1,           2,          3,         4,         5,          6,         7)
## Channels names
ORG <- c("UVA-TUVR", "CM21-INC", "EPPLEY-IR", "CHP1-DIR", "ERY-GLB", "UVA-EKO", "CM21-HOR", "ERY-DIF")

## start with all files
allfiles <- list.files(path        = basedir,
                       pattern     = paste0("[0-9]{6}0[0-9].lap"),
                       ignore.case = TRUE,
                       recursive   = TRUE,
                       full.names  = TRUE)

cat("Filter out OLD files\n\n")
allfiles  <- grep("/OLD/", allfiles, value = TRUE, invert = TRUE)

cat("Filter out binary files this is very slow\n\n")
textfiles <- sapply(allfiles,
                    function(x) grepl("ASCII text,", system(paste("file -b ",x), intern = TRUE)),
                    simplify = TRUE)
textfiles <- unlist(textfiles)

cat("List binary and ascii files\n\n")
cat(allfiles[!textfiles], file = paste0(logdir, "Possible_binary_lap_files.list"), sep = "\n")
cat(allfiles[ textfiles], file = paste0(logdir, "Possible_text_lap_files.list"),   sep = "\n")


allfiles  <- allfiles[textfiles]
allfiles  <- allfiles[!is.na(allfiles)]

all_dates <- unique(sub(paste0(CHA[1], ".lap"), "", basename(allfiles), ignore.case = T))
all_dates <- sort(strptime(all_dates, "%d%m%y"), decreasing = TRUE)
all_dates <- unique(as.Date(all_dates))
all_dates <- all_dates[!is.na(all_dates)]

all_years <- sort(unique(year(all_dates)))

## dummy dates just for the day axis
Date <- seq(as.POSIXct("2000-01-01 00:00:30"),
            as.POSIXct("2000-01-01 24:00:00"),
            by = "mins")

## override for this years
all_years <- all_years[all_years >= START_YEAR]

for (ay in all_years) {
  year_dates <- all_dates[year(all_dates) == ay]
  year_dates <- sort(year_dates)

  cat("Do year", ay, "\n\n")

  pdf(paste0(outputdir, "/Broad_band_signals_", ay, ".pdf"), width = 9, height = 5)

  for (ad in year_dates) {
    today    <- as.Date(ad, origin = "1970-01-01")
    basename <- strftime(today, "%d%m%y")
    if (is.na(basename)) next()

    cat("Do ", format(today), "\n\n")

    ## start a plot for today
    par(mar = c(3,1,2,1))
    plot.new()
    title(main = paste0(today, " d:", yday(today), " ", basename, "0*.LAP"), cex.main = .8)

    ## make a plot for each channel
    names  <- c()
    colors <- c()
    plotis <- FALSE
    for (ac in CHA) {

      ## recreate file name and search for it
      afile <- grep(paste0(basename, "0", ac, ".lap"), allfiles, ignore.case = T, value = T)
      afile <- grep("/OLD/",afile, value = T, invert = T)

      ## keep only the first one
      if (length(afile) > 1) {
        cat("Ignore multiple matched files\n\n")
        cat(afile, file = paste0(logdir, "Multiple_matched_files.log"), append = T, sep = "\n")
        next()
      }
      afile <- afile[1]

      if (!file.exists(afile)) {
        cat("No existing file", basename, ac, "\n\n")
        next()
      }

      cat(afile, "\n")

      ## test if file is binary
      filetype <- system(paste("file -b ", afile), intern = T)
      if (!grepl("CSV text|ASCII text", filetype)) {
        cat(paste("Skip binary file:", afile, "\n"))
        cat(afile, file = paste0(logdir, "Skipped_files.log"), append = T, sep = "\n")
        next()
      } else {
        cat(paste(filetype, "file:", afile, "\n"))
        cat(afile, file = paste0(logdir, "Parsed_files.log"), append = T, sep = "\n")
      }

      ## There are files separated with comma tabs spaces AND mixes of them
      data <- fread(
        paste(
          sub("[,\t]", " ", readLines(afile)),
          collapse = "\n"))

      if (nrow(data) > 1440) {
        cat(paste("File has more than 1440 minutes:", afile, "\n"))
        cat(afile, file = paste0(logdir, "More_than_1440.log"), append = T, sep = "\n")
        ## FIX
        data <- data[1:1440]
      }
      if (nrow(data) < 1440) {
        cat(paste("File has less than 1440 minutes:", afile, "\n"))
        cat(afile, file = paste0(logdir, "Less_than_1440.log"), append = T, sep = "\n")
      }
      if (any(grepl("\t", readLines(afile)))) {
        cat(paste("File contain tabs:", afile, "\n"))
        cat(afile, file = paste0(logdir, "Has_tabs.log"), append = T, sep = "\n")
      }
      if (any(grepl(",", readLines(afile)))) {
        cat(paste("File contain commas:", afile, "\n"))
        cat(afile, file = paste0(logdir, "Has_commas.log"), append = T, sep = "\n")
      }
      if (any(grepl(";", readLines(afile)))) {
        cat(paste("File contain semicolons:", afile, "\n"))
        cat(afile, file = paste0(logdir, "Has_semicolons.log"), append = T, sep = "\n")
      }

      ## set NA values
      data[ V1 <= -8.9 , V1 := NA]
      data[ V2 <= -8.9 , V2 := NA]
      if (all(is.na(data$V1))) {
        cat(paste("File contain only NA at V1:", afile, "\n"))
        cat(afile, file = paste0(logdir, "V1_only_NA.log"), append = T, sep = "\n")
        next()
      }
      if (all(is.na(data$V2))) {
        cat(paste("File contain only NA at V2:", afile, "\n"))
        cat(afile, file = paste0(logdir, "V2_only_NA.log"), append = T, sep = "\n")
      }

      ## do the plot
      par(new = T)
      plot( Date, data$V1, "l",
            col  = COLS[ac+1],
            xlab = "",  ylab = "",
            xaxt = "n", yaxt = "n",
            xlim = range(Date),
            xaxs = "i", bty  = "n",
            lwd  = 1.5)

      plotis <- TRUE
      names  <- c( names,  ORG[ ac+1] )
      colors <- c( colors, COLS[ac+1] )
    }
    if (plotis) {
      axis.POSIXct(1, Date)
      legend("topright", names, col = colors, lty = 1, bty = "n", cex =  .8, lwd = 2 )
    }
  }
  dev.off()
}

## sort log files
system(paste("find ", logdir, " -type f -exec sort -u -o {} {} \\;" ))


tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
