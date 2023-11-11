#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Project perfomance"
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
#' ---

#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
#'
#+ echo=F, include=T


#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_db/99_Self_evaluation.R"
Script.ID   <- "99"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)


##  Parse data  ----------------------------------------------------------------
DATA <- fread("~/BBand_LAP/REPORTS/LOGs/Run.log",
              fill = TRUE,
              blank.lines.skip = TRUE)
## Create datetime
DATA$Date <- as.POSIXct(strptime(DATA[, paste(V1,V2)], "%F %H:%M:%OS"))
DATA[, V1 := NULL]
DATA[, V2 := NULL]
## Check units
stopifnot(DATA[, all(V6 == "mins")])
DATA[, V6 := NULL]
## Check execution time
if (is.numeric(DATA$V5)) {
    names(DATA)[names(DATA) == "V5"] <- "Minutes"
}
## Parse script
DATA[, Script   := basename(V4)]
DATA[, Category := basename(dirname(V4))]
DATA[, V4 := NULL]
## Parse host
DATA[, c("User", "Host") := tstrsplit(V3, "@")]
DATA[, V3 := NULL]


##  Evaluate  ------------------------------------------------------------------
#'
#' ## Last executions
#'
#+ echo=F, include=T
last <- DATA[DATA[, .I[which.max(Date)], by = .(Script, Category)]$V1]
last <- last[, .(Script, Category, Date, Minutes)]
last$Minutes <- round(last$Minutes, 2)
setorder(last, Date)
pander(last, justify = "lllr")
cat(" \n \n")



#' \newpage
#' ## Executions statistics
#'
#+ echo=F, include=T
stats <-
    DATA[, .(
        Median = round(median(Minutes), 2),
        Min    = round(min(Minutes)   , 2),
        Max    = round(max(Minutes)   , 2),
        Mean   = round(mean(Minutes)  , 2),
        .N
    ),
    by = Script]
stats <- stats[order(match(Script, last$Script))]
pander(stats, justify = "lrrrrr")
cat(" \n \n")




for (as in last$Script) {

}








#' **END**
#+ include=T, echo=F
# myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

