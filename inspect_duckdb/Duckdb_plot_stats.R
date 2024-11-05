# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Inspect duckdb "
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
#'  **SIG**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_duckdb/Duckdb_plot_stats.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/duck/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/LOGs/duck/",    basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

overview_data <- "~/BBand_LAP/SIDE_DATA/Data_size_duckdb.Rds"

gather <- readRDS(overview_data)



## TODO plots

varstat   <- data.table()
datstat   <- data.table()

databases <- unique(sapply(gather, "[[", "base_name"))
for (adb in databases) {
  lls <- sapply(gather, "[[", "base_name") == adb

  temp <- data.frame(
    Date     = data.table(date = as.POSIXct(sapply(gather[lls], "[[", "date"), origin = origin)),
    Size     = sapply(gather[lls], "[[", "file_sise"),
    Densisty = sapply(gather[lls], "[[", "data_density"),
    Data     = adb
  )

  datstat <- rbind(datstat, temp)

  chosen <- gather[lls]

  for (il in 1:length(chosen)) {
    ll <- chosen[il][[1]]

    varstat <- rbind(varstat,
                     data.frame(ll$db_stats,
                                Date = ll$date,
                                Data = adb))
  }
}

colstat <- varstat[, .N ,by = .(Data, Date, Table)]

library(ggplot2)

ggplot(data = datstat) +
 geom_step(aes(x = date, y = Size, colour = Data))

ggplot(data = datstat) +
  geom_step(aes(x = date, y = Densisty, colour = Data))

ggplot(data = colstat) +
  geom_step(aes(x = Date, y = N, colour = Table))


varstat <- varstat[missing != 0]
setorder(varstat, Variable, Date)

for (at in unique(varstat$Table)) {
  pp <- varstat[Table == at]
  p <- ggplot(data = pp) +
    geom_step(aes(x = Date, y = fill_pc, colour = Variable))
  show(p)
}



#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

