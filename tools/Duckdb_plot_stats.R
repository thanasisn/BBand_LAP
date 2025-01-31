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
#'   html_document:
#'     toc:              true
#'     fig_width:        7.5
#'     keep_tex:         no
#'     keep_md:          no
#'     fig_height:       5
#'   bookdown::pdf_document2:
#'     keep_tex:         no
#'     keep_md:          no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        4
#'     fig_width:        8
#'     fig_height:       5
#'
#' date: "`r format(Sys.time(), '%F')`"
#'
#' params:
#'   CLEAN: TRUE
#'
#' ---

#'
#' **DB Stats**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.cap   = " empty caption ")
knitr::opts_chunk$set(fig.pos   = '!h'    )
knitr::opts_chunk$set(tidy = TRUE,
                      tidy.opts = list(
                        indent       = 4,
                        blank        = FALSE,
                        comment      = FALSE,
                        args.newline = TRUE,
                        arrow        = TRUE)
)

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_duckdb/Duckdb_plot_stats.R"

if (!interactive()) {
  pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
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
library(ggplot2,    warn.conflicts = FALSE, quietly = TRUE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

overview_data <- "~/BBand_LAP/SIDE_DATA/Data_size_duckdb.Rds"
gather        <- readRDS(overview_data)


varstat   <- data.table()
datstat   <- data.table()
rowstat   <- data.table()

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

    tt <- ll$db_stats
    tt <- tt[, .(Rows = unique(N)), by = Table]
    tt[, Date := ll$date ]
    rowstat <- rbind(rowstat, tt)

    varstat <- rbind(varstat,
                     data.frame(ll$db_stats,
                                Date = ll$date,
                                Data = adb))
  }
}

colstat <- varstat[, .N ,by = .(Data, Date, Table)]

## __ Data density and size  ---------------------------------------------------
#'
#' ## Data density and size
#'
#+ echo=F, include=T

ggplot(data = datstat[Data == "LAP_SUN.duckdb"]) +
  geom_step(aes(x = date, y = Size, colour = Data))
cat(" \n \n")

ggplot(data = datstat[Data == "Broad_Band_LAP.duckdb"]) +
  geom_step(aes(x = date, y = Size, colour = Data))
cat(" \n \n")

ggplot(data = datstat[Data == "LAP_SUN.duckdb"]) +
  geom_step(aes(x = date, y = Densisty, colour = Data))
cat(" \n \n")

ggplot(data = datstat[Data == "Broad_Band_LAP.duckdb"]) +
  geom_step(aes(x = date, y = Densisty, colour = Data))
cat(" \n \n")

ggplot(data = colstat[Data == "LAP_SUN.duckdb"]) +
  geom_step(aes(x = Date, y = N, colour = Table))
cat(" \n \n")

ggplot(data = colstat[Data == "Broad_Band_LAP.duckdb"]) +
  geom_step(aes(x = Date, y = N, colour = Table))
cat(" \n \n")


## __ Data Rows  ---------------------------------------------------
#'
#' ## Data Rows
#'
#+ echo=F, include=T

ggplot(data = rowstat[Table == "META"]) +
  geom_step(aes(x = Date, y = Rows, colour = Table))
cat(" \n \n")

ggplot(data = rowstat[Table == "LAP"]) +
  geom_step(aes(x = Date, y = Rows, colour = Table))
cat(" \n \n")

ggplot(data = rowstat[Table == "params"]) +
  geom_step(aes(x = Date, y = Rows, colour = Table))
cat(" \n \n")




## __ Data fill  ---------------------------------------------------
#'
#' ## Data fill
#'
#+ echo=F, include=T
varstat <- varstat[missing != 0]
setorder(varstat, Variable, Date)

for (at in unique(varstat$Table)) {
  pp <- varstat[Table == at]

  for (av in unique(pp$Variable)) {
    tt <- pp[Variable == av, ]
    if (nrow(tt) > 1) {
      p <- ggplot(data = tt) +
        geom_step(aes(x = Date, y = fill_pc)) +
        labs(title = paste(at, av)) +
        ylab("Date fill [%]") +
        xlab(element_blank())
      print(p)
      cat(" \n \n")
    }
  }
}


#'
#' end
#'



#+ results="asis", echo=FALSE
goodbye()
