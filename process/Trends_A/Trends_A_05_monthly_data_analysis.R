# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Trends A"
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
#+ include=F

#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' Trends from daily values
#'


#+ include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(message   = FALSE   )
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
Script.Name  <- "~/BBand_LAP/process/Trends_A/Trends_A_05_monthly_data_analysis.R"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/process/Trends_A/Trends_A_DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2,    warn.conflicts = FALSE, quietly = TRUE)
library(ggpubr,     warn.conflicts = FALSE, quietly = TRUE)


#+ include=T, echo=F, results="asis"
##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))

## list of daily tables
dbs <- sort(grep("_MONTHLY_", dbListTables(con), value = TRUE))
stop()
##  All data points  -----------------------------------------------------------
vars <- c(
  "DIR_trnd_A",
  "HOR_trnd_A",
  "GLB_trnd_A",
  "DIFF_trnd_A"
)

##  Plot daily mean values
#'
#' ## Daily mean values
#'
#+ daily-mean-trends, include=T, echo=F, results="asis"
for (DBn in dbs) {
  DATA <- tbl(con, DBn)

  DATA |> colnames()

  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n###", var_name(DBn), "\n\n"))

  for (avar in vars) {
    avar <- paste0(avar, "_mean")

    p <- DATA |>
      ggplot(aes(x = Decimal_date, y = !!sym(avar))) +
      geom_point(col = var_col(avar), size = 0.6)    +
      geom_smooth(method = 'lm', formula = y ~ x, colour = "red", fill = "red") +
      stat_regline_equation(label.y.npc = 1) +
      labs(x = element_blank(),
           y = bquote(.(var_name(avar)) ~ ~ group("[", W/m^2, "]")),
           subtitle = paste(var_name(DBn), var_name(avar))) +
      theme_bw()
    show(p)
  }
}



##  Plot daily anomaly values
#'
#' ## Daily departure from the climatology
#'
#+ daily-anomaly-trends, include=T, echo=F, results="asis"
for (DBn in dbs) {
  DATA <- tbl(con, DBn)

  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n###", var_name(DBn), "\n\n"))

  for (avar in vars) {
    avar <- paste0(avar, "_mean_anom")

    p <- DATA |>
      ggplot(aes(x = Decimal_date, y = !!sym(avar))) +
      geom_point(col = var_col(avar), size = 0.6)    +
      geom_smooth(method = 'lm', formula = y ~ x, colour = "red", fill = "red") +
      stat_regline_equation(label.y.npc = 1) +
      labs(x = element_blank(),
           y = bquote(.(var_name(avar)) ~ ~ group("[","%","]")),
           subtitle = paste(var_name(DBn), var_name(avar))) +
      theme_bw()
    show(p)
  }
}




#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
goodbye()
