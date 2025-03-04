# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Trends A 13: Analysis by season of year"
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
#'     toc_depth:        5
#'     fig_width:        8
#'     fig_height:       4
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

#+ include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(message   = FALSE   )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.cap   = " empty caption ")
knitr::opts_chunk$set(fig.pos   = "!h"    )
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
Script.Name  <- "~/BBand_LAP/process/Trends_A/Trends_A_14_consistency.R"

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
library(zoo,        warn.conflicts = FALSE, quietly = TRUE)
library(tidyquant,  warn.conflicts = FALSE, quietly = TRUE)


#+ include=T, echo=F, results="asis"
##  Open dataset  --------------------------------------------------------------
if (Sys.info()["nodename"] == Main.Host) {
  con <- dbConnect(duckdb(dbdir = DB_BROAD))
} else {
  con <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))
}

Seasons <- c("Winter", "Spring", "Summer", "Autumn")



##  Daily data -----------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Daily data
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_DAILY_", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn)

  vars <- sort(DATA |> select(ends_with("_anom")) |> colnames())

  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Daily anomaly", var_name(DBn), "\n\n"))

  for (av in vars) {
    cat("Daily anomaly for ", av, "\n\n")

    ## prepare cusum
    pp <- DATA |> select(Decimal_date, !!av) |> collect() |> data.table()
    setorder(pp, Decimal_date)
    pp[is.na(get(av)), eval(av) := 0 ]
    pp[, cusum := cumsum(get(av))]

    ## plot simple cusum
    p <- ggplot(pp, aes(x = Decimal_date, y = cusum)) +
      geom_line(col = var_col(av)) +
      labs(x = element_blank(),
           y = bquote("CUSUM" ~ .(var_name(av)) ~ ~ group("[", "%", "]")),
           subtitle = paste(var_name(DBn), var_name(av))) +
      theme_bw()
    print(p)

    ## prepare some more data to plot
    # merge(
    #   pp[get(av) > 0, .(posSum = cumsum(get(av))), by = .(year = Decimal_date %/% 1) ],
    #   pp[get(av) > 0, .(posSum = cumsum(get(av))), by = .(year = Decimal_date %/% 1) ],
    #   by = "year", all = TRUE
    # )
    pp[get(av) > 0, posSum := cumsum(tidyr::replace_na(get(av), 0))]
    pp[get(av) < 0, negSum := cumsum(tidyr::replace_na(get(av), 0))]


    setnafill(pp, "locf", cols = c("negSum", "posSum"))

    p <- ggplot(pp, aes(x = Decimal_date)) +
      geom_line(aes(y =   posSum), col = "blue") +
      geom_line(aes(y = - negSum), col = "red") +
      theme_bw()
    print(p)

    p <- ggplot(pp, aes(x = Decimal_date)) +
      geom_line(aes(y = -posSum/negSum), col = "cyan") +
      theme_bw()
    print(p)

    ## plot yearly sums
    testdb <- pp[, .(sum = sum(get(av))), by = .(year = Decimal_date %/% 1) ]
    p <- ggplot(testdb, aes(x = year, y = sum)) +
      geom_col(data = testdb[sum <= 0], fill = "red") +
      geom_col(data = testdb[sum >= 0], fill = "blue") +
      ggtitle(paste("Sum of all year values", var_name(DBn), var_name(av))) +
      xlab("Year")     +
      ylab("Year Sum") +
      theme_bw()
    print(p)

  }


}



#+ Clean_exit, echo=FALSE
if (!interactive()) { dbDisconnect(con, shutdown = TRUE); rm(con) }

#' \FloatBarrier
#+ results="asis", echo=FALSE
goodbye()
