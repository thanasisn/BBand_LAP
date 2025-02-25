# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Create meta statistics for the broadband measurements."
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
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#'
#' Do some aggregation over the daily, monthly, and yearly statistics and export data object to be used on reports
#'

#+ include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.cap   = " empty caption ")
knitr::opts_chunk$set(fig.pos   = "!ht"   )
knitr::opts_chunk$set(tidy = TRUE,
                      tidy.opts = list(
                        indent       = 4,
                        blank        = FALSE,
                        comment      = FALSE,
                        args.newline = TRUE,
                        arrow        = TRUE)
)

## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_duckdb/70_Level_1_stats.R"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/duck/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")
source("~/BBand_LAP/functions/Functions_variables_names.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2,    warn.conflicts = FALSE, quietly = TRUE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

## __  Variables  --------------------------------------------------------------
## Date range to run
START_day <- as.POSIXct("1993-04-12") ## start of cm21
UNTIL_day <- as.POSIXct(Sys.Date())

## Variables for statistics
vars <- c("GLB_wpsm", "DIR_wpsm", "GLB_SD_wpsm", "DIR_SD_wpsm", "SZA")


##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))
BB  <- tbl(con, "LAP")


##  Yearly statistics  ---------------------------------------------------------
stats_yearly <- BB          |>
  filter(Elevat > 0)        |>
  filter(Date >= START_day) |>
  filter(Date <= UNTIL_day) |>
  group_by(year)            |>
  summarise(
    across(
      all_of(vars),
      list(mean   = ~ mean(  .x, na.rm = TRUE),
           median = ~ median(.x, na.rm = TRUE),
           min    = ~ min(   .x, na.rm = TRUE),
           max    = ~ max(   .x, na.rm = TRUE),
           N      = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~0L), na.rm = TRUE)
      ),
      .names = "{.col}.{.fn}"
    )
  )                         |>
  arrange(year)             |>
  collect()                 |>
  data.table()
## create proper date
stats_yearly[, Date := as.Date(paste(year, "1", "1"), format = "%Y %m %d")]


##  Daily statistics  ----------------------------------------------------------
stats_daily <- BB |>
  filter(Elevat > 0)           |>
  filter(Date >= START_day)    |>
  filter(Date <= UNTIL_day)    |>
  mutate(Date = as.Date(Date)) |>
  group_by(Date)               |>
  summarise(
    across(
      all_of(vars),
      list(mean   = ~ mean(  .x, na.rm = TRUE),
           median = ~ median(.x, na.rm = TRUE),
           min    = ~ min(   .x, na.rm = TRUE),
           max    = ~ max(   .x, na.rm = TRUE),
           N      = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~0L), na.rm = TRUE)
      ),
      .names = "{.col}.{.fn}"
    )
  )                            |>
  arrange(Date)                |>
  collect()                    |>
  data.table()


##  Monthly statistics  --------------------------------------------------------
stats_monthly <- BB         |>
  filter(Elevat > 0)        |>
  filter(Date >= START_day) |>
  filter(Date <= UNTIL_day) |>
  group_by(year, month)     |>
  summarise(
    across(
      all_of(vars),
      list(mean   = ~ mean(  .x, na.rm = TRUE),
           median = ~ median(.x, na.rm = TRUE),
           min    = ~ min(   .x, na.rm = TRUE),
           max    = ~ max(   .x, na.rm = TRUE),
           N      = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~0L), na.rm = TRUE)
      ),
      .names = "{.col}.{.fn}"
    ),
    .groups = "keep"  ## TODO test are groups used correctly?
  ) |>
  arrange(year, month)      |>
  collect()                 |>
  data.table()
## create proper date
stats_monthly[, Date := as.Date(paste(year, month, "15"), format = "%Y %m %d")]


save(list = ls(pattern = "stats_"),
     file = "~/BBand_LAP/SIDE_DATA/BB_Statistics_L1.Rda")








##  Find some negative days  ---------------------------------------------------

#'
#' Days with negative global
#'
#+ echo=F, include=T
pander(
  BB |>
    filter(GLB_wpsm < -17) |>
    select(Date, GLB_wpsm) |>
    mutate(Date = as.Date(Date)) |>
    collect() |>
    unique()
)


#'
#' Days with negative direct
#'
#+ echo=F, include=T
pander(
  BB |>
    filter(DIR_wpsm < -3) |>
    select(Date, DIR_wpsm) |>
    mutate(Date = as.Date(Date)) |>
    collect() |>
    unique()
)





#+ include=TRUE, echo=FALSE, results="asis"
datas <- ls(pattern = "stats_")
for (dbn in datas) {
  DB     <- get(dbn)

  cat("\n\\FloatBarrier\n\n")
  cat("\n## ", toupper(sub(".*_", "", dbn)), "\n\n")

  wecare <- grep("year|month|Date", names(DB), value = T, invert = T)
  for (avar in wecare) {
    ## skip empty
    if (all(!DB[[avar]] %in% c(NA, NaN, 0))) next()

    ## basic plot
    p <- ggplot() +
      aes(x = DB$Date, y = DB[[avar]]) +
      geom_point() +
      labs(title = paste(tr_var(avar),  sub(".*\\.", "", avar), sub(".*\\_", "", dbn)),
           x = "",
           y = avar) +
      theme_bw()
    suppressWarnings(print(p))
    # theme(
    #     panel.background = element_rect(fill='transparent'), #transparent panel bg
    #     plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    #     panel.grid.major = element_blank(), #remove major gridlines
    #     panel.grid.minor = element_blank(), #remove minor gridlines
    #     legend.background = element_rect(fill='transparent'), #transparent legend bg
    #     legend.box.background = element_rect(fill='transparent') #transparent legend panel
    # )
    cat(" \n")
  }
}


#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
goodbye()
