# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Trends"
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
#' Create raw data to use for investigating long terms changes of radiation
#' due to interaction with the atmosphere
#'

#+ include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
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
Script.Name  <- "~/BBand_LAP/process/Trends_A/Trends_A_02_daily_data.R"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/REPORTS/Trends_A", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/process/Trends_A/Trends_A_DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2,    warn.conflicts = FALSE, quietly = TRUE)

#+ include=T, echo=F, results="asis"
##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_BROAD))

LAP  <- tbl(con, "LAP")
META <- tbl(con, "META")

LAP <- LAP |>
  filter(SKY %in% c("Cloud", "Clear")) |>  ## only data for trends
  select(
  Date,
  ends_with("_trnd_A"),
  ends_with("_strict"),
  Decimal_date,
  TSI,
  Day,
  SKY
)

##  Create data sets  ----------------------------------------------------------
##  cloud and clear are prepared for this analysis
ALL   <- LAP |>                        |> select(-SKY)
CLOUD <- LAP |> filter(SKY == "Cloud") |> select(-SKY)
CLEAR <- LAP |> filter(SKY == "Clear") |> select(-SKY)

vars <- c(
  "DIR_trnd_A",
  "HOR_trnd_A",
  "GLB_trnd_A",
  "DIFF_trnd_A"
)

dbs <- c(
  "ALL",
  "CLOUD",
  "CLEAR"
)

for (DBn in dbs) {
  DATA <- get(DBn)

  # summarise(across(
  #   .cols = everything(),
  #   .fns = list(
  #     n = ~ n(),
  #     n_na = ~ sum(case_match(is.na(.x),
  #                             TRUE ~ 1L,
  #                             FALSE ~0L)),
  #     mean = ~ mean(.x, na.rm = TRUE),
  #     n_lt_p = ~ sum(case_match(.x < p_,
  #                               TRUE ~ 1L,
  #                               FALSE ~0L), na.rm = TRUE),
  #     pct_lt_p = ~ mean(case_match(.x < p_,
  #                                  TRUE ~ 1L,
  #                                  FALSE ~0L), na.rm = TRUE) * 100
  #   )
  # ))


  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n##", var_name(DBn), "\n\n"))

  ## drop data to datatable
  ## This needs ~ 4Gb of ram
  DATA <- DATA |> collect() |> data.table()

  daily <- DATA[, .(
    DIR_trnd_A      = mean(DIR_trnd_A,  na.rm = T),
    HOR_trnd_A      = mean(HOR_trnd_A,  na.rm = T),
    GLB_trnd_A      = mean(GLB_trnd_A,  na.rm = T),
    DIFF_trnd_A     = mean(DIFF_trnd_A, na.rm = T),
    DIR_trnd_A_NAs  = sum( is.na(DIR_trnd_A)),
    DIR_trnd_A_N    = sum(!is.na(DIR_trnd_A)),
    HOR_trnd_A_NAs  = sum( is.na(HOR_trnd_A)),
    HOR_trnd_A_N    = sum(!is.na(HOR_trnd_A)),
    GLB_trnd_A_NAs  = sum( is.na(GLB_trnd_A)),
    GLB_trnd_A_N    = sum(!is.na(GLB_trnd_A)),
    DIFF_trnd_A_NAs = sum( is.na(DIFF_trnd_A)),
    DIFF_trnd_A_N   = sum(!is.na(DIFF_trnd_A)),
    DIR_strict      = sum(!is.na(DIR_strict)),
    GLB_strict      = sum(!is.na(GLB_strict)),
    DIFF_strict     = sum(!is.na(DIFF_strict))
  ),
  by = Day]






  stop()
  for (avar in vars) {
    pp <- DATA

  }
}





stop()

#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
goodbye()
