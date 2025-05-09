# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Clound Enhancements A 00: Prepare raw data"
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
Script.Name  <- "~/BBand_LAP/process/Cloud_enchancements_A/Enchanc_A_00_raw_data.R"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/process/Cloud_enchancements_A/Enchanc_A_DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2,    warn.conflicts = FALSE, quietly = TRUE)

##  Open dataset  --------------------------------------------------------------
if (Sys.info()["nodename"] == Main.Host) {
  con <- dbConnect(duckdb(dbdir = DB_BROAD))
} else {
  con <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))
}

LAP  <- tbl(con, "LAP")
META <- tbl(con, "META")

#'
#' # Select data to use
#'
#' - Only screened data
#' - Sun above horizon
#' - Sun above obstructions
#'
#+ include=T, echo=T, results="asis"
##  Range of data ready to use  ------------------------------------------------
LAP <- LAP |>
  filter(Date > FIRST_DAY) |>
  filter(Date < LAST_DAY)


##  Keep daylight only  --------------------------------------------------------
LAP <- LAP |> filter(Elevat >= 0)


##  Only days with good data  --------------------------------------------------
LAP <- LAP |>
  filter(
    !is.na(GLB_strict) | !is.na(DIR_strict)
  )

##  Exclude data near horizon  -------------------------------------------------
LAP <- LAP |> filter(Elevat >= BIO_ELEVA)


# ##  Remove days with partial observations  -------------------------------------
# #'
# #' # Filter out some days
# #'
# #' Remove days with too few data, as they can not be representative of a
# #' normal day.
# #'
# #+ include=T, echo=T, results="asis"
#
# daylengths <- META |> select(Day, Daylength)
#
# removedays_GLB <- left_join(
#   LAP |>
#     filter(!is.na(GLB_strict)) |>
#     group_by(Day) |>
#     summarise(N_GLB = n()),
#   daylengths,
#   by = "Day"
# ) |>
#   filter(N_GLB < All_daily_ratio_lim * Daylength) |>
#   select(Day) |>
#   pull()
#
# removedays_DIR <- left_join(
#   LAP |>
#     filter(!is.na(DIR_strict)) |>
#     group_by(Day) |>
#     summarise(N_DIR = n()),
#   daylengths,
#   by = "Day"
# ) |>
#   filter(N_DIR < All_daily_ratio_lim * Daylength) |>
#   select(Day) |>
#   pull()
#
# removedays_DIFF <- left_join(
#   LAP |>
#     filter(!is.na(DIFF_strict)) |>
#     group_by(Day) |>
#     summarise(N_DIFF = n()),
#   daylengths,
#   by = "Day"
# ) |>
#   filter(N_DIFF < All_daily_ratio_lim * Daylength) |>
#   select(Day) |>
#   pull()
#
#
# LAP <- LAP |> mutate(
#   GLB_strict  := case_when(Day %in% removedays_GLB  ~ NA, .default = GLB_strict ),
#   DIR_strict  := case_when(Day %in% removedays_DIR  ~ NA, .default = DIR_strict ),
#   HOR_strict  := case_when(Day %in% removedays_DIR  ~ NA, .default = HOR_strict ),
#   DIFF_strict := case_when(Day %in% removedays_DIFF ~ NA, .default = DIFF_strict)
# )
#
#
# ##  Move measurements to mean earth distance  ----------------------------------
# #'
# #' # Remove Sun distance variation
# #'
# #+ include=T, echo=T, results="asis"
#
# LAP <- LAP |>
#   mutate(
#     DIR_ench_A  := DIR_strict  * (Sun_Dist_Astropy ^ 2),
#     GLB_ench_A  := GLB_strict  * (Sun_Dist_Astropy ^ 2),
#     HOR_ench_A  := HOR_strict  * (Sun_Dist_Astropy ^ 2),
#     DIFF_ench_A := DIFF_strict * (Sun_Dist_Astropy ^ 2)
#   )
#
# ##  Set flag for sky conditions  -----------------------------------------------
# #'
# #' # Create proper flags describing all data for trend analysis
# #'
# LAP <- LAP |>
#   mutate(
#     SKY := case_when(
#       CSRHv14_2_flag == 0 ~ "Clear",
#       CSRHv14_2_flag != 0 ~ "Cloud",
#       .default = as.character(NA)
#     ),
#   )
#
#
# ##  Add data to main DB --------------------------------------------------------
# ADD <- LAP |>
#   select(
#     Date,
#     contains("ench_A"),
#     SKY
#   ) |> collect()  ## have to load data before removing LAP column
#
# if (Sys.info()["nodename"] == Main.Host) {
#   con <- dbConnect(duckdb(dbdir = DB_BROAD))
#   ## make sure we do not have stale data
#   remove_column(con, "LAP", "DIR_ench_A"    )
#   remove_column(con, "LAP", "GLB_ench_A"    )
#   remove_column(con, "LAP", "HOR_ench_A"    )
#   remove_column(con, "LAP", "DIFF_ench_A"   )
#   remove_column(con, "LAP", "TSI_GLB_ench_A")
#   remove_column(con, "LAP", "TSI_DIR_ench_A")
#   remove_column(con, "LAP", "SKY"           )
#
#   res <- update_table(con, ADD, "LAP", "Date", quiet = TRUE)
# }














#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#' \FloatBarrier
#+ results="asis", echo=FALSE
goodbye()
