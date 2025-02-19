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
#' # Create daily means, and deseasonal anomaly
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
Script.Name  <- "~/BBand_LAP/process/Trends_A/Trends_A_03_SZA_data.R"

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
library(zoo,        warn.conflicts = FALSE, quietly = TRUE)

#+ include=T, echo=F, results="asis", warning=F
##  Open dataset  --------------------------------------------------------------
if (Sys.info()["nodename"] == Main.Host) {
  con <- dbConnect(duckdb(dbdir = DB_BROAD))
} else {
  con <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))
}

##
## Load and prepare dataset with the same process as first script
##
LAP  <- tbl(con, "LAP")

LAP <- LAP |>
  filter(SKY %in% c("Cloud", "Clear")) |>  ## only data for trends
  select(
    Date,
    ends_with("_trnd_A"),
    ends_with("_strict"),
    Decimal_date,
    TSI,
    preNoon,
    SZA,
    Day,
    SKY
  )

##  Create TSI data for comparison with observations  --------------------------
## No point to store these in the DB
LAP <- LAP |>
  mutate(
    TSI_GLB_trnd_A := case_when(!is.na(GLB_strict) ~ TSI, .default = NA),
    TSI_DIR_trnd_A := case_when(!is.na(DIR_strict) ~ TSI, .default = NA)
  )


##  Create sky data sets  ------------------------------------------------------
ALL   <- LAP |>                           select(-SKY)
CLOUD <- LAP |> filter(SKY == "Cloud") |> select(-SKY)
CLEAR <- LAP |> filter(SKY == "Clear") |> select(-SKY)
dbs   <- sort(c( "ALL", "CLOUD", "CLEAR"))

##  Create SZA values  ---------------------------------------------------------

#' \FloatBarrier
#' \newpage
#'
#' ## Create daily SZA means for each data set
#'
#+ include=T, echo=T, results="asis", warning=FALSE
for (DBn in dbs) {
  DATA <- get(DBn)
  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Daily SZA means", var_name(DBn), "\n\n"))

  vars <- sort(DATA |> select(ends_with(c("_trnd_A", "_strict"))) |> colnames())
  ## __ Create daily values and statistics  ------------------------------------
  DAILY <- DATA   |>
    group_by(Day,
             preNoon,
             SZA = (SZA - SZA_BIN / 2 ) %/% SZA_BIN) |>
    summarise(
      ## stats on every variable
      across(
        .cols = all_of(vars),
        .fns  = list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd   = ~ sd  (.x, na.rm = TRUE),
          NAs  = ~ sum(case_match( is.na(.x), TRUE ~ 1L, FALSE ~0L), na.rm = TRUE),
          N    = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~0L), na.rm = TRUE)
        )
      ),
      ## Stats on every group
      SZA_N = n()
    ) |>
    collect() |>
    data.table()
  DAILY[, Year := year(Day)]

  ## __ Flag daily data with season  -------------------------------------------
  ## create continuous seasonal variable
  DAILY[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Day), month(Day), sep = "-")) + 1/12)]
  ## Flag seasons using quarters
  DAILY[season_Yqrt %% 1 == 0   , Season := "Winter"]
  DAILY[season_Yqrt %% 1 == 0.25, Season := "Spring"]
  DAILY[season_Yqrt %% 1 == 0.50, Season := "Summer"]
  DAILY[season_Yqrt %% 1 == 0.75, Season := "Autumn"]


  ## inspect bin counts
  hist(DAILY[!is.na(GLB_trnd_A_mean), GLB_trnd_A_N], breaks = 100,
       main = paste(var_name(DBn), var_name("GLB_trnd_A_N")),
       ylab = "Valid data count")
  abline(v = SZA_aggregation_N_lim, col = "red")

  hist(DAILY[!is.na(DIR_trnd_A_mean), DIR_trnd_A_N], breaks = 100,
       main = paste(var_name(DBn), var_name("GLB_trnd_A_N")),
       ylab = "Valid data count")
  abline(v = SZA_aggregation_N_lim, col = "red")

  ## Store daily values as is
  if (Sys.info()["nodename"] == Main.Host) {
    tbl_name <- paste0("Trend_A_SZA_", DBn)
    if (dbExistsTable(con , tbl_name)) {
      dbRemoveTable(con, tbl_name)
    }
    dbCreateTable(conn = con, name = tbl_name, DAILY)
    res <- insert_table(con, DAILY, tbl_name, "Day", quiet = TRUE)
  }
  rm(DAILY); res <- gc()
}





##  SZA data representation  -------------------------------------------------
dbs <- sort(grep("A_SZA_", dbListTables(con), value = TRUE))

#' \FloatBarrier
#' \newpage
#'
#' ## Apply some filtering on the data to use
#'
#' We choose to use only SZA bins with at least `r SZA_aggregation_N_lim` data
#' points.
#'
#+ include=T, echo=T, results="asis", warning=FALSE

for (DBn in dbs) {
  DATA <- tbl(con, DBn)
  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Daily SZA ", var_name(DBn), "\n\n"))

  ##  Variables to restrict  ---
  vars <- DATA |> select(ends_with("_mean")) |> colnames()

  ## restrict each variable
  for (avar in vars)  {
    checkvar <- sub("_mean", "_N", avar)

    years <- tbl(con, DBn) |> select(Year) |> distinct() |> arrange(Year) |> pull()

    for (yyyy in years) {
      cat(DBn, "-", avar, "-", yyyy, "\n")
      PART <- tbl(con, DBn)  |>
        filter(Year == yyyy) |>
        select(Day, !!avar, !!checkvar)

      ## apply
      PART <- PART |>
        mutate(
          !!avar := case_when(
            !!sym(checkvar) <= SZA_aggregation_N_lim ~ NA,
            !!sym(checkvar) >  SZA_aggregation_N_lim ~ !!sym(avar)
          )
        )  #|> collect() |> data.table()

      ## store the table in the database
      if (Sys.info()["nodename"] == Main.Host) {
        res <- update_table(con, PART, tbl_name, "Day", quiet = TRUE)
      }
    }
  }
}


## TODO test data was excluded




#+ Clean_exit, echo=FALSE
if (!interactive()) { dbDisconnect(con, shutdown = TRUE); rm(con) }

#' \FloatBarrier
#+ results="asis", echo=FALSE
goodbye()
