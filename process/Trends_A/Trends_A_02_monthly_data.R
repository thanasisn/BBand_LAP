# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Trends A 02: Create monthly means from daily and deseasonal anomaly"
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
Script.Name  <- "~/BBand_LAP/process/Trends_A/Trends_A_02_monthly_data.R"

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


##  Create monthly values  -----------------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Create monthly values
#'
#' Use daily data to create monthly values
#' will compute mean of daily means, not anomaly
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_DAILY", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn)
  type <- sub(".*_", "", DBn)
  ## variables to aggregate
  vars <- sort(DATA |> select(ends_with("_mean")) |> colnames())

  cat("\n\\FloatBarrier\n\n")
  cat("\n\\newpage\n\n")
  cat(paste("\n## Monthly means", var_name(DBn), "\n\n"))

  ## __ Create monthly values and statistics  ----------------------------------
  MONTHLY <- DATA   |>
    group_by(Year = year(Day), Month =  month(Day)) |>
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
      Month_N = n(),
      .groups = "drop"
    ) |> collect() |> data.table()
  ## create proper dates
  MONTHLY$Day <- as.Date(strptime(paste(MONTHLY$Year, MONTHLY$Month, "01"), "%Y %m %d"))
  MONTHLY[, Decimal_date := decimal_date(Day)]

  ## __ Flag monthly data with season  -----------------------------------------
  ## create continuous seasonal variable
  MONTHLY[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Day), month(Day), sep = "-")) + 1/12)]
  ## Flag seasons using quarters
  MONTHLY[season_Yqrt %% 1 == 0   , Season := "Winter"]
  MONTHLY[season_Yqrt %% 1 == 0.25, Season := "Spring"]
  MONTHLY[season_Yqrt %% 1 == 0.50, Season := "Summer"]
  MONTHLY[season_Yqrt %% 1 == 0.75, Season := "Autumn"]

  ## inspect fill ratios of observations
  hist(MONTHLY[!is.na(GLB_trnd_A_mean_mean), GLB_trnd_A_mean_N], breaks = 100,
       main = paste("Occurance of days with data", var_name("GLB_trnd_A_mean_N")),
       ylab = "Occurances")

  hist(MONTHLY[!is.na(DIR_trnd_A_mean_mean), DIR_trnd_A_mean_N], breaks = 100,
       main = paste("Occurance of days with data", var_name("DIR_trnd_A_mean_N")),
       ylab = "Occurances")

  ##  Store monthly values as is
  if (Sys.info()["nodename"] == Main.Host) {
    tbl_name <- paste0("Trend_A_MONTHLY_", type)
    if (dbExistsTable(con , tbl_name)) {
      dbRemoveTable(con, tbl_name)
    }
    dbCreateTable(conn = con, name = tbl_name, MONTHLY)
    res <- insert_table(con, MONTHLY, tbl_name, "Day", quiet = TRUE)
  }
}


##  Monthly data representation  -----------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Filter acceptable monthly values
#'
#' We choose to use monthly values only if `r Monthly_aggegation_N_lim` days
#' with data exist for each month.
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_MONTHLY", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA        <- tbl(con, DBn) |> collect() |> data.table()
  varstochech <- sort(DATA |> select(ends_with("_mean_mean")) |> colnames())

  cat("\n\\FloatBarrier\n\n")
  cat("\n\\newpage\n\n")
  cat(paste("\n## Selection for", var_name(DBn), "\n\n"))

  ## set means on days with less than n days to NA
  for (av in varstochech) {
    tv <- paste0(sub("_mean$", "", av), "_N")

    ## plot all occurrences
    hist(DATA[, get(tv)], breaks = 33,
         ylab = "Occurrences",
         xlab = "Available days for each monthly mean",
         main = paste("Days with data for",  var_name(av)))
    abline(v = Monthly_aggegation_N_lim, col = "red")

    ## apply limit
    DATA[get(tv) < Monthly_aggegation_N_lim, eval(av) := NA]
    cat("Days", DATA[get(tv) >= Monthly_aggegation_N_lim, .N], "days droped for", av, "\n")

    ## plog
    hist(DATA[!is.na(get(av)), get(tv)], breaks = 33,
         ylab = "Occurrences",
         xlab = "Available days for used monthly mean",
         main = paste("Days with data for",  var_name(av))
    )
  }

  ## store data to db
  if (Sys.info()["nodename"] == Main.Host) {
    dbRemoveTable(con, DBn)
    dbCreateTable(conn = con, name = DBn, DATA)
    res <- insert_table(con, DATA, DBn, "Day", quiet = TRUE)
  }
}



##  Monthly deseasonalized values  ---------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Departure from monthly climatology
#'
#' Compute monthly anomaly `_mean_anom` from `_mean_mean` - `_mean_clima`
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_MONTHLY", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn)
  type <- sub(".*_", "", DBn)

  cat("\n\\FloatBarrier\n\n")
  cat("\n\\newpage\n\n")
  cat(paste("\n## Monthly deseasonal", var_name(DBn), "\n\n"))

  vars <- sort(DATA |> select(contains("_mean_mean")) |> colnames())

  ## __ Compute monthly climatology --------------------------------------------
  CLIMA <- DATA |>
    group_by(Month) |>
    summarise(
      Clima_N = n(),
      ## get the totals of data in each mean
      across(
        .cols = ends_with(c("_NAs", "_N")),
        .fns  = list(
          clima_total = ~ sum(.x, na.rm = TRUE)
        )
      ),
      ## create climatology for each mean
      across(
        .cols = ends_with("_mean"),
        .fns  = list(
          clima     = ~ mean(.x, na.rm = TRUE),
          clima_NAs = ~ sum(case_match( is.na(.x), TRUE ~ 1L, FALSE ~0L), na.rm = TRUE),
          clima_N   = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~0L), na.rm = TRUE)
        )
      )
    ) |> collect() |> data.table()

  ## Plot climatology values
  p <- CLIMA |> select(Month,
                       !starts_with("TSI") &
                         ends_with(c("_mean_mean_clima"))) |>
    melt(id.vars = 'Month', variable.name = 'Radiation') |>
    ggplot(aes(x = Month, y = value)) +
    geom_point( aes(colour = Radiation)) +
    geom_smooth(aes(colour = Radiation), method = "loess", formula = "y ~ x") +
    labs(subtitle = paste("Monthly climatology for ", var_name(DBn)),
         y        = bquote(.("Irradiance") ~ ~ group("[", W/m^2, "]"))) +
    theme_bw()
  show(p)

  ## __ Create deseasonal anomaly  ---------------------------------------------
  DATA <- left_join(
    DATA,
    CLIMA,
    by = "Month",
    copy = TRUE
  ) |> collect() |> data.table()

  for (av in vars) {
    status_msg(ScriptName = Script.Name,
               msg        = c("Compute anomaly by month of year for ", DBn, av))

    climavar <- paste0(av, "_clima")
    anomvar  <- paste0(av, "_anom" )

    DATA <- DATA |> mutate(
      !!anomvar := 100 * (get(av) - get(climavar)) / get(climavar),
      Decimal_date := decimal_date(Day)
    ) |> collect()

    ## protect database numeric type
    DATA[get(anomvar) >  9999, eval(anomvar) :=  9999]
    DATA[get(anomvar) < -9999, eval(anomvar) := -9999]
  }

  ## Store monthly anomaly data
  if (Sys.info()["nodename"] == Main.Host) {
    res <- update_table(con, DATA, DBn, "Day", quiet = TRUE)
  }
}



##  Monthly deseasonalized values by season of year  ---------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Create climatology data and anomaly by season of year
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_MONTHLY", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn)
  DATA <- DATA |> select(-contains("_seas"))
  vars <- sort(DATA |> select(ends_with("_mean_mean")) |> colnames())

  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Season of year daily deseasonal", var_name(DBn), "\n\n"))

  ## __ Compute monthly climatology by season  ---------------------------------
  SEAS <- DATA |>
    group_by(Season) |>
    summarise(
      Seas_N = n(),
      ## get the totals of data in each mean
      across(
        .cols = ends_with(c("_NAs", "_N")),
        .fns  = list(
          seas_total = ~ sum(.x, na.rm = TRUE)
        )
      ),
      ## create climatology for each mean
      across(
        .cols = ends_with("_mean"),
        .fns  = list(
          seas     = ~ mean(.x, na.rm = TRUE),
          seas_NAs = ~ sum(case_match( is.na(.x), TRUE ~ 1L, FALSE ~0L), na.rm = TRUE),
          seas_N   = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~0L), na.rm = TRUE)
        )
      )
    ) |> collect() |> data.table()

  ## Plot seasonal climatology values
  p <- SEAS |> select(Season,
                      !starts_with("TSI") &
                        ends_with(c("mean_mean_seas"))) |>
    arrange(match(Season, c("Winter", "Spring", "Summer", "Autumn"))) |>
    melt(id.vars = 'Season', variable.name = 'Radiation')  |>
    ggplot(aes(x = Season, y = value)) +
    geom_point(aes(colour = Radiation)) +
    geom_line( aes(colour = Radiation, group = Radiation) ) +
    geom_smooth(aes(colour = Radiation), method = 'loess', formula = 'y ~ x') +
    labs(subtitle = paste("Season climatology for ", var_name(DBn)),
         y        = bquote(.("Irradiance") ~ ~ group("[", W/m^2, "]"))) +
    theme_bw()
  show(p)

  ## __ Create deseasonal anomaly  ---------------------------------------------
  DATA <- left_join(
    DATA,
    SEAS,
    by   = "Season",
    copy = TRUE
  ) |> collect() |> data.table()

  for (av in vars) {
    status_msg(ScriptName = Script.Name,
               msg        = c("Compute anomaly by season of year for ", DBn, av))

    climavar <- paste0(av, "_seas")
    anomvar  <- paste0(av, "_seasanom" )

    DATA <- DATA |> mutate(
      !!anomvar := 100 * (get(av) - get(climavar)) / get(climavar),
      Decimal_date := decimal_date(Day)
    ) |> collect()

    ## protect database numeric type
    DATA[get(anomvar) >  9999, eval(anomvar) :=  9999]
    DATA[get(anomvar) < -9999, eval(anomvar) := -9999]
  }

  ## Store daily anomaly data
  if (Sys.info()["nodename"] == Main.Host) {
    res <- update_table(con, DATA, DBn, "Day", quiet = TRUE)
  }
}


#+ Clean_exit, echo=FALSE
if (!interactive()) { dbDisconnect(con, shutdown = TRUE); rm(con) }

#' \FloatBarrier
#+ results="asis", echo=FALSE
goodbye()
