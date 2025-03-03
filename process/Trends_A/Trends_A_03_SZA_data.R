# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Trends A 03: Create daily monthly means, and deseasonal anomaly by SZA"
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
                        arrow        = TRUE
                      )
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

# TEST
# Main.Host <- "tyler"

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
  filter(SKY %in% c("Cloud", "Clear")) |>  ## only data for trends, same as the first script
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

##  Create daily SZA values  ---------------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Create daily SZA means for each data set
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs   <- sort(c("ALL", "CLOUD", "CLEAR"))
for (DBn in dbs) {
  DATA <- get(DBn)

  cat("\n\\FloatBarrier\n\n")
  cat("\n\\newpage\n\n")
  cat(paste("\n## Daily SZA means", var_name(DBn), "\n\n"))
  status_msg(ScriptName = Script.Name,
             msg        = c(DBn, "Daily SZA means"))

  vars <- sort(DATA |> select(ends_with(c("_trnd_A", "_strict"))) |> colnames())
  ## __ Create daily values and statistics  ------------------------------------
  DAILY <- DATA   |>
    group_by(Day,
             preNoon,
             SZA = (SZA - SZA_BIN / 2) %/% SZA_BIN) |>
    summarise(
      ## stats on every variable
      across(
        .cols = all_of(vars),
        .fns  = list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd   = ~ sd  (.x, na.rm = TRUE),
          NAs  = ~ sum(case_match( is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE),
          N    = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE)
        )
      ),
      ## Stats on every group
      SZA_N = n(),
      .groups = "keep"
    ) |>
    collect() |> data.table()
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
    tbl_name <- paste0("Trend_A_SZA_DAILY_", DBn)
    if (dbExistsTable(con , tbl_name)) {
      dbRemoveTable(con, tbl_name)
    }
    dbCreateTable(conn = con, name = tbl_name, DAILY)
    res <- insert_table(con,
                        DAILY,
                        tbl_name,
                        matchvar = c("Day", "preNoon", "SZA"),
                        quiet = TRUE)
  }
  rm(DAILY); res <- gc()
}



##  Filter daily SZA values  ---------------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Apply some filtering on the daily data before use
#'
#' In order to preserve the representation of the data,
#' we choose to use only SZA bins with at least `r SZA_aggregation_N_lim` data
#' points.
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_SZA_DAILY_", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn)

  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Filter daily SZA ", var_name(DBn), "\n\n"))
  status_msg(ScriptName = Script.Name,
             msg        = c(DBn, "Filter daily SZA means"))

  ##  Variables to restrict
  vars <- DATA |> select(ends_with("_mean")) |> colnames()

  ## restrict each variable
  for (avar in vars)  {
    checkvar <- sub("_mean", "_N", avar)

    years <- tbl(con, DBn) |> select(Year) |> distinct() |> arrange(Year) |> pull()

    for (yyyy in years) {
      status_msg(ScriptName = Script.Name,
                 msg        = c(DBn, avar, yyyy))

      PART <- tbl(con, DBn)  |>
        filter(Year == yyyy) |>
        select(Day, preNoon, SZA, !!avar, !!checkvar)

      if (PART |> filter(!is.na(!!sym(avar))) |> tally() |> pull() == 0) { next() }

      ## apply
      PART <- PART |>
        mutate(
          !!avar := case_when(
            !!sym(checkvar) <= SZA_aggregation_N_lim ~ NA,
            !!sym(checkvar) >  SZA_aggregation_N_lim ~ !!sym(avar)
          )
        ) |> collect() |> data.table()

      # ## test
      # hist(PART |> filter(!is.na(!!sym(avar))) |> select(!!checkvar) |> pull(),
      #      breaks = 50)
      # abline(v = SZA_aggregation_N_lim, col = "red")

      ## store the table in the database
      if (Sys.info()["nodename"] == Main.Host) {
        res <- update_table(con,
                            PART,
                            tbl_name,
                            matchvar = c("Day", "preNoon", "SZA"),
                            quiet = TRUE)
      }
    }
  }
}




##  Daily deseasonalized anomaly SZA  ------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Create daily climatology data and anomaly
#'
#' We compute daily anomaly `_anom` as 100 (`_mean` - `_clima`) / `_clima`
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_SZA_DAILY_", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn) |> select(-contains("_clima"))
  vars <- sort(DATA |> select(ends_with("_mean")) |> colnames())

  cat("\n\\FloatBarrier\n\n")
  cat("\n\\newpage\n\n")
  cat(paste("\n## Daily deseasonal", var_name(DBn), "\n\n"))

  ## __ Compute daily climatology values  --------------------------------------
  CLIMA <- DATA |>
    group_by(DOY     = yday(Day),
             preNoon,
             SZA     = (SZA - SZA_BIN / 2) %/% SZA_BIN) |>
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
          clima_NAs = ~ sum(case_match( is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE),
          clima_N   = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE)
        )
      ),
      .groups = "keep"
    ) |> collect() |> data.table()

  ## not practical to plot all
  pvars <- CLIMA |> select(ends_with(c("trnd_A_mean_clima"))) |> colnames()
  for (apv in pvars) {
    pp <- CLIMA |>
      filter(SZA == 55) |>
      select(DOY,
             SZA,
             preNoon, !!apv)
    p <- ggplot(pp, aes(x = DOY, y = !!sym(apv))) +
      geom_point(aes(colour = preNoon)) +
      geom_smooth(aes(colour = preNoon), method = 'loess', formula = 'y ~ x') +
      labs(subtitle = paste("Daily SZA 55 climatology for ", var_name(DBn)),
           y        = bquote(.("Irradiance") ~ ~ group("[", W/m^2, "]"))) +
     theme_bw()
    show(p)
  }

  ## __ Create deseasonal anomaly  ---------------------------------------------
  for (av in vars) {

    cat("Compute anomaly by DOY for ", av, "\n\n")
    status_msg(ScriptName = Script.Name,
               msg        = c(DBn, "compute SZA anomaly", av))

    climavar <- paste0(av, "_clima")
    anomvar  <- paste0(av, "_anom" )
    checkvar <- sub("_mean", "_N", av)

    ## get only relevant data and re-apply restrictions
    DATA <- tbl(con, DBn) |>
      select(-contains("_clima")) |>
      mutate(DOY = yday(Day)) |>
      mutate(
        !!av := case_when(
          !!sym(checkvar) <= SZA_aggregation_N_lim ~ NA,
          !!sym(checkvar) >  SZA_aggregation_N_lim ~ !!sym(av)
        )
      ) |>
      select(DOY, SZA, preNoon, Day, !!av, !!checkvar)

    ## merge relevant data with climatology
    DATA <- left_join(
      DATA,
      CLIMA |>
        select(DOY, SZA, preNoon, !!climavar),
      by   = c("DOY", "preNoon", "SZA"),
      copy = TRUE
    ) |> collect() |> data.table()

    ## plot to check data input
    hist(DATA[!is.na(get(av)), get(checkvar)],
         breaks = 50,
         main = paste(av, checkvar))
    abline(v = SZA_aggregation_N_lim, col = "red")

    ## create anomaly
    DATA <- DATA |> mutate(
      !!anomvar := 100 * (get(av) - get(climavar)) / get(climavar),
      Decimal_date := decimal_date(Day)
    ) |> collect()

    ## protect database numeric type
    DATA[get(anomvar) >  9999, eval(anomvar) :=  9999]
    DATA[get(anomvar) < -9999, eval(anomvar) := -9999]
    DATA <- DATA |> mutate_if(is.numeric, ~ ifelse(is.nan(.), NA, .))

    ## Store daily anomaly data
    if (Sys.info()["nodename"] == Main.Host) {
      res <- update_table(con,
                          DATA,
                          DBn,
                          matchvar = c("Day", "preNoon", "SZA"),
                          quiet = TRUE)
    }
  }
}



##  Create monthly SZA values  -------------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Create monthly SZA means for each data set from daily
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_SZA_DAILY_", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn) |> select(-contains("_clima"))
  vars <- sort(DATA |> select(ends_with("_mean")) |> colnames())

  cat("\n\\FloatBarrier\n\n")
  cat("\n\\newpage\n\n")
  cat(paste("\n## Monthly SZA", var_name(DBn), "\n\n"))

  ## __ Create monthly values and statistics  ----------------------------------
  MONTHLY <- DATA   |>
    group_by(Year  = year(Day),
             Month = month(Day),
             preNoon,
             SZA) |>
    summarise(
      ## stats on every variable
      across(
        .cols = all_of(vars),
        .fns  = list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd   = ~ sd  (.x, na.rm = TRUE),
          NAs  = ~ sum(case_match( is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE),
          N    = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE)
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
       main = paste("Occurrence of days with data", var_name("GLB_trnd_A_mean_N")),
       ylab = "Occurrences")
  abline(v = SZA_Monthly_aggregation_N_lim, col = "red")

  hist(MONTHLY[!is.na(DIR_trnd_A_mean_mean), DIR_trnd_A_mean_N], breaks = 100,
       main = paste("Occurrence of days with data", var_name("DIR_trnd_A_mean_N")),
       ylab = "Occurrences")
  abline(v = SZA_Monthly_aggregation_N_lim, col = "red")


  ##  Store monthly values as is
  if (Sys.info()["nodename"] == Main.Host) {
    tbl_name <- sub("DAILY", "MONTHLY", DBn)
    if (dbExistsTable(con , tbl_name)) {
      dbRemoveTable(con, tbl_name)
    }
    dbCreateTable(conn = con, name = tbl_name, MONTHLY)
    res <- insert_table(con,
                        MONTHLY,
                        tbl_name,
                        matchvar = c("Year", "Month", "preNoon", "SZA"),
                        quiet = TRUE)
  }
  rm(MONTHLY); res <- gc()
}




##  Filter monthly SZA values  -------------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Apply some filtering on the monthly data before use
#'
#' In order to preserve the representation of the data,
#' we choose to use only SZA bins with at least `r SZA_Monthly_aggregation_N_lim` daily data
#' points.
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_SZA_MONTHLY_", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn)

  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Restrict monthly SZA ", var_name(DBn), "\n\n"))

  ##  Variables to restrict
  vars <- DATA |> select(ends_with("_mean_mean")) |> colnames()

  ## restrict each variable
  for (avar in vars)  {
    checkvar <- sub("_mean$", "_N", avar)

    status_msg(ScriptName = Script.Name,
               msg        = c("Filter daily SZA means",DBn, avar))

    PART <- tbl(con, DBn) |>
      select(Year, Month, preNoon, SZA, !!avar, !!checkvar)

    ## apply
    PART <- PART |>
      mutate(
        !!avar := case_when(
          !!sym(checkvar) <= SZA_Monthly_aggregation_N_lim ~ NA,
          !!sym(checkvar) >  SZA_Monthly_aggregation_N_lim ~ !!sym(avar)
        )
      ) |> collect() |> data.table()

    PART |> filter(!is.na(!!sym(avar))) |> select(!!checkvar) |> pull()
    ## test plot
    hist(PART |> filter(!is.na(!!sym(avar))) |> select(!!checkvar) |> pull(),
         breaks = 50,
         xlab = checkvar, main = avar)
    abline(v = SZA_Monthly_aggregation_N_lim, col = "red")

    ## store the table in the database
    if (Sys.info()["nodename"] == Main.Host) {
      res <- update_table(con,
                          PART,
                          tbl_name,
                          matchvar = c("Year", "Month", "preNoon", "SZA"),
                          quiet = TRUE)
    }
  }
}


##  Daily deseasonalized values by season of year  -----------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Create daily climatology data and anomaly by season of year
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_SZA_DAILY_", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn)
  DATA <- DATA |> select(-contains("_seas"))
  vars <- sort(DATA |> select(ends_with("_mean")) |> colnames())

  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Season of year daily deseasonal", var_name(DBn), "\n\n"))

  ## __ Compute daily climatology by season  -----------------------------------
  SEAS <- DATA |>
    group_by(
      Season,
      SZA,
      preNoon,
      DOY
    ) |>
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
          seas_NAs = ~ sum(case_match( is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE),
          seas_N   = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE)
        )
      ),
      .groups = "keep"
    ) |> collect() |> data.table()

  #   ## Plot seasonal climatology values
  #   p <- SEAS |> select(Season,
  #                       !starts_with("TSI") &
  #                         ends_with(c("trnd_A_mean_seas"))) |>
  #     arrange(match(Season, c("Winter", "Spring", "Summer", "Autumn"))) |>
  #     melt(id.vars = "Season", variable.name = "Radiation")  |>
  #     ggplot(aes(x = Season, y = value)) +
  #     geom_point( aes(colour = Radiation)) +
  #     geom_line(  aes(colour = Radiation, group = Radiation)) +
  #     geom_smooth(aes(colour = Radiation), method = "loess", formula = "y ~ x") +
  #     labs(subtitle = paste("Season climatology for ", var_name(DBn)),
  #          y        = bquote(.("Irradiance") ~ ~ group("[", W/m^2, "]"))) +
  #     theme_bw()
  #   show(p)

  ## __ Create deseasonal anomaly  ---------------------------------------------
  DATA <- left_join(
    DATA,
    SEAS,
    by = c(
      "Season",
      "DOY",
      "SZA",
      "preNoon"
    ),
    copy = TRUE
  ) |> collect() |> data.table()

  for (av in vars) {
    cat("Compute anomaly by season for ", av, "\n\n")

    climavar <- paste0(av, "_seas")
    anomvar  <- paste0(av, "_seasanom")

    DATA <- DATA |> mutate(
      !!anomvar := 100 * (get(av) - get(climavar)) / get(climavar),
      Decimal_date := decimal_date(Day)
    ) |> collect()

    ## protect database numeric type
    DATA[get(anomvar) >  9999, eval(anomvar) :=  9999]
    DATA[get(anomvar) < -9999, eval(anomvar) := -9999]
    DATA <- DATA |> mutate_if(is.numeric, ~ ifelse(is.nan(.), NA, .))
  }

  ## Store daily anomaly data
  if (Sys.info()["nodename"] == Main.Host) {
    res <- update_table(con,
                        DATA,
                        DBn,
                        matchvar = c("Day", "SZA", "preNoon"),
                        quiet = TRUE)
  }
}




##  Monthly deseasonalized values by season of year  ---------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Create monthly climatology data and anomaly by season of year
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_SZA_MONTHLY_", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn)
  DATA <- DATA |> select(-contains("_seas"))
  vars <- sort(DATA |> select(ends_with("_mean_mean")) |> colnames())

  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Season of year daily deseasonal", var_name(DBn), "\n\n"))

  ## __ Compute monthly climatology by season  ---------------------------------
  SEAS <- DATA |>
    group_by(
      Season,
      SZA,
      preNoon,
      Month
    ) |>
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
        .cols = ends_with("_mean_mean"),
        .fns  = list(
          seas     = ~ mean(.x, na.rm = TRUE),
          seas_NAs = ~ sum(case_match( is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE),
          seas_N   = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE)
        )
      )
    ) |> collect() |> data.table()

  #   ## Plot seasonal climatology values
  #   p <- SEAS |> select(Season,
  #                       !starts_with("TSI") &
  #                         ends_with(c("trnd_A_mean_seas"))) |>
  #     arrange(match(Season, c("Winter", "Spring", "Summer", "Autumn"))) |>
  #     melt(id.vars = "Season", variable.name = "Radiation")  |>
  #     ggplot(aes(x = Season, y = value)) +
  #     geom_point( aes(colour = Radiation)) +
  #     geom_line(  aes(colour = Radiation, group = Radiation)) +
  #     geom_smooth(aes(colour = Radiation), method = "loess", formula = "y ~ x") +
  #     labs(subtitle = paste("Season climatology for ", var_name(DBn)),
  #          y        = bquote(.("Irradiance") ~ ~ group("[", W/m^2, "]"))) +
  #     theme_bw()
  #   show(p)

  ## __ Create deseasonal anomaly  ---------------------------------------------
  DATA <- left_join(
    DATA,
    SEAS,
    by = c(
      "Season",
      "Month",
      "SZA",
      "preNoon"
    ),
    copy = TRUE
  ) |> collect() |> data.table()

  for (av in vars) {
    cat("Compute anomaly by season for ", av, "\n\n")

    climavar <- paste0(av, "_seas")
    anomvar  <- paste0(av, "_seasanom")

    DATA <- DATA |> mutate(
      !!anomvar := 100 * (get(av) - get(climavar)) / get(climavar),
      Decimal_date := decimal_date(Day)
    ) |> collect()

    ## protect database numeric type
    DATA[get(anomvar) >  9999, eval(anomvar) :=  9999]
    DATA[get(anomvar) < -9999, eval(anomvar) := -9999]
    DATA <- DATA |> mutate_if(is.numeric, ~ ifelse(is.nan(.), NA, .))
  }

  ## Store daily anomaly data
  if (Sys.info()["nodename"] == Main.Host) {
    res <- update_table(con,
                        DATA,
                        DBn,
                        matchvar = c("Year", "Month", "preNoon", "SZA"),
                        quiet = TRUE)
  }
}


#+ Clean_exit, echo=FALSE
if (!interactive()) { dbDisconnect(con, shutdown = TRUE); rm(con) }

#' \FloatBarrier
#+ results="asis", echo=FALSE
goodbye()
