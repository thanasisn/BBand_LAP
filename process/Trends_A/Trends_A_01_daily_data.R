# /* #!/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Trends A 01: Create daily means, and deseasonal anomaly"
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
Script.Name  <- "~/BBand_LAP/process/Trends_A/Trends_A_01_daily_data.R"

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

LAP  <- tbl(con, "LAP")
META <- tbl(con, "META") |> select(Day, Daylength)

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


##  Create daily values  -------------------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Create daily means for each data set
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(c("ALL", "CLOUD", "CLEAR"))
for (DBn in dbs) {
  DATA <- get(DBn)
  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Daily means", var_name(DBn), "\n\n"))

  vars <- sort(DATA |> select(ends_with(c("_trnd_A", "_strict"))) |> colnames())

  ## __ Create daily values and statistics  ------------------------------------
  DAILY <- DATA   |>
    group_by(Day) |>
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
      Day_N = n()
    )

  ## Add daylength and load all data
  DAILY <- left_join(
    DAILY, META, by = "Day"
  ) |> collect() |> data.table()

  DAILY[, Decimal_date := decimal_date(Day)]
  ## __ Flag daily data with season  -------------------------------------------
  ## create continuous seasonal variable
  DAILY[, season_Yqrt := as.yearqtr(as.yearmon(paste(year(Day), month(Day), sep = "-")) + 1 / 12)]
  ## Flag seasons using quarters
  DAILY[season_Yqrt %% 1 == 0   , Season := "Winter"]
  DAILY[season_Yqrt %% 1 == 0.25, Season := "Spring"]
  DAILY[season_Yqrt %% 1 == 0.50, Season := "Summer"]
  DAILY[season_Yqrt %% 1 == 0.75, Season := "Autumn"]

  ## inspect fill ratios of observations
  hist(DAILY[!is.na(GLB_trnd_A_mean), GLB_trnd_A_N / Daylength], breaks = 100,
       main = paste(var_name(DBn), var_name("GLB_trnd_A_N")),
       ylab = "Valid data ratio")
  abline(v = Cloud_daily_ratio_lim, col = "red")

  hist(DAILY[!is.na(DIR_trnd_A_mean), DIR_trnd_A_N / Daylength], breaks = 100,
       main = paste(var_name(DBn), var_name("GLB_trnd_A_N")),
       ylab = "Valid data ratio")
  abline(v = Cloud_daily_ratio_lim, col = "red")

  ## Store daily values as is
  if (Sys.info()["nodename"] == Main.Host) {
    tbl_name <- paste0("Trend_A_DAILY_", DBn)
    if (dbExistsTable(con, tbl_name)) {
      dbRemoveTable(con, tbl_name)
    }
    dbCreateTable(conn = con, name = tbl_name, DAILY)
    res <- insert_table(con, DAILY, tbl_name, "Day", quiet = TRUE)
  }
}

##  Daily data representation  -------------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Apply some filtering on the daily data
#'
#' We choose to use a ratio of `r Cloud_daily_ratio_lim` to set the
#' sky type characterization for the whole day.
#'
#' **Daily data representation applied only on GHI!!**
#' i.e. GLB_trnd_A_mean for CLOUD and CLEAR
#'
#+ include=T, echo=T, results="asis", warning=FALSE
warning("This breaks other variables for Clear and Cloud!! This is only for GHI")
CLOUD <- tbl(con, "Trend_A_DAILY_CLOUD")
CLOUD <- CLOUD |>
  mutate(
    GLB_trnd_A_mean := case_when(
      GLB_trnd_A_N / Daylength >  Cloud_daily_ratio_lim ~ GLB_trnd_A_mean,
      GLB_trnd_A_N / Daylength <= Cloud_daily_ratio_lim ~ NA )
  )
if (Sys.info()["nodename"] == Main.Host) {
  res <- update_table(con, CLOUD, "Trend_A_DAILY_CLOUD", "Day", quiet = TRUE)
}
CLOUD <- CLOUD |> collect() |> data.table()
hist(CLOUD[!is.na(GLB_trnd_A_mean), GLB_trnd_A_N/Daylength], breaks = 100,
     main = paste(var_name("CLOUD"), var_name("GLB_trnd_A_N")),
     ylab = "Valid data ratio")


CLEAR <- tbl(con, "Trend_A_DAILY_CLEAR")
CLEAR <- CLEAR |>
  mutate(
    GLB_trnd_A_mean := case_when(
      GLB_trnd_A_N / Daylength >  Clear_daily_ratio_lim ~ GLB_trnd_A_mean,
      GLB_trnd_A_N / Daylength <= Clear_daily_ratio_lim ~ NA )
  )
if (Sys.info()["nodename"] == Main.Host) {
  res <- update_table(con, CLEAR, "Trend_A_DAILY_CLEAR", "Day", quiet = TRUE)
}
CLEAR <- CLEAR |> collect() |> data.table()
hist(CLEAR[!is.na(GLB_trnd_A_mean), GLB_trnd_A_N/Daylength], breaks = 100,
     main = paste(var_name("CLEAR"), var_name("GLB_trnd_A_N")),
     ylab = "Valid data ratio")


##  Daily deseasonalized values  -----------------------------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Create daily climatology data and anomaly
#'
#' We compute daily anomaly `_anom` as 100 (`_mean` - `_clima`) / `_clima`
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_DAILY_", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn)
  DATA <- DATA |> select(-contains("_clima"))
  vars <- sort(DATA |> select(ends_with("_mean")) |> colnames())

  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Daily deseasonal", var_name(DBn), "\n\n"))

  ## __ Compute daily climatology values ----------------------------------------
  CLIMA <- DATA |>
    group_by(DOY = yday(Day)) |>
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
  p <- CLIMA |> select(DOY,
                       !starts_with("TSI") &
                       ends_with(c("trnd_A_mean_clima"))) |>
    melt(id.vars = "DOY", variable.name = "Radiation")   |>
    ggplot(aes(x = DOY, y = value)) +
    geom_point( aes(colour = Radiation)) +
    geom_smooth(aes(colour = Radiation), method = "loess", formula = "y ~ x") +
    labs(subtitle = paste("Daily climatology for ", var_name(DBn)),
         y        = bquote(.("Irradiance") ~ ~ group("[", W/m^2, "]"))) +
    theme_bw()
  show(p)

  ## __ Create deseasonal anomaly  ---------------------------------------------
  DATA <- left_join(
    DATA |> mutate(DOY = yday(Day)),
    CLIMA,
    by   = "DOY",
    copy = TRUE
  ) |> collect() |> data.table()

  for (av in vars) {
    cat("Compute anomaly by DOY for ", av, "\n\n")

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

  ## Store daily anomaly data
  if (Sys.info()["nodename"] == Main.Host) {
    res <- update_table(con, DATA, DBn, "Day", quiet = TRUE)
  }
}




##  Daily deseasonalized values by season of year  -----------------------------
#'
#' \FloatBarrier
#' \newpage
#'
#' # Create climatology data and anomaly by season of year
#'
#+ include=T, echo=T, results="asis", warning=FALSE
dbs <- sort(grep("Trend_A_DAILY_", dbListTables(con), value = TRUE))
for (DBn in dbs) {
  DATA <- tbl(con, DBn)
  DATA <- DATA |> select(-contains("_seas"))
  vars <- sort(DATA |> select(ends_with("_mean")) |> colnames())

  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Season of year daily deseasonal", var_name(DBn), "\n\n"))

  ## __ Compute daily climatology by season  -----------------------------------
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
          seas_NAs = ~ sum(case_match( is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE),
          seas_N   = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~ 0L), na.rm = TRUE)
        )
      )
    ) |> collect() |> data.table()

  ## Plot seasonal climatology values
  p <- SEAS |> select(Season,
                      !starts_with("TSI") &
                      ends_with(c("trnd_A_mean_seas"))) |>
    arrange(match(Season, c("Winter", "Spring", "Summer", "Autumn"))) |>
    melt(id.vars = "Season", variable.name = "Radiation")  |>
    ggplot(aes(x = Season, y = value)) +
    geom_point( aes(colour = Radiation)) +
    geom_line(  aes(colour = Radiation, group = Radiation)) +
    geom_smooth(aes(colour = Radiation), method = "loess", formula = "y ~ x") +
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
