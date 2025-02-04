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
#' Create daily data and deseasonalize data
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

##  Create data sets  ----------------------------------------------------------
##  cloud and clear are prepared for this analysis
ALL   <- LAP |>                           select(-SKY)
CLOUD <- LAP |> filter(SKY == "Cloud") |> select(-SKY)
CLEAR <- LAP |> filter(SKY == "Clear") |> select(-SKY)

vars <- c(
  "DIR_trnd_A",
  "HOR_trnd_A",
  "GLB_trnd_A",
  "DIFF_trnd_A"
)

vars_obs <- c(
  "GLB_strict",
  "DIR_strict"
)

dbs <- c(
  "ALL",
  "CLOUD",
  "CLEAR"
)

##  Create daily values  -------------------------------------------------------
for (DBn in dbs) {
  DATA <- get(DBn)
  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Daily means", var_name(DBn), "\n\n"))

  ## Create daily values and stats
  DAILY <- DATA   |>
    group_by(Day) |>
    summarise(
      ## stats on every variable
      across(
        .cols = all_of(c(vars, vars_obs)),
        .fns  = list(
          mean = ~ mean(.x, na.rm = TRUE),
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


  ## inspect fill ratios for observations
  hist(DAILY[!is.na(GLB_trnd_A_mean), GLB_trnd_A_N/Daylength], breaks = 100,
       main = paste(var_name(DBn), var_name("GLB_trnd_A_N")),
       ylab = "Valid data ratio")

  hist(DAILY[!is.na(DIR_trnd_A_mean), DIR_trnd_A_N/Daylength], breaks = 100,
       main = paste(var_name(DBn), var_name("GLB_trnd_A_N")),
       ylab = "Valid data ratio")

  if (Sys.info()["nodename"] == Main.Host) {
    ## Store daily values as is
    tbl_name <- paste0("Trend_A_DAILY_", DBn)
    if (dbExistsTable(con , tbl_name)) {
      cat("\n Remove table", tbl_name, "\n\n")
      dbRemoveTable(con, tbl_name)
    }
    dbCreateTable(conn = con, name = tbl_name, DAILY)
    res <- insert_table(con, DAILY, tbl_name, "Day")
    cat("\n Created", tbl_name, "\n\n")
  }
}

##  Daily data representation  -------------------------------------------------
warning("This breaks other variables for Clear and Cloud!! This is only for GHI")

#'
#' We choose to use a ratio of `r Cloud_daily_ratio_lim` to set the
#' sky type characterization for the whole day.
#'
#' **Daily data representation applied only on GHI!!**
#'

CLOUD <- tbl(con, "Trend_A_DAILY_CLOUD")
CLOUD <- CLOUD |>
  mutate(
    GLB_trnd_A_mean := case_when(
      GLB_trnd_A_N / Daylength >  Cloud_daily_ratio_lim ~ GLB_trnd_A_mean,
      GLB_trnd_A_N / Daylength <= Cloud_daily_ratio_lim ~ NA )
  )
if (Sys.info()["nodename"] == Main.Host) {
  res <- update_table(con, CLOUD, "Trend_A_DAILY_CLOUD", "Day")
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
  res <- update_table(con, CLEAR, "Trend_A_DAILY_CLEAR", "Day")
}
CLEAR <- CLEAR |> collect() |> data.table()
hist(CLEAR[!is.na(GLB_trnd_A_mean), GLB_trnd_A_N/Daylength], breaks = 100,
     main = paste(var_name("CLEAR"), var_name("GLB_trnd_A_N")),
     ylab = "Valid data ratio")



# dbListTables(con)
# tbl(con, "Trend_A_DAILY_CLEAR") |> glimpse()
# tbl(con, "Trend_A_DAILY_CLOUD") |> glimpse()


##  Daily deseasonal values  ---------------------------------------------------

dbs <- c(
  "Trend_A_DAILY_ALL",
  "Trend_A_DAILY_CLOUD",
  "Trend_A_DAILY_CLEAR"
)

vars <- c(
  "DIR_trnd_A_mean",
  "HOR_trnd_A_mean",
  "GLB_trnd_A_mean",
  "DIFF_trnd_A_mean",
  "GLB_strict_mean"
)

##  Create daily values  -------------------------------------------------------
for (DBn in dbs) {
  DATA <- tbl(con, DBn)
  cat("\n\\FloatBarrier\n\n")
  cat(paste("\n## Daily deseasonal", var_name(DBn), "\n\n"))

  # DATA |> colnames()


  ##  Compute seasonal daily values --------------------------------------------
  SEAS <- DATA |>
    group_by(DOY = yday(Day)) |>
    summarise(
      across(
        .cols = ends_with("mean"),
        .fns  = list(
          seas = ~ mean(.x, na.rm = TRUE),
          NAs  = ~ sum(case_match( is.na(.x), TRUE ~ 1L, FALSE ~0L), na.rm = TRUE),
          N    = ~ sum(case_match(!is.na(.x), TRUE ~ 1L, FALSE ~0L), na.rm = TRUE)
        )
      ),
      across(
        .cols = ends_with(c("NAs", "N")),
        .fns  = list(
          total = ~ sum(.x, na.rm = TRUE)
        )
      ),
      Seas_N = n()
    ) |> collect() |> data.table()


  ## Plot seasonal values
  p <- SEAS |> select(DOY,
                      GLB_strict_mean_seas,
                      ends_with(c("trnd_A_mean_seas"))) |>
    melt(id.vars = 'DOY', variable.name = 'Radiation') |>
    ggplot(aes(x = DOY, y = value)) +
    geom_point(aes(colour = Radiation)) +
    labs(subtitle = paste("Daily climatology for ", var_name(DBn)),
         y        = bquote(.("Irradiance") ~ ~ group("[", W/m^2, "]"))) +
    theme_bw()
  show(p)


  ## Create deseasonal anomaly
  DATA <- left_join(
    DATA |> mutate(DOY = yday(Day)),
    SEAS,
    by = "DOY",
    copy = TRUE
  ) |> collect() |> data.table()

  for (av in vars) {
    cat("Compute anomaly for ", av, "\n")
    # grep(av, DATA |> colnames(), value = T)

    seasvar <- paste0(av, "_seas")
    anomvar <- paste0(av, "_anom")

    DATA <- DATA |> mutate(
      !!anomvar := 100 * (get(av) - get(seasvar)) / get(seasvar)
    ) |> collect()

    ## protect database numeric type
    DATA[get(anomvar) >  9999, eval(anomvar) :=  9999]
    DATA[get(anomvar) < -9999, eval(anomvar) := -9999]
  }

  ## Store anomaly data
  if (Sys.info()["nodename"] == Main.Host) {
    res <- update_table(con, DATA, DBn, "Day")
  }

}




#+ Clean_exit, echo=FALSE
dbDisconnect(con, shutdown = TRUE); rm(con)

#+ results="asis", echo=FALSE
goodbye()
