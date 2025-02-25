# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:     "Test the fitness of GHI clear sky models and data"
#' author:    "Natsis Athanasios"
#' institute: "AUTH"
#' date: "`r format(Sys.time(), '%F')`"
#'
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
#'   html_document:
#'     toc:        true
#'     fig_width:  9
#'     fig_height: 4
#'   bookdown::pdf_document2:
#'     number_sections:  no
#'     fig_caption:      no
#'     keep_tex:         no
#'     keep_md:          no
#'     latex_engine:     xelatex
#'     toc:              yes
#'     toc_depth:        4
#'     fig_width:        8
#'     fig_height: 5
#' ---
#+ include=F

#' **CSid Naive optim**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#'
#' This do an optimization for the $a$ coefficient of each
#' GHI model, for different data set partitioning.
#'
#' May apply an better selection of data, near the clear limit
#'

#+ include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"  )
knitr::opts_chunk$set(fig.align  = "center")
knitr::opts_chunk$set(fig.cap   = " empty caption ")
knitr::opts_chunk$set(fig.pos    = "!ht"   )
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
Script.Name  <- "~/BBand_LAP/process/CSid_RenoHansen/CSid_RenoHansen_01_alldata_optim_v15.R"
Script.ID    <- "CS1"
save_file    <- paste0("~/BBand_LAP/SIDE_DATA/CSid_RenoHansen/", basename(sub("\\.R$", ".Rds", Script.Name)))

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")
source("~/BBand_LAP/process/CSid_RenoHansen/CSid_RenoHansen_functions.R")
source("~/CODE/R_myRtools/myRtools/R/trigonometric.R")
source("~/BBand_LAP/parameters/theory/Air_mass_models.R")
source("~/BBand_LAP/parameters/theory/Clear_sky_irradiance_models.R")

library(RColorBrewer, warn.conflicts = FALSE, quietly = TRUE)
library(caTools,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table,   warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,       warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,        warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,       warn.conflicts = FALSE, quietly = TRUE)
library(pander,       warn.conflicts = FALSE, quietly = TRUE)
library(scales,       warn.conflicts = FALSE, quietly = TRUE)
library(metrica,      warn.conflicts = FALSE, quietly = TRUE)
library(rlang,        warn.conflicts = FALSE, quietly = TRUE)
library(ggplot2,      warn.conflicts = FALSE, quietly = TRUE)

def.par <- par(no.readonly = TRUE) # save default, for resetting...
kcols   <- brewer.pal(11, "Set3")

##  Execution control  ---------------------------------------------------------

## __ Select global models  ----------------------------------------------------
CS_models_list <- c(
  "DPP",
  "KC",
  "HAU",
  "BD",       ## too poor results
  "ABCG",
  "RS",
  "KASTEN",
  "INEICHEN",
  NULL
)

## __ Select air mass models  --------------------------------------------------
AM_models_list <- c(
  "AM_simple",
  "AM_kasten_young",
  "AM_young",
  "AM_rodgers",
  NULL
)

## __ Select data  -------------------------------------------------------------
START_DAY     <- "1993-01-01"
END_DAY       <- "2024-01-01"
MIN_Elevation <- 5            ## Sun elevation for optimization input data


##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_BROAD))

DT <- tbl(con, "LAP")

## __ Select data range  -------------------------------------------------------
cat(paste("Data range:", START_DAY, "--", END_DAY), "\n\n")
DT <- DT |>
  filter(Date > START_DAY) |>
  filter(Date < END_DAY)

## __ Choose quality data  -----------------------------------------------------
flags_used <- DT |> select(starts_with("QCV10"), -contains("_dir_")) |> colnames()
cat(c("Quality flags for GHI:", flags_used), "\n\n")

GLB <- DT |>
  filter(Elevat > MIN_Elevation)              |> # Limit above horizon
  select(-contains("chp1"), -contains("dir")) |> # Ignore DNI data
  filter(                                        # Keep only very good GHI data
    if_all(
      starts_with("QCV10"), -contains("_dir_")   # Use all flags without DNI
      ~ . %in% c("pass", "empty")
    )
  ) |>
  select(
    Date, GLB_strict, SZA, preNoon
  )

## __ Select data to train  ----------------------------------------------------
TEST_DT <- GLB |> filter(!is.na(GLB_strict)) |> collect() |> data.table()


##  Test data vs models  -------------------------------------------------------

## __ Compute combinations for all data  ---------------------------------------
comb_all <- expand.grid(CS_models  = CS_models_list,
                        AM_models  = AM_models_list,
                        alpha      = NA,
                        mse        = NA,  ## cost
                        rmse       = NA,
                        mbe        = NA,
                        mae        = NA,
                        YEAR       = as.numeric(NA),
                        MONTH      = as.numeric(NA),
                        DATA_N     = nrow(TEST_DT),
                        DATA_Start = min(TEST_DT$Date),
                        DATA_End   = max(TEST_DT$Date),
                        Scheme     = "All",
                        stringsAsFactors = F)

## find what is not done
if (file.exists(save_file)) {
  old <- readRDS(save_file)
  comb_all <- anti_join(
    comb_all, old,
    by = c("CS_models", "AM_models", "DATA_N", "DATA_Start", "DATA_End", "Scheme")
  )
}

for (ii in rownames(comb_all)) {
  ll <- comb_all[ii,]
  ii <- as.numeric(ii)

  cat(sprintf(
    "%7s %5.1f%% %s %15s %8s %g\n",
    paste0(ii, "/", nrow(comb_all)),
    round(100 * ii / nrow(comb_all), 1),
    ll$Scheme,
    paste(ll$AM_models),
    paste(ll$CS_models),
    ll$DATA_N
  ))

  ## Get models instance
  CS_model <- get(ll$CS_models)
  AM       <- get(ll$AM_models)

  ## Create a cost function
  costf <- function(alpha) {
    TEST_DT[, MSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  }

  res   <- optim(c(-.3, 1.7), costf)
  alpha <- mean(res$par)

  comb_all[ii, "alpha"]    <- alpha
  comb_all[ii, "opt_N"]    <- res$counts[1]
  comb_all[ii, "computed"] <- Sys.time()

  ## model stats
  comb_all[ii, "rmse"] <- TEST_DT[, RMSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_all[ii, "mbe"]  <- TEST_DT[,  MBE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_all[ii, "mse"]  <- TEST_DT[,  MSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_all[ii, "mae"]  <- TEST_DT[,  MAE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
}

if (!file.exists(save_file)) {
  saveRDS(comb_all, save_file)
} else {
  old <- readRDS(save_file)
  save <- rows_upsert(
    old, comb_all,
    by = c("CS_models", "AM_models", "DATA_N", "DATA_Start", "DATA_End", "Scheme")
  )
  saveRDS(save, save_file)
}
rm(comb_all)


## __ Compute combinations for each years data  --------------------------------
comb_year <- expand.grid(CS_models  = CS_models_list,
                         AM_models  = AM_models_list,
                         alpha      = NA,
                         mse        = NA,  ## cost
                         rmse       = NA,
                         mbe        = NA,
                         mae        = NA,
                         YEAR       = unique(year(TEST_DT$Date)),
                         MONTH      = as.numeric(NA),
                         DATA_Start = min(TEST_DT$Date),
                         DATA_End   = max(TEST_DT$Date),
                         Scheme     = "Yearly",
                         stringsAsFactors = F)

comb_year <- left_join(comb_year,
          TEST_DT[, .(DATA_N = .N), by = .(YEAR = year(Date))],
          by = "YEAR")

## find what is not done
if (file.exists(save_file)) {
  old <- readRDS(save_file)
  comb_year <- anti_join(
    comb_year, old,
    by = c("CS_models", "AM_models", "DATA_N", "Scheme", "YEAR")
  )
}

for (ii in rownames(comb_year)) {
  ll <- comb_year[ii,]
  ii <- as.numeric(ii)

  cat(sprintf(
    "%7s %5.1f%% %s %15s %8s %s %g\n",
    paste0(ii, "/", nrow(comb_year)),
    round(100 * ii / nrow(comb_year), 1),
    ll$Scheme,
    paste(ll$AM_models),
    paste(ll$CS_models),
    ll$YEAR,
    TEST_DT[year(Date) == ll$YEAR, .N]
  ))

  ## Get models instance
  CS_model <- get(ll$CS_models)
  AM       <- get(ll$AM_models)

  ## Create a cost function
  costf <- function(alpha) {
    TEST_DT[year(Date) == ll$YEAR, MSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  }

  res   <- optim(c(-.3, 1.7), costf)
  alpha <- mean(res$par)

  comb_year[ii, "alpha"]    <- alpha
  comb_year[ii, "opt_N"]    <- res$counts[1]
  comb_year[ii, "computed"] <- Sys.time()
  comb_year[ii, "DATA_N"]   <- TEST_DT[year(Date) == ll$YEAR, .N]

  ## model stats
  comb_year[ii, "rmse"] <- TEST_DT[, RMSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_year[ii, "mbe"]  <- TEST_DT[,  MBE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_year[ii, "mse"]  <- TEST_DT[,  MSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_year[ii, "mae"]  <- TEST_DT[,  MAE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
}

if (!file.exists(save_file)) {
  saveRDS(comb_year, save_file)
} else {
  old <- readRDS(save_file)
  save <- rows_upsert(
    old, comb_year,
    by = c("CS_models", "AM_models", "DATA_N", "Scheme", "YEAR")
  )
  saveRDS(save, save_file)
}
rm(comb_year)


## __ Compute combinations for each month  -------------------------------------
comb_month <- expand.grid(CS_models  = CS_models_list,
                          AM_models  = AM_models_list,
                          alpha      = NA,
                          mse        = NA,  ## cost
                          rmse       = NA,
                          mbe        = NA,
                          mae        = NA,
                          YEAR       = as.numeric(NA),
                          MONTH      = 1:12,
                          DATA_Start = min(TEST_DT$Date),
                          DATA_End   = max(TEST_DT$Date),
                          Scheme     = "Monthly Clima",
                          stringsAsFactors = F)

comb_month <- left_join(comb_month,
                        TEST_DT[, .(DATA_N = .N), by = .(MONTH = month(Date))],
                        by = "MONTH")

## find what is not done
if (file.exists(save_file)) {
  old <- readRDS(save_file)
  comb_month <- anti_join(
    comb_month, old,
    by = c("CS_models", "AM_models", "DATA_N", "Scheme", "MONTH")
  )
}

for (ii in rownames(comb_month)) {
  ll <- comb_month[ii,]
  ii <- as.numeric(ii)

  cat(sprintf(
    "%7s %5.1f%% %s %15s %8s %s %g\n",
    paste0(ii, "/", nrow(comb_month)),
    round(100 * ii / nrow(comb_month), 1),
    ll$Scheme,
    paste(ll$AM_models),
    paste(ll$CS_models),
    ll$MONTH,
    TEST_DT[month(Date) == ll$MONTH, .N]
  ))

  ## Get models instance
  CS_model <- get(ll$CS_models)
  AM       <- get(ll$AM_models)

  ## Create a cost function
  costf <- function(alpha) {
    TEST_DT[month(Date) == ll$MONTH, MSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  }

  res   <- optim(c(-.3, 1.7), costf)
  alpha <- mean(res$par)

  comb_month[ii, "alpha"]    <- alpha
  comb_month[ii, "opt_N"]    <- res$counts[1]
  comb_month[ii, "computed"] <- Sys.time()
  comb_month[ii, "DATA_N"]   <- TEST_DT[month(Date) == ll$MONTH, .N]

  ## model stats
  comb_month[ii, "rmse"] <- TEST_DT[, RMSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_month[ii, "mbe"]  <- TEST_DT[,  MBE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_month[ii, "mse"]  <- TEST_DT[,  MSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_month[ii, "mae"]  <- TEST_DT[,  MAE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
}

if (!file.exists(save_file)) {
  saveRDS(comb_month, save_file)
} else {
  old <- readRDS(save_file)
  save <- rows_upsert(
    old, comb_month,
    by = c("CS_models", "AM_models", "DATA_N", "Scheme", "MONTH")
  )
  saveRDS(save, save_file)
}
rm(comb_month)





## __ Compute combinations for each month/year  --------------------------------
comb_bymonth <- expand.grid(CS_models  = CS_models_list,
                          AM_models  = AM_models_list,
                          alpha      = NA,
                          mse        = NA,  ## cost
                          rmse       = NA,
                          mbe        = NA,
                          mae        = NA,
                          YEAR       = unique(year(TEST_DT$Date)),
                          MONTH      = unique(month(TEST_DT$Date)),
                          DATA_Start = min(TEST_DT$Date),
                          DATA_End   = max(TEST_DT$Date),
                          Scheme     = "Monthly Clima",
                          stringsAsFactors = F)

# comb_month <- left_join(comb_month,
#                         TEST_DT[, .(DATA_N = .N), by = .(MONTH = month(Date))],
#                         by = "MONTH")
#
# ## find what is not done
# if (file.exists(save_file)) {
#   old <- readRDS(save_file)
#   comb_month <- anti_join(
#     comb_month, old,
#     by = c("CS_models", "AM_models", "DATA_N", "Scheme", "MONTH")
#   )
# }

setorder(comb_bymonth, YEAR, MONTH)

for (ii in rownames(comb_bymonth)) {
  ll <- comb_bymonth[ii,]
  ii <- as.numeric(ii)

  if (!TEST_DT[month(Date) == ll$MONTH & year(Date) == ll$YEAR, .N] > 10) next()

  cat(sprintf(
    "%7s %5.1f%% %s %15s %8s %s %s %g\n",
    paste0(ii, "/", nrow(comb_bymonth)),
    round(100 * ii / nrow(comb_bymonth), 1),
    ll$Scheme,
    paste(ll$AM_models),
    paste(ll$CS_models),
    ll$YEAR,
    ll$MONTH,
    TEST_DT[month(Date) == ll$MONTH & year(Date) == ll$YEAR, .N]
  ))

  ## Get models instance
  CS_model <- get(ll$CS_models)
  AM       <- get(ll$AM_models)

  ## Create a cost function
  costf <- function(alpha) {
    TEST_DT[month(Date) == ll$MONTH & year(Date) == ll$YEAR,
            MSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  }

  res   <- optim(c(-.3, 1.7), costf)
  alpha <- mean(res$par)

  comb_bymonth[ii, "alpha"]    <- alpha
  comb_bymonth[ii, "opt_N"]    <- res$counts[1]
  comb_bymonth[ii, "computed"] <- Sys.time()
  comb_bymonth[ii, "DATA_N"]   <- TEST_DT[month(Date) == ll$MONTH, .N]

  ## model stats
  comb_bymonth[ii, "rmse"] <- TEST_DT[, RMSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_bymonth[ii, "mbe"]  <- TEST_DT[,  MBE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_bymonth[ii, "mse"]  <- TEST_DT[,  MSE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
  comb_bymonth[ii, "mae"]  <- TEST_DT[,  MAE(obs = GLB_strict, pred = alpha * CS_model(SZA))]
}

if (!file.exists(save_file)) {
  saveRDS(comb_bymonth, save_file)
} else {
  old <- readRDS(save_file)
  save <- rows_upsert(
    old, comb_bymonth,
    by = c("CS_models", "AM_models", "DATA_N", "Scheme", "MONTH", "YEAR")
  )
  saveRDS(save, save_file)
}
rm(comb_month)






DT <- data.table(readRDS(save_file))

#'
#' \newpage
#'
#' ## DETECTION OF CLEAR PERIODS IN GHI MEASUREMENTS ##
#'
#+ include=T, echo=FALSE

all <- DT[Scheme == "All"]
all <- all[DATA_N == max(DATA_N), ]

ggplot(data = all[AM_models == "AM_simple"],
       aes(x = alpha, y = rmse, label = CS_models)) +
  geom_point() +
  geom_text_repel() +
  theme_bw()

yearly <- DT[Scheme == "Yearly"]

ggplot(data = yearly[AM_models == "AM_simple"]) +
  geom_point(aes(x = alpha, y = rmse)) +
  theme_bw()

ggplot(data = yearly[AM_models == "AM_simple"]) +
  geom_line(aes(x = YEAR, y = alpha, colour = CS_models)) +
  theme_bw()

test <- yearly[AM_models == "AM_simple"]
lm(test$alpha ~ test$YEAR)

monthly <- DT[Scheme == "Monthly Clima"]
monthly <- monthly[DATA_Start < "1994-01-01"]


ggplot(data = monthly[AM_models == "AM_simple"]) +
  geom_line(aes(x = MONTH, y = alpha, colour = CS_models)) +
  theme_bw()

sel <- monthly[AM_models == "AM_simple", min(rmse), by = "MONTH"]

monthly <- merge(monthly[AM_models == "AM_simple"], sel)
monthly[rmse == V1]


## TODO plot aplhas
## TODO creat a clear value with a statistic approach
## median of max( doy/week/month, sza)
## TODO best rmse for each years



TEST_DT[, median(GLB_strict), by = .(yday(Date), SZA %/% 1)]

TEST_DT[, .(max    = max(GLB_strict),
            median = median(GLB_strict),
            mean   = mean(GLB_strict)),
        by = .( SZA %/% 1)]  %>%
  ggplot(.) +
  geom_point(aes(x = SZA, y = max   )) +
  geom_point(aes(x = SZA, y = median), color = "blue" ) +
  geom_point(aes(x = SZA, y = mean),   color = "green")



#+ results="asis", echo=FALSE
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
