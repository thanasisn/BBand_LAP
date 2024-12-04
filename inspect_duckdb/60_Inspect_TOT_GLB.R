# /* !/usr/bin/env Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Compare GLB / TOT"
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
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )

## __ Set environment  ---------------------------------------------------------
closeAllConnections()
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_duckdb/60_Inspect_TOT_GLB.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/duck/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )

## __  Variables  --------------------------------------------------------------
TEST  <- FALSE
# TEST  <- TRUE

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_DUCK, read_only = TRUE))
DT  <- tbl(con, "LAP")

missingGLB <- DT |>
  filter(is.na(tot_glb) & !is.na(GLB_wpsm)) |>
  select(Date) |> collect() |> data.table()

missingTOT <- DT |>
  filter(!is.na(tot_glb) & is.na(GLB_wpsm)) |>
  select(Date) |> collect() |> data.table()

missingGLB <- missingGLB[, .(Min_date = min(Date),
                             Max_date = max(Date),
                             N = .N),
                         by = .(Day = as.Date(Date))]

missingTOT <- missingTOT[, .(Min_date = min(Date),
                             Max_date = max(Date),
                             N = .N),
                         by = .(Day = as.Date(Date))]

DT <- DT |> select(GLB_wpsm, tot_glb, Elevat, Date, SZA, year)



#'
#' ## Comparison between my Global and TOT from sirena.
#'
#' There are `r sum(missingGLB$N)` missing GLB values but not TOT.
#'
#' There are `r sum(missingTOT$N)` missing TOT values but not GLB.
#'
#+ include=TRUE, echo=FALSE

plot(missingGLB$Day, missingGLB$N,
     pch  = 19,
     cex  = 0.3,
     main = "Days with missing Global but not TOT")

plot(missingTOT$Day, missingTOT$N,
     pch  = 19,
     cex  = 0.3,
     main = "Days with missing TOT but not Global")

##  Plots for day
pp <- DT |>
  filter(Elevat > 0)                         |>
  filter(!is.na(tot_glb) & !is.na(GLB_wpsm)) |>
  mutate(
    differ = tot_glb - GLB_wpsm,
    ratio  = tot_glb / GLB_wpsm,
    percen = (GLB_wpsm - tot_glb) / tot_glb) |>
  select(differ, ratio, percen, Date)        |>
  collect() |> data.table()

plot(pp[, differ, Date],
     main = "tot_glb - GLB_wpsm  Elevation>0")

plot(pp[, ratio, Date],
     main = "tot_glb / GLB_wpsm  Elevation>0")

plot(pp[, percen, Date],
     main = "(GLB_wpsm - tot_glb) / tot_glb  Elevation>0")


##  Plots for night
pp <- DT |>
  filter(Elevat < 0)                         |>
  filter(!is.na(tot_glb) & !is.na(GLB_wpsm)) |>
  mutate(
    differ = tot_glb - GLB_wpsm,
    ratio  = tot_glb / GLB_wpsm,
    percen = (GLB_wpsm - tot_glb) / tot_glb) |>
  select(differ, ratio, percen, Date)        |>
  collect() |> data.table()


plot(pp[, differ, Date],
     main = "tot_glb - GLB_wpsm  Elevation<0")

plot(pp[, ratio, Date],
     main = "tot_glb / GLB_wpsm  Elevation<0")

plot(pp[, percen, Date],
     main = "(GLB_wpsm - tot_glb) / tot_glb  Elevation<0")

rm(pp)
gc()


#' ### Missing GLB by date
#+ include=TRUE, echo=FALSE
pander(
    missingGLB,
    caption = "Days with missing Global but not TOT"
)

setorder(missingGLB, -N)
#' ### Missing GLB by N
#+ include=TRUE, echo=FALSE
pander(
    missingGLB,
    caption = "Days with missing Global but not TOT"
)


#' ### Missing TOT by date
#+ include=TRUE, echo=FALSE
pander(
    missingTOT,
    caption = "Days with missing TOT but not Global"
)

setorder(missingTOT, -N)
#' ### Missing TOT by N
#+ include=TRUE, echo=FALSE
pander(
    missingTOT,
    caption = "Days with missing TOT but not Global"
)


##  Yearly plots  --------------------------------------------------------------
datayears <- DT |>
    filter(!is.na(GLB_wpsm) | !is.na(tot_glb) ) |>
    select(year) |>
    distinct() |> collect() |> pull()

#'
#' ## Yearly plots
#'
#+ include=TRUE, echo=FALSE, results="asis"
for (YYYY in sort(datayears)) {
    cat("\n\n\\FloatBarrier\n\n")
    cat("\\newpage\n\n")
    cat("\n### Year:", YYYY, "\n\n")

    year_data <- DT |> filter(year == YYYY) |> collect() |> data.table()

    sun_up   <- year_data[Elevat > 0]
    sun_down <- year_data[Elevat < 0]

    plot(sun_up[, tot_glb - GLB_wpsm, Date],
         main = paste(YYYY, "tot_glb - GLB_wpsm  Elevation>0"))

    plot(sun_up[, tot_glb / GLB_wpsm, Date],
         main = paste(YYYY, "tot_glb / GLB_wpsm  Elevation>0"))

    plot(sun_up[, (GLB_wpsm - tot_glb) / tot_glb, Date],
         main = paste(YYYY, "(GLB_wpsm - tot_glb) / tot_glb  Elevation>0"))

    plot(sun_down[, tot_glb - GLB_wpsm, Date],
         main = paste(YYYY, "tot_glb - GLB_wpsm  Elevation<0"))

    plot(sun_down[, tot_glb / GLB_wpsm, Date],
         main = paste(YYYY, "tot_glb / GLB_wpsm  Elevation<0"))

    plot(sun_down[, (GLB_wpsm - tot_glb) / tot_glb, Date],
         main = paste(YYYY, "(GLB_wpsm - tot_glb) / tot_glb  Elevation<0"))

}

## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

#' **END**
#+ include=T, echo=F
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
