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
#' params:
#'   CLEAN: TRUE
#'
#' ---

#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
#+ echo=F, include=T


#+ echo=F, include=F
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"  )
knitr::opts_chunk$set(fig.align  = "center")
knitr::opts_chunk$set(fig.pos    = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/inspect_db/Inspect_CM21_sig.R"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)

library(arrow,      warn.conflicts = TRUE, quietly = TRUE)
library(dplyr,      warn.conflicts = TRUE, quietly = TRUE)
library(lubridate,  warn.conflicts = TRUE, quietly = TRUE)
library(data.table, warn.conflicts = TRUE, quietly = TRUE)
library(tools,      warn.conflicts = TRUE, quietly = TRUE)
library(pander,     warn.conflicts = TRUE, quietly = TRUE)

panderOptions("table.alignment.default", "right")
panderOptions("table.split.table",        120   )


## __  Variables  --------------------------------------------------------------
CLEAN        <- TRUE
CLEAN        <- FALSE


## __ Execution control  -------------------------------------------------------
## When knitting
if (exists("params")) {
    # params <- list(CLEAN = CLEAN)
    CLEAN <- params$CLEAN
}
## When running
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    if (any(args == "CLEAN")) { CLEAN <- TRUE  }
    if (any(args == "DIRTY")) { CLEAN <- FALSE }
    cat("Arguments", paste(args),"\n")
}

cat(paste("\n**CLEAN:", CLEAN, "**\n"))


BB <- opendata()


missingGLB <- data.table(
    BB |> filter(is.na(tot_glb) & !is.na(GLB_wpsm)) |>
        select(Date) |> collect())

missingTOT <- data.table(
    BB |> filter(!is.na(tot_glb) & is.na(GLB_wpsm)) |>
        select(Date) |> collect())


missingGLB <- missingGLB[, .(Min_date = min(Date),
                             Max_date = max(Date),
                             N = .N),
                         by = .(Day = as.Date(Date))]


missingTOT <- missingTOT[, .(Min_date = min(Date),
                             Max_date = max(Date),
                             N = .N),
                         by = .(Day = as.Date(Date))]




all_data <- data.table(
    BB |> select(GLB_wpsm, tot_glb, Elevat, Date, SZA) |>
        collect()
)

sun_up   <- all_data[Elevat > 0]
sun_down <- all_data[Elevat < 0]

plot(sun_up[, tot_glb - GLB_wpsm, Date],
     main = "tot_glb - GLB_wpsm  Elevation>0")

plot(sun_up[, tot_glb / GLB_wpsm, Date],
     main = "tot_glb / GLB_wpsm  Elevation>0")

plot(sun_up[, (GLB_wpsm - tot_glb) / tot_glb, Date],
     main = "(GLB_wpsm - tot_glb) / tot_glb  Elevation>0")

plot(sun_down[, tot_glb - GLB_wpsm, Date],
     main = "tot_glb - GLB_wpsm  Elevation<0")

plot(sun_down[, tot_glb / GLB_wpsm, Date],
     main = "tot_glb / GLB_wpsm  Elevation<0")

plot(sun_down[, (GLB_wpsm - tot_glb) / tot_glb, Date],
     main = "(GLB_wpsm - tot_glb) / tot_glb  Elevation<0")

rm(all_data, sun_down, sun_up)
gc()




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
     main = "Days with missing Global but not TOT")






all_data <- data.table(
    BB |> select(GLB_wpsm, tot_glb, Elevat, Date, SZA) |>
        collect()
)

sun_up   <- all_data[Elevat > 0]
sun_down <- all_data[Elevat < 0]

plot(sun_up[, tot_glb - GLB_wpsm, Date],
     main = "tot_glb - GLB_wpsm  Elevation>0")

plot(sun_up[, tot_glb / GLB_wpsm, Date],
     main = "tot_glb / GLB_wpsm  Elevation>0")

plot(sun_up[, (GLB_wpsm - tot_glb) / tot_glb, Date],
     main = "(GLB_wpsm - tot_glb) / tot_glb  Elevation>0")

plot(sun_down[, tot_glb - GLB_wpsm, Date],
     main = "tot_glb - GLB_wpsm  Elevation<0")

plot(sun_down[, tot_glb / GLB_wpsm, Date],
     main = "tot_glb / GLB_wpsm  Elevation<0")

plot(sun_down[, (GLB_wpsm - tot_glb) / tot_glb, Date],
     main = "(GLB_wpsm - tot_glb) / tot_glb  Elevation<0")

rm(all_data, sun_down, sun_up)
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



datayears <- BB |>
    filter(!is.na(GLB_wpsm) | !is.na(tot_glb) ) |>
    select(year) |>
    unique()     |>
    collect()    |>
    pull()


#'
#' ## Yearly plots
#'


#+ include=TRUE, echo=FALSE, results="asis"
for (YYYY in sort(datayears)) {
    days_of_year <- seq.Date(as.Date(paste0(YYYY, "-01-01")),
                             as.Date(paste0(YYYY, "-12-31")), by = "day")
    ## don't go to the future
    days_of_year <- days_of_year[days_of_year <= Sys.Date()]

    cat("\n\n\\FloatBarrier\n\n")
    cat("\\newpage\n\n")
    cat("\n### Year:", YYYY, "\n\n")

    ## load data for year
    year_data <- data.table(
        BB |>
            filter(year == YYYY) |>
            select(GLB_wpsm, tot_glb, Elevat, Date, SZA) |>
        collect()
    )

    sun_up   <- year_data[Elevat > 0]
    sun_down <- year_data[Elevat < 0]

    plot(sun_up[, tot_glb - GLB_wpsm, Date],
         main = "tot_glb - GLB_wpsm  Elevation>0")

    plot(sun_up[, tot_glb / GLB_wpsm, Date],
         main = "tot_glb / GLB_wpsm  Elevation>0")

    plot(sun_up[, (GLB_wpsm - tot_glb) / tot_glb, Date],
         main = "(GLB_wpsm - tot_glb) / tot_glb  Elevation>0")

    plot(sun_down[, tot_glb - GLB_wpsm, Date],
         main = "tot_glb - GLB_wpsm  Elevation<0")

    plot(sun_down[, tot_glb / GLB_wpsm, Date],
         main = "tot_glb / GLB_wpsm  Elevation<0")

    plot(sun_down[, (GLB_wpsm - tot_glb) / tot_glb, Date],
         main = "(GLB_wpsm - tot_glb) / tot_glb  Elevation<0")

}





#' **END**
#+ include=T, echo=F
# myunlock(DB_lock)
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
