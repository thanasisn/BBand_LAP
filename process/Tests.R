# /* #!/opt/R/4.2.3/bin/Rscript */
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Tests"
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
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

#'
#' **TESTS**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
#'
#+ echo=F, include=T

#+ echo=F, include=T
## __ Document options ---------------------------------------------------------
knitr::opts_chunk$set(comment   = ""      )
knitr::opts_chunk$set(dev       = "png"   )
knitr::opts_chunk$set(out.width = "100%"  )
knitr::opts_chunk$set(fig.align = "center")
knitr::opts_chunk$set(fig.pos   = '!h'    )


## __ Set environment  ---------------------------------------------------------
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/process/Tests.R"
renv::load("~/BBand_LAP")

source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_BBand_LAP.R")
source("~/BBand_LAP/functions/Functions_CM21.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(scales,     warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,     warn.conflicts = FALSE, quietly = TRUE)

con   <- dbConnect(duckdb(dbdir = DB_DUCK, read_only = TRUE))

tt <- tbl(con, "LAP")

ex <- tt |>
  select(Date, SZA, Azimuth, Day,
         DIFF_strict, DIR_strict, GLB_strict) |>
  filter(Day %in% c("2024-05-06", "2023-06-21")) |>
  collect() |> data.table()

write.csv2(ex, "DimitrisK.csv")

stop()

## . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ----

## open data base
BB <- opendata()


## biology building
test1 <- data.table(BB |>
    filter(!is.na(QCv9_08_bth_flag) &
               Elevat  <  30 &
               Azimuth < 130 &
               Azimuth > 80 ) |>
        collect())

plot(test1[, .(Azimuth, Elevat)],
     main = "test1")


test2 <- data.table(BB |>
                       filter(!is.na(QCv9_03_obs_flag) &
                                  Elevat  <  30 &
                                  Azimuth < 130 &
                                  Azimuth > 80 ) |>
                       collect())

plot(test2[, .(Azimuth, Elevat)],
     main = "test2")


max(test2$Elevat)

max(test1$Elevat)

range(test1$Date, test2$Date)


DT <- read_parquet(DB_META_fl)


# ## export dark data for thesis
# DARK <- DT |> select(contains("dark"), day)
# DARK$cm21_Daily_dark_watt <- cm21factor(as.POSIXct(DARK$day)) * DARK$cm21_Daily_dark
# DARK$chp1_Daily_dark_watt <- chp1factor(as.POSIXct(DARK$day)) * DARK$chp1_Daily_dark
# saveRDS(DARK, "~/MANUSCRIPTS/03_thesis/MAIN_el/data/dark_signal.Rds")






#' **END**
#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
