# /* #!/opt/R/4.2.3/bin/Rscript */
# /* Copyright (C) 2024 Athanasios Natsis <natsisphysicist@gmail.com> */
#' ---
#' title:         "Radiation Quality Control **QCRad** "
#' author:        "Natsis Athanasios"
#' institute:     "AUTH"
#' affiliation:   "Laboratory of Atmospheric Physics"
#' abstract:      "Data quality for radiation measurements as described by
#'                 CN Long and Y Shi, September 2006, DOE/SC-ARM/TR-074.
#'                 - The QCRad Value Added Product Surface
#'                 Radiation Measurement Quality Control Testing Including
#'                 Climatology_Long2006.pdf"
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

#' **QCRad T08**
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#'
#+ echo=F, include=T

#+ echo=F, include=T
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
Script.Name  <- "~/BBand_LAP/process/QCRad_ThanasisN/QCRad_ThanasisN_T08_v10.R"
Script.ID    <- "Q8"
parameter_fl <- "~/BBand_LAP/SIDE_DATA/QCRad_LongShi_v10_duck_parameters.Rds"

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/",   basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(pander,     warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
require(scales,     warn.conflicts = FALSE, quietly = TRUE)

##  Variables  -----------------------------------------------------------------
if (file.exists(parameter_fl)) {
  QS <<- readRDS(parameter_fl)
} else {
  stop("File not initialized")
}

## mostly for daily plots
DO_PLOTS     <- TRUE
if (interactive()) {
  DO_PLOTS <- FALSE
}

# Daily plots
DO_PLOTS       <- TRUE
# Ignore previous flagged points in plots (not fully implemented yet)
IGNORE_FLAGGED <- TRUE   ## TRUE is the default of the original
# IGNORE_FLAGGED <- FALSE

flagname_BTH     <- "QCv10_08_bth_flag"
QS$plot_elev_T08 <- 2

if (FALSE | Sys.info()["nodename"] == "sagan") {

  ##  Open dataset  ------------------------------------------------------------
  con <- dbConnect(duckdb(dbdir = DB_BROAD))

  ## 8. Test for inverted values  --------------------------------------------
  #'
  #' ## 8. Test for inverted values
  #'
  #' Test the ratio of Diffuse / Global radiation.
  #' When the Diffuse is too lower than Global, (less than a % limit).
  #'
  #' This denotes obstacles on the mornings mostly, or very low
  #' signals when Sun is near the horizon.
  #' Due to the time difference of sun shine, due to geometry, location and
  #' obstacles.
  #'
  #' Mornings before inversion only diffuse radiation can be measured
  #'
  #' SEE: https://stats.stackexchange.com/questions/143622/fitting-an-envelope-to-x-y-data-in-r
  #' SEE: https://cran.r-project.org/web/packages/GET/vignettes/pointpatterns.pdf
  #'
  #' And possible cases of Instrument windows cleaning shadowing.
  #'
  #' Probably these value should be removed for CS when occurring on low
  #' elevation angles, as the measurements can not be considered to reflect
  #' the same condition of Sun visibility.
  #'
  #' Additional criteria is needed for any data drop.
  #'
  #+ echo=F, include=T, results="asis"

  QS$dir_glo_invert   <- 0.05  # Diffuse Inversion threshold factor
  QS$invert_glo_limit <- 5     # Global minimum for this test to work

  cat(paste("\n8. Inversion test.\n\n"))

  ## __ Make categorical columns  ----------------------------------------------
  categories <- c("empty",
                  "pass",
                  "Direct > global soft (14)",
                  "Direct > global hard (15)")

  remove_column(con, "LAP", flagname_BTH)
  make_categorical_column(flagname_BTH, categories, con, "LAP")

  ## __ Both  ------------------------------------------------------------
  ADD <- tbl(con, "LAP")               |>
    filter(Elevat > QS$sun_elev_min)   |>
    filter(!is.na(GLB_strict))         |>
    filter(!is.na(HOR_strict))         |>
    select(Date,
           GLB_strict, HOR_strict)     |>
    mutate(

      !!flagname_BTH := case_when(

        ## case with direct too close to global
        HOR_strict / GLB_strict >= 1 - QS$dir_glo_invert  &
          GLB_strict > QS$invert_glo_limit  ~ "Direct > global soft (14)",

        ## case with direct greater than global
        HOR_strict / GLB_strict >= 1  &
          GLB_strict > QS$invert_glo_limit  ~ "Direct > global hard (15)",

        .default = "pass"
      ))
  res <- update_table(con, ADD, "LAP", "Date")


  # datapart[, Relative_diffuse := 100 * (HOR_strict  - GLB_strict) / GLB_strict ]
  # datapart[ is.infinite(Relative_diffuse), Relative_diffuse := NA]
  #
  # datapart[Relative_diffuse > QS$dir_glo_invert  &
  #            GLB_strict       > QS$dir_glo_glo_off,
  #          (flagname_BTH) := "Direct > global soft (14)"]
  # datapart[Relative_diffuse > QS$dir_glo_invert,
  #          (flagname_BTH) := "Direct > global hard (15)"]

  ## __  Store used filters parameters  ----------------------------------------
  saveRDS(object = QS,
          file   = parameter_fl)
}

##  Plots  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  ----

##  Open dataset
con <- dbConnect(duckdb(dbdir = DB_BROAD, read_only = TRUE))

## Select data to plot
DT <- tbl(con, "LAP")                  |>
  filter(Day    > QCrad_plot_date_min) |>
  filter(Day    < QCrad_plot_date_max) |>
  filter(Elevat > QS$plot_elev_T08)

## TODO when plotting ignore previous flagged data or not, but fully apply flag

## __  Statistics  -------------------------------------------------------------
#' ### Statistics
#+ echo=F, include=T, sesults="asis"
cat(pander(DT |> select(!!flagname_BTH) |> pull() |> table(),
           caption = flagname_BTH))
cat(" \n \n")



if (FALSE | Sys.info()["nodename"] == "tyler") {
  ## TEST
  DT <- DT |>
    filter(year %in% c(2018:2020))
  # QS$dir_glo_invert   <- 0.05  # Diffuse Inversion threshold factor
  # QS$invert_glo_limit <- 5     # Global minimum for this test to work

}


test <- DT |>
  filter(Elevat > QS$plot_elev_T08) |>
  filter(!is.na(GLB_wpsm))   |>
  filter(!is.na(DIR_wpsm))   |>
  select(
    Elevat, SZA, Day, Azimuth,
    GLB_wpsm, DIR_wpsm,
    DIR_strict, GLB_strict, HOR_strict, DIFF_strict,
    !!flagname_BTH
    ) |>
  collect() |> data.table()


## create dirty diffuse
test[, HOR_wpsm  := DIR_strict * cos(SZA * pi / 180)]
# test[, DIFF_wpsm := GLB_wpsm - HOR_wpsm]
test[, DIFF_wpsm := GLB_strict - HOR_wpsm]
test[, kd_wpsm   := DIFF_wpsm / GLB_strict]
test[, Relative_diffuse := 100 * (GLB_strict - HOR_strict) / GLB_strict]

test[, Relative := HOR_strict / GLB_strict]

test[kd_wpsm < QS$dir_glo_invert,
     temp_flag := "Hit"]


plot(test[, GLB_strict, HOR_strict])
plot(test[, Relative])

test[HOR_strict / GLB_strict > 1  &
       GLB_strict > QS$invert_glo_limit, ]

test[HOR_strict / GLB_strict > 1 - QS$dir_glo_invert  &
       GLB_strict > QS$invert_glo_limit, ]


hist(test$Relative, breaks = 100)


hist(test[kd_wpsm < 0.2, kd_wpsm], breaks = 100)
abline(v = QS$dir_glo_invert, lty = 3, col = "red")
cat(" \n \n")



hist(test$DIFF_wpsm         , breaks = 100)
hist(test[, kd_wpsm]        , breaks = 100)



plot(test[temp_flag == "Hit", Elevat, Azimuth])

plot(test[!get(flagname_BTH) %in% c("empty", "pass"), Elevat, Azimuth])



hist(test[kd_wpsm < QS$dir_glo_invert & Elevat  > 3, Elevat],    breaks = 100)
hist(test[kd_wpsm < QS$dir_glo_invert & Elevat  > 3, DIFF_wpsm], breaks = 100)
hist(test[kd_wpsm < QS$dir_glo_invert & GLB_strict > QS$invert_glo_limit, Elevat],    breaks = 100)
hist(test[kd_wpsm < QS$dir_glo_invert & GLB_strict > QS$invert_glo_limit, DIFF_wpsm], breaks = 100)


test <- test[!is.na(temp_flag) & (!is.na(HOR_strict) | !is.na(GLB_strict) ) ]


for (ad in sort(unique(test$Day))) {
  ddd <- as.Date(ad, origin = origin)
  pp  <- DT |>
    filter(Day == ddd) |>
    select(
      Elevat, SZA, Date, Azimuth,
      GLB_wpsm, DIR_wpsm, DIR_strict, GLB_strict, HOR_strict,
      DIFF_strict,
      !!flagname_BTH) |>
    collect() |> data.table()
  setorder(pp, Date)

  ## create dirty diffuse
  pp[, HOR_wpsm  := DIR_strict * cos(SZA * pi / 180)]
  # test[, DIFF_wpsm := GLB_wpsm - HOR_wpsm]
  pp[, DIFF_wpsm := GLB_strict - HOR_wpsm]
  pp[, kd_wpsm   := DIFF_wpsm / GLB_strict]

  pp[kd_wpsm < QS$dir_glo_invert,
       temp_flag := "Hit"]

  ylim <- range(pp[, HOR_strict, GLB_strict], na.rm = T)
  plot( pp$Azimuth, pp$HOR_strict, "l",
        ylim = ylim, col = "blue", ylab = "", xlab = "")
  lines(pp$Azimuth, pp$GLB_strict, col = "green")
  title(paste("#8", as.Date(ad, origin = "1970-01-01")))

  points(pp[!is.na(temp_flag), HOR_strict, Azimuth],
         col = "red")
  points(pp[!is.na(temp_flag), GLB_strict, Azimuth],
         col = "magenta")

  points(pp[!get(flagname_BTH) %in% c("empty", "pass"), GLB_strict, Azimuth], col = alpha("blue", 0.5), cex = 1.5)
  points(pp[!get(flagname_BTH) %in% c("empty", "pass"), HOR_strict, Azimuth], col = alpha("blue", 0.5), cex = 1.5)

}




## __  Yearly plots  -----------------------------------------------------------
#' ### Yearly plots
#+ echo=F, include=T, results="asis"


## clean exit
dbDisconnect(con, shutdown = TRUE); rm("con"); closeAllConnections()

#+ include=T, echo=F, results="asis"
tac <- Sys.time()
cat(sprintf("\n**END** %s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
