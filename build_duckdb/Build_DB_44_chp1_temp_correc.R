#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2023 Athanasios Natsis <natsisphysicist@gmail.com> */
#'
#' Compute temperature correction for CHP-1.
#'
#' Fills with valid only data:
#'
#' - `chp1_t_cor_factor` temperature correction factor from CHP-1 manual.
#' - `DIR_wpsm_temp_cor` temperature corrected direct beam radiation
#' - `HOR_wpsm_temp_cor` temperature corrected direct radiation on level surface
#'
#' TODO:
#'
#' - compute uncertainties...
#'
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.github.io`](https://thanasisn.github.io/)**
#'
#+ echo=F, include=T

#+ echo=F, include=F
## __ Document options  --------------------------------------------------------
knitr::opts_chunk$set(comment    = ""      )
knitr::opts_chunk$set(dev        = "png"   )
knitr::opts_chunk$set(out.width  = "100%"  )
knitr::opts_chunk$set(fig.align  = "center")
knitr::opts_chunk$set(fig.cap   = " - empty caption - ")
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
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_44_chp1_temp_correc.R"
Script.ID   <- "44"

if (!interactive()) {
  pdf(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
}

## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/BBand_LAP/functions/Functions_CHP1.R")
source("~/BBand_LAP/functions/Functions_duckdb_LAP.R")

library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(duckdb,     warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)

stop("todo")

##  Open dataset  --------------------------------------------------------------
con <- dbConnect(duckdb(dbdir = DB_DUCK))





## - - - - - - - - - - - - - - - - - - - - -

##  Create new variables to the whole database  --------------------------------
BB <- opendata()
for (avar in c("chp1_t_cor_factor",
               "DIR_wpsm_temp_cor",
               "HOR_wpsm_temp_cor")) {
  if (!any(names(BB) == avar)) {
    cat("Create column: ", avar, "\n")
    BB <- BB |> mutate( !!avar := as.numeric(NA)) |> compute()
    writedata(BB)
  }
}


##  Edit database where it is needed  ------------------------------------------


## select what dataset files to touch
filelist <- filelist[temp_to_do, on = .(flmonth = month, flyear = year)]
rm(temp_to_do)


## loop data base files computing black for CHP-1
for (af in filelist$names) {
  datapart <- data.table(read_parquet(af))
  datapart[, month := as.integer(month(Date))]
  datapart[, year  := as.integer(year(Date)) ]

  ## work only on valid data
  data_use <- datapart[is.na(chp1_bad_temp_flag) &
                         !is.na(chp1_temperature) &
                         is.na(chp1_t_cor_factor)]

  cat("44 Load: ", af, "\n")

  ## Ignore bad and missing data
  if (nrow(data_use) == 0) {
    cat("\nNo useful CHP-1 data in this file\n\n")
    next()
  }


  ##  Temperature correction of direct radiation  ----------------------------
  CHP_temp_dep_fun <- CHP_temp_dep(fun = 'lin')

  ## Temperature correction factor for each minute in percentage units
  data_use[, chp1_t_cor_factor := (100 - CHP_temp_dep_fun(chp1_temperature)) / 100]
  ## Apply temperature correction to direct irradiance
  data_use[, DIR_wpsm_temp_cor := DIR_wpsm * chp1_t_cor_factor  ]
  data_use[, HOR_wpsm_temp_cor := DIR_wpsm_temp_cor * cosde(SZA)]

  ## see: level_1_CHP1_x119.R
  ## Temperature correction factor for each minute in percentage units
  # DAY_$chp1TempCF     <- ( 100 - CHP_tempdep(DAY_$CHP1temp) ) / 100
  ## Apply temperature correction to direct irradiance
  # DAY_$wattDIR_tmp_cr <- DAY_$chp1TempCF     * DAY_$wattDIR
  # DAY_$wattHOR_tmp_cr <- DAY_$wattDIR_tmp_cr * cosSZA
  ## Uncertainty with temperature known (_WT With Temperatures)
  # DAY_$wattDIR_unc_WT <- abs( DAY_$wattDIR_tmp_cr * chp1_UNC_WT )
  # DAY_$wattHOR_unc_WT <- abs( DAY_$wattHOR_tmp_cr * chp1_UNC_WT )


  ## update data base with new data
  datapart <- rows_update(datapart, data_use, by = "Date")
  datapart <- as_tibble(datapart)

  ## store actual data
  # write_parquet(x = datapart, sink = af)
  # write_parquet(BB_meta, DB_META_fl)
  ## clean
  rm(datapart, meta_day)
}



## clean exit
dbDisconnect(con, shutdown = TRUE); rm(con); closeAllConnections()

tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)

