#!/opt/R/4.2.3/bin/Rscript
# /* Copyright (C) 2022-2023 Athanasios Natsis <natsisphysicist@gmail.com> */

#'
#' Read Astropy
#'
#' This also initializes a lot of columns in the dataset and meta data.
#'
#' Populates:
#'  - Date
#'  - Azimuth
#'  - Elevat
#'  - SZA
#'  - preNoon
#'
#'
#' **Details and source code: [`github.com/thanasisn/BBand_LAP`](https://github.com/thanasisn/BBand_LAP)**
#'
#' **Data display: [`thanasisn.netlify.app/3-data_display`](https://thanasisn.netlify.app/3-data_display)**
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
Sys.setenv(TZ = "UTC")
tic <- Sys.time()
Script.Name <- "~/BBand_LAP/build_duckdb/Build_DB_01_sun.R"
Script.ID   <- "01"
# renv::load("~/BBand_LAP")

if (!interactive()) {
    pdf( file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".pdf", Script.Name))))
    sink(file = paste0("~/BBand_LAP/REPORTS/RUNTIME/", basename(sub("\\.R$", ".out", Script.Name))), split = TRUE)
}


## __ Load libraries  ----------------------------------------------------------
source("~/BBand_LAP/DEFINITIONS.R")
source("~/CODE/FUNCTIONS/R/execlock.R")
# mylock(DB_lock)

library(arrow,      warn.conflicts = FALSE, quietly = TRUE)
library(dplyr,      warn.conflicts = FALSE, quietly = TRUE)
library(lubridate,  warn.conflicts = FALSE, quietly = TRUE)
library(data.table, warn.conflicts = FALSE, quietly = TRUE)
library(tools,      warn.conflicts = FALSE, quietly = TRUE)
library(dbplyr,     warn.conflicts = FALSE, quietly = TRUE)
require(duckdb,     warn.conflicts = FALSE, quietly = TRUE)


cat("\n Initialize DB or import Sun data\n\n")


##  Open dataset  --------------------------------------------------------------
con   <- dbConnect(duckdb(dbdir = DB_DUCK))




##  Get Astropy files  ---------------------------------------------------------
SUN <- data.table(readRDS(ASTROPY_FL))
names(SUN)[names(SUN) == "Dist"]      <- "Sun_Dist_Astropy"
names(SUN)[names(SUN) == "Elevation"] <- "Elevat"
setorder(SUN, Date)
stopifnot(length(unique(SUN$Date)) == nrow(SUN))

## drop existing dates
if (dbExistsTable(con, "LAP")) {
  SUN <- anti_join(SUN,
            tbl(con, "LAP") |>
              select(Date) |>
              filter(!is.na(Date)) |>
              collect(),
            by = "Date")
}

# SUN <- first(SUN, 10000)

names(SUN)[names(SUN) == "Dist"] <- "Sun_Dist_Astropy"
SUN <- SUN |> relocate(Date) |> data.table()
SUN[, month := month(Date)]
SUN[, year  := year( Date)]
SUN[, doy   := yday( Date)]
SUN[, SZA   := 90 - Elevat]
SUN[Azimuth <= 180, preNoon := TRUE ]
SUN[Azimuth >  180, preNoon := FALSE]


if (!dbExistsTable(con, "LAP")) {
  cat("\n Init table 'LAP' \n\n")
  dbWriteTable(con, "LAP", SUN)
  db_create_index(con, "LAP", columns = "Date", unique = TRUE)
}

if (dbExistsTable(con, "LAP")) {
  cat("\n Add data to 'LAP' \n\n")
  dbWriteTable(con, "LAP", SUN, append = TRUE)
}



tbl(con, "LAP") |> tally()
tbl(con, "LAP") |> glimpse()
SUN             |> tally()
SUN             |> glimpse()


dbDisconnect(con)



stop("DONE")
test <- tbl(con, "LAP") |> collect() |> data.table()

plot(test[Elevat > 0, SZA, Azimuth ])



##  Import PySolar files  ------------------------------------------------------
for (YYYY in unique(year(inp_filelist$day))) {
    subyear <- inp_filelist[year(day) == YYYY]
    ## Months to do
    for (mm in subyear[, unique(month(day))]) {

        ##  Read this month set files
        gathermeta <- data.table()
        for (ad in submonth$day) {
            ss <- submonth[day == ad]
            ## Read sun data file  ---------------------------------------------
            sun_temp[, DIST  := NULL]
            sun_temp[, SZA   := 90 - Elevat]
            sun_temp[, year  := year( Date)]
            sun_temp[, month := month(Date)]
            sun_temp[, doy   := yday( Date)]

            ## Init DB variables for next processes ----------------------------
            ## For CM-21
            sun_temp[, CM21_sig                := as.numeric(NA)  ]
            sun_temp[, CM21_sig_sd             := as.numeric(NA)  ]
            sun_temp[, CM21_sig_wo_dark        := as.numeric(NA)  ]
            sun_temp[, cm21_bad_data_flag      := as.character(NA)]
            ## For INCLINED CM-21
            sun_temp[, CM21INC_sig             := as.numeric(NA)  ]
            sun_temp[, CM21INC_sig_sd          := as.numeric(NA)  ]
            sun_temp[, CM21INC_sig_wo_dark     := as.numeric(NA)  ]
            sun_temp[, cm21INC_bad_data_flag   := as.character(NA)]
            ## For CHP-1
            sun_temp[, Async_step_count        := as.integer(NA)  ]
            sun_temp[, Async_tracker_flag      := TRUE            ]
            sun_temp[, CHP1_sig                := as.numeric(NA)  ]
            sun_temp[, CHP1_sig_sd             := as.numeric(NA)  ]
            sun_temp[, CHP1_sig_wo_dark        := as.numeric(NA)  ]
            sun_temp[, chp1_R_SD_therm         := as.numeric(NA)  ]
            sun_temp[, chp1_R_meas_ERR         := as.numeric(NA)  ]
            sun_temp[, chp1_R_therm            := as.numeric(NA)  ]
            sun_temp[, chp1_bad_data_flag      := as.character(NA)]
            sun_temp[, chp1_bad_temp_flag      := as.character(NA)]
            sun_temp[, chp1_t_cor_factor       := as.numeric(NA)  ]
            sun_temp[, chp1_temp_UNC           := as.numeric(NA)  ]
            sun_temp[, chp1_temperature        := as.numeric(NA)  ]
            sun_temp[, chp1_temperature_SD     := as.numeric(NA)  ]
            ## For TOT
            sun_temp[, tot_glb                 := as.numeric(NA)  ]
            sun_temp[, tot_glb_sd              := as.numeric(NA)  ]
            sun_temp[, lap_sza                 := as.numeric(NA)  ]
            ## Radiation
            sun_temp[, DIR_SD_wpsm             := as.numeric(NA)  ]
            sun_temp[, DIR_strict              := as.numeric(NA)  ]
            sun_temp[, DIR_wpsm                := as.numeric(NA)  ]
            sun_temp[, DIR_wpsm_temp_cor       := as.numeric(NA)  ]
            sun_temp[, GLB_SD_wpsm             := as.numeric(NA)  ]
            sun_temp[, GLB_strict              := as.numeric(NA)  ]
            sun_temp[, GLB_wpsm                := as.numeric(NA)  ]
            sun_temp[, GLBINC_SD_wpsm          := as.numeric(NA)  ]
            sun_temp[, GLBINC_strict           := as.numeric(NA)  ]
            sun_temp[, GLBINC_wpsm             := as.numeric(NA)  ]
            sun_temp[, HOR_SD_wpsm             := as.numeric(NA)  ]
            sun_temp[, HOR_strict              := as.numeric(NA)  ]
            sun_temp[, HOR_wpsm                := as.numeric(NA)  ]
            sun_temp[, HOR_wpsm_temp_cor       := as.numeric(NA)  ]
            ## Computed
            sun_temp[, DIFF_strict             := as.numeric(NA)  ]
            sun_temp[, DiffuseFraction_kd      := as.numeric(NA)  ]
            sun_temp[, ClearnessIndex_kt       := as.numeric(NA)  ] ## replaced with transmittance
            sun_temp[, Transmittance_GLB       := as.numeric(NA)  ]
            ## Sun
            sun_temp[, Sun_Dist_Astropy        := as.numeric(NA)  ]
            sun_temp[, TSI_TOA                 := as.numeric(NA)  ]
            sun_temp[, TSI_1au                 := as.numeric(NA)  ]
            sun_temp[, TSI_source              := as.character(NA)]
            ## Pressure
            sun_temp[, Pressure                := as.numeric(NA)  ]
            sun_temp[, Pressure_source         := as.character(NA)]
            ## QCRad
            sun_temp[, QCv9_01_dir_flag        := as.character(NA)]
            sun_temp[, QCv9_01_glb_flag        := as.character(NA)]
            sun_temp[, QCv9_02_dir_flag        := as.character(NA)]
            sun_temp[, QCv9_02_glb_flag        := as.character(NA)]
            sun_temp[, QCv9_03_upp_flag        := as.character(NA)]
            sun_temp[, QCv9_03_low_flag        := as.character(NA)]
            sun_temp[, QCv9_03_obs_flag        := as.character(NA)]
            sun_temp[, QCv9_04_dir_flag        := as.character(NA)]
            sun_temp[, QCv9_04_glb_flag        := as.character(NA)]
            sun_temp[, QCv9_05_dir_flag        := as.character(NA)]
            sun_temp[, QCv9_06_bth_flag        := as.character(NA)]
            sun_temp[, QCv9_08_bth_flag        := as.character(NA)]
            sun_temp[, QCv9_09_glb_flag        := as.character(NA)]
            sun_temp[, QCv9_10_all_flag        := as.character(NA)]

            ## Gather data
            if (nrow(gather) == 0) {
                ## This inits the database table!!
                gather     <- as_tibble(sun_temp)
            } else {
                gather     <- rows_upsert(gather, sun_temp, by = "Date")
            }
            gathermeta <- rbind(gathermeta, sun_meta)
            rm(sun_temp, sun_meta, ss)
        }

        BB_meta <- rows_update(BB_meta, gathermeta, by = "day")

    }
}



tac <- Sys.time()
cat(sprintf("%s %s@%s %s %f mins\n\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")))
cat(sprintf("\n%s %s@%s %s %f mins\n",Sys.time(),Sys.info()["login"],Sys.info()["nodename"],Script.Name,difftime(tac,tic,units="mins")),
    file = "~/BBand_LAP/REPORTS/LOGs/Run.log", append = TRUE)
