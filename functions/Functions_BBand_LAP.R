

#' Open the dataset for BBand_LAP
#'
#' @details  This is unified method to load this project dataset
#'
#' @return   An arrow table
#' @export
#'
opendata <- function() {
    open_dataset(sources       = DB_DIR,
                 unify_schemas = TRUE,
                 hive_style    = FALSE,
                 partitioning  = c("year", "month"))
}




#' Write the dataset for BBand_LAP
#'
#' @details  This is unified method to write this project dataset
#'
#' @return   Nothing.
#' @export
#'
writedata <- function(.) {
    write_dataset(., path      = DB_DIR,
                  format       = "parquet",
                  partitioning = c("year", "month"),
                  hive_style   = FALSE)
    cat("Written dataset: ", DB_DIR, "\n")
}




#' Create and init a new column/variable in the Broad Band dataset.
#'
#' @param varname  The name of the new column to create.
#' @param vartype  The data type use to fill the new column.
#'
#' @return         Nothing. It edit the dataset in place and writes to disk.
#' @export
#'
#' @examples       InitVariableBBDB("new_varile_name", as.character(NA))
#'
InitVariableBBDB <- function(varname, vartype) {
    BB <- opendata()
    if (!is.character(varname)) stop()
    if (is.null(vartype)) stop()

    if (!any(names(BB) == varname)) {
        cat("Create column: ", varname, "\n")
        BB <- BB |> mutate( !!varname := vartype) |> compute()
        writedata(BB)
    }
    BB <- opendata()
}




#' Overwrite an existing variable in the dataset.
#'
#' @param varname  The name of the column, have to exist already
#' @param vartype  The data type of the column
#'
#' @return         Nothing. It edit the dataset in place and writes to disk.
#' @export
#'
OVERWRITEVariableBBDB <- function(varname, vartype) {
    BB <- opendata()
    if (!is.character(varname)) stop()
    if (is.null(vartype)) stop()

    if (any(names(BB) == varname)) {
        cat("Overwrite column: ", varname, "\n")
        BB <- BB |> mutate( !!varname := vartype) |> compute()
        writedata(BB)
    } else {
        stop("The column not exist to overwrite")
    }
    BB <- opendata()
}



## BB DB Dictionary ----------
dict_BB_DB <- list(
# key                   sort name                      long name                                             description                                        data type
Date                  = c("Date",                      "Date and Time",                                      "Record time stamp at middle of minute",           "POSIXt numeric"),
Azimuth               = c("Azim.",                     "Sun Azimuth Angle",                                  "Sun Azimuth Angle",                               "float"),
Elevat                = c("Elevat.",                   "Sun Elevation Angle",                                "Sun Elevation Angle",                             "float"),
SZA                   = c("SZA",                       "Sun Zenith Angle",                                   "Sun Zenith Angle",                                "float"),
year                  = c("Year",                      "Year",                                               "Year as number",                                  "integer"),
month                 = c("Month",                     "Month",                                              "Month as number",                                 "integer"),
doy                   = c("DOY",                       "Day of the year",                                    "Day of the year",                                 "integer"),
preNoon               = c("PreNoon",                   "Pre Noon flag",                                      "Pre Noon flag",                                   "logical"),
CM21_sig              = c("CM-21 sig.",                "CM-21 signal",                                       "Recorded signal from CM-21",                      "float"),
CM21_sig_sd           = c("CM-21 sig. SD",             "CM-21 signal Standard Deviation",                    "CM-21 signal Standard Deviation",                 "float"),
CM21_sig_wo_dark      = c("CM-21 sig dark cor.",       "CM-21 signal with dark correction",                  "CM-21 signal with dark correction",               "float"),
cm21_bad_data_flag    = c("CM-21 bad data flag",       "CM-21 bad data flag",                                "CM-21 bad data flag",                             "logical"),
CM21INC_sig           = c("Inc. CM-21 sig.",           "Inclined CM-21 signal",                              "Recorded signal from Inclined CM-21",             "float"),
CM21INC_sig_sd        = c("Inc. CM-21 sig. SD",        "Inclined CM-21 signal SD",                           "Inclined CM-21 signal SD",                        "float"),
CM21INC_sig_wo_dark   = c("Inc. CM-21 sig. dark cor.", "Inclined CM-21 signal dark cor.",                    "Inclined CM-21 signal dark cor.",                 "float"),
cm21INC_bad_data_flag = c("Inc. CM-21 bad data flag",  "Inclined CM-21 bad data flag",                       "Inclined CM-21 bad data flag",                    "logical"),
Async_step_count      = c("Async. count",              "Tracker Async. count",                               "CHP-1 Tracker Async. count",                      "integer"),
Async_tracker_flag    = c("Async. flag",               "Tracker Async. flag",                                "CHP-1 Tracker Async. flag",                       "logical"),
CHP1_sig              = c("CHP-1 sig.",                "CHP-1 signal",                                       "Recorded signal from CHP-1",                      "float"),
CHP1_sig_sd           = c("CHP-1 sig. SD",             "CHP-1 signal Standard Deviation",                    "CHP-1 signal Standard Deviation",                 "float"),
CHP1_sig_wo_dark      = c("CHP-1 sig dark cor.",       "CHP-1 signal with dark correction",                  "CHP-1 signal with dark correction",               "float"),
chp1_R_SD_therm       = c("CHP-1 R SD",                "CHP-1 Thermistor Resistance SD",                     "CHP-1 Thermistor Resistance SD",                  "float"),
chp1_R_meas_ERR       = c("CHP-1 R er",                "CHP-1 Thermistor Resistance error",                  "CHP-1 Thermistor Resistance measurement error",   "float"),
chp1_R_therm          = c("CHP-1 R",                   "CHP-1 Thermistor Resistance",                        "CHP-1 Thermistor Resistance",                     "float"),
chp1_bad_data_flag    = c("CHP-1 bad data flag",       "CHP-1 bad data flag",                                "CHP-1 bad data flag",                             "logical"),
chp1_bad_temp_flag    = c("CHP-1 Temp. bad data flag", "CHP-1 Temperature bad data flag",                    "CHP-1 Temperature bad data flag",                 "logical"),
chp1_t_cor_factor     = c("CHP-1 Temp. cor.",          "CHP-1 Temp. correction factor",                      "CHP-1 Temp. correction factor",                   "float"),
chp1_temp_UNC         = c("CHP-1 Temp. unc.",          "CHP-1 Temp. uncertainty",                            "CHP-1 Temperature uncertainty",                   "float"),
chp1_temperature      = c("CHP-1 Temp.",               "CHP-1 Temperature",                                  "CHP-1 Instrument Temperature",                    "float"),
chp1_temperature_SD   = c("CHP-1 Temp. SD",            "CHP-1 Temp. SD",                                     "CHP-1 Instrument Temperature Standard Deviation", "float"),
tot_glb               = c("GHI Sirena",                "GHI from Sirena",                                    "GHI from Sirena",                                 "float"),
tot_glb_sd            = c("GHI Sirena SD",             "GHI from Sirena SD",                                 "GHI from Sirena SD",                              "float"),
lap_sza               = c("SZA Sirena",                "SZA from Sirena",                                    "SZA from Sirena",                                 "float"),
DIR_SD_wpsm           = c("DNI SD",                    "Direct Normal Irradiance SD",                        "Direct Normal Irradiance SD",                     "float"),
DIR_strict            = c("DNI QCRad",                 "Direct Normal Irradiance for QCRad",                 "Direct Normal Irradiance for QCRad",              "float"),
DIR_wpsm              = c("DNI",                       "Direct Normal Irradiance",                           "Direct Normal Irradiance",                        "float"),
DIR_wpsm_temp_cor     = c("DNI temp. cor.",            "Direct Normal Irradiance Temperature corrected",     "Direct Normal Irradiance Temperature corrected",  "float"),
GLB_SD_wpsm           = c("GHI SD",                    "Global Horizontal Irradiance SD",                    "Global Horizontal Irradiance SD",                 "float"),
GLB_strict            = c("GHI QCRad",                 "Global Horizontal Irradiance QCRad",                 "Global Horizontal Irradiance for QCRad",          "float"),
GLB_wpsm              = c("GHI",                       "Global Horizontal Irradiance",                       "Global Horizontal Irradiance",                    "float"),
GLBINC_SD_wpsm        = c("Inc. GHI SD",               "not deffined",                                       "not defffined",                                "not deffffined"),
GLBINC_strict         = c("Inc. GHI QCRad",            "not deffined",                                       "not defffined",                                "not deffffined"),
GLBINC_wpsm           = c("Inc. GHI",                  "not deffined",                                       "not defffined",                                "not deffffined"),
HOR_SD_wpsm           = c("DHI SD",                    "Direct Horizontal Irradiance SD",                    "Direct Horizontal Irradiance SD",                    "float"),
HOR_strict            = c("DHI QCRad",                 "Direct Horizontal Irradiance for QCRad",             "Direct Horizontal Irradiance for QCRad",             "float"),
HOR_wpsm              = c("DHI",                       "Direct Horizontal Irradiance",                       "Direct Horizontal Irradiance",                       "float"),
HOR_wpsm_temp_cor     = c("DHI temp. cor.",            "Direct Horizontal Irradiance Temperature corrected", "Direct Horizontal Irradiance Temperature corrected", "float"),
DIFF_strict           = c("Diffuse Ir.",               "Diffuse Irradiance",                                 "Diffuse Irradiance",                                 "float"),
DiffuseFraction_kd    = c("Diff. Frac.",               "Diffuse Fraction",                                   "Diffuse Fraction kd",                                "float"),
ClearnessIndex_kt     = c("Clear. In.",                "Clearness Index",                                    "Clearness Index kt",                                 "float"),
Sun_Dist_Astropy      = c("Sun Dist.",                 "not deffined",                                       "not defffined",                                "not deffffined"),
TSI_TOA               = c("TSI at TOA",                "not deffined",                                       "not defffined",                                "not deffffined"),
TSI_1au               = c("TSI at 1au",                "not deffined",                                       "not defffined",                                "not deffffined"),
TSI_source            = c("TSI source",                "not deffined",                                       "not defffined",                                "not deffffined"),
Pressure              = c("Atm. Pres.",                "not deffined",                                       "not defffined",                                "not deffffined"),
Pressure_source       = c("Atm. Pres. source",         "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_01_dir_flag      = c("QC 01 Dir flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_01_glb_flag      = c("QC 01 Glb flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_02_dir_flag      = c("QC 02 Dir flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_02_glb_flag      = c("QC 02 Glb flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_03_upp_flag      = c("QC 03 Upp flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_03_low_flag      = c("QC 03 Low flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_04_dir_flag      = c("QC 04 Dir flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_04_glb_flag      = c("QC 04 Glb flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_05_dir_flag      = c("QC 05 Dir flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_06_bth_flag      = c("QC 06 Bth flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_08_bth_flag      = c("QC 08 Bth flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
QCv9_09_glb_flag      = c("QC 09 Glb flag",            "not deffined",                                       "not defffined",                                "not deffffined"),
Glo_max_ref           = c("Max GHI ref.",              "not deffined",                                       "not defffined",                                "not deffffined"),
Direct_max            = c("Max DNI",                   "not deffined",                                       "not defffined",                                "not deffffined"),
Global_max            = c("Max GHI",                   "not deffined",                                       "not defffined",                                "not deffffined"),
Dir_First_Clim_lim    = c("Lim. DNI Clim.",            "not deffined",                                       "not defffined",                                "not deffffined"),
Dir_Secon_Clim_lim    = c("Lim. DNI Clim.",            "not deffined",                                       "not defffined",                                "not deffffined"),
Glo_First_Clim_lim    = c("Lim. GHI Clim.",            "not deffined",                                       "not defffined",                                "not deffffined"),
Glo_Secon_Clim_lim    = c("Lim. GHI Clim.",            "not deffined",                                       "not defffined",                                "not deffffined"),
ClrSW_ref2            = c("Max GHI ref. 2",            "not deffined",                                       "not defffined",                                "not deffffined"),
RaylDIFF              = c("Rayleigh Diffuse",          "not deffined",                                       "not defffined",                                "not deffffined"),
Relative_diffuse      = c("Relative Diffuse",          "not deffined",                                       "not defffined",                                "not deffffined"),
NULL
)

