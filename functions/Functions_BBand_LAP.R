

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
    } else {
        warning(paste0("Variable exist: ", varname, "\n", " !! IGNORING VARIABLE INIT !!"))
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



#' Create and init a new column/variable in the Broad Band metadata dataset.
#'
#' @param varname  The name of the new column to create.
#' @param vartype  The data type use to fill the new column.
#'
#' @return         Nothing. It edit the dataset in place and writes to disk.
#' @export
#'
#' @examples       InitVariableBBmeta("new_varile_name", as.character(NA))
#'
InitVariableBBmeta <- function(varname, vartype) {
    BB_meta   <- read_parquet(DB_META_fl)
    if (!is.character(varname)) stop()
    if (is.null(vartype)) stop()

    if (!any(names(BB_meta) == varname)) {
        cat("Create column: ", varname, "\n")
        BB_meta <- BB_meta |> mutate( !!varname := vartype) |> compute()
        write_parquet(BB_meta, DB_META_fl)
        cat("Metadata saved to file")
    } else {
        warning(paste0("Variable exist: ", varname, "\n", " !! IGNORING VARIABLE INIT !!"))
    }
}




## BB DB Dictionary ----------
dict_BB_DB <- list(
# key                   sort name                      long name                                             description                                           data type
Date                  = c("Date",                      "Date and Time",                                      "Record time stamp at middle of minute",              "POSIXt numeric"),
Azimuth               = c("Azim.",                     "Sun Azimuth Angle",                                  "Sun Azimuth Angle",                                  "float"),
Elevat                = c("Elevat.",                   "Sun Elevation Angle",                                "Sun Elevation Angle",                                "float"),
SZA                   = c("SZA",                       "Sun Zenith Angle",                                   "Sun Zenith Angle",                                   "float"),
year                  = c("Year",                      "Year",                                               "Year as number",                                     "integer"),
month                 = c("Month",                     "Month",                                              "Month as number",                                    "integer"),
doy                   = c("DOY",                       "Day of the year",                                    "Day of the year",                                    "integer"),
preNoon               = c("PreNoon",                   "Pre Noon flag",                                      "Pre Noon flag",                                      "logical"),
CM21_sig              = c("CM-21 sig.",                "CM-21 signal",                                       "Recorded signal from CM-21",                         "float"),
CM21_sig_sd           = c("CM-21 sig. SD",             "CM-21 signal Standard Deviation",                    "CM-21 signal Standard Deviation",                    "float"),
CM21_sig_wo_dark      = c("CM-21 sig dark cor.",       "CM-21 signal with dark correction",                  "CM-21 signal with dark correction",                  "float"),
cm21_bad_data_flag    = c("CM-21 bad data flag",       "CM-21 bad data flag",                                "CM-21 bad data flag",                                "logical"),
CM21INC_sig           = c("Inc. CM-21 sig.",           "Inclined CM-21 signal",                              "Recorded signal from Inclined CM-21",                "float"),
CM21INC_sig_sd        = c("Inc. CM-21 sig. SD",        "Inclined CM-21 signal SD",                           "Inclined CM-21 signal SD",                           "float"),
CM21INC_sig_wo_dark   = c("Inc. CM-21 sig. dark cor.", "Inclined CM-21 signal dark cor.",                    "Inclined CM-21 signal dark cor.",                    "float"),
cm21INC_bad_data_flag = c("Inc. CM-21 bad data flag",  "Inclined CM-21 bad data flag",                       "Inclined CM-21 bad data flag",                       "logical"),
Async_step_count      = c("Async. count",              "Tracker Async. count",                               "CHP-1 Tracker Async. count",                         "integer"),
Async_tracker_flag    = c("Async. flag",               "Tracker Async. flag",                                "CHP-1 Tracker Async. flag",                          "logical"),
CHP1_sig              = c("CHP-1 sig.",                "CHP-1 signal",                                       "Recorded signal from CHP-1",                         "float"),
CHP1_sig_sd           = c("CHP-1 sig. SD",             "CHP-1 signal Standard Deviation",                    "CHP-1 signal Standard Deviation",                    "float"),
CHP1_sig_wo_dark      = c("CHP-1 sig dark cor.",       "CHP-1 signal with dark correction",                  "CHP-1 signal with dark correction",                  "float"),
chp1_R_SD_therm       = c("CHP-1 R SD",                "CHP-1 Thermistor Resistance SD",                     "CHP-1 Thermistor Resistance SD",                     "float"),
chp1_R_meas_ERR       = c("CHP-1 R er",                "CHP-1 Thermistor Resistance error",                  "CHP-1 Thermistor Resistance measurement error",      "float"),
chp1_R_therm          = c("CHP-1 R",                   "CHP-1 Thermistor Resistance",                        "CHP-1 Thermistor Resistance",                        "float"),
chp1_bad_data_flag    = c("CHP-1 bad data flag",       "CHP-1 bad data flag",                                "CHP-1 bad data flag",                                "logical"),
chp1_bad_temp_flag    = c("CHP-1 Temp. bad data flag", "CHP-1 Temperature bad data flag",                    "CHP-1 Temperature bad data flag",                    "logical"),
chp1_t_cor_factor     = c("CHP-1 Temp. cor.",          "CHP-1 Temp. correction factor",                      "CHP-1 Temp. correction factor",                      "float"),
chp1_temp_UNC         = c("CHP-1 Temp. unc.",          "CHP-1 Temp. uncertainty",                            "CHP-1 Temperature uncertainty",                      "float"),
chp1_temperature      = c("CHP-1 Temp.",               "CHP-1 Temperature",                                  "CHP-1 Instrument Temperature",                       "float"),
chp1_temperature_SD   = c("CHP-1 Temp. SD",            "CHP-1 Temp. SD",                                     "CHP-1 Instrument Temperature Standard Deviation",    "float"),
tot_glb               = c("GHI Sirena",                "GHI from Sirena",                                    "GHI from Sirena",                                    "float"),
tot_glb_sd            = c("GHI Sirena SD",             "GHI from Sirena SD",                                 "GHI from Sirena SD",                                 "float"),
lap_sza               = c("SZA Sirena",                "SZA from Sirena",                                    "SZA from Sirena",                                    "float"),
DIR_SD_wpsm           = c("DNI SD",                    "Direct Normal Irradiance SD",                        "Direct Normal Irradiance SD",                        "float"),
DIR_strict            = c("DNI QCRad",                 "Direct Normal Irradiance for QCRad",                 "Direct Normal Irradiance for QCRad",                 "float"),
DIR_wpsm              = c("DNI",                       "Direct Normal Irradiance",                           "Direct Normal Irradiance",                           "float"),
DIR_wpsm_temp_cor     = c("DNI temp. cor.",            "Direct Normal Irradiance Temperature corrected",     "Direct Normal Irradiance Temperature corrected",     "float"),
GLB_SD_wpsm           = c("GHI SD",                    "Global Horizontal Irradiance SD",                    "Global Horizontal Irradiance SD",                    "float"),
GLB_strict            = c("GHI QCRad",                 "Global Horizontal Irradiance QCRad",                 "Global Horizontal Irradiance for QCRad",             "float"),
GLB_wpsm              = c("GHI",                       "Global Horizontal Irradiance",                       "Global Horizontal Irradiance",                       "float"),
GLBINC_SD_wpsm        = c("Inc. GHI SD",               NA,                                                   NA,                                                   NA),
GLBINC_strict         = c("Inc. GHI QCRad",            NA,                                                   NA,                                                   NA),
GLBINC_wpsm           = c("Inc. GHI",                  NA,                                                   NA,                                                   NA),
HOR_SD_wpsm           = c("DHI SD",                    "Direct Horizontal Irradiance SD",                    "Direct Horizontal Irradiance SD",                    "float"),
HOR_strict            = c("DHI QCRad",                 "Direct Horizontal Irradiance for QCRad",             "Direct Horizontal Irradiance for QCRad",             "float"),
HOR_wpsm              = c("DHI",                       "Direct Horizontal Irradiance",                       "Direct Horizontal Irradiance",                       "float"),
HOR_wpsm_temp_cor     = c("DHI temp. cor.",            "Direct Horizontal Irradiance Temperature corrected", "Direct Horizontal Irradiance Temperature corrected", "float"),
DIFF_strict           = c("Diffuse Ir.",               "Diffuse Irradiance",                                 "Diffuse Irradiance",                                 "float"),
DiffuseFraction_kd    = c("Diff. Frac.",               "Diffuse Fraction",                                   "Diffuse Fraction kd",                                "float"),
ClearnessIndex_kt     = c("Clear. In.",                "Clearness Index",                                    "Clearness Index kt",                                 "float"),
Sun_Dist_Astropy      = c("Sun Dist.",                 "Sun Distance",                                       "Sun Distance from Thessaloniki",                     "float"),
TSI_TOA               = c("TSI at TOA",                "Total Solar Irradiance at TOA",                      "Total Solar Irradiance at Top Of Atmosphere",        "float"),
TSI_1au               = c("TSI at 1au",                "Total Solar Irradiance at 1au",                      "Total Solar Irradiance at 1au",                      "float"),
TSI_source            = c("TSI source",                "Total Solar Irradiance data source",                 "Total Solar Irradiance data source",                 "string"),
Pressure              = c("Atm. Pres.",                "Atmospheric Pressure",                               "Atmospheric Pressure at Instruments level",          "float"),
Pressure_source       = c("Atm. Pres. source",         "Atmospheric Pressure data source",                   "Atmospheric Pressure data source",                   "string"),
QCv9_01_dir_flag      = c("QC 01 Dir flag",            "QC 01 Dir flag",                                     "QC 01 Dir flag",                                     NA),
QCv9_01_glb_flag      = c("QC 01 Glb flag",            "QC 01 Glb flag",                                     "QC 01 Glb flag",                                     NA),
QCv9_02_dir_flag      = c("QC 02 Dir flag",            "QC 02 Dir flag",                                     "QC 02 Dir flag",                                     NA),
QCv9_02_glb_flag      = c("QC 02 Glb flag",            "QC 02 Glb flag",                                     "QC 02 Glb flag",                                     NA),
QCv9_03_upp_flag      = c("QC 03 Upp flag",            "QC 03 Upp flag",                                     "QC 03 Upp flag",                                     NA),
QCv9_03_low_flag      = c("QC 03 Low flag",            "QC 03 Low flag",                                     "QC 03 Low flag",                                     NA),
QCv9_04_dir_flag      = c("QC 04 Dir flag",            "QC 04 Dir flag",                                     "QC 04 Dir flag",                                     NA),
QCv9_04_glb_flag      = c("QC 04 Glb flag",            "QC 04 Glb flag",                                     "QC 04 Glb flag",                                     NA),
QCv9_05_dir_flag      = c("QC 05 Dir flag",            "QC 05 Dir flag",                                     "QC 05 Dir flag",                                     NA),
QCv9_06_bth_flag      = c("QC 06 Bth flag",            "QC 06 Bth flag",                                     "QC 06 Bth flag",                                     NA),
QCv9_08_bth_flag      = c("QC 08 Bth flag",            "QC 08 Bth flag",                                     "QC 08 Bth flag",                                     NA),
QCv9_09_glb_flag      = c("QC 09 Glb flag",            "QC 09 Glb flag",                                     "QC 09 Glb flag",                                     NA),
Glo_max_ref           = c("Max GHI ref.",              NA,                                                   NA,                                                   NA),
Direct_max            = c("Max DNI",                   NA,                                                   NA,                                                   NA),
Global_max            = c("Max GHI",                   NA,                                                   NA,                                                   NA),
Dir_First_Clim_lim    = c("Lim. DNI Clim.",            NA,                                                   NA,                                                   NA),
Dir_Secon_Clim_lim    = c("Lim. DNI Clim.",            NA,                                                   NA,                                                   NA),
Glo_First_Clim_lim    = c("Lim. GHI Clim.",            NA,                                                   NA,                                                   NA),
Glo_Secon_Clim_lim    = c("Lim. GHI Clim.",            NA,                                                   NA,                                                   NA),
ClrSW_ref2            = c("Max GHI ref. 2",            NA,                                                   NA,                                                   NA),
RaylDIFF              = c("Rayleigh Diffuse",          NA,                                                   NA,                                                   NA),
Relative_diffuse      = c("Relative Diffuse",          NA,                                                   NA,                                                   NA)
)



#' Get nice name for a variable in DV
#'
#' @param x    Name of a column
#' @param type What to return one of c("short", "long", "description", "data")
#'
#' @return     A string for a variable
#' @export
#'
tr_var <- function(x, type = "short") {
    require(stringr)
    types <- c("short", "long", "description", "data")

    if (!type %in% types) {
        cat("No such column:", type, "\n")
        return(NA)
    }

    ty <- which(type == types)
    cat(ty,"\n")
    res <- c()
    for (ax in x) {
        res <- c(res,
                 as.vector(
                     unlist(dict_BB_DB[str_detect(ax, names(dict_BB_DB))])[[ty]]
                 )
        )
    }
    return(res)
}

