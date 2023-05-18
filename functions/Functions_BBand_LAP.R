

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
    # key                     sort name                    long name                             description                             data type
    Date                  = c("Date",                      "Date and Time",                     "Record time stamp at middle of minute", "POSIXt numeric"),
    Azimuth               = c("Azim.",                     "Sun Azimuth Angle",                 "Sun Azimuth Angle",                     "float"),
    Elevat                = c("Elevat.",                   "Sun Elevation Angle",               "Sun Elevation Angle",                   "float"),
    SZA                   = c("SZA",                       "Sun Zenith Angle",                  "Sun Zenith Angle",                      "float"),
    year                  = c("Year",                      "Year",                              "Year as number",                        "integer"),
    month                 = c("Month",                     "Month",                             "Month as number",                       "integer"),
    doy                   = c("DOY",                       "Day of the year",                   "Day of the year",                       "integer"),
    preNoon               = c("PreNoon",                   "Pre Noon flag",                     "Pre Noon flag",                         "logical"),
    CM21_sig              = c("CM-21 sig.",                "CM-21 signal",                      "Recorded signal from CM-21",            "float"),
    CM21_sig_sd           = c("CM-21 sig. SD",             "CM-21 signal Standard Deviation",   "CM-21 signal Standard Deviation",       "float"),
    CM21_sig_wo_dark      = c("CM-21 sig dark cor.",       "CM-21 signal with dark correction", "CM-21 signal with dark correction",     "float"),
    cm21_bad_data_flag    = c("CM-21 bad data flag",       "CM-21 bad data flag",               "CM-21 bad data flag",                   "logical"),
    CM21INC_sig           = c("Inc. CM-21 sig.",           "Inclined CM-21 signal",             "Recorded signal from Inclined CM-21",   "float"),
    CM21INC_sig_sd        = c("Inc. CM-21 sig. SD",        "Inclined CM-21 signal SD",          "Inclined CM-21 signal SD",              "float"),
    CM21INC_sig_wo_dark   = c("Inc. CM-21 sig. dark cor.", "Inclined CM-21 signal dark cor.",   "Inclined CM-21 signal dark cor.",       "float"),
    cm21INC_bad_data_flag = c("Inc. CM-21 bad data flag",  "Inclined CM-21 bad data flag",      "Inclined CM-21 bad data flag",          "logical"),
    Async_step_count      = c("Async. count",              "Tracker Async. count",              "CHP-1 Tracker Async. count",            "integer"),
    Async_tracker_flag    = c("Async. flag",               "Tracker Async. flag",               "CHP-1 Tracker Async. flag",             "logical"),
    CHP1_sig              = c("CHP-1 sig.",                "CHP-1 signal",                      "Recorded signal from CHP-1",            "float"),
    CHP1_sig_sd           = c("CHP-1 sig. SD",             "CHP-1 signal Standard Deviation",   "CHP-1 signal Standard Deviation",       "float"),
    CHP1_sig_wo_dark      = c("CHP-1 sig dark cor.",       "CHP-1 signal with dark correction", "CHP-1 signal with dark correction",     "float"),
    chp1_R_SD_therm       = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    chp1_R_meas_ERR       = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    chp1_R_therm          = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    chp1_bad_data_flag    = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    chp1_bad_temp_flag    = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    chp1_t_cor_factor     = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    chp1_temp_UNC         = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    chp1_temperature      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    chp1_temperature_SD   = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    tot_glb               = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    tot_glb_sd            = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    lap_sza               = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    DIR_SD_wpsm           = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    DIR_strict            = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    DIR_wpsm              = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    DIR_wpsm_temp_cor     = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    GLB_SD_wpsm           = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    GLB_strict            = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    GLB_wpsm              = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    GLBINC_SD_wpsm        = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    GLBINC_strict         = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    GLBINC_wpsm           = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    HOR_SD_wpsm           = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    HOR_strict            = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    HOR_wpsm              = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    HOR_wpsm_temp_cor     = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    DIFF_strict           = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    DiffuseFraction_kd    = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    ClearnessIndex_kt     = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    Sun_Dist_Astropy      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    TSI_TOA               = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    TSI_1au               = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    TSI_source            = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    Pressure              = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    Pressure_source       = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_01_dir_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_01_glb_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_02_dir_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_02_glb_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_03_upp_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_03_low_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_04_dir_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_04_glb_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_05_dir_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_06_bth_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_08_bth_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    QCv9_09_glb_flag      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    Glo_max_ref           = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    Direct_max            = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    Global_max            = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    Dir_First_Clim_lim    = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    Dir_Secon_Clim_lim    = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    Glo_First_Clim_lim    = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    Glo_Secon_Clim_lim    = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    ClrSW_ref2            = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    RaylDIFF              = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
    Relative_diffuse      = c("not defined",               "not defined",                       "not deffined",                          "not deffined"),
NULL
)

