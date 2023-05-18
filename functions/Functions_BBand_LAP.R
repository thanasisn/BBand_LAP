

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
    # key                   sort name         long name              description   type
    Date                  = c("Date",         "Date and Time",       "Record time stamp at middle of minute", "POSIXt numeric"),
    Azimuth               = c("Azim.",        "Sun Azimuth Angle",   "Sun Azimuth Angle",                    "float"),
    Elevat                = c("Elevat.",      "Sun Elevation Angle", "Sun Elevation Angle",                  "float"),
    SZA                   = c("SZA",          "Sun Zenith Angle",    "Sun Zenith Angle",                     "float"),
    year                  = c("Year",         "Year",                "Year as number",                       "integer"),
    month                 = c("Month",        "Month",               "Month as number",                      "integer"),
    doy                   = c("DOY",          "Day of the year",     "Day of the year",                      "integer"),
    preNoon               = c("PreNoon",      "Pre Noon flag",       "Pre Noon flag",                        "boolean"),
    CM21_sig              = c("CM-21 sig.", "CM-21 signal",        "not deffined",                         "not deffined"),
    CM21_sig_sd           = c("CM-21 deffined", "not deffined",        "not deffined",                         "not deffined"),
    CM21_sig_wo_dark      = c("CM-21 deffined", "not deffined",        "not deffined",                         "not deffined"),
    cm21_bad_data_flag    = c("CM-21 deffined", "not deffined",        "not deffined",                         "not deffined"),
    CM21INC_sig           = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    CM21INC_sig_sd        = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    CM21INC_sig_wo_dark   = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    cm21INC_bad_data_flag = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Async_step_count      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Async_tracker_flag    = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    CHP1_sig              = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    CHP1_sig_sd           = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    CHP1_sig_wo_dark      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    chp1_R_SD_therm       = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    chp1_R_meas_ERR       = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    chp1_R_therm          = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    chp1_bad_data_flag    = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    chp1_bad_temp_flag    = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    chp1_t_cor_factor     = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    chp1_temp_UNC         = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    chp1_temperature      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    chp1_temperature_SD   = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    tot_glb               = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    tot_glb_sd            = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    lap_sza               = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    DIR_SD_wpsm           = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    DIR_strict            = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    DIR_wpsm              = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    DIR_wpsm_temp_cor     = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    GLB_SD_wpsm           = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    GLB_strict            = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    GLB_wpsm              = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    GLBINC_SD_wpsm        = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    GLBINC_strict         = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    GLBINC_wpsm           = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    HOR_SD_wpsm           = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    HOR_strict            = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    HOR_wpsm              = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    HOR_wpsm_temp_cor     = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    DIFF_strict           = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    DiffuseFraction_kd    = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    ClearnessIndex_kt     = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Sun_Dist_Astropy      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    TSI_TOA               = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    TSI_1au               = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    TSI_source            = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Pressure              = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Pressure_source       = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_01_dir_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_01_glb_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_02_dir_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_02_glb_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_03_upp_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_03_low_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_04_dir_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_04_glb_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_05_dir_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_06_bth_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_08_bth_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    QCv9_09_glb_flag      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Glo_max_ref           = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Direct_max            = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Global_max            = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Dir_First_Clim_lim    = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Dir_Secon_Clim_lim    = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Glo_First_Clim_lim    = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Glo_Secon_Clim_lim    = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    ClrSW_ref2            = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    RaylDIFF              = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
    Relative_diffuse      = c("not deffined", "not deffined",        "not deffined",                         "not deffined"),
NULL
)

