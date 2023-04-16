

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
    cat("Data set written at", DB_DIR, "\n")
}




#' Create and init a new column/variable in the Broad Band dataset.
#'
#' @param varname  The name of the new column to create
#' @param vartype  The data type use to fill the new column.
#'
#' @return         Nothing. It edit the dataset in place
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
