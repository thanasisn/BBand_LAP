
require(dplyr)
require(lubridate)

#' Check if is an integer for R
#'
#' @param  x   A vector or column
#'
#' @return    `TRUE` when is integer
#' @export
#'
is_whole <- function(x) {
  if (!is.numeric(x)) return(FALSE)
  all(floor(x) == x)
}


#' Give an appropriate duckdb data type, for this project
#'
#' @param column  An R column or vector or scalar
#' @param rela    An expansion factor for the default ranges
#'
#' @return        A string of the duck data type
#' @export
#'
duckdb_datatypes <- function(column, rela = 1) {
  case_when(
    is.logical(column)                                                                                        ~         "BOOLEAN",
    is.character(column)                                                                                      ~         "VARCHAR",
    is.Date(column)                                                                                           ~            "DATE",
    is.POSIXt(column)                                                                                         ~    "TIMESTAMP_MS",
    is_whole(column) & all(column >=                   0 * rela) & all(column <=                65535 * rela) ~       "USMALLINT",
    is_whole(column) & all(column >=                   0 * rela) & all(column <=           4294967295 * rela) ~        "UINTEGER",
    is_whole(column) & all(column >=                   0 * rela) & all(column <= 18446744073709551615 * rela) ~         "UBIGINT",
    is_whole(column) & all(column >               -32768 * rela) & all(column <=                32767 * rela) ~        "SMALLINT",
    is_whole(column) & all(column >          -2147483648 * rela) & all(column <=           2147483647 * rela) ~         "INTEGER",
    is_whole(column) & all(column > -9223372036854775808 * rela) & all(column <=  9223372036854775807 * rela) ~          "BIGINT",
    is.numeric(column)                                                                                        ~ "DECIMAL(18, 14)",
    .default = "Have to include more data types"
  )
}



#' Create all columns for a table with default types
#'
#' @param con      Data base connection
#' @param new_data An R table used to add data in the data base
#' @param table    The name of the data base table to add
#' @details
#' Hopefully the table used to initialize the new columns have the appropriate
#' values to infer the correct data type for  duckdb
#'
#'
#' @return         Nothing. It executes an SQL query
#' @export
#'
create_missing_columns <- function(con, new_data, table, quiet = FALSE) {
  ## detect data types
  tt1 <- data.table(names = colnames(tbl(con, table)),
                    types = tbl(con, table) |> head(1) |> collect() |> sapply(class))
  dd1 <- data.table(names = colnames(new_data),
                    types = new_data |> head(1) |> collect() |> sapply(class))

  if (!quiet) {
    if (is.data.frame(new_data) | is.data.table(new_data)) {
      cat("Is R data\n")
    } else {
      cat("Is NOT R data\n")
    }
  }

  ## check columns names
  new_vars <- dd1[!names %in% tt1$names, ]

  if (sum(!is.na(new_vars)) > 0) {
    # cat("New", new_vars$names)

    for (i in 1:nrow(new_vars)) {

      if (is.data.frame(new_data) | is.data.table(new_data)) {
        ## a new more general approach
        ctype <- duckdb_datatypes(new_data[[new_vars$names[i]]])
      } else {
        # ## translate data types to duckdb
        ctype <- switch(paste0(unlist(new_vars$types[i]), collapse = ""),
                        POSIXctPOSIXt = "TIMESTAMP_MS",    ## all dates except radiation date
                        numeric       = "DECIMAL(18, 14)", ## change default numeric values for all data
                        ## ~9999.99999999999999
                        unlist(new_vars$types[i]))
      }

      ## info
      cat("\nNEW VAR:", paste(new_vars[i, ]), "->", ctype, "\n")

      ## create new columns with a query
      qq <- paste0("ALTER TABLE  ",  table,
                   "  ADD COLUMN  ", new_vars$names[i],
                   "  ",             ctype,
                   "  DEFAULT null")
      cat(qq, "\n\n")
      res <- dbSendQuery(con, qq)
    }
  }
}







#' Create a null column eve if exists
#'
#' @param con      A connection to duckdb
#' @param table    The name of the table to create column in
#' @param acolname New column name
#' @param acoltype Data type of duckdb
#'
#' @return  Nothing. It executes duckdb commands directly
#' @export
#'
make_null_column <- function(con, table, acolname, acoltype = "DECIMAL(18, 14)") {

  if (any(dbListFields(con, table) %in% acolname)) {
    qq <- paste0("ALTER TABLE  ",  table,
                 "  DROP        ", acolname)
    cat(qq, "\n")
    res <- dbSendQuery(con, qq)
  }
  ## create new columns with a query
  qq <- paste0("ALTER TABLE  ",   table,
               "  ADD COLUMN  ",  acolname,
               "  ",              acoltype,
               "  DEFAULT null")
  cat(qq, "\n\n")
  res <- dbSendQuery(con, qq)
}



#'
#' Create a new column with a data type in a duckdb table
#'
#' @param con      Connection to the database
#' @param table    Name of the table in the database
#' @param acolname Name of the new column
#' @param acoltype Database data type for the new column
#'
#' @details
#' Create a column or do nothing if already exist
#'
#' @return Nothing create the column with an SQL query
#' @export
#'
make_new_column <- function(con, table, acolname, acoltype = "DECIMAL(18, 14)") {

  if (any(dbListFields(con, table) %in% acolname)) {
    cat(" Column ", acolname, " already exist! >> No new column to be created!! <<\n\n")
    return()
  } else {
    ## create new columns with a query
    qq <- paste0("ALTER TABLE  ",   table,
                 "  ADD COLUMN  ",  acolname,
                 "  ",              acoltype,
                 "  DEFAULT null")
    cat(qq, "\n\n")
    res <- dbSendQuery(con, qq)
  }
}



#' Create enum type and column for a vector of factors
#'
#' @param flagname   The name of the enum categories and the new column
#' @param categories A vector of strings with the categories
#' @param con        A data base connection
#' @param table      The name of the table in the data base
#'
#' @return           Nothing, it creates an categorical column in the data base
#' @export
#'
make_categorical_column <- function(flagname, categories, con, table) {

  if (any(dbListFields(con, table) %in% flagname)) {
    cat(" Column ", flagname, " already exist! >> No new column to be created!! <<\n\n")
    return()
  } else {

    ## remove type anyway
    try({
      qq <- paste0("DROP TYPE ", flagname, ";")
      cat(qq, "\n")
      res <- dbSendQuery(con, qq)
    })

    ## create "enum" type
    qq <- paste0("CREATE TYPE  ", flagname,
                 "  AS ENUM  (",  paste(paste0("'", categories, "'"), collapse = ", " ),
                 ");")
    cat(qq, "\n")
    res <- dbSendQuery(con, qq)

    ## add a column for the "enum" type
    qq <- paste0("ALTER TABLE  ",   table,
                 "  ADD COLUMN  ",  flagname,
                 "  ",              flagname,
                 "  DEFAULT 'empty'")
    cat(qq, "\n\n")
    res <- dbSendQuery(con, qq)
  }
}



#' Drop a column from a table
#'
#' @param con      Connection to the database
#' @param table    Name of the table in the database
#' @param acolname Name of the new column to delete
#'
#' @return Nothing, it drops the column with an SQL query
#' @export
#'
remove_column <- function(con, table, acolname) {
  if (any(dbListFields(con, table) %in% acolname)) {
    qq <- paste0("ALTER TABLE  ",  table,
                 "  DROP        ", acolname)
    cat(qq, "\n\n")
    res <- dbSendQuery(con, qq)
  } else {
    warning("No column to remove:", acolname)
  }
}



#'
#' Modifies existing rows. Key values in `new_data` must be unique, and,
#' by default, key values in `new_data` must exist in `con`.
#'
#' @param con      A connection to a duckdb
#' @param new_data A table of data to update
#' @param table    The name of the database table to update
#' @param matchvar The name of the key variable to match
#'
#' @return         A tibble and an in place mutation of the data base
#' @export
#'
update_table <- function(con, new_data, table, matchvar, quiet = FALSE) {

  create_missing_columns(con      = con,
                         new_data = new_data,
                         table    = table)

  res <- rows_update(x         = tbl(con, table),
                     y         = new_data,
                     by        = matchvar,
                     unmatched = "ignore",
                     in_place  = TRUE,
                     copy      = TRUE)

  if (!quiet) {
    nm <- deparse(substitute(new_data))
    cat(paste("Updated >>", table, "<< with >>", nm, "<<\n\n"))
  }

  return(res)
}



#'
#' Adds new rows. By default, key values in `new_data` must not exist in `con`.
#'
#' @param con      A connection to a duckdb
#' @param new_data A table of data to update
#' @param table    The name of the database table to update
#' @param matchvar The name of the key variable to match
#'
#' @return         A tibble and an in place mutation of the data base
#' @export
#'
insert_table <- function(con, new_data, table, matchvar, quiet = FALSE) {

  create_missing_columns(con      = con,
                         new_data = new_data,
                         table    = table)

  res <- rows_insert(x        = tbl(con, table),
                     y        = new_data,
                     by       = matchvar,
                     conflict = "ignore",  ## only option for duckdb
                     in_place = TRUE,
                     copy     = TRUE)

  if (!quiet) {
    nm <- deparse(substitute(new_data))
    cat(paste("Inserted >>", table, "<< with >>", nm, "<<\n\n"))
  }

  return(res)
}



#'
#' inserts or updates depending on whether or not the key value in `new_data`
#' already exists in `con`. Key values in `new_data` must be unique.
#'
#' @param con      A connection to a duckdb
#' @param new_data A table of data to update
#' @param table    The name of the database table to update
#' @param matchvar The name of the key variable to match
#'
#' @return         A tibble and an in place mutation of the data base
#' @export
#'
upsert_table <- function(con, new_data, table, matchvar, quiet = FALSE) {

  create_missing_columns(con      = con,
                         new_data = new_data,
                         table    = table)

  res <- rows_upsert(x        = tbl(con, table),
                     y        = new_data,
                     by       = matchvar,
                     in_place = TRUE,
                     copy     = TRUE)

  if (!quiet) {
    nm <- deparse(substitute(new_data))
    cat(paste("Inserted >>", table, "<< with >>", nm, "<<\n\n"))
  }

  return(res)
}



#' Helper function for scripts end
#'
#' @param logfile    The global file for output
#' @param ScriptName The current script name
#' @param .tic       Starting datetime
#'
#' @return           Outputs stings on the terminal and/or file
#' @export
#'
goodbye <- function(
    logfile     = "~/BBand_LAP/REPORTS/LOGs/Run.log",
    ScriptName  = Script.Name,
    .tic        = tic
) {
  .tac <- Sys.time()
  ## output for knitr and terminal
  cat(
    sprintf("\n**END** %s %s@%s %s %f mins\n\n",
            Sys.time(), Sys.info()["login"], Sys.info()["nodename"], ScriptName, difftime(.tac, .tic, units = "mins"))
    )
  ## logging to master log file
  if (!interactive()) {
    cat(
      sprintf("%s %s@%s %s %f mins\n",
              Sys.time(), Sys.info()["login"], Sys.info()["nodename"], ScriptName, difftime(.tac, .tic, units = "mins")),
      file = logfile, append = TRUE)
  }
}


#' Output a simple message to monitor progress
#'
#' @param ScriptName  The name of the running script
#' @param timezone    Timezone to use
#' @param output      Target file
#' @param msg         The message to display
#' @details
#' Use `tail -F /dev/shm/BBand_LAP.status` to display the status
#'
#' @return
#' @export
#'
#' @examples
status_msg <- function(
    ScriptName = Script.Name,
    timezone   = "Europe/Athens",
    output     = "/dev/shm/BBand_LAP.status",
    msg        = "")
{
  cat(
    format(Sys.time(), tz = "Europe/Athens"),
    ScriptName,
    msg,
    "                    \r", ## white space padding
    sep = "::",
    file = output,
    append = TRUE
  )
}

## test data types  ----------------------------------------------
if (FALSE) {

  test <- data.frame(c(-10,2147483648.1),
                     c(  0,      214741),
                     c(  1, 9223372036854775807),
                     c(as.POSIXct("1970-01-01")),
                     c("ddd", "ddddddd"),
                     c(F, T),
                     as.Date("1990-09-09")
  )

  for (ac in colnames(test)) {
    cat(ac, duckdb_datatypes( test[[ac]] ),"\n")
  }


  helper <- function(c=1, ...) {
    if (!exists(quiet)) quiet = T
    cat("Q1", quiet,"\n")
  }

  important <- function(a, b =2, quiet = F){
    environment(helper) <- environment()
    cat("Q2", quiet,"\n")

    # return(list(...))
  }


  important(1, quiet = F)
  helper()
  helper(quiet = T)

}

