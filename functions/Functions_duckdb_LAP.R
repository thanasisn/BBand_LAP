

#' Create all columns for a table with default types
#'
#' @param con      Data base connection
#' @param new_data An R table used to add data in the data base
#' @param table    The name of the data base table to add
#'
#' @return         Nothing. It executes an DQL query
#' @export
#'
create_missing_columns <- function(con, new_data, table) {
  ## detect data types
  tt1 <- data.table(names = colnames(tbl(con, table)),
                    types = tbl(con, table) |> head(1) |> collect() |> sapply(class))
  dd1 <- data.table(names = colnames(new_data),
                    types = new_data |> head(1) |> collect() |> sapply(class))

  ## check columns names
  new_vars <- dd1[!names %in% tt1$names, ]

  if (sum(!is.na(new_vars)) > 0) {
    # cat("New", new_vars$names)

    for (i in 1:nrow(new_vars)) {

      ## translate data types to duckdb
      ctype <- switch(paste0(unlist(new_vars$types[i]), collapse = ""),
                      POSIXctPOSIXt = "TIMESTAMP_MS",    ## all dates except radiation date
                      numeric       = "DECIMAL(18, 14)", ## change default numeric values for all data
                                                         ## ~9999.99999999999999
                      unlist(new_vars$types[i]))

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


