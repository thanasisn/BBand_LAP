

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
      qq <- paste("ALTER TABLE", table,
                  "ADD COLUMN",  new_vars$names[i], ctype, "DEFAULT null")
      dbSendQuery(con, qq)
    }
  }
}



make_empty_column <- function(con, table, acolname, acoltype) {

  if (any(dbListFields(con, table) %in% acolname)) {
    qq <- paste0("ALTER TABLE ", table,
                 " DROP ",  acolname)
    res <- dbSendQuery(con, qq)
  }
  ## create new columns with a query
  qq <- paste("ALTER TABLE", table,
              "ADD COLUMN",  acolname,  acoltype, "DEFAULT null")
  res <- dbSendQuery(con, qq)
}



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
make_new_column <- function(con, table, acolname, acoltype) {

  if (any(dbListFields(con, table) %in% acolname)) {
    cat(" Column ", acolname, " already exist! >> Do nothing!! <<\n\n")
    return()
  } else {
    ## create new columns with a query
    qq <- paste("ALTER TABLE", table,
                "ADD COLUMN",  acolname,  acoltype, "DEFAULT null")
    res <- dbSendQuery(con, qq)
  }
}



remove_column <- function(con, table, acolname) {
  if (any(dbListFields(con, table) %in% acolname)) {
    qq <- paste0("ALTER TABLE ", table,
                 " DROP ",  acolname)
    res <- dbSendQuery(con, qq)
  } else {
    warning("No column to remove:", acolname)
  }
}

update_table <- function(con, new_data, table, matchvar) {

  create_missing_columns(con      = con,
                         new_data = new_data,
                         table    = table)

  res <- rows_update(x         = tbl(con, table),
                     y         = new_data,
                     by        = matchvar,
                     unmatched = "ignore",
                     in_place  = TRUE,
                     copy      = TRUE)
  return(res)
}



insert_table <- function(con,  new_data, table, matchvar) {

  create_missing_columns(con      = con,
                         new_data = new_data,
                         table    = table)

  res <- rows_insert(x        = tbl(con, table),
                     y        = new_data,
                     by       = matchvar,
                     conflict = "ignore",  ## only option for duckdb
                     in_place = TRUE,
                     copy     = TRUE)
  return(res)
}



upsert_table <- function(con,  new_data, table, matchvar) {

  create_missing_columns(con      = con,
                         new_data = new_data,
                         table    = table)

  res <- rows_upsert(x        = tbl(con, table),
                     y        = new_data,
                     by       = matchvar,
                     in_place = TRUE,
                     copy     = TRUE)
  return(res)
}



