



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



update_table <- function(con, new_data, table, matchvar) {
  ## detect data types
  tt1 <- data.table(names = colnames(tbl(con, table)),
                    types = tbl(con, table) |> head(1) |> collect() |> sapply(class))
  dd1 <- data.table(names = colnames(new_data),
                    types = new_data |> head(1) |> collect() |> sapply(class))

  if (!all(dd1$names %in% tt1$names)) {
    ## get new variables
    new_vars <- dd1[!names %in% tt1$names, ]
    # cat("New", new_vars$names)

    for (i in 1:nrow(new_vars)) {

      ## translate data types to duckdb
      ctype <- switch(paste0(unlist(new_vars$types[i]), collapse = ""),
                      POSIXctPOSIXt = "datetime",
                      unlist(new_vars$types[i]))

      cat("\nNEW VAR:", paste(new_vars[i, ]), "\n")

      ## create new columns with a query
      qq <- paste("ALTER TABLE", table,
                  "ADD COLUMN",  new_vars$names[i], ctype, "DEFAULT null")
      dbSendQuery(con, qq)
    }
  }

  res <- rows_update(x         = tbl(con, table),
                     y         = new_data,
                     by        = matchvar,
                     unmatched = "ignore",
                     in_place  = TRUE,
                     copy      = TRUE)
  return(res)
}



insert_table <- function(con,  new_data, table, matchvar) {
  ## detect data types
  tt1 <- data.table(names = colnames(tbl(con, table)),
                    types = tbl(con, table) |> head(1) |> collect() |> sapply(class))
  dd1 <- data.table(names = colnames(new_data),
                    types = new_data |> head(1) |> collect() |> sapply(class))

  if (!all(dd1$names %in% tt1$names)) {
    ## get new variables
    new_vars <- dd1[!names %in% tt1$names, ]
    # cat("New", new_vars$names)

    for (i in 1:nrow(new_vars)) {

      ## translate data types to duckdb
      ctype <- switch(paste0(unlist(new_vars$types[i]), collapse = ""),
                      POSIXctPOSIXt = "datetime",
                      unlist(new_vars$types[i]))

      cat("\nNEW VAR:", paste(new_vars[i, ]), "\n")

      ## create new columns with a query
      qq <- paste("ALTER TABLE", table,
                  "ADD COLUMN",  new_vars$names[i], ctype, "DEFAULT null")
      dbSendQuery(con, qq)
    }
  }

  res <- rows_insert(x        = tbl(con, table),
                     y        = new_data,
                     by       = matchvar,
                     conflict = "ignore",
                     in_place = TRUE,
                     copy     = TRUE)
  return(res)
}

