#' View tables from a MatSamp .db file
#'
#' Opens a file chooser for a local `.db` file (unless `db_path` is supplied),
#' then reads and returns table data from that database.
#'
#' @param db_path Optional path to a `.db` file. If `NULL`, `file.choose()` is used.
#' @param table Optional table name. If provided, returns only that table.
#'
#' @return A data frame when `table` is provided, otherwise a named list of
#'   data frames for all tables in the database.
#' @import DBI
#' @import RSQLite
#' @export
mat.tab <- function(db_path = NULL, table = NULL) {
  if (is.null(db_path)) {
    db_path <- file.choose()
  }

  if (!file.exists(db_path)) {
    stop("Database file not found: ", db_path, call. = FALSE)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- DBI::dbListTables(con)

  if (length(tables) == 0) {
    stop("No tables found in database: ", db_path, call. = FALSE)
  }

  if (!is.null(table)) {
    if (!table %in% tables) {
      stop("Table not found: ", table, call. = FALSE)
    }
    return(DBI::dbReadTable(con, table))
  }

  out <- stats::setNames(lapply(tables, function(tbl) DBI::dbReadTable(con, tbl)), tables)
  out
}
