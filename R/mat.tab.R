#' View tables from a MatSamp .db file
#'
#' Opens a file chooser for a local `.db` file (unless `db_path` is supplied),
#' then reads and returns table data from that database. In interactive sessions,
#' tables can also be opened in the RStudio data viewer.
#'
#' @param db_path Optional path to a `.db` file. If `NULL`, `file.choose()` is used.
#' @param table Optional table name. If provided, returns only that table.
#' @param view Logical; if `TRUE`, open returned table(s) in the RStudio data viewer.
#'
#' @return A data frame when `table` is provided, otherwise a named list of
#'   data frames for all tables in the database.
#' @import DBI
#' @import RSQLite
#' @export
mat.tab <- function(db_path = NULL, table = NULL, view = interactive()) {
  view_table <- function(x, title) {
    if (!is.data.frame(x) || ncol(x) == 0) {
      warning("Skipping viewer for table with no displayable columns: ", title, call. = FALSE)
      return(invisible(FALSE))
    }

    tryCatch({
      utils::View(x, title = title)
      invisible(TRUE)
    }, error = function(e) {
      warning("Could not open viewer for table '", title, "': ", conditionMessage(e), call. = FALSE)
      invisible(FALSE)
    })
  }

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

    out <- DBI::dbReadTable(con, table)
    if (isTRUE(view) && interactive()) {
      view_table(out, table)
    }
    return(out)
  }

  out <- stats::setNames(lapply(tables, function(tbl) {
    x <- tryCatch(DBI::dbReadTable(con, tbl), error = function(e) NULL)
    if (is.null(x)) return(data.frame())
    as.data.frame(x, stringsAsFactors = FALSE)
  }), tables)

  if (isTRUE(view) && interactive()) {
    for (tbl in names(out)) {
      view_table(out[[tbl]], tbl)
    }
  }

  out
}
