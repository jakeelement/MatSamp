#' Launch the lab ovary sample data entry app
#'
#' Starts a Shiny app for entering lab ovary data against an existing at-sea
#' trip database. Load the `.db` file created by \code{\link{at.sea}} first;
#' the lobster IDs from that trip's SAMPLE table will populate the form
#' automatically.
#'
#' @return A Shiny app launched in the current R session.
#' @import shiny
#' @import DBI
#' @import RSQLite
#' @export
ovary <- function() {

  # --- UI helper ------------------------------------------------------------

  ovary_row_ui <- function(i, lobster_id = "", length_val = NA, whole_w = NA,
                           gastrolith = "", colour = "", yellow = "",
                           weight = NA, image = "", observer_val = "") {
    shiny::fluidRow(
      shiny::column(width = 2, htmltools::tagAppendAttributes(
        shiny::textInput(paste0("ov_lobster_id_", i), if (i == 1) "LOBSTER ID" else NULL, value = lobster_id),
        .cssSelector = "input", readonly = "readonly")),
      shiny::column(width = 1, shiny::numericInput(paste0("ov_length_",    i), if (i == 1) "LENGTH"                  else NULL, value = length_val, min = 0)),
      shiny::column(width = 1, shiny::numericInput(paste0("ov_whole_w_",   i), if (i == 1) "Whole W."                else NULL, value = whole_w,    min = 0)),
      shiny::column(width = 1, shiny::textInput(   paste0("ov_gastrolith_",i), if (i == 1) "GASTROLITH"              else NULL, value = gastrolith)),
      shiny::column(width = 1, shiny::textInput(   paste0("ov_colour_",    i), if (i == 1) "OVARY COLOUR"            else NULL, value = colour)),
      shiny::column(width = 1, shiny::textInput(   paste0("ov_yellow_",    i), if (i == 1) "YELLOW SPOTS"            else NULL, value = yellow)),
      shiny::column(width = 1, shiny::numericInput(paste0("ov_weight_",    i), if (i == 1) "OVARY WEIGHT"            else NULL, value = weight,     min = 0)),
      shiny::column(width = 3, shiny::textInput(   paste0("ov_image_",     i), if (i == 1) "IMAGE NAME(S)/COMMENTS"  else NULL, value = image)),
      shiny::column(width = 2, shiny::textInput(   paste0("ov_observer_",  i), if (i == 1) "OBSERVER"                else NULL, value = observer_val))
    )
  }

  # --- DB helpers -----------------------------------------------------------

  read_ovary_db <- function(db_path) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    if (!DBI::dbExistsTable(con, "TRIP"))   stop("TRIP table not found — load an at-sea .db file first")
    if (!DBI::dbExistsTable(con, "SAMPLE")) stop("SAMPLE table not found — load an at-sea .db file first")

    trip_df <- DBI::dbGetQuery(con, "SELECT TRIPID, ORG, TRIP_DATE, PORT, LFA, SAMPLER FROM TRIP ORDER BY TRIPID")
    if (nrow(trip_df) == 0) stop("No TRIP records found")
    if (nrow(trip_df) > 1)  stop("Multiple TRIPIDs found; expected one trip per .db file")
    trip_id <- trip_df$TRIPID[1]

    sample_df <- DBI::dbGetQuery(con,
      "SELECT LOBSTER_ID FROM SAMPLE WHERE TRIPID = ? ORDER BY LOBSTER_ID",
      params = list(trip_id))

    ovary_df <- if (DBI::dbExistsTable(con, "LAB_OVARY")) {
      DBI::dbGetQuery(con,
        "SELECT * FROM LAB_OVARY WHERE TRIPID = ? ORDER BY ROW_NO",
        params = list(trip_id))
    } else data.frame()

    list(
      trip = data.frame(trip_id = trip_df$TRIPID, org = trip_df$ORG, date = trip_df$TRIP_DATE,
                        port = trip_df$PORT, lfa = trip_df$LFA, sampler = trip_df$SAMPLER,
                        stringsAsFactors = FALSE),
      lobster_ids = sample_df$LOBSTER_ID,
      ovary       = ovary_df
    )
  }

  write_ovary_db <- function(db_path, trip_id, ovary_df) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS LAB_OVARY (
        TRIPID TEXT NOT NULL, LAB_COUNT TEXT, LAB_DATE TEXT, ROW_NO INTEGER NOT NULL,
        LOBSTER_ID TEXT, LENGTH REAL, WHOLE_W REAL, GASTROLITH TEXT, OVARY_COLOUR TEXT,
        YELLOW_SPOTS TEXT, OVARY_WEIGHT REAL, IMAGE_COMMENTS TEXT, OBSERVER TEXT,
        PRIMARY KEY (TRIPID, ROW_NO),
        FOREIGN KEY (TRIPID, LOBSTER_ID) REFERENCES SAMPLE(TRIPID, LOBSTER_ID)
      )")
    DBI::dbExecute(con, "DELETE FROM LAB_OVARY WHERE TRIPID = ?", params = list(trip_id))
    if (nrow(ovary_df) > 0)
      DBI::dbWriteTable(con, "LAB_OVARY", ovary_df, append = TRUE, row.names = FALSE)
  }

  # --- UI -------------------------------------------------------------------

  ui <- shiny::fluidPage(
    mat_js_tags(),
    shiny::titlePanel("CLRN SOM50 LAB OVARY SAMPLE DATA FORM"),
    shiny::h4("Load Existing Trip"),
    shiny::actionButton("load_db_file", "Choose .db File", class = "btn-secondary"),
    shiny::tags$hr(),
    shiny::h3("LAB INFORMATION"),
    shiny::fluidRow(
      shiny::column(width = 4, htmltools::tagAppendAttributes(
        shiny::textInput("lab_trip_id", "TRIPID"), .cssSelector = "input", readonly = "readonly")),
      shiny::column(width = 4, shiny::textInput("lab_count", "# OVARIES")),
      shiny::column(width = 4, shiny::textInput("lab_date",  "LAB DATE (DDMMYY)"))
    ),
    shiny::tags$hr(),
    shiny::h3("SAMPLE INFORMATION"),
    shiny::uiOutput("ovary_rows"),
    mat_export_ui(),
    shiny::tableOutput("ovary_table")
  )

  # --- Server ---------------------------------------------------------------

  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(
      trip                  = NULL,
      lobster_ids           = character(),
      lab_ovary             = data.frame(),
      db_folder_path        = "",
      has_selected_db_folder = FALSE,
      db_status             = ""
    )

    # Rows reactive on lobster_ids and lab_ovary — values baked in at render time
    output$ovary_rows <- shiny::renderUI({
      ids       <- rv$lobster_ids
      lab_ovary <- rv$lab_ovary
      if (length(ids) == 0) return(shiny::p("No lobsters loaded. Choose a .db file above."))
      shiny::tagList(lapply(seq_along(ids), function(i) {
        has_data <- nrow(lab_ovary) >= i
        ovary_row_ui(
          i, ids[[i]],
          length_val   = if (has_data) lab_ovary$LENGTH[i]                       else NA,
          whole_w      = if (has_data) lab_ovary$WHOLE_W[i]                      else NA,
          gastrolith   = if (has_data) chr_or_empty(lab_ovary$GASTROLITH[i])     else "",
          colour       = if (has_data) chr_or_empty(lab_ovary$OVARY_COLOUR[i])   else "",
          yellow       = if (has_data) chr_or_empty(lab_ovary$YELLOW_SPOTS[i])   else "",
          weight       = if (has_data) lab_ovary$OVARY_WEIGHT[i]                 else NA,
          image        = if (has_data) chr_or_empty(lab_ovary$IMAGE_COMMENTS[i]) else "",
          observer_val = if (has_data) chr_or_empty(lab_ovary$OBSERVER[i])       else ""
        )
      }))
    })

    # Load .db file
    shiny::observeEvent(input$load_db_file, {
      db_path_tcl <- with_topmost_tk(function()
        tcltk::tkgetOpenFile(filetypes = "{{Database files} {.db}} {{All files} *}", title = "Choose trip database"))
      db_path <- as.character(tcltk::tclvalue(db_path_tcl))
      if (!nzchar(db_path)) return()
      if (!file.exists(db_path)) { rv$db_status <- paste("File not found:", db_path); return() }

      tryCatch({
        loaded <- read_ovary_db(db_path)
        rv$trip        <- loaded$trip
        rv$lobster_ids <- loaded$lobster_ids
        rv$lab_ovary   <- loaded$ovary

        shiny::updateTextInput(session, "lab_trip_id", value = loaded$trip$trip_id[1])
        if (nrow(loaded$ovary) > 0) {
          shiny::updateTextInput(session, "lab_count", value = chr_or_empty(loaded$ovary$LAB_COUNT[1]))
          shiny::updateTextInput(session, "lab_date",  value = chr_or_empty(loaded$ovary$LAB_DATE[1]))
        }

        loaded_folder <- dirname(normalizePath(db_path, winslash = "/", mustWork = FALSE))
        shiny::updateTextInput(session, "db_folder", value = loaded_folder)
        rv$db_folder_path         <- loaded_folder
        rv$has_selected_db_folder <- TRUE
        rv$db_status <- paste("Loaded:", normalizePath(db_path, winslash = "/", mustWork = FALSE))
      }, error = function(e) {
        rv$db_status <- paste("Load failed:", conditionMessage(e))
      })
    })

    # Choose save directory
    shiny::observeEvent(input$choose_db_folder, {
      selected <- with_topmost_tk(function()
        tcltk::tk_choose.dir(default = input$db_folder, caption = "Choose save directory"))
      if (!is.null(selected) && nzchar(selected)) {
        shiny::updateTextInput(session, "db_folder", value = selected)
        rv$db_folder_path         <- selected
        rv$has_selected_db_folder <- TRUE
        rv$db_status <- paste("Save directory selected:", selected)
      }
    })

    output$save_db_ui <- shiny::renderUI({
      shiny::actionButton("save_db", "Save to .db", class = "btn-success",
        disabled = !isTRUE(rv$has_selected_db_folder))
    })

    # Save to .db
    shiny::observeEvent(input$save_db, {
      shiny::req(nzchar(input$db_folder), !is.null(rv$trip), length(rv$lobster_ids) > 0)
      if (!dir.exists(input$db_folder)) { rv$db_status <- paste("Folder not found:", input$db_folder); return() }

      trip_id <- rv$trip$trip_id[1]
      db_path <- file.path(input$db_folder, paste0("MAT_", trip_id, ".db"))

      ovary_df <- do.call(rbind, lapply(seq_along(rv$lobster_ids), function(i) {
        data.frame(
          TRIPID        = trip_id,
          LAB_COUNT     = na_if_empty(input$lab_count),
          LAB_DATE      = na_if_empty(input$lab_date),
          ROW_NO        = i,
          LOBSTER_ID    = rv$lobster_ids[i],
          LENGTH        = input[[paste0("ov_length_",    i)]],
          WHOLE_W       = input[[paste0("ov_whole_w_",   i)]],
          GASTROLITH    = na_if_empty(input[[paste0("ov_gastrolith_", i)]]),
          OVARY_COLOUR  = na_if_empty(input[[paste0("ov_colour_",    i)]]),
          YELLOW_SPOTS  = na_if_empty(input[[paste0("ov_yellow_",    i)]]),
          OVARY_WEIGHT  = input[[paste0("ov_weight_",    i)]],
          IMAGE_COMMENTS = na_if_empty(input[[paste0("ov_image_",    i)]]),
          OBSERVER      = na_if_empty(input[[paste0("ov_observer_",  i)]]),
          stringsAsFactors = FALSE
        )
      }))

      tryCatch({
        write_ovary_db(db_path, trip_id, ovary_df)
        rv$lab_ovary <- ovary_df
        rv$db_status <- paste("Saved:", db_path)
      }, error = function(e) {
        rv$db_status <- paste("Save failed:", conditionMessage(e))
      })
    })

    output$db_status  <- shiny::renderText(rv$db_status)
    output$ovary_table <- shiny::renderTable(rv$lab_ovary)
  }

  shiny::runApp(shiny::shinyApp(ui = ui, server = server))
}
