#' Launch the at-sea sample data entry app
#'
#' Starts a Shiny app for entering hierarchical at-sea sampling data at three
#' levels: TRIP, STRING#, and SAMPLE, with export to a local SQLite database
#' file (.db).
#'
#' @return A Shiny app launched in the current R session.
#' @import shiny
#' @import DBI
#' @import RSQLite
#' @import shinyFeedback
#' @export
at.sea <- function() {

  # --- UI helpers -----------------------------------------------------------

  sample_row_ui <- function(i) {
    shiny::tags$div(
      id = paste0("sample_row_", i),
      shiny::fluidRow(
        shiny::column(width = 2, shiny::numericInput(paste0("lobster_no_", i), "LOBSTER #", value = NA, min = 1, step = 1)),
        shiny::column(width = 2, shiny::numericInput(paste0("length_", i),    "LENGTH",    value = NA, min = 0)),
        shiny::column(width = 2, shiny::selectInput( paste0("hardness_", i),  "HARDNESS",  choices = c("", 1:5), selected = "")),
        shiny::column(width = 2, shiny::selectInput( paste0("egg_", i),       "EGG",       choices = c("", 1:4), selected = "")),
        shiny::column(width = 2, shiny::selectInput( paste0("pleopod_", i),   "PLEOPOD",   choices = c("", 0:1), selected = "")),
        shiny::column(width = 2, shiny::selectInput( paste0("ovary_", i),     "OVARY",     choices = c("", 0:1), selected = ""))
      ),
      shiny::fluidRow(
        shiny::column(width = 12, shiny::textInput(paste0("comments_", i), "COMMENTS"))
      ),
      shiny::tags$hr()
    )
  }

  # --- DB helpers -----------------------------------------------------------

  db_ensure_tables <- function(con) {
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS TRIP (
        TRIPID TEXT PRIMARY KEY, ORG TEXT, TRIP_DATE TEXT, PORT TEXT, LFA TEXT, SAMPLER TEXT
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS STRING_INFO (
        TRIPID TEXT NOT NULL, STRING_NO INTEGER NOT NULL,
        LAT TEXT, LONG TEXT, GRID TEXT, DEPTH REAL,
        PRIMARY KEY (TRIPID, STRING_NO),
        FOREIGN KEY (TRIPID) REFERENCES TRIP(TRIPID)
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS SAMPLE (
        TRIPID TEXT NOT NULL, STRING_NO INTEGER NOT NULL, LOBSTER_ID TEXT NOT NULL,
        LENGTH REAL, HARDNESS TEXT, EGG TEXT, PLEOPOD TEXT, OVARY TEXT, COMMENTS TEXT,
        PRIMARY KEY (TRIPID, LOBSTER_ID),
        FOREIGN KEY (TRIPID, STRING_NO) REFERENCES STRING_INFO(TRIPID, STRING_NO)
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS LAB_PLEOPOD (
        TRIPID TEXT NOT NULL, LAB_COUNT TEXT, LAB_DATE TEXT, ROW_NO INTEGER NOT NULL,
        LOBSTER_ID TEXT, CG_STAGE TEXT, MOULT_STAGE TEXT, IMAGE_FILE TEXT, OBSERVER TEXT,
        PRIMARY KEY (TRIPID, ROW_NO),
        FOREIGN KEY (TRIPID, LOBSTER_ID) REFERENCES SAMPLE(TRIPID, LOBSTER_ID)
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS LAB_OVARY (
        TRIPID TEXT NOT NULL, LAB_COUNT TEXT, LAB_DATE TEXT, ROW_NO INTEGER NOT NULL,
        LOBSTER_ID TEXT, LENGTH REAL, WHOLE_W REAL, GASTROLITH TEXT, OVARY_COLOUR TEXT,
        YELLOW_SPOTS TEXT, OVARY_WEIGHT REAL, IMAGE_COMMENTS TEXT, OBSERVER TEXT,
        PRIMARY KEY (TRIPID, ROW_NO),
        FOREIGN KEY (TRIPID, LOBSTER_ID) REFERENCES SAMPLE(TRIPID, LOBSTER_ID)
      )")
  }

  write_atsea_db <- function(db_path, trip_df, string_df, sample_df) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    db_ensure_tables(con)

    trip_id <- trip_df$trip_id[1]
    DBI::dbExecute(con, "DELETE FROM SAMPLE     WHERE TRIPID = ?", params = list(trip_id))
    DBI::dbExecute(con, "DELETE FROM STRING_INFO WHERE TRIPID = ?", params = list(trip_id))
    DBI::dbExecute(con, "DELETE FROM TRIP        WHERE TRIPID = ?", params = list(trip_id))

    DBI::dbExecute(con,
      "INSERT INTO TRIP (TRIPID, ORG, TRIP_DATE, PORT, LFA, SAMPLER) VALUES (?, ?, ?, ?, ?, ?)",
      params = unname(as.list(trip_df[1, c("trip_id", "org", "date", "port", "lfa", "sampler")]))
    )

    if (nrow(string_df) > 0) {
      DBI::dbWriteTable(con, "STRING_INFO",
        data.frame(TRIPID = string_df$trip_id, STRING_NO = string_df$string_no,
                   LAT = string_df$lat, LONG = string_df$long,
                   GRID = string_df$grid, DEPTH = string_df$depth,
                   stringsAsFactors = FALSE),
        append = TRUE, row.names = FALSE)
    }

    if (nrow(sample_df) > 0) {
      DBI::dbWriteTable(con, "SAMPLE",
        data.frame(TRIPID = sample_df$trip_id, STRING_NO = sample_df$string_no,
                   LOBSTER_ID = sample_df$lobster_id, LENGTH = sample_df$length,
                   HARDNESS = sample_df$hardness, EGG = sample_df$egg,
                   PLEOPOD = sample_df$pleopod, OVARY = sample_df$ovary,
                   COMMENTS = sample_df$comments, stringsAsFactors = FALSE),
        append = TRUE, row.names = FALSE)
    }
  }

  read_atsea_db <- function(db_path) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    if (!DBI::dbExistsTable(con, "TRIP")) stop("TRIP table not found in database")
    trip_df <- DBI::dbGetQuery(con, "SELECT TRIPID, ORG, TRIP_DATE, PORT, LFA, SAMPLER FROM TRIP ORDER BY TRIPID")
    if (nrow(trip_df) == 0) stop("No TRIP records found in database")
    if (nrow(trip_df) > 1) stop("Database contains multiple TRIPIDs; expected one trip per .db file")
    trip_id <- trip_df$TRIPID[1]

    string_df <- if (DBI::dbExistsTable(con, "STRING_INFO")) {
      DBI::dbGetQuery(con,
        "SELECT TRIPID, STRING_NO, LAT, LONG, GRID, DEPTH FROM STRING_INFO WHERE TRIPID = ? ORDER BY STRING_NO",
        params = list(trip_id))
    } else data.frame()

    sample_df <- if (DBI::dbExistsTable(con, "SAMPLE")) {
      DBI::dbGetQuery(con,
        "SELECT TRIPID, STRING_NO, LOBSTER_ID, LENGTH, HARDNESS, EGG, PLEOPOD, OVARY, COMMENTS FROM SAMPLE WHERE TRIPID = ? ORDER BY LOBSTER_ID",
        params = list(trip_id))
    } else data.frame()

    list(
      trip = data.frame(trip_id = trip_df$TRIPID, org = trip_df$ORG, date = trip_df$TRIP_DATE,
                        port = trip_df$PORT, lfa = trip_df$LFA, sampler = trip_df$SAMPLER,
                        stringsAsFactors = FALSE),
      strings = if (nrow(string_df) > 0) {
        data.frame(trip_id = string_df$TRIPID, string_no = string_df$STRING_NO,
                   lat = string_df$LAT, long = string_df$LONG,
                   grid = string_df$GRID, depth = string_df$DEPTH, stringsAsFactors = FALSE)
      } else data.frame(),
      samples = if (nrow(sample_df) > 0) {
        data.frame(trip_id = sample_df$TRIPID, string_no = sample_df$STRING_NO,
                   lobster_id = sample_df$LOBSTER_ID, length = sample_df$LENGTH,
                   hardness = sample_df$HARDNESS, egg = sample_df$EGG,
                   pleopod = sample_df$PLEOPOD, ovary = sample_df$OVARY,
                   comments = sample_df$COMMENTS, stringsAsFactors = FALSE)
      } else data.frame()
    )
  }

  # --- UI -------------------------------------------------------------------

  ui <- shiny::fluidPage(
    mat_js_tags(),
    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('toggleButtons', function(msg) {
        $('button.validate-gated').prop('disabled', msg.disabled);
      });
    ")),
    shinyFeedback::useShinyFeedback(),
    shiny::titlePanel("CLRN SOM50 AT-SEA SAMPLE DATA FORM"),
    shiny::h4("Load Existing Trip"),
    shiny::actionButton("load_db_file", "Choose .db File", class = "btn-secondary"),
    shiny::tags$hr(),
    shiny::h3("TRIP INFORMATION"),
    shiny::fluidRow(
      shiny::column(width = 3, htmltools::tagAppendAttributes(shiny::textInput("trip_id", "TRIPID"), .cssSelector = "input", readonly = "readonly")),
      shiny::column(width = 3, shiny::textInput("trip_org",     "ORG.")),
      shiny::column(width = 3, shiny::dateInput( "trip_date",   "DATE")),
      shiny::column(width = 3, shiny::textInput("trip_port",    "PORT"))
    ),
    shiny::fluidRow(
      shiny::column(width = 3, shiny::selectInput("trip_lfa", "LFA",
        choices = c("", "27", "28", "29", "30", "31A", "31B", "32", "33", "34", "35", "36", "37A", "37B", "38"),
        selected = "")),
      shiny::column(width = 6, shiny::textInput("trip_sampler", "SAMPLER")),
      shiny::column(width = 3, shiny::actionButton("save_trip", "Create / Update Trip", class = "btn-primary validate-gated"))
    ),
    shiny::tags$hr(),
    shiny::h3("LOCATION INFORMATION (STRING#)"),
    shiny::fluidRow(
      shiny::column(width = 3, shiny::numericInput("string_no", "STRING #", value = NA, min = 1, step = 1)),
      shiny::column(width = 3, htmltools::tagAppendAttributes(shiny::textInput("lat",  "LAT (DDMM.MM)"),  .cssSelector = "input", maxlength = "7")),
      shiny::column(width = 3, htmltools::tagAppendAttributes(shiny::textInput("long", "LONG (DDMM.MM)"), .cssSelector = "input", maxlength = "7")),
      shiny::column(width = 3, shiny::numericInput("grid", "GRID", value = NA, min = 1, step = 1))
    ),
    shiny::fluidRow(
      shiny::column(width = 3, shiny::numericInput("depth", "DEPTH (FM)", value = NA, min = 0)),
      shiny::column(width = 3, shiny::actionButton("next_string", "Next String / Update", class = "btn-primary validate-gated"))
    ),
    shiny::tags$hr(),
    shiny::h3("SAMPLE INFORMATION"),
    shiny::tags$div(id = "sample_rows_container", sample_row_ui(1)),
    mat_export_ui(),
    shiny::tags$hr(),
    shiny::h4("Current Trip"),    shiny::tableOutput("trip_table"),
    shiny::h4("Current Strings"), shiny::tableOutput("string_table"),
    shiny::h4("Current Samples"), shiny::tableOutput("sample_table")
  )

  # --- Server ---------------------------------------------------------------

  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(
      trip             = NULL,
      strings          = data.frame(),
      samples          = data.frame(),
      sample_row_count = 1,
      db_folder_path   = "",
      has_selected_db_folder = FALSE,
      db_status        = "",
      sample_autofill_active = FALSE
    )

    # Pre-populate string + sample fields from loaded data
    fill_for_string <- function(string_no) {
      trip_df    <- shiny::isolate(rv$trip)
      strings_df <- shiny::isolate(rv$strings)
      samples_df <- shiny::isolate(rv$samples)
      row_count  <- shiny::isolate(rv$sample_row_count)
      if (is.null(trip_df) || nrow(trip_df) == 0 || is.na(string_no)) return()
      rv$sample_autofill_active <- TRUE
      on.exit({ rv$sample_autofill_active <- FALSE }, add = TRUE)

      srow <- strings_df[strings_df$string_no == as.integer(string_no), , drop = FALSE]
      if (nrow(srow) > 0) {
        shiny::updateTextInput(  session, "lat",  value = ifelse(is.na(srow$lat[1]),  "", srow$lat[1]))
        shiny::updateTextInput(  session, "long", value = ifelse(is.na(srow$long[1]), "", srow$long[1]))
        shiny::updateNumericInput(session, "grid",  value = srow$grid[1])
        shiny::updateNumericInput(session, "depth", value = srow$depth[1])
      }

      sample_rows <- if (ncol(samples_df) > 0) {
        samples_df[samples_df$string_no == as.integer(string_no), , drop = FALSE]
      } else data.frame()
      if (nrow(sample_rows) > 0)
        sample_rows <- sample_rows[order(sample_rows$lobster_id), , drop = FALSE]
      target_n    <- max(1, nrow(sample_rows) + 1)

      if (row_count < target_n) {
        for (i in seq(row_count + 1, target_n))
          shiny::insertUI(selector = "#sample_rows_container", where = "beforeEnd", ui = sample_row_ui(i))
      } else if (row_count > target_n) {
        for (i in seq(target_n + 1, row_count))
          shiny::removeUI(selector = paste0("#sample_row_", i), immediate = TRUE)
      }
      rv$sample_row_count <- target_n

      for (i in seq_len(target_n)) {
        if (i <= nrow(sample_rows)) {
          lid       <- as.character(sample_rows$lobster_id[i])
          lob_no    <- suppressWarnings(as.integer(substr(lid, nchar(trip_df$trip_id[1]) + 3, nchar(trip_df$trip_id[1]) + 4)))
          shiny::updateNumericInput(session, paste0("lobster_no_", i), value = lob_no)
          shiny::updateNumericInput(session, paste0("length_",    i), value = sample_rows$length[i])
          shiny::updateSelectInput( session, paste0("hardness_",  i), selected = ifelse(is.na(sample_rows$hardness[i]),  "", sample_rows$hardness[i]))
          shiny::updateSelectInput( session, paste0("egg_",       i), selected = ifelse(is.na(sample_rows$egg[i]),       "", sample_rows$egg[i]))
          shiny::updateSelectInput( session, paste0("pleopod_",   i), selected = ifelse(is.na(sample_rows$pleopod[i]),   "", sample_rows$pleopod[i]))
          shiny::updateSelectInput( session, paste0("ovary_",     i), selected = ifelse(is.na(sample_rows$ovary[i]),     "", sample_rows$ovary[i]))
          shiny::updateTextInput(   session, paste0("comments_",  i), value    = ifelse(is.na(sample_rows$comments[i]),  "", sample_rows$comments[i]))
        } else {
          shiny::updateNumericInput(session, paste0("lobster_no_", i), value = NA)
          shiny::updateNumericInput(session, paste0("length_",    i), value = NA)
          shiny::updateSelectInput( session, paste0("hardness_",  i), selected = "")
          shiny::updateSelectInput( session, paste0("egg_",       i), selected = "")
          shiny::updateSelectInput( session, paste0("pleopod_",   i), selected = "")
          shiny::updateSelectInput( session, paste0("ovary_",     i), selected = "")
          shiny::updateTextInput(   session, paste0("comments_",  i), value = "")
        }
      }
    }

    clear_sample_row <- function(i) {
      shiny::updateNumericInput(session, paste0("lobster_no_", i), value = NA)
      shiny::updateNumericInput(session, paste0("length_",    i), value = NA)
      shiny::updateSelectInput( session, paste0("hardness_",  i), selected = "")
      shiny::updateSelectInput( session, paste0("egg_",       i), selected = "")
      shiny::updateSelectInput( session, paste0("pleopod_",   i), selected = "")
      shiny::updateSelectInput( session, paste0("ovary_",     i), selected = "")
      shiny::updateTextInput(   session, paste0("comments_",  i), value = "")
    }

    reset_sample_rows <- function(target_rows = 1) {
      current_rows <- shiny::isolate(rv$sample_row_count)
      target_rows  <- max(1, as.integer(target_rows))

      if (current_rows < target_rows) {
        for (i in seq(current_rows + 1, target_rows)) {
          shiny::insertUI(selector = "#sample_rows_container", where = "beforeEnd", ui = sample_row_ui(i))
        }
      } else if (current_rows > target_rows) {
        for (i in seq(target_rows + 1, current_rows)) {
          shiny::removeUI(selector = paste0("#sample_row_", i), immediate = TRUE)
        }
      }
      rv$sample_row_count <- target_rows

      for (i in seq_len(target_rows)) clear_sample_row(i)
    }

    clear_location_and_samples <- function() {
      shiny::updateTextInput(  session, "lat",   value = "")
      shiny::updateTextInput(  session, "long",  value = "")
      shiny::updateNumericInput(session, "grid", value = NA)
      shiny::updateNumericInput(session, "depth", value = NA)
      reset_sample_rows(1)
    }


    lobster_numbers <- function(n = rv$sample_row_count) {
      vapply(seq_len(n), function(i) {
        val <- input[[paste0("lobster_no_", i)]]
        if (is.null(val) || is.na(val)) NA_integer_ else as.integer(val)
      }, integer(1))
    }

    # Auto-derive TRIPID from ORG + DATE
    shiny::observe({
      shiny::req(!is.null(input$trip_org), !is.null(input$trip_date))
      trip_id <- if (nzchar(input$trip_org)) paste0(input$trip_org, format(input$trip_date, "%d%m%y")) else ""
      shiny::updateTextInput(session, "trip_id", value = trip_id)
    })

    # Lat/long validation feedback
    shiny::observe({
      lat <- input$lat
      if (is.null(lat) || !nzchar(lat)) { shinyFeedback::hideFeedback("lat"); return() }
      shinyFeedback::feedbackDanger("lat", !is_valid_ddmm(lat, "lat"),
        "Must be DDMM.MM — degrees 00-89, minutes 00-59 (e.g. 4430.55)")
    })

    shiny::observe({
      long <- input$long
      if (is.null(long) || !nzchar(long)) { shinyFeedback::hideFeedback("long"); return() }
      shinyFeedback::feedbackDanger("long", !is_valid_ddmm(long, "long"),
        "Must be DDMM.MM — minutes 00-59 (e.g. 6645.20)")
    })

    # Duplicate LOBSTER # feedback
    shiny::observe({
      n      <- rv$sample_row_count
      values <- lobster_numbers(n)
      non_na <- values[!is.na(values)]
      dupes  <- unique(non_na[duplicated(non_na)])
      for (i in seq_len(n)) {
        v <- values[[i]]
        shinyFeedback::feedbackDanger(paste0("lobster_no_", i), !is.na(v) && v %in% dupes, "Duplicate LOBSTER #")
      }
    })

    # Gate validate-gated buttons on validation errors
    shiny::observe({
      lat_val  <- input$lat
      long_val <- input$long
      lat_err  <- !is.null(lat_val)  && nzchar(lat_val)  && !is_valid_ddmm(lat_val,  "lat")
      long_err <- !is.null(long_val) && nzchar(long_val) && !is_valid_ddmm(long_val, "long")
      n        <- rv$sample_row_count
      values   <- lobster_numbers(n)
      lob_err  <- any(duplicated(values[!is.na(values)]))
      session$sendCustomMessage("toggleButtons", list(disabled = lat_err || long_err || lob_err))
    })

    # Keep exactly one trailing blank sample row
    shiny::observe({
      if (isTRUE(rv$sample_autofill_active)) return()
      n <- rv$sample_row_count
      values <- lobster_numbers(n)
      filled <- which(!is.na(values))
      desired_rows <- max(1, if (length(filled) == 0) 1 else max(filled) + 1)

      if (desired_rows > n) {
        for (i in seq(n + 1, desired_rows)) {
          shiny::insertUI(selector = "#sample_rows_container", where = "beforeEnd", ui = sample_row_ui(i))
        }
        rv$sample_row_count <- desired_rows
      } else if (desired_rows < n) {
        for (i in seq(desired_rows + 1, n)) {
          shiny::removeUI(selector = paste0("#sample_row_", i), immediate = TRUE)
        }
        rv$sample_row_count <- desired_rows
      }
    })

    # Save / update trip
    shiny::observeEvent(input$save_trip, {
      rv$trip <- data.frame(
        trip_id = na_if_empty(input$trip_id),
        org     = na_if_empty(input$trip_org),
        date    = na_if_empty(as.character(input$trip_date)),
        port    = na_if_empty(input$trip_port),
        lfa     = na_if_empty(input$trip_lfa),
        sampler = na_if_empty(input$trip_sampler),
        stringsAsFactors = FALSE
      )
    })

    # Next String / Update — save current string+samples, advance string_no, clear rows
    shiny::observeEvent(input$next_string, {
      shiny::req(!is.null(rv$trip), nzchar(input$trip_id), !is.na(input$string_no))

      new_string <- data.frame(
        trip_id   = input$trip_id,
        string_no = as.integer(input$string_no),
        lat       = na_if_empty(input$lat),
        long      = na_if_empty(input$long),
        grid      = input$grid,
        depth     = input$depth,
        stringsAsFactors = FALSE
      )
      rv$strings <- unique(rbind(rv$strings, new_string))

      sample_rows <- lapply(seq_len(rv$sample_row_count), function(i) {
        lob_no <- input[[paste0("lobster_no_", i)]]
        if (is.null(lob_no) || is.na(lob_no)) return(NULL)
        data.frame(
          trip_id   = input$trip_id,
          string_no = as.integer(input$string_no),
          lobster_id = format_lobster_id(input$trip_id, input$string_no, lob_no),
          length    = input[[paste0("length_",   i)]],
          hardness  = na_if_empty(input[[paste0("hardness_", i)]]),
          egg       = na_if_empty(input[[paste0("egg_",      i)]]),
          pleopod   = na_if_empty(input[[paste0("pleopod_",  i)]]),
          ovary     = na_if_empty(input[[paste0("ovary_",    i)]]),
          comments  = na_if_empty(input[[paste0("comments_", i)]]),
          stringsAsFactors = FALSE
        )
      })
      sample_rows <- Filter(Negate(is.null), sample_rows)
      if (length(sample_rows) > 0)
        rv$samples <- unique(rbind(rv$samples, do.call(rbind, sample_rows)))

      shiny::updateNumericInput(session, "string_no", value = input$string_no + 1)
      clear_location_and_samples()
    })

    # Populate fields when string_no changes (clear first, then load if available)
    shiny::observeEvent(input$string_no, {
      shiny::req(!is.null(rv$trip), nrow(rv$trip) > 0, !is.na(input$string_no))
      clear_location_and_samples()
      fill_for_string(input$string_no)
    }, ignoreInit = TRUE)

    # Load existing .db file
    shiny::observeEvent(input$load_db_file, {
      db_path_tcl <- with_topmost_tk(function()
        tcltk::tkgetOpenFile(filetypes = "{{Database files} {.db}} {{All files} *}", title = "Choose trip database"))
      db_path <- as.character(tcltk::tclvalue(db_path_tcl))
      if (!nzchar(db_path)) return()
      if (!file.exists(db_path)) { rv$db_status <- paste("File not found:", db_path); return() }

      tryCatch({
        loaded <- read_atsea_db(db_path)
        rv$trip    <- loaded$trip
        rv$strings <- loaded$strings
        rv$samples <- loaded$samples

        shiny::updateTextInput(  session, "trip_id",     value    = rv$trip$trip_id[1])
        shiny::updateTextInput(  session, "trip_org",    value    = rv$trip$org[1])
        shiny::updateDateInput(  session, "trip_date",   value    = rv$trip$date[1])
        shiny::updateTextInput(  session, "trip_port",   value    = rv$trip$port[1])
        shiny::updateSelectInput(session, "trip_lfa",    selected = rv$trip$lfa[1])
        shiny::updateTextInput(  session, "trip_sampler", value   = rv$trip$sampler[1])

        first_string <- if (nrow(rv$strings) > 0) min(rv$strings$string_no, na.rm = TRUE) else 1
        rv$sample_row_count <- 1
        shiny::updateNumericInput(session, "string_no", value = first_string)

        loaded_folder <- dirname(normalizePath(db_path, winslash = "/", mustWork = FALSE))
        shiny::updateTextInput(session, "db_folder", value = loaded_folder)
        rv$db_folder_path      <- loaded_folder
        rv$has_selected_db_folder <- TRUE
        rv$db_status <- paste("Loaded:", normalizePath(db_path, winslash = "/", mustWork = FALSE))
      }, error = function(e) {
        rv$db_status <- paste("Load failed:", conditionMessage(e))
      })
    })

    # Choose save directory
    shiny::observeEvent(input$choose_db_folder, {
      selected <- tryCatch(
        with_topmost_tk(function()
          tcltk::tk_choose.dir(default = input$db_folder, caption = "Choose save directory")),
        error = function(e) {
          rv$db_status <- paste("Choose directory failed:", conditionMessage(e))
          NULL
        }
      )
      if (!is.null(selected) && nzchar(selected)) {
        shiny::updateTextInput(session, "db_folder", value = selected)
        rv$db_folder_path      <- selected
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
      shiny::req(nzchar(input$db_folder), !is.null(rv$trip), nrow(rv$trip) > 0, nzchar(rv$trip$trip_id[1]))
      if (!dir.exists(input$db_folder)) { rv$db_status <- paste("Folder not found:", input$db_folder); return() }

      db_path <- file.path(input$db_folder, paste0("MAT_", rv$trip$trip_id[1], ".db"))
      tryCatch({
        write_atsea_db(db_path, rv$trip, rv$strings, rv$samples)
        rv$db_status <- paste("Saved:", db_path)
      }, error = function(e) {
        rv$db_status <- paste("Save failed:", conditionMessage(e))
      })
    })

    output$db_status    <- shiny::renderText(rv$db_status)
    output$trip_table   <- shiny::renderTable(rv$trip)
    output$string_table <- shiny::renderTable(rv$strings)
    output$sample_table <- shiny::renderTable(rv$samples)
  }

  shiny::runApp(shiny::shinyApp(ui = ui, server = server))
}
