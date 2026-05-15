#' Launch the MatSamp multi-level data entry app
#'
#' Starts a Shiny app for entering hierarchical sampling data at three levels:
#' TRIP, STRING#, and SAMPLE, with export to a local SQL database file.
#'
#' @return A Shiny app launched in the current R session.
#' @import shiny
#' @import DBI
#' @import RSQLite
#' @export
matinput <- function() {
  sample_row_ui <- function(i) {
    shiny::tags$div(
      id = paste0("sample_row_", i),
      shiny::fluidRow(
        shiny::column(width = 2, shiny::numericInput(paste0("lobster_no_", i), "LOBSTER #", value = NA, min = 1, step = 1)),
        shiny::column(width = 2, shiny::numericInput(paste0("length_", i), "LENGTH", value = NA)),
        shiny::column(width = 2, shiny::textInput(paste0("hardness_", i), "HARDNESS")),
        shiny::column(width = 2, shiny::textInput(paste0("egg_", i), "EGG")),
        shiny::column(width = 2, shiny::textInput(paste0("pleopod_", i), "PLEOPOD")),
        shiny::column(width = 2, shiny::textInput(paste0("ovary_", i), "OVARY"))
      ),
      shiny::fluidRow(
        shiny::column(width = 12, shiny::textInput(paste0("comments_", i), "COMMENTS"))
      ),
      shiny::tags$hr()
    )
  }

  write_to_db <- function(db_path, trip_df, string_df, sample_df) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS TRIP (
        TRIPID TEXT PRIMARY KEY,
        ORG TEXT,
        TRIP_DATE TEXT,
        PORT TEXT,
        LFA TEXT,
        SAMPLER TEXT
      )
    ")

    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS STRING_INFO (
        TRIPID TEXT NOT NULL,
        STRING_NO INTEGER NOT NULL,
        LAT TEXT,
        LONG TEXT,
        GRID TEXT,
        DEPTH REAL,
        PRIMARY KEY (TRIPID, STRING_NO),
        FOREIGN KEY (TRIPID) REFERENCES TRIP(TRIPID)
      )
    ")

    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS SAMPLE (
        TRIPID TEXT NOT NULL,
        STRING_NO INTEGER NOT NULL,
        LOBSTER_NO INTEGER NOT NULL,
        LENGTH REAL,
        HARDNESS TEXT,
        EGG TEXT,
        PLEOPOD TEXT,
        OVARY TEXT,
        COMMENTS TEXT,
        PRIMARY KEY (TRIPID, STRING_NO, LOBSTER_NO),
        FOREIGN KEY (TRIPID, STRING_NO) REFERENCES STRING_INFO(TRIPID, STRING_NO)
      )
    ")

    if (!is.null(trip_df) && nrow(trip_df) > 0) {
      DBI::dbExecute(con, "DELETE FROM SAMPLE WHERE TRIPID = ?", params = list(trip_df$trip_id[1]))
      DBI::dbExecute(con, "DELETE FROM STRING_INFO WHERE TRIPID = ?", params = list(trip_df$trip_id[1]))
      DBI::dbExecute(con, "DELETE FROM TRIP WHERE TRIPID = ?", params = list(trip_df$trip_id[1]))
      DBI::dbExecute(con, "INSERT INTO TRIP (TRIPID, ORG, TRIP_DATE, PORT, LFA, SAMPLER) VALUES (?, ?, ?, ?, ?, ?)",
        params = unname(as.list(trip_df[1, c("trip_id", "org", "date", "port", "lfa", "sampler")]))
      )
    }

    if (nrow(string_df) > 0) {
      string_df <- unique(string_df)
      DBI::dbWriteTable(
        con,
        "STRING_INFO",
        data.frame(
          TRIPID = string_df$trip_id,
          STRING_NO = string_df$string_no,
          LAT = string_df$lat,
          LONG = string_df$long,
          GRID = string_df$grid,
          DEPTH = string_df$depth,
          stringsAsFactors = FALSE
        ),
        append = TRUE,
        row.names = FALSE
      )
    }

    if (nrow(sample_df) > 0) {
      sample_df <- unique(sample_df)
      DBI::dbWriteTable(
        con,
        "SAMPLE",
        data.frame(
          TRIPID = sample_df$trip_id,
          STRING_NO = sample_df$string_no,
          LOBSTER_NO = sample_df$lobster_no,
          LENGTH = sample_df$length,
          HARDNESS = sample_df$hardness,
          EGG = sample_df$egg,
          PLEOPOD = sample_df$pleopod,
          OVARY = sample_df$ovary,
          COMMENTS = sample_df$comments,
          stringsAsFactors = FALSE
        ),
        append = TRUE,
        row.names = FALSE
      )
    }
  }

  read_from_db <- function(db_path) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    if (!DBI::dbExistsTable(con, "TRIP")) stop("TRIP table not found in database")

    trip_df <- DBI::dbGetQuery(con, "SELECT TRIPID, ORG, TRIP_DATE, PORT, LFA, SAMPLER FROM TRIP ORDER BY TRIPID")
    if (nrow(trip_df) == 0) stop("No TRIP records found in database")
    if (nrow(trip_df) > 1) stop("Database contains multiple TRIPIDs; expected one trip per .db file")
    trip_id <- trip_df$TRIPID[1]

    string_df <- if (DBI::dbExistsTable(con, "STRING_INFO")) {
      DBI::dbGetQuery(con, "SELECT TRIPID, STRING_NO, LAT, LONG, GRID, DEPTH FROM STRING_INFO WHERE TRIPID = ? ORDER BY STRING_NO", params = list(trip_id))
    } else {
      data.frame()
    }

    sample_df <- if (DBI::dbExistsTable(con, "SAMPLE")) {
      DBI::dbGetQuery(con, "SELECT TRIPID, STRING_NO, LOBSTER_NO, LENGTH, HARDNESS, EGG, PLEOPOD, OVARY, COMMENTS FROM SAMPLE WHERE TRIPID = ? ORDER BY STRING_NO, LOBSTER_NO", params = list(trip_id))
    } else {
      data.frame()
    }

    list(
      trip = data.frame(
        trip_id = trip_df$TRIPID, org = trip_df$ORG, date = trip_df$TRIP_DATE,
        port = trip_df$PORT, lfa = trip_df$LFA, sampler = trip_df$SAMPLER, stringsAsFactors = FALSE
      ),
      strings = data.frame(
        trip_id = string_df$TRIPID, string_no = string_df$STRING_NO, lat = string_df$LAT,
        long = string_df$LONG, grid = string_df$GRID, depth = string_df$DEPTH, stringsAsFactors = FALSE
      ),
      samples = data.frame(
        trip_id = sample_df$TRIPID, string_no = sample_df$STRING_NO, lobster_no = sample_df$LOBSTER_NO,
        length = sample_df$LENGTH, hardness = sample_df$HARDNESS, egg = sample_df$EGG,
        pleopod = sample_df$PLEOPOD, ovary = sample_df$OVARY, comments = sample_df$COMMENTS, stringsAsFactors = FALSE
      )
    )
  }

  trip_ui <- shiny::tagList(
    shiny::h3("TRIP INFORMATION"),
    shiny::fluidRow(
      shiny::column(width = 3, shiny::textInput("trip_id", "TRIPID")),
      shiny::column(width = 3, shiny::textInput("trip_org", "ORG.")),
      shiny::column(width = 3, shiny::dateInput("trip_date", "DATE")),
      shiny::column(width = 3, shiny::textInput("trip_port", "PORT"))
    ),
    shiny::fluidRow(
      shiny::column(width = 3, shiny::textInput("trip_lfa", "LFA")),
      shiny::column(width = 6, shiny::textInput("trip_sampler", "SAMPLER")),
      shiny::column(width = 3, shiny::actionButton("save_trip", "Create / Update Trip", class = "btn-primary"))
    )
  )

  location_ui <- shiny::tagList(
    shiny::tags$hr(),
    shiny::h3("LOCATION INFORMATION (STRING#)"),
    shiny::fluidRow(
      shiny::column(width = 3, shiny::numericInput("string_no", "STRING #", value = NA, min = 1, step = 1)),
      shiny::column(width = 3, shiny::textInput("lat", "LAT (DDMM.MM)")),
      shiny::column(width = 3, shiny::textInput("long", "LONG (DDMM.MM)")),
      shiny::column(width = 3, shiny::textInput("grid", "GRID"))
    ),
    shiny::fluidRow(
      shiny::column(width = 3, shiny::numericInput("depth", "DEPTH (FM)", value = NA)),
      shiny::column(width = 3, shiny::actionButton("next_string", "Next String / Update", class = "btn-primary"))
    )
  )

  sample_ui <- shiny::tagList(
    shiny::tags$hr(),
    shiny::h3("SAMPLE INFORMATION"),
    shiny::tags$div(id = "sample_rows_container", sample_row_ui(1))
  )

  export_ui <- shiny::tagList(
    shiny::tags$hr(),
    shiny::h3("DATABASE EXPORT"),
    shiny::fluidRow(
      shiny::column(width = 12, shiny::textInput("db_folder", "Local folder path", value = ""))
    ),
    shiny::fluidRow(
      shiny::column(width = 4, shiny::actionButton("choose_db_folder", "Choose Save Directory", class = "btn-primary")),
      shiny::column(width = 4, shiny::uiOutput("save_db_ui"))
    ),
    shiny::verbatimTextOutput("db_status")
  )

  shiny::runApp(
    shiny::shinyApp(
      ui = shiny::fluidPage(
        shiny::tags$script(shiny::HTML("
          document.addEventListener('keydown', function(e) {
            if (e.key === 'Enter') {
              e.preventDefault();

              var focusable = Array.prototype.filter.call(
                document.querySelectorAll('input, select, textarea, button, [tabindex]'),
                function(el) {
                  return el.tabIndex >= 0 && !el.disabled && el.offsetParent !== null;
                }
              );

              var index = focusable.indexOf(document.activeElement);
              if (index > -1 && index + 1 < focusable.length) {
                var next = focusable[index + 1];
                next.focus();
                if (next.tagName === 'INPUT' || next.tagName === 'TEXTAREA') {
                  next.select();
                }
              }
            }
          });
        ")),
        shiny::tags$script(shiny::HTML("
          document.addEventListener('keydown', function(e) {
            if (e.key === 'Escape') {
              e.preventDefault();
              document.execCommand('undo');
            }
          });
        ")),
        shiny::tags$script(shiny::HTML("
          $(document).ready(function() {
            function disableNumericScroll(target) {
              $(target).on('wheel', function(e) {
                e.preventDefault();
              });
            }

            disableNumericScroll('input[type=\"number\"]');

            const observer = new MutationObserver(function(mutations) {
              mutations.forEach(function(mutation) {
                mutation.addedNodes.forEach(function(node) {
                  if (!(node instanceof Element)) return;
                  if ($(node).is('input[type=\"number\"]')) {
                    disableNumericScroll(node);
                  }
                  disableNumericScroll($(node).find('input[type=\"number\"]'));
                });
              });
            });

            observer.observe(document.body, { childList: true, subtree: true });
          });
        ")),
        shiny::titlePanel("CLRN SOM50 AT-SEA SAMPLE DATA FORM"),
        shiny::tags$h4("Load Existing Trip"),
        shiny::actionButton("load_db_file", "Choose .db File", class = "btn-secondary"),
        trip_ui,
        location_ui,
        sample_ui,
        export_ui,
        shiny::tags$hr(),
        shiny::h4("Current Trip"),
        shiny::tableOutput("trip_table"),
        shiny::h4("Current Strings"),
        shiny::tableOutput("string_table"),
        shiny::h4("Current Samples"),
        shiny::tableOutput("sample_table")
      ),
      server = function(input, output, session) {
        rv <- shiny::reactiveValues(
          trip = NULL,
          strings = data.frame(),
          samples = data.frame(),
          sample_row_count = 1,
          db_status = "",
          has_selected_db_folder = FALSE
        )

        shiny::observe({
          i <- rv$sample_row_count
          lobster_value <- input[[paste0("lobster_no_", i)]]
          if (!is.null(lobster_value) && !is.na(lobster_value)) {
            next_i <- rv$sample_row_count + 1
            shiny::insertUI(selector = "#sample_rows_container", where = "beforeEnd", ui = sample_row_ui(next_i))
            rv$sample_row_count <- next_i
          }
        })



        output$save_db_ui <- shiny::renderUI({
          shiny::actionButton(
            "save_db",
            "Save to .db",
            class = "btn-success",
            disabled = !isTRUE(rv$has_selected_db_folder)
          )
        })

        shiny::observeEvent(input$choose_db_folder, {
          selected_dir <- tcltk::tk_choose.dir(default = input$db_folder, caption = "Choose save directory")
          if (!is.null(selected_dir) && nzchar(selected_dir)) {
            shiny::updateTextInput(session, "db_folder", value = selected_dir)
            rv$has_selected_db_folder <- TRUE
            rv$db_status <- paste("Save directory selected:", selected_dir)
          }
        })
        shiny::observeEvent(input$save_trip, {
          rv$trip <- data.frame(
            trip_id = input$trip_id,
            org = input$trip_org,
            date = as.character(input$trip_date),
            port = input$trip_port,
            lfa = input$trip_lfa,
            sampler = input$trip_sampler,
            stringsAsFactors = FALSE
          )
        })

        shiny::observeEvent(input$next_string, {
          shiny::req(!is.null(rv$trip), nzchar(input$trip_id), !is.na(input$string_no))

          new_string <- data.frame(
            trip_id = input$trip_id,
            string_no = as.integer(input$string_no),
            lat = input$lat,
            long = input$long,
            grid = input$grid,
            depth = input$depth,
            stringsAsFactors = FALSE
          )
          rv$strings <- unique(rbind(rv$strings, new_string))

          sample_rows <- lapply(seq_len(rv$sample_row_count), function(i) {
            lobster_no <- input[[paste0("lobster_no_", i)]]
            if (is.null(lobster_no) || is.na(lobster_no)) return(NULL)
            data.frame(
              trip_id = input$trip_id,
              string_no = as.integer(input$string_no),
              lobster_no = as.integer(lobster_no),
              length = input[[paste0("length_", i)]],
              hardness = input[[paste0("hardness_", i)]],
              egg = input[[paste0("egg_", i)]],
              pleopod = input[[paste0("pleopod_", i)]],
              ovary = input[[paste0("ovary_", i)]],
              comments = input[[paste0("comments_", i)]],
              stringsAsFactors = FALSE
            )
          })

          sample_rows <- Filter(Negate(is.null), sample_rows)
          if (length(sample_rows) > 0) rv$samples <- unique(rbind(rv$samples, do.call(rbind, sample_rows)))

          shiny::updateTextInput(session, "lat", value = "")
          shiny::updateTextInput(session, "long", value = "")
          shiny::updateTextInput(session, "grid", value = "")
          shiny::updateNumericInput(session, "depth", value = NA)
          shiny::updateNumericInput(session, "string_no", value = ifelse(is.na(input$string_no), NA, input$string_no + 1))

          if (rv$sample_row_count > 1) {
            for (i in 2:rv$sample_row_count) shiny::removeUI(selector = paste0("#sample_row_", i), immediate = TRUE)
          }
          rv$sample_row_count <- 1

          shiny::updateNumericInput(session, "lobster_no_1", value = NA)
          shiny::updateNumericInput(session, "length_1", value = NA)
          shiny::updateTextInput(session, "hardness_1", value = "")
          shiny::updateTextInput(session, "egg_1", value = "")
          shiny::updateTextInput(session, "pleopod_1", value = "")
          shiny::updateTextInput(session, "ovary_1", value = "")
          shiny::updateTextInput(session, "comments_1", value = "")
        })

        shiny::observeEvent(input$save_db, {
          shiny::req(nzchar(input$db_folder), !is.null(rv$trip), nrow(rv$trip) > 0, nzchar(rv$trip$trip_id[1]))
          if (!dir.exists(input$db_folder)) {
            rv$db_status <- paste("Folder does not exist:", input$db_folder)
            return()
          }

          db_path <- file.path(input$db_folder, paste0("MAT_", rv$trip$trip_id[1], ".db"))
          tryCatch({
            write_to_db(db_path, rv$trip, rv$strings, rv$samples)
            rv$db_status <- paste("Saved database:", db_path)
          }, error = function(e) {
            rv$db_status <- paste("Database save failed:", conditionMessage(e))
          })
        })

        shiny::observeEvent(input$load_db_file, {
          db_path_tcl <- tcltk::tkgetOpenFile(filetypes = "{{Database files} {.db}} {{All files} *}", title = "Choose trip database")
          db_path <- as.character(tcltk::tclvalue(db_path_tcl))
          if (!nzchar(db_path)) return()
          if (!file.exists(db_path)) {
            rv$db_status <- paste("Database file not found:", db_path)
            return()
          }

          tryCatch({
            loaded <- read_from_db(db_path)
            rv$trip <- loaded$trip
            rv$strings <- loaded$strings
            rv$samples <- loaded$samples

            shiny::updateTextInput(session, "trip_id", value = rv$trip$trip_id[1])
            shiny::updateTextInput(session, "trip_org", value = rv$trip$org[1])
            shiny::updateDateInput(session, "trip_date", value = rv$trip$date[1])
            shiny::updateTextInput(session, "trip_port", value = rv$trip$port[1])
            shiny::updateTextInput(session, "trip_lfa", value = rv$trip$lfa[1])
            shiny::updateTextInput(session, "trip_sampler", value = rv$trip$sampler[1])
            next_string <- if (nrow(rv$strings) > 0) max(rv$strings$string_no, na.rm = TRUE) + 1 else 1
            shiny::updateNumericInput(session, "string_no", value = next_string)
            loaded_folder <- dirname(normalizePath(db_path, winslash = "/", mustWork = FALSE))
            shiny::updateTextInput(session, "db_folder", value = loaded_folder)
            rv$has_selected_db_folder <- TRUE
            rv$db_status <- paste("Loaded database:", normalizePath(db_path, winslash = "/", mustWork = FALSE))
          }, error = function(e) {
            rv$db_status <- paste("Database load failed:", conditionMessage(e))
          })
        })

        output$db_status <- shiny::renderText(rv$db_status)
        output$trip_table <- shiny::renderTable(rv$trip)
        output$string_table <- shiny::renderTable(rv$strings)
        output$sample_table <- shiny::renderTable(rv$samples)
      }
    )
  )
}
