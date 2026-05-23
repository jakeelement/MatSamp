#' Launch the MatSamp multi-level data entry app
#'
#' Starts a Shiny app for entering hierarchical sampling data at three levels:
#' TRIP, STRING#, and SAMPLE, with export to a local SQL database file.
#'
#' @return A Shiny app launched in the current R session.
#' @import shiny
#' @import DBI
#' @import RSQLite
#' @import shinyFeedback
#' @export
matinput <- function() {
  na_if_empty <- function(x) if (is.character(x) && !nzchar(x)) NA_character_ else x
  format_lobster_id <- function(trip_id, string_no, lobster_no) {
    paste0(trip_id, sprintf("%02d%02d", as.integer(string_no), as.integer(lobster_no)))
  }

  with_topmost_tk <- function(f) {
    tt <- tcltk::tktoplevel()
    on.exit(tcltk::tkdestroy(tt))
    tcltk::tkwm.geometry(tt, "1x1+0+0")
    tcltk::tcl("wm", "attributes", tt, "-topmost", TRUE)
    tcltk::tkfocus(tt)
    f()
  }

  is_valid_ddmm <- function(x, type = c("lat", "long")) {
    type <- match.arg(type)
    if (!grepl("^\\d{4}\\.\\d{2}$", x)) return(FALSE)
    deg <- as.integer(substr(x, 1, 2))
    min <- as.integer(substr(x, 3, 4))
    if (min > 59) return(FALSE)
    if (type == "lat" && deg > 89) return(FALSE)
    TRUE
  }

  sample_row_ui <- function(i) {
    shiny::tags$div(
      id = paste0("sample_row_", i),
      shiny::fluidRow(
        shiny::column(width = 2, shiny::numericInput(paste0("lobster_no_", i), "LOBSTER #", value = NA, min = 1, step = 1)),
        shiny::column(width = 2, shiny::numericInput(paste0("length_", i), "LENGTH", value = NA, min = 0)),
        shiny::column(width = 2, shiny::selectInput(paste0("hardness_", i), "HARDNESS", choices = c("", 1:5), selected = "")),
        shiny::column(width = 2, shiny::selectInput(paste0("egg_", i), "EGG", choices = c("", 1:4), selected = "")),
        shiny::column(width = 2, shiny::selectInput(paste0("pleopod_", i), "PLEOPOD", choices = c("", 0:1), selected = "")),
        shiny::column(width = 2, shiny::selectInput(paste0("ovary_", i), "OVARY", choices = c("", 0:1), selected = ""))
      ),
      shiny::fluidRow(
        shiny::column(width = 12, shiny::textInput(paste0("comments_", i), "COMMENTS"))
      ),
      shiny::tags$hr()
    )
  }



  pleopod_row_ui <- function(i, lobster_id = "") {
    shiny::fluidRow(
      shiny::column(width = 3, htmltools::tagAppendAttributes(shiny::textInput(paste0("pl_lobster_id_", i), if (i == 1) "LOBSTER ID" else NULL, value = lobster_id), .cssSelector = "input", readonly = "readonly")),
      shiny::column(width = 2, shiny::textInput(paste0("pl_cg_stage_", i), if (i == 1) "CG STAGE" else NULL)),
      shiny::column(width = 2, shiny::textInput(paste0("pl_moult_stage_", i), if (i == 1) "MOULT STAGE" else NULL)),
      shiny::column(width = 3, shiny::textInput(paste0("pl_image_", i), if (i == 1) "PLEOPOD IMAGE FILE NAME(S)" else NULL)),
      shiny::column(width = 2, shiny::textInput(paste0("pl_observer_", i), if (i == 1) "OBSERVER" else NULL))
    )
  }

  ovary_row_ui <- function(i, lobster_id = "") {
    shiny::fluidRow(
      shiny::column(width = 2, htmltools::tagAppendAttributes(shiny::textInput(paste0("ov_lobster_id_", i), if (i == 1) "LOBSTER ID" else NULL, value = lobster_id), .cssSelector = "input", readonly = "readonly")),
      shiny::column(width = 1, shiny::numericInput(paste0("ov_length_", i), if (i == 1) "LENGTH" else NULL, value = NA, min = 0)),
      shiny::column(width = 1, shiny::numericInput(paste0("ov_whole_w_", i), if (i == 1) "Whole W." else NULL, value = NA, min = 0)),
      shiny::column(width = 1, shiny::textInput(paste0("ov_gastrolith_", i), if (i == 1) "GASTROLITH" else NULL)),
      shiny::column(width = 1, shiny::textInput(paste0("ov_colour_", i), if (i == 1) "OVARY COLOUR" else NULL)),
      shiny::column(width = 1, shiny::textInput(paste0("ov_yellow_", i), if (i == 1) "YELLOW SPOTS" else NULL)),
      shiny::column(width = 1, shiny::numericInput(paste0("ov_weight_", i), if (i == 1) "OVARY WEIGHT" else NULL, value = NA, min = 0)),
      shiny::column(width = 3, shiny::textInput(paste0("ov_image_", i), if (i == 1) "IMAGE NAME(S)/COMMENTS" else NULL)),
      shiny::column(width = 2, shiny::textInput(paste0("ov_observer_", i), if (i == 1) "OBSERVER" else NULL))
    )
  }

  write_to_db <- function(db_path, trip_df, string_df, sample_df, pleopod_df = data.frame(), ovary_df = data.frame()) {
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
        LOBSTER_ID TEXT NOT NULL,
        LENGTH REAL,
        HARDNESS TEXT,
        EGG TEXT,
        PLEOPOD TEXT,
        OVARY TEXT,
        COMMENTS TEXT,
        PRIMARY KEY (TRIPID, LOBSTER_ID),
        FOREIGN KEY (TRIPID, STRING_NO) REFERENCES STRING_INFO(TRIPID, STRING_NO)
      )
    ")

    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS LAB_PLEOPOD (
        TRIPID TEXT NOT NULL,
        LAB_COUNT TEXT,
        LAB_DATE TEXT,
        ROW_NO INTEGER NOT NULL,
        LOBSTER_ID TEXT,
        CG_STAGE TEXT,
        MOULT_STAGE TEXT,
        IMAGE_FILE TEXT,
        OBSERVER TEXT,
        PRIMARY KEY (TRIPID, ROW_NO),
        FOREIGN KEY (TRIPID, LOBSTER_ID) REFERENCES SAMPLE(TRIPID, LOBSTER_ID)
      )
    ")

    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS LAB_OVARY (
        TRIPID TEXT NOT NULL,
        LAB_COUNT TEXT,
        LAB_DATE TEXT,
        ROW_NO INTEGER NOT NULL,
        LOBSTER_ID TEXT,
        LENGTH REAL,
        WHOLE_W REAL,
        GASTROLITH TEXT,
        OVARY_COLOUR TEXT,
        YELLOW_SPOTS TEXT,
        OVARY_WEIGHT REAL,
        IMAGE_COMMENTS TEXT,
        OBSERVER TEXT,
        PRIMARY KEY (TRIPID, ROW_NO),
        FOREIGN KEY (TRIPID, LOBSTER_ID) REFERENCES SAMPLE(TRIPID, LOBSTER_ID)
      )
    ")

    if (!is.null(trip_df) && nrow(trip_df) > 0) {
      DBI::dbExecute(con, "DELETE FROM SAMPLE WHERE TRIPID = ?", params = list(trip_df$trip_id[1]))
      DBI::dbExecute(con, "DELETE FROM LAB_PLEOPOD WHERE TRIPID = ?", params = list(trip_df$trip_id[1]))
      DBI::dbExecute(con, "DELETE FROM LAB_OVARY WHERE TRIPID = ?", params = list(trip_df$trip_id[1]))
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
          LOBSTER_ID = sample_df$lobster_id,
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

    if (nrow(pleopod_df) > 0) {
      DBI::dbWriteTable(con, "LAB_PLEOPOD", pleopod_df, append = TRUE, row.names = FALSE)
    }

    if (nrow(ovary_df) > 0) {
      DBI::dbWriteTable(con, "LAB_OVARY", ovary_df, append = TRUE, row.names = FALSE)
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
      DBI::dbGetQuery(con, "SELECT TRIPID, STRING_NO, LOBSTER_ID, LENGTH, HARDNESS, EGG, PLEOPOD, OVARY, COMMENTS FROM SAMPLE WHERE TRIPID = ? ORDER BY LOBSTER_ID", params = list(trip_id))
    } else {
      data.frame()
    }

    pleopod_df <- if (DBI::dbExistsTable(con, "LAB_PLEOPOD")) {
      DBI::dbGetQuery(con, "SELECT * FROM LAB_PLEOPOD WHERE TRIPID = ? ORDER BY ROW_NO", params = list(trip_id))
    } else {
      data.frame()
    }

    ovary_df <- if (DBI::dbExistsTable(con, "LAB_OVARY")) {
      DBI::dbGetQuery(con, "SELECT * FROM LAB_OVARY WHERE TRIPID = ? ORDER BY ROW_NO", params = list(trip_id))
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
        trip_id = sample_df$TRIPID, string_no = sample_df$STRING_NO, lobster_id = sample_df$LOBSTER_ID,
        length = sample_df$LENGTH, hardness = sample_df$HARDNESS, egg = sample_df$EGG,
        pleopod = sample_df$PLEOPOD, ovary = sample_df$OVARY, comments = sample_df$COMMENTS, stringsAsFactors = FALSE
      ),
      pleopod = pleopod_df,
      ovary = ovary_df
    )
  }

  trip_ui <- shiny::tagList(
    shiny::h3("TRIP INFORMATION"),
    shiny::fluidRow(
      shiny::column(width = 3, htmltools::tagAppendAttributes(shiny::textInput("trip_id", "TRIPID"), .cssSelector = "input", readonly = "readonly")),
      shiny::column(width = 3, shiny::textInput("trip_org", "ORG.")),
      shiny::column(width = 3, shiny::dateInput("trip_date", "DATE")),
      shiny::column(width = 3, shiny::textInput("trip_port", "PORT"))
    ),
    shiny::fluidRow(
      shiny::column(width = 3, shiny::selectInput("trip_lfa", "LFA",
        choices = c("", "27", "28", "29", "30", "31A", "31B", "32", "33", "34", "35", "36", "37A", "37B", "38"),
        selected = "")),
      shiny::column(width = 6, shiny::textInput("trip_sampler", "SAMPLER")),
      shiny::column(width = 3, shiny::actionButton("save_trip", "Create / Update Trip", class = "btn-primary"))
    )
  )



  lab_info_ui <- shiny::tagList(
    shiny::h3("LAB INFORMATION"),
    shiny::fluidRow(
      shiny::column(width = 4, htmltools::tagAppendAttributes(shiny::textInput("lab_trip_id", "TRIPID"), .cssSelector = "input", readonly = "readonly")),
      shiny::column(width = 4, shiny::textInput("lab_count", "# PLEOPODS / #Ovary")),
      shiny::column(width = 4, shiny::textInput("lab_date", "LAB DATE (DDMMYY)"))
    )
  )

  location_ui <- shiny::tagList(
    shiny::tags$hr(),
    shiny::h3("LOCATION INFORMATION (STRING#)"),
    shiny::fluidRow(
      shiny::column(width = 3, shiny::numericInput("string_no", "STRING #", value = NA, min = 1, step = 1)),
      shiny::column(width = 3, htmltools::tagAppendAttributes(shiny::textInput("lat", "LAT (DDMM.MM)"), .cssSelector = "input", maxlength = "7")),
      shiny::column(width = 3, htmltools::tagAppendAttributes(shiny::textInput("long", "LONG (DDMM.MM)"), .cssSelector = "input", maxlength = "7")),
      shiny::column(width = 3, shiny::numericInput("grid", "GRID", value = NA, min = 1, step = 1))
    ),
    shiny::fluidRow(
      shiny::column(width = 3, shiny::numericInput("depth", "DEPTH (FM)", value = NA, min = 0)),
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

            function preventBelowMin(target) {
              $(target).on('input', function() {
                var val = parseFloat(this.value);
                var min = parseFloat(this.min);
                if (!isNaN(val) && !isNaN(min) && val < min) {
                  this.value = min;
                  $(this).trigger('change');
                }
              });
            }

            $('input[type=\"number\"]').each(function() {
              disableNumericScroll(this);
              preventBelowMin(this);
            });

            const observer = new MutationObserver(function(mutations) {
              mutations.forEach(function(mutation) {
                mutation.addedNodes.forEach(function(node) {
                  if (!(node instanceof Element)) return;
                  if ($(node).is('input[type=\"number\"]')) {
                    disableNumericScroll(node);
                    preventBelowMin(node);
                  }
                  $(node).find('input[type=\"number\"]').each(function() {
                    disableNumericScroll(this);
                    preventBelowMin(this);
                  });
                });
              });
            });

            observer.observe(document.body, { childList: true, subtree: true });
          });
        ")),
        shinyFeedback::useShinyFeedback(),
        shiny::tags$script(shiny::HTML("
          Shiny.addCustomMessageHandler('toggleButtons', function(msg) {
            $('button').prop('disabled', msg.disabled);
          });
        ")),
        shiny::fluidRow(
          shiny::column(12, shiny::h3("Choose Input Window")),
          shiny::column(4, shiny::actionButton("choose_atsea", "At-Sea Sample", class = "btn-primary")),
          shiny::column(4, shiny::actionButton("choose_pleopod", "Lab Pleopod Sample", class = "btn-primary")),
          shiny::column(4, shiny::actionButton("choose_ovary", "Lab Ovary Sample", class = "btn-primary"))
        ),
        shiny::tags$hr(),
        shiny::uiOutput("form_ui"),

      ),
      server = function(input, output, session) {

        rv <- shiny::reactiveValues(
          form_type = NULL,
          trip = NULL,
          strings = data.frame(),
          samples = data.frame(),
          lab_pleopod = data.frame(),
          lab_ovary = data.frame(),
          lab_lobster_ids = character(),
          lab_context_loaded = FALSE,
          db_folder_path = "",
          sample_row_count = 1,
          db_status = "",
          has_selected_db_folder = FALSE
        )

        fill_atsea_for_string <- function(string_no) {
          if (is.null(rv$trip) || nrow(rv$trip) == 0 || is.na(string_no)) return()
          srow <- rv$strings[rv$strings$string_no == as.integer(string_no), , drop = FALSE]
          if (nrow(srow) > 0) {
            shiny::updateTextInput(session, "lat", value = ifelse(is.na(srow$lat[1]), "", srow$lat[1]))
            shiny::updateTextInput(session, "long", value = ifelse(is.na(srow$long[1]), "", srow$long[1]))
            shiny::updateNumericInput(session, "grid", value = srow$grid[1])
            shiny::updateNumericInput(session, "depth", value = srow$depth[1])
          }
          sample_rows <- rv$samples[rv$samples$string_no == as.integer(string_no), , drop = FALSE]
          sample_rows <- sample_rows[order(sample_rows$lobster_id), , drop = FALSE]
          target_n <- max(1, nrow(sample_rows))
          if (rv$sample_row_count < target_n) {
            for (i in seq((rv$sample_row_count + 1), target_n)) {
              shiny::insertUI(selector = "#sample_rows_container", where = "beforeEnd", ui = sample_row_ui(i))
            }
            rv$sample_row_count <- target_n
          } else if (rv$sample_row_count > target_n) {
            for (i in seq((target_n + 1), rv$sample_row_count)) {
              shiny::removeUI(selector = paste0("#sample_row_", i), immediate = TRUE)
            }
            rv$sample_row_count <- target_n
          }
          for (i in seq_len(rv$sample_row_count)) {
            if (i <= nrow(sample_rows)) {
              lid <- as.character(sample_rows$lobster_id[i])
              lobster_no <- suppressWarnings(as.integer(substr(lid, nchar(rv$trip$trip_id[1]) + 3, nchar(rv$trip$trip_id[1]) + 4)))
              shiny::updateNumericInput(session, paste0("lobster_no_", i), value = lobster_no)
              shiny::updateNumericInput(session, paste0("length_", i), value = sample_rows$length[i])
              shiny::updateSelectInput(session, paste0("hardness_", i), selected = ifelse(is.na(sample_rows$hardness[i]), "", sample_rows$hardness[i]))
              shiny::updateSelectInput(session, paste0("egg_", i), selected = ifelse(is.na(sample_rows$egg[i]), "", sample_rows$egg[i]))
              shiny::updateSelectInput(session, paste0("pleopod_", i), selected = ifelse(is.na(sample_rows$pleopod[i]), "", sample_rows$pleopod[i]))
              shiny::updateSelectInput(session, paste0("ovary_", i), selected = ifelse(is.na(sample_rows$ovary[i]), "", sample_rows$ovary[i]))
              shiny::updateTextInput(session, paste0("comments_", i), value = ifelse(is.na(sample_rows$comments[i]), "", sample_rows$comments[i]))
            } else {
              shiny::updateNumericInput(session, paste0("lobster_no_", i), value = NA)
              shiny::updateNumericInput(session, paste0("length_", i), value = NA)
              shiny::updateSelectInput(session, paste0("hardness_", i), selected = "")
              shiny::updateSelectInput(session, paste0("egg_", i), selected = "")
              shiny::updateSelectInput(session, paste0("pleopod_", i), selected = "")
              shiny::updateSelectInput(session, paste0("ovary_", i), selected = "")
              shiny::updateTextInput(session, paste0("comments_", i), value = "")
            }
          }
        }

        fill_lab_forms <- function() {
          if (is.null(rv$trip) || nrow(rv$trip) == 0) return()
          shiny::updateTextInput(session, "lab_trip_id", value = rv$trip$trip_id[1])
          rv$lab_lobster_ids <- unique(rv$samples$lobster_id)
          if (nrow(rv$lab_pleopod) > 0) {
            shiny::updateTextInput(session, "lab_count", value = as.character(rv$lab_pleopod$LAB_COUNT[1]))
            shiny::updateTextInput(session, "lab_date", value = as.character(rv$lab_pleopod$LAB_DATE[1]))
          } else if (nrow(rv$lab_ovary) > 0) {
            shiny::updateTextInput(session, "lab_count", value = as.character(rv$lab_ovary$LAB_COUNT[1]))
            shiny::updateTextInput(session, "lab_date", value = as.character(rv$lab_ovary$LAB_DATE[1]))
          }
        }

        fill_lab_row_values <- function() {
          if (identical(rv$form_type, "pleopod") && nrow(rv$lab_pleopod) > 0) {
            for (i in seq_len(min(length(rv$lab_lobster_ids), nrow(rv$lab_pleopod)))) {
              shiny::updateTextInput(session, paste0("pl_lobster_id_", i), value = rv$lab_lobster_ids[i])
              shiny::updateTextInput(session, paste0("pl_cg_stage_", i), value = rv$lab_pleopod$CG_STAGE[i])
              shiny::updateTextInput(session, paste0("pl_moult_stage_", i), value = rv$lab_pleopod$MOULT_STAGE[i])
              shiny::updateTextInput(session, paste0("pl_image_", i), value = rv$lab_pleopod$IMAGE_FILE[i])
              shiny::updateTextInput(session, paste0("pl_observer_", i), value = rv$lab_pleopod$OBSERVER[i])
            }
          }
          if (identical(rv$form_type, "ovary") && nrow(rv$lab_ovary) > 0) {
            for (i in seq_len(min(length(rv$lab_lobster_ids), nrow(rv$lab_ovary)))) {
              shiny::updateTextInput(session, paste0("ov_lobster_id_", i), value = rv$lab_lobster_ids[i])
              shiny::updateNumericInput(session, paste0("ov_length_", i), value = rv$lab_ovary$LENGTH[i])
              shiny::updateNumericInput(session, paste0("ov_whole_w_", i), value = rv$lab_ovary$WHOLE_W[i])
              shiny::updateTextInput(session, paste0("ov_gastrolith_", i), value = rv$lab_ovary$GASTROLITH[i])
              shiny::updateTextInput(session, paste0("ov_colour_", i), value = rv$lab_ovary$OVARY_COLOUR[i])
              shiny::updateTextInput(session, paste0("ov_yellow_", i), value = rv$lab_ovary$YELLOW_SPOTS[i])
              shiny::updateNumericInput(session, paste0("ov_weight_", i), value = rv$lab_ovary$OVARY_WEIGHT[i])
              shiny::updateTextInput(session, paste0("ov_image_", i), value = rv$lab_ovary$IMAGE_COMMENTS[i])
              shiny::updateTextInput(session, paste0("ov_observer_", i), value = rv$lab_ovary$OBSERVER[i])
            }
          }
        }


        output$form_ui <- shiny::renderUI({
          if (is.null(rv$form_type)) return(NULL)
          common_top <- shiny::tagList(
            shiny::tags$h4("Load Existing Trip"),
            shiny::actionButton("load_db_file", "Choose .db File", class = "btn-secondary")
          )
          if (identical(rv$form_type, "atsea")) {
            return(shiny::tagList(
              shiny::titlePanel("CLRN SOM50 AT-SEA SAMPLE DATA FORM"), common_top,
              trip_ui, location_ui, sample_ui, export_ui,
              shiny::tags$hr(), shiny::h4("Current Trip"), shiny::tableOutput("trip_table"),
              shiny::h4("Current Strings"), shiny::tableOutput("string_table"),
              shiny::h4("Current Samples"), shiny::tableOutput("sample_table")
            ))
          }
          if (identical(rv$form_type, "pleopod")) {
            return(shiny::tagList(shiny::titlePanel("CLRN SOM50 LAB PLEOPOD SAMPLE DATA FORM"), common_top,
              lab_info_ui,
              shiny::h3("SAMPLE INFORMATION"),
              shiny::uiOutput("pleopod_rows"), export_ui, shiny::tableOutput("pleopod_table")))
          }
          shiny::tagList(shiny::titlePanel("CLRN SOM50 LAB OVARY SAMPLE DATA FORM"), common_top,
            lab_info_ui,
            shiny::h3("SAMPLE INFORMATION"),
            shiny::uiOutput("ovary_rows"), export_ui, shiny::tableOutput("ovary_table"))
        })
        shiny::observeEvent(input$choose_atsea, {
          rv$form_type <- "atsea"
          if (!is.null(rv$trip) && nrow(rv$trip) > 0) {
            shiny::updateTextInput(session, "trip_org", value = rv$trip$org[1])
            shiny::updateDateInput(session, "trip_date", value = rv$trip$date[1])
            shiny::updateTextInput(session, "trip_port", value = rv$trip$port[1])
            shiny::updateSelectInput(session, "trip_lfa", selected = rv$trip$lfa[1])
            shiny::updateTextInput(session, "trip_sampler", value = rv$trip$sampler[1])
            first_string <- if (nrow(rv$strings) > 0) min(rv$strings$string_no, na.rm = TRUE) else 1
            shiny::updateNumericInput(session, "string_no", value = first_string)
            fill_atsea_for_string(first_string)
          }
        })
        shiny::observeEvent(input$choose_pleopod, {
          rv$form_type <- "pleopod"
          fill_lab_forms()
        })
        shiny::observeEvent(input$choose_ovary, {
          rv$form_type <- "ovary"
          fill_lab_forms()
        })
        output$pleopod_rows <- shiny::renderUI({
          ids <- rv$lab_lobster_ids
          if (length(ids) == 0) return(NULL)
          shiny::tagList(lapply(seq_along(ids), function(i) pleopod_row_ui(i, ids[[i]])))
        })
        output$ovary_rows <- shiny::renderUI({
          ids <- rv$lab_lobster_ids
          if (length(ids) == 0) return(NULL)
          shiny::tagList(lapply(seq_along(ids), function(i) ovary_row_ui(i, ids[[i]])))
        })
        shiny::observeEvent(list(rv$form_type, rv$lab_lobster_ids, rv$lab_pleopod, rv$lab_ovary), {
          if (nzchar(rv$db_folder_path)) shiny::updateTextInput(session, "db_folder", value = rv$db_folder_path)
          fill_lab_row_values()
        }, ignoreInit = TRUE)

        shiny::observe({
          shiny::req(!is.null(input$trip_org), !is.null(input$trip_date))
          org  <- input$trip_org
          date <- input$trip_date
          trip_id <- if (!is.null(date) && nzchar(org)) {
            paste0(org, format(date, "%d%m%y"))
          } else {
            ""
          }
          shiny::updateTextInput(session, "trip_id", value = trip_id)
        })

        shiny::observe({
          if (!identical(rv$form_type, "atsea")) return()
          lat <- input$lat
          if (is.null(lat) || length(lat) == 0 || !nzchar(lat)) {
            shinyFeedback::hideFeedback("lat")
          } else {
            shinyFeedback::feedbackDanger("lat", !is_valid_ddmm(lat, "lat"),
              "Must be DDMM.MM — degrees 00–89, minutes 00–59 (e.g. 4430.55)")
          }
        })

        shiny::observe({
          if (!identical(rv$form_type, "atsea")) return()
          long <- input$long
          if (is.null(long) || length(long) == 0 || !nzchar(long)) {
            shinyFeedback::hideFeedback("long")
          } else {
            shinyFeedback::feedbackDanger("long", !is_valid_ddmm(long, "long"),
              "Must be DDMM.MM — minutes 00–59 (e.g. 6645.20)")
          }
        })

        shiny::observe({
          if (!identical(rv$form_type, "atsea")) return()
          n      <- rv$sample_row_count
          values <- sapply(seq_len(n), function(i) input[[paste0("lobster_no_", i)]])
          non_na <- values[!is.na(values)]
          dupes  <- unique(non_na[duplicated(non_na)])
          for (i in seq_len(n)) {
            v <- values[[i]]
            shinyFeedback::feedbackDanger(
              paste0("lobster_no_", i),
              !is.na(v) && v %in% dupes,
              "Duplicate LOBSTER #"
            )
          }
        })

        shiny::observe({
          if (!identical(rv$form_type, "atsea")) {
            session$sendCustomMessage("toggleButtons", list(disabled = FALSE))
            return()
          }

          lat_val <- input$lat
          long_val <- input$long
          lat_error  <- !is.null(lat_val) && length(lat_val) > 0 && nzchar(lat_val) && !is_valid_ddmm(lat_val,  "lat")
          long_error <- !is.null(long_val) && length(long_val) > 0 && nzchar(long_val) && !is_valid_ddmm(long_val, "long")

          n      <- rv$sample_row_count
          values <- sapply(seq_len(n), function(i) input[[paste0("lobster_no_", i)]])
          non_na <- values[!is.na(values)]
          lobster_error <- any(duplicated(non_na))

          session$sendCustomMessage("toggleButtons", list(disabled = lat_error || long_error || lobster_error))
        })

        shiny::observe({
          if (!identical(rv$form_type, "atsea")) return()
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
          selected_dir <- with_topmost_tk(function() tcltk::tk_choose.dir(default = input$db_folder, caption = "Choose save directory"))
          if (!is.null(selected_dir) && nzchar(selected_dir)) {
            shiny::updateTextInput(session, "db_folder", value = selected_dir)
            rv$has_selected_db_folder <- TRUE
            rv$db_folder_path <- selected_dir
            rv$db_status <- paste("Save directory selected:", selected_dir)
          }
        })
        shiny::observeEvent(input$save_trip, {
          rv$trip <- data.frame(
            trip_id = na_if_empty(input$trip_id),
            org = na_if_empty(input$trip_org),
            date = na_if_empty(as.character(input$trip_date)),
            port = na_if_empty(input$trip_port),
            lfa = na_if_empty(input$trip_lfa),
            sampler = na_if_empty(input$trip_sampler),
            stringsAsFactors = FALSE
          )
        })

        shiny::observeEvent(input$next_string, {
          shiny::req(!is.null(rv$trip), nzchar(input$trip_id), !is.na(input$string_no))

          new_string <- data.frame(
            trip_id = input$trip_id,
            string_no = as.integer(input$string_no),
            lat = na_if_empty(input$lat),
            long = na_if_empty(input$long),
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
              lobster_id = format_lobster_id(input$trip_id, input$string_no, lobster_no),
              length = input[[paste0("length_", i)]],
              hardness = na_if_empty(input[[paste0("hardness_", i)]]),
              egg = na_if_empty(input[[paste0("egg_", i)]]),
              pleopod = na_if_empty(input[[paste0("pleopod_", i)]]),
              ovary = na_if_empty(input[[paste0("ovary_", i)]]),
              comments = na_if_empty(input[[paste0("comments_", i)]]),
              stringsAsFactors = FALSE
            )
          })

          sample_rows <- Filter(Negate(is.null), sample_rows)
          if (length(sample_rows) > 0) rv$samples <- unique(rbind(rv$samples, do.call(rbind, sample_rows)))

          shiny::updateTextInput(session, "lat", value = "")
          shiny::updateTextInput(session, "long", value = "")
          shiny::updateNumericInput(session, "grid", value = NA)
          shiny::updateNumericInput(session, "depth", value = NA)
          shiny::updateNumericInput(session, "string_no", value = ifelse(is.na(input$string_no), NA, input$string_no + 1))

          if (rv$sample_row_count > 1) {
            for (i in 2:rv$sample_row_count) shiny::removeUI(selector = paste0("#sample_row_", i), immediate = TRUE)
          }
          rv$sample_row_count <- 1

          shiny::updateNumericInput(session, "lobster_no_1", value = NA)
          shiny::updateNumericInput(session, "length_1", value = NA)
          shiny::updateSelectInput(session, "hardness_1", selected = "")
          shiny::updateSelectInput(session, "egg_1", selected = "")
          shiny::updateSelectInput(session, "pleopod_1", selected = "")
          shiny::updateSelectInput(session, "ovary_1", selected = "")
          shiny::updateTextInput(session, "comments_1", value = "")
        })

        shiny::observeEvent(input$string_no, {
          if (!identical(rv$form_type, "atsea")) return()
          shiny::req(!is.null(rv$trip), nrow(rv$trip) > 0, !is.na(input$string_no))
          fill_atsea_for_string(input$string_no)
        }, ignoreInit = TRUE)

        shiny::observeEvent(input$save_db, {
          shiny::req(nzchar(input$db_folder), !is.null(rv$trip), nrow(rv$trip) > 0, nzchar(rv$trip$trip_id[1]))
          if (!dir.exists(input$db_folder)) {
            rv$db_status <- paste("Folder does not exist:", input$db_folder)
            return()
          }

          db_path <- file.path(input$db_folder, paste0("MAT_", rv$trip$trip_id[1], ".db"))
          tryCatch({
            if (identical(rv$form_type, "pleopod")) {
              rv$lab_pleopod <- do.call(rbind, lapply(seq_along(rv$lab_lobster_ids), function(i) {
                data.frame(
                  TRIPID = rv$trip$trip_id[1],
                  LAB_COUNT = na_if_empty(input$lab_count),
                  LAB_DATE = na_if_empty(input$lab_date),
                  ROW_NO = i,
                  LOBSTER_ID = rv$lab_lobster_ids[i],
                  CG_STAGE = na_if_empty(input[[paste0("pl_cg_stage_", i)]]),
                  MOULT_STAGE = na_if_empty(input[[paste0("pl_moult_stage_", i)]]),
                  IMAGE_FILE = na_if_empty(input[[paste0("pl_image_", i)]]),
                  OBSERVER = na_if_empty(input[[paste0("pl_observer_", i)]]),
                  stringsAsFactors = FALSE
                )
              }))
            }

            if (identical(rv$form_type, "ovary")) {
              rv$lab_ovary <- do.call(rbind, lapply(seq_along(rv$lab_lobster_ids), function(i) {
                data.frame(
                  TRIPID = rv$trip$trip_id[1],
                  LAB_COUNT = na_if_empty(input$lab_count),
                  LAB_DATE = na_if_empty(input$lab_date),
                  ROW_NO = i,
                  LOBSTER_ID = rv$lab_lobster_ids[i],
                  LENGTH = input[[paste0("ov_length_", i)]],
                  WHOLE_W = input[[paste0("ov_whole_w_", i)]],
                  GASTROLITH = na_if_empty(input[[paste0("ov_gastrolith_", i)]]),
                  OVARY_COLOUR = na_if_empty(input[[paste0("ov_colour_", i)]]),
                  YELLOW_SPOTS = na_if_empty(input[[paste0("ov_yellow_", i)]]),
                  OVARY_WEIGHT = input[[paste0("ov_weight_", i)]],
                  IMAGE_COMMENTS = na_if_empty(input[[paste0("ov_image_", i)]]),
                  OBSERVER = na_if_empty(input[[paste0("ov_observer_", i)]]),
                  stringsAsFactors = FALSE
                )
              }))
            }

            write_to_db(db_path, rv$trip, rv$strings, rv$samples, rv$lab_pleopod, rv$lab_ovary)
            rv$db_status <- paste("Saved database:", db_path)
          }, error = function(e) {
            rv$db_status <- paste("Database save failed:", conditionMessage(e))
          })
        })

        shiny::observeEvent(input$load_db_file, {
          db_path_tcl <- with_topmost_tk(function() tcltk::tkgetOpenFile(filetypes = "{{Database files} {.db}} {{All files} *}", title = "Choose trip database"))
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
            rv$lab_pleopod <- loaded$pleopod
            rv$lab_ovary <- loaded$ovary

            shiny::updateTextInput(session, "trip_org", value = rv$trip$org[1])
            shiny::updateDateInput(session, "trip_date", value = rv$trip$date[1])
            shiny::updateTextInput(session, "trip_port", value = rv$trip$port[1])
            shiny::updateSelectInput(session, "trip_lfa", selected = rv$trip$lfa[1])
            shiny::updateTextInput(session, "trip_sampler", value = rv$trip$sampler[1])
            rv$lab_context_loaded <- TRUE
            fill_lab_forms()
            fill_lab_row_values()
            first_string <- if (nrow(rv$strings) > 0) min(rv$strings$string_no, na.rm = TRUE) else 1
            shiny::updateNumericInput(session, "string_no", value = first_string)
            fill_atsea_for_string(first_string)
            loaded_folder <- dirname(normalizePath(db_path, winslash = "/", mustWork = FALSE))
            shiny::updateTextInput(session, "db_folder", value = loaded_folder)
            rv$db_folder_path <- loaded_folder
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
        output$pleopod_table <- shiny::renderTable(rv$lab_pleopod)
        output$ovary_table <- shiny::renderTable(rv$lab_ovary)
      }
    )
  )
}
