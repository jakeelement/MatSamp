#' Launch the lab pleopod sample data entry app
#'
#' Starts a standalone Shiny app for entering lab pleopod data. Each lobster row
#' is entered directly and can be assigned its own trip and location information
#' through the row's Add Location / Change Location button. Lab sampling
#' databases are named from LAB DATE as `MAT_LAB_ddmmyy.db`.
#'
#' @return A Shiny app launched in the current R session.
#' @import shiny
#' @import DBI
#' @import RSQLite
#' @import shinyjs
#' @import shinyFeedback
#' @export
pleopod <- function() {

  # --- UI helpers -----------------------------------------------------------

  location_empty <- function() {
    data.frame(TRIPID = NA_character_, ORG = NA_character_, TRIP_DATE = NA_character_,
               PORT = NA_character_, LFA = NA_character_, SAMPLER = NA_character_,
               LAT = NA_character_, LONG = NA_character_, GRID = NA_real_, DEPTH = NA_real_,
               stringsAsFactors = FALSE)
  }

  pleopod_row_ui <- function(i, lobster_no = NA, carapace_length = NA, cg_stage = "", moult_stage = "", image = "",
                             observer_val = "", comments = "", trip_id = "", has_location = FALSE) {
    btn_class <- if (isTRUE(has_location)) "btn-success" else "btn-secondary"
    btn_label <- if (isTRUE(has_location)) "Change Location" else "Add Location"
    shiny::tags$div(
      id = paste0("pleopod_row_", i),
      shiny::fluidRow(
        shiny::column(width = 2, shiny::numericInput(paste0("pl_lobster_no_", i), if (i == 1) "LOBSTER NUMBER" else NULL,
          value = lobster_no, min = 1, step = 1)),
        shiny::column(width = 2, shiny::numericInput(paste0("pl_carapace_length_", i), if (i == 1) "CARAPACE LENGTH (MM)" else NULL,
                                                     value = carapace_length, min = 0)),
        shiny::column(width = 1, shiny::selectInput(paste0("pl_cg_stage_", i), if (i == 1) "CG STAGE" else NULL, choices = c("", "1", "2", "3", "4", "9"), selected = cg_stage)),
        shiny::column(width = 2, shiny::selectInput(paste0("pl_moult_stage_", i), if (i == 1) "MOULT STAGE" else NULL, choices = c("", "0", "1.0", "1.5", "2.0", "2.5", "3.0", "3.5", "4.0", "4.5", "5.0", "5.5"), selected = moult_stage)),
        shiny::column(width = 2, shiny::textInput(paste0("pl_image_", i), if (i == 1) "PLEOPOD IMAGE FILE NAME(S)" else NULL, value = image), shiny::actionButton(paste0("pl_image_select_", i), "Select Image", class = "btn-secondary btn-xs")),
        shiny::column(width = 1, shiny::textInput(paste0("pl_observer_", i), if (i == 1) "OBSERVER" else NULL, value = observer_val)),
        shiny::column(width = 2, shiny::textInput(paste0("pl_comments_", i), if (i == 1) "COMMENTS" else NULL, value = comments)),
        shiny::column(width = 2, shiny::actionButton(paste0("pl_location_", i), btn_label, class = btn_class)),
        shiny::column(width = 2, htmltools::tagAppendAttributes(
          shiny::textInput(paste0("pl_trip_id_", i), if (i == 1) "TRIPID" else NULL, value = trip_id),
          .cssSelector = "input", readonly = "readonly"))
      ),
      shiny::tags$hr()
    )
  }

  lab_date_from_db_path <- function(db_path) {
    base <- basename(db_path)
    matched <- regexec("^MAT_LAB_([0-9]{6})\\.db$", base, ignore.case = TRUE)
    parts <- regmatches(base, matched)[[1]]
    if (length(parts) == 2) parts[[2]] else NA_character_
  }

  lab_date_to_date <- function(lab_date) {
    if (is.null(lab_date) || is.na(lab_date) || !grepl("^[0-9]{6}$", lab_date)) return(NA)
    as.Date(lab_date, format = "%d%m%y")
  }

  location_modal_ui <- function(row_no, location) {
    shiny::modalDialog(
      title = paste("Trip Information for Lobster Row", row_no),
      size = "l", easyClose = TRUE,
      shiny::h4("TRIP INFORMATION"),
      shiny::fluidRow(
        shiny::column(width = 3, htmltools::tagAppendAttributes(shiny::textInput("loc_trip_id", "TRIPID", value = chr_or_empty(location$TRIPID[1])), .cssSelector = "input", readonly = "readonly")),
        shiny::column(width = 3, shiny::textInput("loc_org", "ORG.", value = chr_or_empty(location$ORG[1]))),
        shiny::column(width = 3, shiny::dateInput("loc_date", "DATE", value = chr_or_empty(location$TRIP_DATE[1]))),
        shiny::column(width = 3, shiny::textInput("loc_port", "PORT", value = chr_or_empty(location$PORT[1])))
      ),
      shiny::fluidRow(
        shiny::column(width = 3, shiny::selectInput("loc_lfa", "LFA",
          choices = c("", "27", "28", "29", "30", "31A", "31B", "32", "33", "34", "35", "36", "37A", "37B", "38","40","41"),
          selected = chr_or_empty(location$LFA[1]))),
        shiny::column(width = 3, shiny::textInput("loc_sampler", "SAMPLER", value = chr_or_empty(location$SAMPLER[1]))),
        shiny::column(width = 3, htmltools::tagAppendAttributes(shiny::textInput("loc_lat", "LAT (DDMM.MM)", value = chr_or_empty(location$LAT[1])), .cssSelector = "input", maxlength = "7")),
        shiny::column(width = 3, htmltools::tagAppendAttributes(shiny::textInput("loc_long", "LONG (DDMM.MM)", value = chr_or_empty(location$LONG[1])), .cssSelector = "input", maxlength = "7"))
      ),
      shiny::fluidRow(
        shiny::column(width = 3, shiny::numericInput("loc_grid", "GRID", value = location$GRID[1], min = 1, step = 1)),
        shiny::column(width = 3, shiny::numericInput("loc_depth", "DEPTH (FM)", value = location$DEPTH[1], min = 0))
      ),
      footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("save_location", "Add Location", class = "btn-primary"))
    )
  }

  # --- DB helpers -----------------------------------------------------------

  db_ensure_pleopod_tables <- function(con) {
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS MAT_TRIP (
        TRIPID TEXT PRIMARY KEY, ORG TEXT, TRIP_DATE TEXT, PORT TEXT, LFA TEXT, SAMPLER TEXT,
        LAT TEXT, LONG TEXT, GRID REAL, DEPTH REAL
      )")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS MAT_PLEOPOD (
        TRIPID TEXT, LAB_DATE TEXT, ROW_NO INTEGER NOT NULL, LOBSTER_NO INTEGER, CARAPACE_LENGTH REAL,
        CG_STAGE TEXT, MOULT_STAGE TEXT, IMAGE_FILE TEXT, OBSERVER TEXT, COMMENTS TEXT,
        PRIMARY KEY (ROW_NO),
        FOREIGN KEY (TRIPID) REFERENCES MAT_TRIP(TRIPID)
      )")
  }

  read_pleopod_db <- function(db_path) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    db_ensure_pleopod_tables(con)
    if (!DBI::dbExistsTable(con, "MAT_PLEOPOD")) stop("MAT_PLEOPOD table not found")
    trip_df <- if (DBI::dbExistsTable(con, "MAT_TRIP")) DBI::dbReadTable(con, "MAT_TRIP") else data.frame()
    pleopod_df <- DBI::dbGetQuery(con, "SELECT * FROM MAT_PLEOPOD ORDER BY ROW_NO")
    list(trips = trip_df, pleopod = pleopod_df)
  }

  write_pleopod_db <- function(db_path, trip_df, pleopod_df) {
    
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    
    db_ensure_pleopod_tables(con)
    
    trip_df <- trip_df[!is.na(trip_df$TRIPID) & trip_df$TRIPID != "", ]
    
    DBI::dbWithTransaction(con, {
      
      ## ----- Upsert MAT_TRIP -----
      
      if (nrow(trip_df) > 0) {
        
        trip_cols <- names(trip_df)
        update_cols <- setdiff(trip_cols, "TRIPID")
        
        sql <- paste0(
          "INSERT INTO MAT_TRIP (",
          paste(trip_cols, collapse = ", "),
          ") VALUES (",
          paste0(":", trip_cols, collapse = ", "),
          ") ON CONFLICT(TRIPID) DO UPDATE SET ",
          paste(
            sprintf("%s = excluded.%s", update_cols, update_cols),
            collapse = ", "
          )
        )
        
        for (i in seq_len(nrow(trip_df))) {
          DBI::dbExecute(
            con,
            sql,
            params = as.list(trip_df[i, trip_cols, drop = FALSE])
          )
        }
      }
      
      ## ----- Replace MAT_PLEOPOD -----
      
      DBI::dbExecute(con, "DELETE FROM MAT_PLEOPOD")
      
      if (nrow(pleopod_df) > 0) {
        DBI::dbWriteTable(
          con,
          "MAT_PLEOPOD",
          pleopod_df,
          append = TRUE,
          row.names = FALSE
        )
      }
      
    })
  }

  # --- UI -------------------------------------------------------------------

  ui <- shiny::fluidPage(
    mat_js_tags(),
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('toggleButtons', function(msg) {
        $('button').prop('disabled', msg.disabled);
      });
      Shiny.addCustomMessageHandler('toggleSaveLocation', function(msg) {
        $('#save_location').prop('disabled', msg.disabled);
      });
      Shiny.addCustomMessageHandler('setLocationButton', function(msg) {
        var button = $('#' + msg.id);
        button.text(msg.label);
        button.removeClass('btn-secondary btn-success');
        button.addClass(msg.className);
      });
    ")),
    shiny::titlePanel("CLRN SOM50 LAB PLEOPOD SAMPLE DATA FORM"),
    shiny::h4("Load Existing Lab Database"),
    shiny::actionButton("load_db_file", "Choose .db File", class = "btn-secondary"),
    shiny::tags$hr(),
    shiny::h3("LAB INFORMATION"),
    shiny::fluidRow(shiny::column(width = 4, shiny::dateInput("lab_date", "LAB DATE"))),
    shiny::tags$hr(),
    shiny::h3("SAMPLE INFORMATION"),
    shiny::tags$div(id = "pleopod_rows_container", pleopod_row_ui(1)),
    mat_export_ui(),
    shiny::tags$hr(),
    shiny::h4("Current Trip Information"), shiny::tableOutput("trip_table"),
    shiny::h4("Current Pleopod Samples"), shiny::tableOutput("pleopod_table")
  )

  # --- Server ---------------------------------------------------------------

  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      trips = data.frame(), lab_pleopod = data.frame(), locations = list(), row_count = 1,
      current_location_row = NA_integer_, autofill_active = FALSE,
      db_folder_path = "", has_selected_db_folder = FALSE, db_status = ""
    )
    location_observers <- integer()
    image_observers <- integer()

    lobster_numbers <- function(n = rv$row_count) {
      vapply(seq_len(n), function(i) {
        val <- input[[paste0("pl_lobster_no_", i)]]
        if (is.null(val) || is.na(val)) NA_integer_ else as.integer(val)
      }, integer(1))
    }
    
    lobster_trips <- function(n = rv$row_count) {
      vapply(seq_len(n), function(i) {
        val <- input[[paste0("pl_trip_id_", i)]]
        if (is.null(val) || is.na(val)) "" else as.character(val)
      }, character(1))
    }

    has_location <- function(i) {
      loc <- rv$locations[[as.character(i)]]
      !is.null(loc) && nrow(loc) > 0 && nzchar(chr_or_empty(loc$TRIPID[1]))
    }

    register_location_observer <- function(i) {
      if (i %in% location_observers) return()
      local({
        ii <- i
        shiny::observeEvent(input[[paste0("pl_location_", ii)]], {
          loc <- rv$locations[[as.character(ii)]]
          if (is.null(loc)) loc <- location_empty()
          rv$current_location_row <- ii
          shiny::showModal(location_modal_ui(ii, loc))
        }, ignoreInit = TRUE)
      })
      location_observers <<- c(location_observers, i)
    }

    register_image_observer <- function(i) {
      if (i %in% image_observers) return()
      local({
        ii <- i
        shiny::observeEvent(input[[paste0("pl_image_select_", ii)]], {
          image_path_tcl <- with_topmost_tk(function() tcltk::tkgetOpenFile(
            filetypes = "{{Image files} {.png .jpg .jpeg .tif .tiff .bmp .gif}} {{All files} *}",
            title = "Choose pleopod image"
          ))
          image_path <- as.character(tcltk::tclvalue(image_path_tcl))
          if (nzchar(image_path)) shiny::updateTextInput(session, paste0("pl_image_", ii), value = normalizePath(image_path, winslash = "/", mustWork = FALSE))
        }, ignoreInit = TRUE)
      })
      image_observers <<- c(image_observers, i)
    }

    clear_pleopod_row <- function(i) {
      shiny::updateNumericInput(session, paste0("pl_lobster_no_", i), value = NA)
      shiny::updateNumericInput(session, paste0("pl_carapace_length_", i), value = NA)
      shiny::updateSelectInput(session, paste0("pl_cg_stage_", i), selected = "")
      shiny::updateSelectInput(session, paste0("pl_moult_stage_", i), selected = "")
      shiny::updateTextInput(session, paste0("pl_image_", i), value = "")
      shiny::updateTextInput(session, paste0("pl_observer_", i), value = "")
      shiny::updateTextInput(session, paste0("pl_comments_", i), value = "")
      shiny::updateTextInput(session, paste0("pl_trip_id_", i), value = "")
      update_location_button(i, has_location = FALSE)
    }

    update_location_button <- function(i, has_location) {
      session$sendCustomMessage("setLocationButton", list(
        id = paste0("pl_location_", i),
        label = if (isTRUE(has_location)) "Change Location" else "Add Location",
        className = if (isTRUE(has_location)) "btn-success" else "btn-secondary"
      ))
    }

    reset_rows <- function(target_rows = 1) {
      current_rows <- shiny::isolate(rv$row_count)
      target_rows <- max(1, as.integer(target_rows))
      if (current_rows < target_rows) {
        for (i in seq(current_rows + 1, target_rows)) {
          shiny::insertUI(selector = "#pleopod_rows_container", where = "beforeEnd", ui = pleopod_row_ui(i, has_location = has_location(i)))
          register_location_observer(i)
          register_image_observer(i)
        }
      } else if (current_rows > target_rows) {
        for (i in seq(target_rows + 1, current_rows)) shiny::removeUI(selector = paste0("#pleopod_row_", i), immediate = TRUE)
      }
      rv$row_count <- target_rows
    }

    rebuild_pleopod_rows <- function(pleopod_df, target_rows) {
      current_rows <- shiny::isolate(rv$row_count)
      for (i in seq_len(current_rows)) {
        shiny::removeUI(selector = paste0("#pleopod_row_", i), immediate = TRUE)
      }
      
        for (i in seq_len(target_rows)) {
          has_data <- nrow(pleopod_df) >= i
          trip_id <- if (has_data) chr_or_empty(pleopod_df$TRIPID[i]) else ""
          shiny::insertUI(
            selector = "#pleopod_rows_container",
            where = "beforeEnd",
            ui = pleopod_row_ui(
              i,
              lobster_no = if (has_data) pleopod_df$LOBSTER_NO[i] else NA,
              carapace_length = if (has_data) pleopod_df$CARAPACE_LENGTH[i] else NA,
              cg_stage = if (has_data) chr_or_empty(pleopod_df$CG_STAGE[i]) else "",
              moult_stage = if (has_data) chr_or_empty(pleopod_df$MOULT_STAGE[i]) else "",
              image = if (has_data) chr_or_empty(pleopod_df$IMAGE_FILE[i]) else "",
              observer_val = if (has_data) chr_or_empty(pleopod_df$OBSERVER[i]) else "",
              comments = if (has_data) chr_or_empty(pleopod_df$COMMENTS[i]) else "",
              trip_id = trip_id,
              has_location = nzchar(trip_id)
            )
          )
          register_location_observer(i)
          register_image_observer(i)
        }


      rv$row_count <- target_rows
    }

    register_location_observer(1)
    register_image_observer(1)

    # --- Error checks -------------------------------------------------------

    # Duplicate LOBSTER NUMBER feedback
    shiny::observe({
      n <- rv$row_count
      nums  <- lobster_numbers(n)
      trips <- lobster_trips(n)
      num_trip <- paste(nums, trips, sep = "|")
      # Find duplicated trip/lobster combinations
      dupes <- unique(num_trip[duplicated(num_trip)])
      for (i in seq_len(n)) {
        shinyFeedback::feedbackDanger(paste0("pl_lobster_no_", i),
                                       num_trip[i] %in% dupes,
                                       "Duplicate LOBSTER NUMBER and TRIPID")
      }
    })
    
    
    shiny::observe({
      for (i in seq_len(rv$row_count)) {
        carapace_length <- input[[paste0("pl_carapace_length_", i)]]
        carapace_warn <- !is.null(carapace_length) && !is.na(carapace_length) && (carapace_length < 40 || carapace_length > 120)
        shinyFeedback::feedbackWarning(paste0("pl_carapace_length_", i), carapace_warn, "Expected range is 40-120 mm")
      }
    }) 

    # Location coordinate validation feedback
    shiny::observe({
      lat <- input$loc_lat
      if (is.null(lat) || !nzchar(lat)) { shinyFeedback::hideFeedback("loc_lat"); return() }
      shinyFeedback::feedbackDanger("loc_lat", !is_valid_ddmm(lat, "lat"),
        "Must be DDMM.MM — degrees 00-89, minutes 00-59 (e.g. 4430.55)")
    })

    shiny::observe({
      long <- input$loc_long
      if (is.null(long) || !nzchar(long)) { shinyFeedback::hideFeedback("loc_long"); return() }
      shinyFeedback::feedbackDanger("loc_long", !is_valid_ddmm(long, "long"),
        "Must be DDMM.MM — minutes 00-59 (e.g. 6645.20)")
    })

    # Gate buttons on validation errors
    shiny::observe({
      nums <- lobster_numbers(rv$row_count)
      trips <- lobster_trips(rv$row_count)
      num_trip <- paste(nums, trips, sep = "|")
      lob_err <- any(duplicated(num_trip))
      #session$sendCustomMessage("toggleButtons", list(disabled = lob_err))

      lat_val <- input$loc_lat
      long_val <- input$loc_long
      lat_err <- !is.null(lat_val) && nzchar(lat_val) && !is_valid_ddmm(lat_val, "lat")
      long_err <- !is.null(long_val) && nzchar(long_val) && !is_valid_ddmm(long_val, "long")
      session$sendCustomMessage("toggleSaveLocation", list(disabled = lat_err || long_err || lob_err))
    })

    # --- Data entry automation ---------------------------------------------

    shiny::observe({
      if (isTRUE(rv$autofill_active)) return()
      n <- rv$row_count
      filled <- which(!is.na(lobster_numbers(n)))
      desired_rows <- if (length(filled) == 0) 1 else max(filled) + 1
      reset_rows(desired_rows)
    })

    shiny::observe({
      org <- input$loc_org; date <- input$loc_date
      if (is.null(org) || is.null(date)) return()
      shiny::updateTextInput(session, "loc_trip_id", value = if (nzchar(org)) paste0(org, format(date, "%d%m%y")) else "")
    })

    shiny::observe({
      trip_id <- input$loc_trip_id
      if (is.null(trip_id) || !nzchar(trip_id) || nrow(rv$trips) == 0) return()
      loc <- rv$trips[rv$trips$TRIPID == trip_id, , drop = FALSE]
      if (nrow(loc) == 0) return()
      loc <- loc[1, , drop = FALSE]
      shiny::updateTextInput(session, "loc_org", value = chr_or_empty(loc$ORG[1]))
      shiny::updateDateInput(session, "loc_date", value = chr_or_empty(loc$TRIP_DATE[1]))
      shiny::updateTextInput(session, "loc_port", value = chr_or_empty(loc$PORT[1]))
      shiny::updateSelectInput(session, "loc_lfa", selected = chr_or_empty(loc$LFA[1]))
      shiny::updateTextInput(session, "loc_sampler", value = chr_or_empty(loc$SAMPLER[1]))
      shiny::updateTextInput(session, "loc_lat", value = chr_or_empty(loc$LAT[1]))
      shiny::updateTextInput(session, "loc_long", value = chr_or_empty(loc$LONG[1]))
      shiny::updateNumericInput(session, "loc_grid", value = loc$GRID[1])
      shiny::updateNumericInput(session, "loc_depth", value = loc$DEPTH[1])
    })

    shiny::observeEvent(input$save_location, {
      i <- rv$current_location_row
      shiny::req(!is.na(i), nzchar(input$loc_trip_id))
      if (nzchar(input$loc_lat) && !is_valid_ddmm(input$loc_lat, "lat")) { rv$db_status <- "Location not saved: LAT must be DDMM.MM"; return() }
      if (nzchar(input$loc_long) && !is_valid_ddmm(input$loc_long, "long")) { rv$db_status <- "Location not saved: LONG must be DDMM.MM"; return() }
      rv$locations[[as.character(i)]] <- data.frame(
        TRIPID = na_if_empty(input$loc_trip_id), ORG = na_if_empty(input$loc_org),
        TRIP_DATE = na_if_empty(as.character(input$loc_date)), PORT = na_if_empty(input$loc_port),
        LFA = na_if_empty(input$loc_lfa), SAMPLER = na_if_empty(input$loc_sampler),
        LAT = na_if_empty(input$loc_lat), LONG = na_if_empty(input$loc_long),
        GRID = input$loc_grid, DEPTH = input$loc_depth, stringsAsFactors = FALSE
      )
      shiny::removeModal()
      shiny::removeUI(selector = paste0("#pleopod_row_", i), immediate = TRUE)
      shiny::insertUI(selector = if (i == 1) "#pleopod_rows_container" else paste0("#pleopod_row_", i - 1),
        where = if (i == 1) "afterBegin" else "afterEnd",
        ui = pleopod_row_ui(i, lobster_no = input[[paste0("pl_lobster_no_", i)]],
                            carapace_length = input[[paste0("pl_carapace_length_", i)]],
          cg_stage = chr_or_empty(input[[paste0("pl_cg_stage_", i)]]),
          moult_stage = chr_or_empty(input[[paste0("pl_moult_stage_", i)]]),
          image = chr_or_empty(input[[paste0("pl_image_", i)]]),
          observer_val = chr_or_empty(input[[paste0("pl_observer_", i)]]),
          comments = chr_or_empty(input[[paste0("pl_comments_", i)]]),
          trip_id = chr_or_empty(input$loc_trip_id), has_location = TRUE))
      update_location_button(i, has_location = TRUE)
    })

    shiny::observeEvent(input$load_db_file, {
      db_path_tcl <- with_topmost_tk(function() tcltk::tkgetOpenFile(filetypes = "{{Database files} {.db}} {{All files} *}", title = "Choose pleopod database"))
      db_path <- as.character(tcltk::tclvalue(db_path_tcl))
      if (!nzchar(db_path)) return()
      tryCatch({
        loaded <- read_pleopod_db(db_path)
        lab_date <- lab_date_from_db_path(db_path)
        if (is.na(lab_date)) {
          lab_dates <- unique(stats::na.omit(loaded$pleopod$LAB_DATE))
          if (length(lab_dates) == 1) lab_date <- lab_dates[[1]]
        }
        if (is.na(lab_date) || !nzchar(lab_date)) stop("Could not determine LAB DATE from MAT_LAB_ddmmyy.db filename or MAT_PLEOPOD table")

        pleopod_df <- loaded$pleopod[loaded$pleopod$LAB_DATE == lab_date, , drop = FALSE]
        rv$trips <- loaded$trips
        rv$lab_pleopod <- pleopod_df
        rv$locations <- list()
        if (nrow(pleopod_df) > 0 && nrow(loaded$trips) > 0) {
          for (i in seq_len(nrow(pleopod_df))) {
            loc <- loaded$trips[loaded$trips$TRIPID == pleopod_df$TRIPID[i], , drop = FALSE]
            if (nrow(loc) > 0) rv$locations[[as.character(i)]] <- loc[1, , drop = FALSE]
          }
        }else{ ## create pleopod table if it doesn't exist in loaded db
          
        }
        target_rows <- max(1, nrow(pleopod_df) + 1)
        rv$autofill_active <- TRUE
        rebuild_pleopod_rows(pleopod_df, target_rows)
        shinyjs::delay((250+ nrow(pleopod_df)*100), {
          for (i in seq_len(target_rows)) {
            update_location_button(i, has_location = i <= nrow(pleopod_df) && nzchar(chr_or_empty(pleopod_df$TRIPID[i])))
          }
          shiny::updateDateInput(session, "lab_date", value = lab_date_to_date(lab_date))
          rv$autofill_active <- FALSE
        })
        loaded_folder <- dirname(normalizePath(db_path, winslash = "/", mustWork = FALSE))
        shiny::updateTextInput(session, "db_folder", value = loaded_folder)
        rv$has_selected_db_folder <- TRUE
        rv$db_status <- paste("Loaded:", normalizePath(db_path, winslash = "/", mustWork = FALSE))
      }, error = function(e) rv$db_status <- paste("Load failed:", conditionMessage(e)))
    })

    shiny::observeEvent(input$choose_db_folder, {
      selected <- with_topmost_tk(function() tcltk::tk_choose.dir(default = input$db_folder, caption = "Choose save directory"))
      if (!is.null(selected) && nzchar(selected)) {
        shiny::updateTextInput(session, "db_folder", value = selected)
        rv$has_selected_db_folder <- TRUE
        rv$db_status <- paste("Save directory selected:", selected)
      }
    })

    output$save_db_ui <- shiny::renderUI({ shiny::actionButton("save_db", "Save to .db", class = "btn-success", disabled = !isTRUE(rv$has_selected_db_folder)) })

    shiny::observeEvent(input$save_db, {
      shiny::req(nzchar(input$db_folder))
      if (!dir.exists(input$db_folder)) { rv$db_status <- paste("Folder not found:", input$db_folder); return() }
      if (is.null(input$lab_date) || is.na(input$lab_date)) { rv$db_status <- "Save failed: LAB DATE is required"; return() }
      lab_date <- format(as.Date(input$lab_date), "%d%m%y")
      pleopod_rows <- Filter(Negate(is.null), lapply(seq_len(rv$row_count), function(i) {
        lob_no <- input[[paste0("pl_lobster_no_", i)]]
        if (is.null(lob_no) || is.na(lob_no)) return(NULL)
        loc <- rv$locations[[as.character(i)]]
        data.frame(TRIPID = if (is.null(loc)) NA_character_ else loc$TRIPID[1], LAB_DATE = lab_date,
          ROW_NO = i, LOBSTER_NO = as.integer(lob_no), CARAPACE_LENGTH = input[[paste0("pl_carapace_length_", i)]],
          CG_STAGE = na_if_empty(input[[paste0("pl_cg_stage_", i)]]),
          MOULT_STAGE = na_if_empty(input[[paste0("pl_moult_stage_", i)]]), IMAGE_FILE = na_if_empty(input[[paste0("pl_image_", i)]]),
          OBSERVER = na_if_empty(input[[paste0("pl_observer_", i)]]),
        COMMENTS = na_if_empty(input[[paste0("pl_comments_", i)]]), stringsAsFactors = FALSE)
      }))
      pleopod_df <- if (length(pleopod_rows) > 0) do.call(rbind, pleopod_rows) else data.frame()
      trip_rows <- Filter(Negate(is.null), rv$locations)
      trip_df <- if (length(trip_rows) > 0) unique(do.call(rbind, trip_rows)) else data.frame()
      if (nrow(trip_df) > 0) trip_df <- trip_df[!duplicated(trip_df$TRIPID), , drop = FALSE]
      db_base <- paste0("MAT_LAB_", lab_date, ".db")
      db_path <- file.path(input$db_folder, db_base)
      tryCatch({
        write_pleopod_db(db_path, trip_df, pleopod_df)
        rv$trips <- trip_df; rv$lab_pleopod <- pleopod_df
        rv$db_status <- paste("Saved:", db_path)
      }, error = function(e) rv$db_status <- paste("Save failed:", conditionMessage(e)))
    })

    output$db_status <- shiny::renderText(rv$db_status)
    output$trip_table <- shiny::renderTable(rv$trips)
    output$pleopod_table <- shiny::renderTable(rv$lab_pleopod)
  }

  shiny::runApp(shiny::shinyApp(ui = ui, server = server))
}
