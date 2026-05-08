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
      DBI::dbExecute(con, "DELETE FROM TRIP WHERE TRIPID = ?", params = list(trip_df$trip_id[1]))
      DBI::dbExecute(con, "INSERT INTO TRIP (TRIPID, ORG, TRIP_DATE, PORT, LFA, SAMPLER) VALUES (?, ?, ?, ?, ?, ?)",
        params = unname(as.list(trip_df[1, c("trip_id", "org", "date", "port", "lfa", "sampler")]))
      )
    }

    if (nrow(string_df) > 0) {
      string_df <- unique(string_df)
      DBI::dbExecute(con, "DELETE FROM STRING_INFO WHERE TRIPID = ?", params = list(string_df$trip_id[1]))
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
      DBI::dbExecute(con, "DELETE FROM SAMPLE WHERE TRIPID = ?", params = list(sample_df$trip_id[1]))
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
      shiny::column(width = 3, shiny::actionButton("save_trip", "Save/Update Trip", class = "btn-primary"))
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
      shiny::column(width = 3, shiny::actionButton("next_string", "Next String", class = "btn-primary"))
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
      shiny::column(width = 8, shiny::textInput("db_folder", "Local folder path", value = getwd())),
      shiny::column(width = 4, shiny::textInput("db_name", "Database file name", value = "matsamp_data.db"))
    ),
    shiny::actionButton("save_db", "Save to .db", class = "btn-success"),
    shiny::verbatimTextOutput("db_status")
  )

  shiny::runApp(
    shiny::shinyApp(
      ui = shiny::fluidPage(
        shiny::titlePanel("CLRN SOM50 AT-SEA SAMPLE DATA FORM"),
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
          db_status = ""
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
          shiny::req(nzchar(input$db_folder), nzchar(input$db_name), !is.null(rv$trip))
          if (!dir.exists(input$db_folder)) {
            rv$db_status <- paste("Folder does not exist:", input$db_folder)
            return()
          }

          db_path <- file.path(input$db_folder, input$db_name)
          tryCatch({
            write_to_db(db_path, rv$trip, rv$strings, rv$samples)
            rv$db_status <- paste("Saved database:", db_path)
          }, error = function(e) {
            rv$db_status <- paste("Database save failed:", conditionMessage(e))
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
