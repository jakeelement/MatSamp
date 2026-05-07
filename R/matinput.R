#' Launch the MatSamp multi-level data entry app
#'
#' Starts a Shiny app for entering hierarchical sampling data at three levels:
#' TRIP, STRING#, and SAMPLE.
#'
#' @return A Shiny app launched in the current R session.
#' @import shiny
#' @export
matinput <- function() {
  sample_row_ui <- function(i) {
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shiny::numericInput(paste0("lobster_no_", i), "LOBSTER #", value = NA, min = 1, step = 1)
        ),
        shiny::column(
          width = 2,
          shiny::numericInput(paste0("length_", i), "LENGTH", value = NA)
        ),
        shiny::column(
          width = 2,
          shiny::textInput(paste0("hardness_", i), "HARDNESS")
        ),
        shiny::column(
          width = 2,
          shiny::textInput(paste0("egg_", i), "EGG")
        ),
        shiny::column(
          width = 2,
          shiny::textInput(paste0("pleopod_", i), "PLEOPOD")
        ),
        shiny::column(
          width = 2,
          shiny::textInput(paste0("ovary_", i), "OVARY")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::textInput(paste0("comments_", i), "COMMENTS")
        )
      ),
      shiny::tags$hr()
    )
  }

  shiny::runApp(
    shiny::shinyApp(
      ui = shiny::fluidPage(
        shiny::titlePanel("CLRN SOM50 AT-SEA SAMPLE DATA FORM"),

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
        ),

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
        ),

        shiny::tags$hr(),
        shiny::h3("SAMPLE INFORMATION"),
        shiny::uiOutput("sample_rows"),

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
          sample_row_count = 1
        )

        output$sample_rows <- shiny::renderUI({
          shiny::tagList(lapply(seq_len(rv$sample_row_count), sample_row_ui))
        })

        shiny::observe({
          i <- rv$sample_row_count
          lobster_value <- input[[paste0("lobster_no_", i)]]
          if (!is.null(lobster_value) && !is.na(lobster_value)) {
            rv$sample_row_count <- rv$sample_row_count + 1
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
            if (is.null(lobster_no) || is.na(lobster_no)) {
              return(NULL)
            }

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
          if (length(sample_rows) > 0) {
            rv$samples <- unique(rbind(rv$samples, do.call(rbind, sample_rows)))
          }

          shiny::updateTextInput(session, "lat", value = "")
          shiny::updateTextInput(session, "long", value = "")
          shiny::updateTextInput(session, "grid", value = "")
          shiny::updateNumericInput(session, "depth", value = NA)
          shiny::updateNumericInput(session, "string_no", value = ifelse(is.na(input$string_no), NA, input$string_no + 1))
          rv$sample_row_count <- 1
        })

        output$trip_table <- shiny::renderTable(rv$trip)
        output$string_table <- shiny::renderTable(rv$strings)
        output$sample_table <- shiny::renderTable(rv$samples)
      }
    )
  )
}
