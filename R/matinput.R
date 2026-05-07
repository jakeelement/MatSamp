#' Launch the MatSamp data entry app
#'
#' Starts a Shiny app for entering sampling data with fields for date,
#' LFA, organization, and measurement.
#'
#' @return A Shiny app launched in the current R session.
#' @import shiny
#' @export
matinput <- function() {
  shiny::runApp(
    shiny::shinyApp(
      ui = shiny::fluidPage(
        shiny::titlePanel("MatSamp Data Entry"),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::dateInput("date", "Date"),
            shiny::textInput("lfa", "LFA"),
            shiny::textInput("organization", "Organization"),
            shiny::numericInput("measurement", "Measurement", value = NA)
          )
        )
      ),
      server = function(input, output, session) {
        invisible(NULL)
      }
    )
  )
}
