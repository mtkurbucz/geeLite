#' @title Configuration File Generation
#'
#' @description This function sets up the configuration file for data generation
#' and update.
#'
#' @param target.path [Optional] Path to the directory. If not provided, the
#' user is prompted to enter a path. Default value is \code{NULL}.
#'
#' @export
#'
#' @examples
#' # Example 1: Run configuration without providing a target path
#' \dontrun{
#'   GenerateConfig()
#' }
#'
#' # Example 2: Run configuration with a target path
#' \dontrun{
#'   GenerateConfig(target.path = "/path/to/custom/directory")
#' }
#'
#' @importFrom shiny fluidPage dateInput checkboxInput conditionalPanel tags
#' @importFrom shiny headerPanel mainPanel hr selectInput radioButtons HTML h1
#' @importFrom shiny actionButton observeEvent runApp stopApp shinyApp
#' @importFrom jsonlite toJSON
#' @importFrom cli cli_alert_info
#'
GenerateConfig <- function(target.path = NULL) {

  # ----------------------------------------------------------------------------
  # S1: Parameter test
  # ----------------------------------------------------------------------------

  # If 'target.path' is not provided or is invalid, prompt the user
  if (is.null(target.path) || !file.exists(target.path)) {
    target.path <- GeneratePath()
  }

  setwd(target.path)

  # ----------------------------------------------------------------------------
  # S2: Generate configuration file using Shiny App
  # ----------------------------------------------------------------------------

  # Function to create Shiny UI
  ui <- fluidPage(

    # Set max-width for the page
    tags$head(
      tags$style(
        HTML(".main-header h1 {font-size: 18px}"),
        HTML(".main-header h3 {font-size: 14px}"),
        HTML(".btn-generate {background-color: #4CAF50; color: white;}")
      )
    ),

    # Header title
    headerPanel("Configuration File Generation"),

    # Subheader
    mainPanel(
      h1("GFIData", style = "font-size: 1.5em; font-weight: bold;"),
      hr(),

      # Single date selector for Start Date
      dateInput("startDate", "Select Start Date:",
                value = "2007-01-01"),

      # Checkbox for Latest Updates
      checkboxInput("latestUpdates", "Latest Updates", value = TRUE),

      # Conditional rendering of the second date selector
      conditionalPanel(
        condition = "input.latestUpdates == false",
        dateInput("endDate", "Select End Date:",
                  value = Sys.Date())
      ),

      # Add selectInput for country selection
      selectInput("country", "Select Countries:",
                  choices = c("Dem Rep of the Congo", "Haiti",
                              "Kenya", "Somalia", "South Sudan", "Yemen"),
                  multiple = TRUE, selected = c("Dem Rep of the Congo",
                                                "Haiti", "Kenya", "Somalia",
                                                "South Sudan", "Yemen")
      ),

      # Add radioButtons for frequency selection
      radioButtons("frequency", "Select Frequency:",
                   choices = c("Daily", "Monthly", "Yearly"),
                   selected = "Monthly"),

      # Select datasets
      selectInput("datasets", "Select Datasets:",
                  choices = c("NDVI", "Precipitation"),
                  multiple = TRUE, selected = c("NDVI", "Precipitation")
      ),

      # Action button to generate JSON
      actionButton("generateJSON", "Generate Config", class = "btn-generate")
    )
  )

  # Shiny server
  server <- function(input, output, session) {
    observeEvent(input$generateJSON, {

      # Create a list with selected values
      endDateValue <- ifelse(input$latestUpdates, "today", format(input$endDate,
                                                                  "%Y-%m-%d"))

      selected.values <- list(
        "DateRange" = c(format(input$startDate, "%Y-%m-%d"), endDateValue),
        "SelectedCountries" = input$country,
        "SelectedFrequency" = input$frequency,
        "SelectedDatasets" = input$datasets
      )

      # Generate directory if it is missing
      if (!dir.exists("config")) {
        dir.create("config", recursive = TRUE)
      }

      # Convert the list to JSON
      json.data <- toJSON(selected.values, pretty = TRUE)

      # Write JSON to a file
      writeLines(json.data, "config/config.json")

      # Print a message indicating successful JSON generation
      cat("\n")
      cli_alert_info(paste0("Configuration file generated successfully. ",
                                 "Check 'config/config.json'."))

      # Close the Shiny app window
      stopApp(session)
    })
  }

  # Run the Shiny app
  runApp(shinyApp(ui, server))
}
