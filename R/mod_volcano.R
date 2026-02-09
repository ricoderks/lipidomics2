#' volcano UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList p
#' @importFrom bslib card layout_sidebar
#'
mod_volcano_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::uiOutput(
            outputId = ns("settingsVolcano")
          ),
          shiny::hr(),
          shiny::actionButton(
            inputId = ns("remove"),
            label = "",
            icon = icon("trash"),
            class = "btn-danger",
            width = "50%"
          )
        ),
        shiny::p("This will show the volcano plot.")
      )
    )
  )
}

#' volcano Server Functions
#'
#' @noRd
mod_volcano_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    print("Volcano plot server started")

    analysis_settings <- shiny::reactiveValues(
      volcano = list(
        table = "raw"
      )
    )

    output$settingsVolcano <- shiny::renderUI({
      selection <- c(
        "Raw data" = "raw",
        "Total area normalization" = "totNorm",
        "PQN normalization" = "pqnNorm"
      )

      selected <- names(unlist(r$analysis$normalization)[unlist(r$analysis$normalization)])
      selected <- c("raw", selected)

      shiny::tagList(
        shiny::div(
          shiny::selectInput(
            inputId = ns("volcanoSelectTable"),
            label = "Select data table:",
            choices = selection[selection %in% selected],
            selected = analysis_settings$volcano$table
          ),
          style = "font-size:75%"
        )
      )
    })


    shiny::observeEvent(
      c(
        input$volcanoSelectTable
      ), {
        analysis_settings$volcano$table <- input$volcanoSelectTable
      }
    )

  })
}
