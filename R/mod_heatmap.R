#' heatmap UI Function
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
mod_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::actionButton(
            inputId = ns("remove"),
            label = "",
            icon = icon("trash"),
            class = "btn-danger",
            width = "50%"
          )
        ),
        plotly::plotlyOutput(
          outputId = ns("heatmap")
        )
      )
    )
  )
}

#' heatmap Server Functions
#'
#' @noRd
mod_heatmap_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    print("Heatmap server started")

    output$heatmap <- plotly::renderPlotly({
      # shiny::req(r$tables$analysis_data)
      print("Show heatmap here")
      print(head(r$tables$analysis_data))

      return(NULL)
    })

  })
}
