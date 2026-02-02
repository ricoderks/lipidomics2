#' heatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::p("This will show Heatmap"),
    shiny::actionButton(
      inputId = ns("remove"),
      label = "Remove this analysis",
      icon = icon("trash"),
      class = "btn-danger"
    )
  )
}

#' heatmap Server Functions
#'
#' @noRd
mod_heatmap_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
  })
}
