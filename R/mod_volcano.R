#' volcano UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_volcano_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::p("This will show Volcano plot"),
    shiny::actionButton(
      inputId = ns("remove"),
      label = "Remove this analysis",
      icon = icon("trash"),
      class = "btn-danger"
    )
  )
}

#' volcano Server Functions
#'
#' @noRd
mod_volcano_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}
