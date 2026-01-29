#' pca UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pca_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::p("This will show PCA"),
    shiny::actionButton(
      inputId = ns("remove"),
      label = "Remove this analysis",
      icon = icon("trash"),
      class = "btn-danger"
    )
  )
}

#' pca Server Functions
#'
#' @noRd
mod_pca_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}
