#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer
#'
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

  )
}

#' data Server Functions
#'
#' @noRd
mod_data_server <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}
