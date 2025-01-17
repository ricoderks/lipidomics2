#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer
#'
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

  )
}

#' export Server Functions
#'
#' @noRd
#'
mod_export_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}
