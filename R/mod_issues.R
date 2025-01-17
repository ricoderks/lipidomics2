#' issues UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer
#'
mod_issues_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(

  )
}

#' issues Server Functions
#'
#' @noRd
#'
mod_issues_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}
