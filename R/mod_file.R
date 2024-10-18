#' file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_file_ui <- function(id) {
  ns <- NS(id)
  tagList(

  )
}

#' file Server Functions
#'
#' @noRd
mod_file_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}
