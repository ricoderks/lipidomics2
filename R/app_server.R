#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # for communication between modules
  r <- shiny::reactiveValues(
    name = NA,
    files = list(
      meta_file = NA,
      data_file = NA,
      rda_file = NA
    )

  )

#--------------------------------------------------------------------- help ----
  mod_help_server(id = "help")

  mod_about_server(id = "about")

}
