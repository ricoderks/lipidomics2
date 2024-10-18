#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # for communication between modules
  r <- shiny::reactiveValues(
    name = NULL,
    files = list(
      meta_file = NULL,
      data_file = NULL,
      rda_file = NULL
    ),
    data = list(
      meta_data = NULL,
      raw_data = NULL,
      clean_data = NULL
    )

  )

#--------------------------------------------------------------------- help ----
  mod_help_server(id = "help")

  mod_about_server(id = "about")

}
