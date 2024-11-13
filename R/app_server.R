#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#'
#' @noRd
app_server <- function(input, output, session) {
  # increase upload limit
  options(shiny.maxRequestSize = 30 * 1024^2)

  # for communication between modules
  r <- shiny::reactiveValues(
    name = NULL,
    omics = NULL,
    files = list(
      meta_file = NULL,
      data_file_pos = NULL,
      data_file_neg = NULL,
      rda_file = NULL
    ),
    columns = list(
      sampleid = NULL,
      sampletype = NULL,
      acqorder = NULL
    ),
    index = list(
      blanks = NULL,
      pools = NULL,
      samples = NULL
    ),
    tables = list(
      meta_data = NULL,
      raw_data_pos = NULL,
      raw_data_neg = NULL,
      raw_data = NULL,
      clean_data = NULL,
      clean_data_wide = NULL
    )
  )

  mod_file_server(id = "file",
                  r = r)

  mod_settings_server(id = "settings",
                      r = r)

#--------------------------------------------------------------------- help ----
  mod_help_server(id = "help")

  mod_about_server(id = "about")

}
