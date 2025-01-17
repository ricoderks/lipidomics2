#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer downloadButton downloadHandler
#' @importFrom bslib card
#'
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      id = "export",
      shiny::h3("Save work"),
      shiny::p("Save the current state of your work into a Rdata file."),
      shiny::downloadButton(outputId = ns("save_rdata"),
                            label = "Save (Rdata)",
                            style = "width:20%;"),
      shiny::hr(),
      shiny::h3("Lipid list"),
      shiny::p("Save the lipids into an Excel file (.xlsx)."),
      shiny::downloadButton(outputId = ns("download_lipid_xlsx"),
                            label = "Download lipid list (xlsx)",
                            style = "width:20%;"),
      shiny::hr()
    )
  )
}

#' export Server Functions
#'
#' @noRd
#'
mod_export_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$download_lipid_xlsx <- shiny::downloadHandler(
      filename = function() {
        paste("Lipid_list_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        shiny::req(r$tables$analysis_data,
                   r$tables$meta_data)

        export <- prepare_export_data(meta_data = r$tables$meta_data,
                                      analysis_data = r$tables$analysis_data,
                                      filename_column = r$columns$filename,
                                      blanks = r$index$selected_blanks,
                                      qcpools = r$index$selected_pools,
                                      samples = r$index$selected_samples)



        write.csv(x = export,
                  file = file,
                  row.names = FALSE)
      }
    )
  })
}
