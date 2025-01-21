#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer downloadButton downloadHandler h3
#' @importFrom bslib card
#' @importFrom openxlsx2 write_xlsx
#' @importFrom waiter Waiter spin_loaders
#'
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      id = "export",
      shiny::h3("Save work"),
      shiny::p("Save the current state of your work into an Rdata file."),
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

    w <- waiter::Waiter$new(
      html = shiny::tagList(
        waiter::spin_loaders(id = 8,
                             color = "black"),
        shiny::h3("Collecting data....")
      ),
      color = "rgba(255, 255, 255, 0.3)"
    )

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

        openxlsx2::write_xlsx(x = export,
                              file = file,
                              na.strings = "")
      }
    )


    output$save_rdata <- shiny::downloadHandler(
      filename = function() {
        paste("Current_state_", Sys.Date(), ".Rdata", sep = "")
      },
      content = function(file) {
        shiny::req(r$tables$analysis_data,
                   r$tables$meta_data)

        w$show()
        save(r,
             file = file)
        w$hide()
      }
    )
  })
}
