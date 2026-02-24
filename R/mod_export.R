#' export UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer downloadButton downloadHandler h3
#' @importFrom bslib card navset_card_tab nav_panel
#' @importFrom openxlsx2 write_xlsx
#' @importFrom waiter Waiter spin_loaders
#' @importFrom rmarkdown render
#'
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::navset_card_tab(
      id = "settings",
      bslib::nav_panel(
        title = "Data",
        value = "export_data",
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
      ),
      bslib::nav_panel(
        title = "Reports",
        value = "export_reports",
        bslib::card(
          shiny::h3("Reports"),
          shiny::p("Select one of the html reports for download."),
          shiny::selectInput(
            inputId = ns("export_select_report"),
            label = "Select report:",
            choices = c("QC report" = "qc",
                        "Analysis report" = "analysis"),
            selected = "qc"
          ),
          shiny::downloadButton(outputId = ns("download_report"),
                                label = "Download report",
                                style = "width:20%;"),
          shiny::conditionalPanel(
            condition = "input.export_select_report === 'analysis'",
            ns = ns,
            shiny::textInput(
              inputId = ns("export_analysis_title"),
              label = "Title analysis report:"
            )
          ),
          shiny::hr()
        )
      )
    )
  )
}

#' export Server Functions
#'
#' @importFrom utils packageVersion
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
        shiny::h3("Preparing download ....", style = "color:black")
      ),
      color = "rgba(255, 255, 255, 0.5)"
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
                                      samples = r$index$selected_samples,
                                      normalization = r$analysis$normalization)

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

    output$download_report <- shiny::downloadHandler(
      filename = function() {
        shiny::req(input$export_select_report)

        switch(
          input$export_select_report,
          "qc" = paste("QC_report_", Sys.Date(), ".html", sep = ""),
          "analysis" = paste("Analysis_report_", Sys.Date(), ".html", sep = "")
        )

      },
      content = function(file) {
        shiny::req(r$tables$analysis_data,
                   r$tables$meta_data,
                   input$export_select_report)

        w$show()

        # create file stuff
        report_name <- switch(
          input$export_select_report,
          "qc" = "qc_report.Rmd",
          "analysis" = "analysis_report.Rmd"
        )
        temp_report <- file.path(tempdir(), report_name)
        report_file <- system.file("report", report_name,
                                   package = "lipidomics2")
        file.copy(from = report_file,
                  to = temp_report,
                  overwrite = TRUE)

        if(input$export_select_report == "qc") {
          # param stuff
          params <- list(
            analysis_data = r$tables$analysis_data,
            trend_data = r$tables$trend_data,
            rsd_data_overall = r$tables$rsd_data_overall,
            rsd_data_batch = r$tables$rsd_data_batch,
            selected_pools = r$index$selected_pools,
            selected_samples = r$index$selected_samples,
            rsd_cutoff = r$settings$rsd_cutoff
          )
        } else {
          analyses <- replicate(
            n = length(r$analysis$modules),
            list(
              label = NULL,
              plot = NULL,
              settings = NULL
            ),
            simplify = FALSE
          )
          names(analyses) <- names(r$analysis$modules)
          for(a in 1:length(r$analysis$modules)) {
            m <- r$analysis$modules[[a]]
            analyses[[a]]$label <- m$label
            analyses[[a]]$plot <- m$export()$plot
            analyses[[a]]$settings <- m$export()$settings
          }

          params <- list(
            title = input$export_analysis_title,
            author = paste0("CPM - Lipidomics | v", utils::packageVersion("lipidomics2")),
            analyses = analyses,
            meta_data = r$tables$meta_data,
            general_settings = r$settings,
            columns = r$columns
          )
        }

        # render
        rmarkdown::render(input = temp_report,
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))

        w$hide()
      }
    )
  })
}
