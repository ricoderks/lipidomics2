#' heatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList p
#' @importFrom bslib card layout_sidebar
#'
mod_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::uiOutput(
            outputId = ns("settingsHeatmap")
          ),
          shiny::actionButton(
            inputId = ns("hmGenerate"),
            label = "Generate heatmap"
          ),
          shiny::hr(),
          shiny::actionButton(
            inputId = ns("remove"),
            label = "",
            icon = icon("trash"),
            class = "btn-danger",
            width = "50%"
          )
        ),
        plotly::plotlyOutput(
          outputId = ns("heatmap")
        )
      )
    )
  )
}

#' heatmap Server Functions
#'
#' @noRd
mod_heatmap_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    print("Heatmap server started")

    analysis_settings <- shiny::reactiveValues(
      heatmap = list(
        table = "raw"
      )
    )

    hmPlot <- reactiveVal()

    output$settingsHeatmap <- shiny::renderUI({
      selection <- c(
        "Raw data" = "raw",
        "Total area normalization" = "totNorm",
        "PQN normalization" = "pqnNorm"
      )

      selected <- names(unlist(r$analysis$normalization)[unlist(r$analysis$normalization)])
      selected <- c("raw", selected)

      shiny::tagList(
        shiny::selectInput(
          inputId = ns("hmSelectTable"),
          label = "Select data table:",
          choices = selection[selection %in% selected],
          selected = "raw"
        ),
        shiny::checkboxGroupInput(
          inputId = ns("hmClustering"),
          label = "Select clustering:",
          choices = c("Features" = "rows",
                      "Samples" = "columns")
        ),
        shiny::selectInput(
          inputId = ns("hmSampleAnnotation"),
          label = "Sample annotation:",
          choices = r$columns$groups,
          multiple = TRUE
        ),
        shiny::selectInput(
          inputId = ns("hmFeatureAnnotation"),
          label = "Feature annotation:",
          choices = c("Lipid class" = "Class"),
          multiple = TRUE
        )
      )
    })


    shiny::observeEvent(input$hmGenerate, {
      shiny::req(r$tables$analysis_data,
                 input$hmSelectTable)

      area_column <- switch(
        input$hmSelectTable,
        "raw" = "area",
        "totNorm" = "totNormArea",
        "pqnNorm" = "pqnNormArea"
      )

      sample_annotation <- input$hmSampleAnnotation
      feature_annotation <- input$hmFeatureAnnotation

      row_clustering <- "rows" %in% input$hmClustering
      col_clustering <- "columns" %in% input$hmClustering

      plot_data <- r$tables$analysis_data[r$tables$analysis$keep == TRUE &
                                            r$tables$analysis$class_keep == TRUE &
                                            r$tables$analysis$sample_name %in% r$index$selected_samples, ]

      hmPlot(show_heatmap(data = plot_data,
                          area_column = area_column,
                          column_annotation = sample_annotation,
                          row_annotation = feature_annotation,
                          row_clustering = row_clustering,
                          col_clustering = col_clustering))
    })


    output$heatmap <- plotly::renderPlotly({
      shiny::req(hmPlot())

      print("Show heatmap here")

      return(hmPlot())
    })

    #--------------------------------------------------------------- export ----
    export <- function() {
      res <- list(
        plot = hmPlot(),
        settings = analysis_settings$heatmap
      )

      return(res)
    }

    return(list(export = export))

  })
}
