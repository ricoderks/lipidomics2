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
    # determine the window height

    shiny::tags$script(
      sprintf(
        "
        function sendHeight() {
          Shiny.setInputValue('%s', window.innerHeight);
        }

        $(document).on('shiny:connected', function() {
          sendHeight();
        });

        $(window).resize(function() {
          sendHeight();
        });
        ",
        ns("window_height")
      )
    ),

    bslib::layout_sidebar(
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
      shiny::uiOutput(
        outputId = ns("hmMain")
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
        table = "raw",
        clustering = NULL,
        sample_annotation = NULL,
        feature_annotation = NULL
      )
    )

    plot_data <- reactiveVal()

    exportplot <- shiny::reactiveValues(
      plot = NULL
    )

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
          selected = shiny::isolate(analysis_settings$heatmap$table)
        ),
        shiny::checkboxGroupInput(
          inputId = ns("hmClustering"),
          label = "Select clustering:",
          choices = c("Features" = "rows",
                      "Samples" = "columns"),
          selected = shiny::isolate(analysis_settings$heatmap$clustering)
        ),
        shiny::selectInput(
          inputId = ns("hmSampleAnnotation"),
          label = "Sample annotation:",
          choices = r$columns$groups,
          selected = shiny::isolate(analysis_settings$heatmap$sample_annotation),
          multiple = TRUE
        ),
        shiny::selectInput(
          inputId = ns("hmFeatureAnnotation"),
          label = "Feature annotation:",
          choices = c("Lipid class" = "Class"),
          selected = shiny::isolate(analysis_settings$heatmap$feature_annotation),
          multiple = TRUE
        )
      )
    })


    output$hmMain <- shiny::renderUI({
      shiny::req(input$window_height)

      shiny::tagList(
        bslib::card(
          plotly::plotlyOutput(
            outputId = ns("hmHeatmap")
          ),
          height = paste0(0.80 * input$window_height, "px")
        )
      )
    })


    shiny::observeEvent(
      c(input$hmSelectTable,
        input$hmClustering,
        input$hmSampleAnnotation,
        input$hmFeatureAnnotation), {
          analysis_settings$heatmap$table <- input$hmSelectTable
          analysis_settings$heatmap$clustering <- input$hmClustering
          analysis_settings$heatmap$sample_annotation <- input$hmSampleAnnotation
          analysis_settings$heatmap$feature_annotation <- input$hmFeatureAnnotation
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

      output$hmHeatmap <- plotly::renderPlotly({
        shiny::req(plot_data,
                   input$window_height)

        ply <- show_heatmap(data = plot_data,
                            area_column = area_column,
                            column_annotation = sample_annotation,
                            row_annotation = feature_annotation,
                            row_clustering = row_clustering,
                            col_clustering = col_clustering,
                            height =  0.78 * input$window_height)

        exportplot$plot <- ply

        return(ply)
      })

    })




    #--------------------------------------------------------------- export ----
    export <- function() {
      res <- list(
        plot = exportplot$plot,
        settings = analysis_settings$heatmap
      )

      return(res)
    }

    return(list(export = export))

  })
}
