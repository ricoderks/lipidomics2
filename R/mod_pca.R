#' pca UI Function
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
mod_pca_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::uiOutput(
            outputId = ns("settingsPca")
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
          outputId = ns("pcaPlot")
        )
      )
    )
  )
}

#' pca Server Functions
#'
#' @noRd
mod_pca_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    print("PCA server started")

    output$settingsPca <- shiny::renderUI({
      selection <- c(
        "Raw data" = "raw",
        "Total area normalization" = "totNorm",
        "PQN normalization" = "pqnNorm"
      )

      selected <- names(unlist(r$analysis$normalization)[unlist(r$analysis$normalization)])
      selected <- c("raw", selected)
      shiny::tagList(
        shiny::div(
          shiny::selectInput(
            inputId = ns("pcaSelectTable"),
            label = "Select data table:",
            choices = selection[selection %in% selected],
            selected = "raw"
          ),
          shiny::sliderInput(
            inputId = ns("pcaNumberPcs"),
            label = "Maximum number of PCs:",
            value = r$analysis$pca$nPcs,
            min = 2,
            max = 10,
            step = 1
          ),
          shiny::selectInput(
            inputId = ns("pcaScaling"),
            label = "Scaling:",
            choices = c("None" = "none",
                        "UV" = "uv",
                        "Pareto" = "pareto"),
            selected = "uv"
          ),
          shiny::selectInput(
            inputId = ns("pcaSampleAnnotation"),
            label = "Sample annotation:",
            choices = c("none", r$columns$groups)
          ),
          shiny::selectInput(
            inputId = ns("pcaFeatureAnnotation"),
            label = "Feature annotation:",
            choices = c("None" = "none",
                        "Lipid class" = "Class")
          ),
          shiny::selectInput(
            inputId = ns("pcaSelectPlot"),
            label = "Show plot:",
            choices = c("Scores" = "scores",
                        "Loadings" = "loadings",
                        "Summary of fit" = "sumfit"),
            selected = "scores"
          ),
          shiny::selectInput(
            inputId = ns("pcaX"),
            label = "x-axis:",
            choices = paste0("PC", 1:r$analysis$pca$nPcs),
            selected = r$analysis$pca$x
          ),
          shiny::selectInput(
            inputId = ns("pcaY"),
            label = "y-axis:",
            choices = paste0("PC", 1:r$analysis$pca$nPcs),
            selected = r$analysis$pca$y
          ),
          style = "font-size:75%"
        )
      )
    })

    shiny::observeEvent(c(input$pcaNumberPcs,
                          input$pcaX,
                          input$pcaY), {
      r$analysis$pca$nPcs <- input$pcaNumberPcs
      r$analysis$pca$x <- input$pcaX
      r$analysis$pca$y <- input$pcaY
    })

    output$pcaPlot <- plotly::renderPlotly({
      shiny::req(r$tables$analysis_data,
                 input$pcaSelectTable,
                 input$pcaScaling,
                 input$pcaNumberPcs,
                 input$pcaSelectPlot)

      area_column <- switch(
        input$pcaSelectTable,
        "raw" = "area",
        "totNorm" = "totNormArea",
        "pqnNorm" = "pqnNormArea"
      )

      pca_data <- r$tables$analysis_data[r$tables$analysis$keep == TRUE &
                                           r$tables$analysis$class_keep == TRUE &
                                           r$tables$analysis$sample_name %in% r$index$selected_samples, ]

      plot_data <- do_pca(data = pca_data,
                          nPcs = input$pcaNumberPcs,
                          area_column = area_column,
                          group_columns = input$pcaSampleAnnotation,
                          feature_annotation = input$pcaFeatureAnnotation,
                          scaling = input$pcaScaling)

      ply <- show_pca(data = plot_data,
                      plot = input$pcaSelectPlot,
                      x = input$pcaX,
                      y = input$pcaY,
                      sample_annotation = input$pcaSampleAnnotation,
                      feature_annotation = input$pcaFeatureAnnotation)

      return(ply)
    })
  })
}
