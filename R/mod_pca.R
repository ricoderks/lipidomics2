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
          shiny::actionButton(
            inputId = ns("pcaComments"),
            label = "Add comments"
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

    analysis_settings <- shiny::reactiveValues(
      pca = list(
        settings = list(
          x = "PC1",
          y = "PC2",
          nPcs = 4,
          plot = "scores",
          scaling = "uv",
          transformation = "log",
          sample_annotation = "none",
          feature_annotation = "none",
          table = "raw"
        ),
        comments = list(
          comment_before = NULL,
          comment_after = NULL
        )
      )
    )

    exportplot <- shiny::reactiveValues(
      plot = list(
        scores = NULL,
        loadings = NULL,
        summary_fit = NULL
      )
    )

    output$settingsPca <- shiny::renderUI({
      # executes twice?!?!
      selection <- c(
        "Raw data" = "raw",
        "Total area normalization" = "totNorm",
        "PQN normalization" = "pqnNorm",
        "Protein normalization" = "protNorm"
      )

      selected <- names(unlist(r$analysis$normalization)[unlist(r$analysis$normalization)])
      selected <- c("raw", selected)
      shiny::tagList(
        shiny::div(
          bslib::accordion(
            multiple = FALSE,
            bslib::accordion_panel(
              title = "PCA settings",
              icon = bsicons::bs_icon("menu-app"),
              shiny::selectInput(
                inputId = ns("pcaSelectTable"),
                label = "Select data table:",
                choices = selection[selection %in% selected],
                selected = shiny::isolate(analysis_settings$pca$settings$table)
              ),
              shiny::sliderInput(
                inputId = ns("pcaNumberPcs"),
                label = "Maximum number of PCs:",
                value = shiny::isolate(analysis_settings$pca$settings$nPcs),
                min = 2,
                max = 10,
                step = 1
              ),
              shiny::selectInput(
                inputId = ns("pcaTransformation"),
                label = "Transformation:",
                choices = c("None" = "none",
                            "Log10" = "log10",
                            "Log1p" = "log1p"),
                selected = shiny::isolate(analysis_settings$pca$settings$transformation)
              ),
              shiny::selectInput(
                inputId = ns("pcaScaling"),
                label = "Scaling:",
                choices = c("None" = "none",
                            "UV" = "uv",
                            "Pareto" = "pareto"),
                selected = shiny::isolate(analysis_settings$pca$settings$scaling)
              )
            ),
            bslib::accordion_panel(
              title = "Plot settings",
              icon = bsicons::bs_icon("menu-app"),
              shiny::selectInput(
                inputId = ns("pcaSampleAnnotation"),
                label = "Sample annotation:",
                choices = c("none", r$columns$groups),
                selected = shiny::isolate(analysis_settings$pca$settings$sample_annotation)
              ),
              shiny::selectInput(
                inputId = ns("pcaFeatureAnnotation"),
                label = "Feature annotation:",
                choices = c("None" = "none",
                            "Lipid class" = "Class"),
                selected = shiny::isolate(analysis_settings$pca$settings$feature_annotation)
              ),
              shiny::selectInput(
                inputId = ns("pcaSelectPlot"),
                label = "Show plot:",
                choices = c("Scores" = "scores",
                            "Loadings" = "loadings",
                            "Summary of fit" = "summary_fit"),
                selected = shiny::isolate(analysis_settings$pca$settings$plot)
              ),
              shiny::selectInput(
                inputId = ns("pcaX"),
                label = "x-axis:",
                choices = paste0("PC", 1:analysis_settings$pca$settings$nPcs),
                selected = shiny::isolate(analysis_settings$pca$settings$x)
              ),
              shiny::selectInput(
                inputId = ns("pcaY"),
                label = "y-axis:",
                choices = paste0("PC", 1:analysis_settings$pca$settings$nPcs),
                selected = shiny::isolate(analysis_settings$pca$settings$y)
              )
            )
          ), # end accordion
          style = "font-size:75%"
        ) # end div
      )
    })

    shiny::observeEvent(
      c(input$pcaSelectTable,
        input$pcaTransformation,
        input$pcaScaling,
        input$pcaNumberPcs,
        input$pcaX,
        input$pcaY,
        input$pcaSelectPlot,
        input$pcaSampleAnnotation,
        input$pcaFeatureAnnotation),
      {
        analysis_settings$pca$settings$nPcs <- input$pcaNumberPcs
        analysis_settings$pca$settings$x <- input$pcaX
        analysis_settings$pca$settings$y <- input$pcaY
        analysis_settings$pca$settings$plot <- input$pcaSelectPlot
        analysis_settings$pca$settings$sample_annotation <- input$pcaSampleAnnotation
        analysis_settings$pca$settings$feature_annotation <- input$pcaFeatureAnnotation
        analysis_settings$pca$settings$table <- input$pcaSelectTable
        analysis_settings$pca$settings$transformation <- input$pcaTransformation
        analysis_settings$pca$settings$scaling <- input$pcaScaling
      })


    output$pcaPlot <- plotly::renderPlotly({
      shiny::req(r$tables$analysis_data,
                 input$pcaSelectTable,
                 input$pcaTransformation,
                 input$pcaScaling,
                 input$pcaNumberPcs,
                 input$pcaSelectPlot)

      area_column <- switch(
        input$pcaSelectTable,
        "raw" = "area",
        "totNorm" = "totNormArea",
        "pqnNorm" = "pqnNormArea",
        "protNorm" = "protNormArea"
      )

      pca_data <- r$tables$analysis_data[r$tables$analysis$keep == TRUE &
                                           r$tables$analysis$class_keep == TRUE &
                                           r$tables$analysis$sample_name %in% r$index$selected_samples, ]

      plot_data <- do_pca(data = pca_data,
                          nPcs = input$pcaNumberPcs,
                          area_column = area_column,
                          group_columns = input$pcaSampleAnnotation,
                          feature_annotation = input$pcaFeatureAnnotation,
                          scaling = input$pcaScaling,
                          transformation = input$pcaTransformation)

      plys <- show_pca(
        data = plot_data,
        x = input$pcaX,
        y = input$pcaY,
        sample_annotation = input$pcaSampleAnnotation,
        feature_annotation = input$pcaFeatureAnnotation
      )

      exportplot$plot <- plys

      return(plys[[input$pcaSelectPlot]])
    })


    shiny::observeEvent(input$pcaComments, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Add comments for PCA",
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton(label = "Close"),
          shiny::textAreaInput(
            label = "Comment before:",
            inputId = ns("pcaCommentBefore"),
            value = analysis_settings$pca$comments$comment_before,
            width = "100%"
          ),
          shiny::textAreaInput(
            label = "Comment after:",
            inputId = ns("pcaCommentAfter"),
            value = analysis_settings$pca$comments$comment_after,
            width = "100%"
          )
        ),
        session = session
      )
    })


    shiny::observeEvent(c(input$pcaCommentBefore), {
      analysis_settings$pca$comments$comment_before <- input$pcaCommentBefore
    })


    shiny::observeEvent(c(input$pcaCommentAfter), {
      analysis_settings$pca$comments$comment_after <- input$pcaCommentAfter
    })

    #--------------------------------------------------------------- export ----
    export <- function() {
      res <- list(
        plot = exportplot$plot,
        settings = analysis_settings$pca$settings,
        comments = analysis_settings$pca$comments
      )

      return(res)
    }

    return(list(export = export))
  })
}
