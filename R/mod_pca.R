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
        ),
        data = list(
         var_data = NULL
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

      # store the data for the variable plot
      analysis_settings$pca$data$var_data <- plot_data[["var_data"]]

      plys <- show_pca(
        data = plot_data,
        name = id,
        x = input$pcaX,
        y = input$pcaY,
        sample_annotation = input$pcaSampleAnnotation,
        feature_annotation = input$pcaFeatureAnnotation
      )

      exportplot$plot <- plys

      if(input$pcaSelectPlot != "summary_fit") {
        show_plot <- plys[[input$pcaSelectPlot]] |>
          plotly::event_register(event = "plotly_click")
      } else {
        show_plot <- plys[[input$pcaSelectPlot]]
      }
      return(show_plot)
    })


    #-------------------------------------------------------- pop-up scores ----
    shiny::observeEvent(
      plotly::event_data(
        event = "plotly_click",
        source = paste0(id, "_scores")
      ), {

        ns <- session$ns

        ed <- plotly::event_data(event = "plotly_click",
                                 source = paste0(id, "_scores"))

        if (is.null(ed) || is.null(ed$customdata)) return()

        sample_id <- ed$customdata

        output$pca_scores_modal <- plotly::renderPlotly({
          plot_data <- analysis_settings$pca$data$var_data
          plot_data <- plot_data[plot_data$sample_name == sample_id, ]

          ply <- show_var_plot(
            plot_data = plot_data,
            feature_annotation = input$pcaFeatureAnnotation
          )

          return(ply)
        })

        shiny::showModal(
          shiny::modalDialog(
            title = paste("Selected sample:", sample_id),
            size = "xl",
            easyClose = TRUE,
            footer = shiny::modalButton(label = "Close"),
            plotly::plotlyOutput(
              outputId = ns("pca_scores_modal"),
              height = "450px"
            )
          ),
          session = session
        )
      })


    #------------------------------------------------------ pop-up loadings ----
    shiny::observeEvent(
      plotly::event_data(
        event = "plotly_click",
        source = paste0(id, "_loadings")
      ), {
        ns <- session$ns

        ed <- plotly::event_data(event = "plotly_click",
                                 source = paste0(id, "_loadings"))

        if (is.null(ed) || is.null(ed$customdata)) return()

        my_id <- ed$customdata

        output$pca_loadings_modal <- plotly::renderPlotly({
          plot_data <- analysis_settings$pca$data$var_data
          plot_data <- plot_data[plot_data$my_id == my_id, ]
          if(input$pcaSampleAnnotation != "none") {
            plot_data <- merge(
              x = plot_data,
              y = r$tables$meta_data,
              by.x = "sample_name",
              by.y = r$columns$filename,
              all.x = TRUE
            )
          }

          ply <- show_obs_plot(
            plot_data = plot_data,
            observation_annotation = input$pcaSampleAnnotation
          )

          return(ply)
        })

        shiny::showModal(
          shiny::modalDialog(
            title = paste("Selected variable:", my_id),
            size = "xl",
            easyClose = TRUE,
            footer = shiny::modalButton(label = "Close"),
            plotly::plotlyOutput(
              outputId = ns("pca_loadings_modal"),
              height = "450px"
            )
          ),
          session = session
        )
      })


    #-------------------------------------------------------------- comment ----
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
