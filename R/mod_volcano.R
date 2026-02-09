#' volcano UI Function
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
mod_volcano_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::uiOutput(
            outputId = ns("settingsVolcano")
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
          outputId = ns("volcanoPlot")
        )
      )
    )
  )
}

#' volcano Server Functions
#'
#' @noRd
mod_volcano_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    print("Volcano plot server started")

    choices_group <- unique(r$tables$meta_data[r$tables$meta_data[[r$columns$sampleid]] %in% r$index$selected_samples, r$columns$group[1]])
    analysis_settings <- shiny::reactiveValues(
      volcano = list(
        table = "raw",
        transformation = "none",
        test = "ttest",
        group = r$columns$groups[1],
        comparison = NULL,
        group1 = choices_group[1],
        group2 = choices_group[2],
        fc_threshold = 2,
        pval_threshold = 0.05
      )
    )

    output$settingsVolcano <- shiny::renderUI({
      selection <- c(
        "Raw data" = "raw",
        "Total area normalization" = "totNorm",
        "PQN normalization" = "pqnNorm"
      )

      selected <- names(unlist(r$analysis$normalization)[unlist(r$analysis$normalization)])
      selected <- c("raw", selected)

      choices_group <- unique(
        r$tables$meta_data[r$tables$meta_data[[r$columns$sampleid]] %in% r$index$selected_samples,
                           shiny::isolate(analysis_settings$volcano$group)]
      )

      shiny::tagList(
        shiny::div(
          shiny::selectInput(
            inputId = ns("volcanoSelectTable"),
            label = "Select data table:",
            choices = selection[selection %in% selected],
            selected = shiny::isolate(analysis_settings$volcano$table)
          ),
          shiny::selectInput(
            inputId = ns("volcanoTransformation"),
            label = "Transformation:",
            choices = c("None" = "none",
                        "Log10" = "log10",
                        "Log1p" = "log1p"),
            selected = shiny::isolate(analysis_settings$volcano$transformation)
          ),
          shiny::selectInput(
            inputId = ns("volcanoTest"),
            label = "Select test:",
            choices = c("t-test" = "ttest",
                        "Mann-Whitney" = "mw"),
            selected = shiny::isolate(analysis_settings$volcano$test)
          ),
          shiny::selectInput(
            inputId = ns("volcanoGroup"),
            label = "Select group column:",
            choices = r$columns$groups,
            selected = r$columns$groups
          ),
          shiny::selectInput(
            inputId = ns("volcanoGroup1"),
            label = "Select group 1:",
            choices = choices_group,
            selected = shiny::isolate(analysis_settings$volcano$group1)
          ),
          shiny::selectInput(
            inputId = ns("volcanoGroup2"),
            label = "Select group 2:",
            choices = choices_group,
            selected = shiny::isolate(analysis_settings$volcano$group2)
          ),
          style = "font-size:75%"
        )
      )
    })


    shiny::observeEvent(
      c(
        input$volcanoSelectTable,
        input$volcanoTransformation,
        input$volcanoTest,
        input$volcanoGroup,
        input$volcanoGroup1,
        input$volcanoGroup2
      ), {
        analysis_settings$volcano$table <- input$volcanoSelectTable
        analysis_settings$volcano$transformation <- input$volcanoTransformation
        analysis_settings$volcano$test <- input$volcanoTest
        analysis_settings$volcano$group <- input$volcanoGroup
        analysis_settings$volcano$group1 <- input$volcanoGroup1
        analysis_settings$volcano$group2 <- input$volcanoGroup2
      }
    )


    shiny::observeEvent(input$volcanoGroup, {
      shiny::req(input$volcanoGroup)
      choices_group <- unique(r$tables$meta_data[r$tables$meta_data[[r$columns$sampleid]] %in% r$index$selected_samples, input$volcanoGroup])
      analysis_settings$volcano$group1 <- choices_group[1]
      analysis_settings$volcano$group2 <- choices_group[2]

      shiny::updateSelectInput(
        session = session,
        inputId = ns("volcanoGroup1"),
        choices = choices_group,
        selected = analysis_settings$volcano$group1
      )
      shiny::updateSelectInput(
        session = session,
        inputId = ns("volcanoGroup2"),
        choices = choices_group,
        selected = analysis_settings$volcano$group2
      )
    })


    output$volcanoPlot <- plotly::renderPlotly({
      shiny::req(input$volcanoGroup1 != input$volcanoGroup2,
                 input$volcanoSelectTable,
                 input$volcanoTransformation,
                 input$volcanoTest,
                 input$volcanoGroup)

      area_column <- switch(
        input$volcanoSelectTable,
        "raw" = "area",
        "totNorm" = "totNormArea",
        "pqnNorm" = "pqnNormArea"
      )

      test_data <- r$tables$analysis_data[r$tables$analysis$keep == TRUE &
                                            r$tables$analysis$class_keep == TRUE &
                                            r$tables$analysis$sample_name %in% r$index$selected_samples, ]

      plot_data <- do_test(data = test_data,
                           area_column = area_column,
                           transformation = input$volcanoTransformation,
                           test = input$volcanoTest,
                           group = input$volcanoGroup,
                           group1 = input$volcanoGroup1,
                           group2 = input$volcanoGroup2)

      ply <- show_volcano(data = plot_data)

      return(ply)
    })

  })
}
