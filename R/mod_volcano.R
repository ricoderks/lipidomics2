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
          shiny::actionButton(
            inputId = ns("volcanoComments"),
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
        bslib::card(
          plotly::plotlyOutput(
            outputId = ns("volcanoPlot")
          ),
          height = "100%"
        )
      )
    )
  )
}

#' volcano Server Functions
#'
#' @importFrom bsicons bs_icon
#' @importFrom bslib accordion accordion_panel
#' @importFrom plotly event_data event_register
#'
#' @noRd
mod_volcano_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    print("Volcano plot server started")

    choices_group <- unique(r$tables$meta_data[r$tables$meta_data[[r$columns$filename]] %in% r$index$selected_samples, r$columns$group[1]])
    analysis_settings <- shiny::reactiveValues(
      volcano = list(
        settings = list(
          table = "raw",
          transformation = "none",
          test = "ttest",
          group = r$columns$groups[1],
          comparison = NULL,
          group1 = choices_group[1],
          group2 = choices_group[2],
          fc_threshold = 2,
          pvalue_threshold = 0.05,
          feature_annotation = "none"
        ),
        comments = list(
          comment_before = NULL,
          comment_after = NULL
        )
      )
    )

    exportplot <- shiny::reactiveValues(
      plot = NULL
    )

    output$settingsVolcano <- shiny::renderUI({
      selection <- c(
        "Raw data" = "raw",
        "Total area normalization" = "totNorm",
        "PQN normalization" = "pqnNorm",
        "Protein normalization" = "protNorm"
      )

      selected <- names(unlist(r$analysis$normalization)[unlist(r$analysis$normalization)])
      selected <- c("raw", selected)

      choices_group <- unique(
        r$tables$meta_data[r$tables$meta_data[[r$columns$filename]] %in% r$index$selected_samples,
                           shiny::isolate(analysis_settings$volcano$settings$group)]
      )

      shiny::tagList(
        shiny::div(
          bslib::accordion(
            multiple = FALSE,
            bslib::accordion_panel(
              title = "Test settings",
              icon = bsicons::bs_icon("menu-app"),
              shiny::selectInput(
                inputId = ns("volcanoSelectTable"),
                label = "Select data table:",
                choices = selection[selection %in% selected],
                selected = shiny::isolate(analysis_settings$volcano$settings$table)
              ),
              shiny::selectInput(
                inputId = ns("volcanoTransformation"),
                label = "Transformation:",
                choices = c("None" = "none",
                            "Log10" = "log10",
                            "Log1p" = "log1p"),
                selected = shiny::isolate(analysis_settings$volcano$settings$transformation)
              ),
              shiny::selectInput(
                inputId = ns("volcanoTest"),
                label = "Select test:",
                choices = c("t-test" = "ttest",
                            "Mann-Whitney" = "mw"),
                selected = shiny::isolate(analysis_settings$volcano$settings$test)
              ),
              shiny::selectInput(
                inputId = ns("volcanoGroup"),
                label = "Select group column:",
                choices = r$columns$groups,
                selected = shiny::isolate(analysis_settings$volcano$settings$group)
              ),
              shiny::selectInput(
                inputId = ns("volcanoGroup1"),
                label = "Select group 1:",
                choices = choices_group,
                selected = shiny::isolate(analysis_settings$volcano$settings$group1)
              ),
              shiny::selectInput(
                inputId = ns("volcanoGroup2"),
                label = "Select group 2:",
                choices = choices_group,
                selected = shiny::isolate(analysis_settings$volcano$settings$group2)
              )
            ),
            bslib::accordion_panel(
              title = "Plot settings",
              icon = bsicons::bs_icon("menu-app"),
              shiny::selectInput(
                inputId = ns("volcanoFeatureAnnotation"),
                label = "Feature annotation",
                choices = c("None" = "none",
                            "Lipid class" = "Class"),
                selected = shiny::isolate(analysis_settings$volcano$settings$feature_annotation)
              ),
              shiny::sliderInput(
                inputId = ns("volcanoFcThreshold"),
                label = "Fold change threshold:",
                value = shiny::isolate(analysis_settings$volcano$settings$fc_threshold),
                min = 1,
                max = 4,
                step = 0.1
              ),
              shiny::sliderInput(
                inputId = ns("volcanoPValueThreshold"),
                label = "p-value threshold:",
                value = shiny::isolate(analysis_settings$volcano$settings$pvalue_threshold),
                min = 0,
                max = 0.1,
                step = 0.005
              )
            )
          ), # end accordion panel
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
        input$volcanoGroup2,
        input$volcanoFcThreshold,
        input$volcanoPValueThreshold
      ), {
        analysis_settings$volcano$settings$table <- input$volcanoSelectTable
        analysis_settings$volcano$settings$transformation <- input$volcanoTransformation
        analysis_settings$volcano$settings$test <- input$volcanoTest
        analysis_settings$volcano$settings$group <- input$volcanoGroup
        analysis_settings$volcano$settings$group1 <- input$volcanoGroup1
        analysis_settings$volcano$settings$group2 <- input$volcanoGroup2
        analysis_settings$volcano$settings$fc_threshold <- input$volcanoFcThreshold
        analysis_settings$volcano$settings$pvalue_threshold <- input$volcanoPValueThreshold
      },
      ignoreInit = TRUE
    )


    shiny::observeEvent(input$volcanoGroup, {
      shiny::req(input$volcanoGroup)
      # this needs to be fixed, sampleid or filename
      # this is in multiple locations
      choices_group <- unique(r$tables$meta_data[r$tables$meta_data[[r$columns$filename]] %in% r$index$selected_samples, input$volcanoGroup])
      analysis_settings$volcano$settings$group1 <- choices_group[1]
      analysis_settings$volcano$settings$group2 <- choices_group[2]

      shiny::updateSelectInput(
        session = session,
        inputId = ns("volcanoGroup1"),
        choices = choices_group,
        selected = analysis_settings$volcano$settings$group1
      )
      shiny::updateSelectInput(
        session = session,
        inputId = ns("volcanoGroup2"),
        choices = choices_group,
        selected = analysis_settings$volcano$settings$group2
      )
    })


    output$volcanoPlot <- plotly::renderPlotly({
      shiny::req(input$volcanoGroup1 != input$volcanoGroup2,
                 input$volcanoSelectTable,
                 input$volcanoTransformation,
                 input$volcanoTest,
                 input$volcanoGroup,
                 input$volcanoFcThreshold,
                 input$volcanoPValueThreshold)

      print("show plot")
      print(id)

      area_column <- switch(
        input$volcanoSelectTable,
        "raw" = "area",
        "totNorm" = "totNormArea",
        "pqnNorm" = "pqnNormArea",
        "protNorm" = "protNormArea"
      )

      test_data <- r$tables$analysis_data[r$tables$analysis$keep == TRUE &
                                            r$tables$analysis$class_keep == TRUE &
                                            r$tables$analysis$sample_name %in% r$index$selected_samples, ]

      plot_data <- do_test(
        data = test_data,
        area_column = area_column,
        transformation = input$volcanoTransformation,
        test = input$volcanoTest,
        group = input$volcanoGroup,
        group1 = input$volcanoGroup1,
        group2 = input$volcanoGroup2
      )

      ply <- show_volcano(
        data = plot_data,
        name = id,
        fc_threshold = as.numeric(input$volcanoFcThreshold),
        pvalue_threshold = as.numeric(input$volcanoPValueThreshold),
        feature_annotation = input$volcanoFeatureAnnotation,
        right_label = input$volcanoGroup1,
        left_label = input$volcanoGroup2
      )

      exportplot$plot <- ply

      ply <- ply |>
        plotly::event_register(event = "plotly_click")

      return(ply)
    })


    #--------------------------------------------------------- popup violin ----
    shiny::observeEvent(
      plotly::event_data(
        event = "plotly_click",
        source = id
      ), {

        ns <- session$ns

        ed <- plotly::event_data(event = "plotly_click",
                                 source = id)

        if (is.null(ed) || is.null(ed$customdata)) return()

        my_id <- ed$customdata

        features <- unique(r$tables$analysis_data[, c("my_id", "LongLipidName")])
        selected_feature <- features$LongLipidName[features$my_id == my_id]

        output$violin_modal <- plotly::renderPlotly({
          area_column <- switch(
            input$volcanoSelectTable,
            "raw" = "area",
            "totNorm" = "totNormArea",
            "pqnNorm" = "pqnNormArea",
            "protNorm" = "protNormArea"
          )

          plot_data <- r$tables$analysis_data[r$tables$analysis$keep == TRUE &
                                                r$tables$analysis$class_keep == TRUE &
                                                r$tables$analysis$sample_name %in% r$index$selected_samples, ]
          plot_data <- plot_data[plot_data$my_id == my_id, ]


          ply <- show_violin(
            plot_data = plot_data,
            area_column = area_column,
            group = input$volcanoGroup,
            group1 = input$volcanoGroup1,
            group2 = input$volcanoGroup2
          )

          return(ply)
        })

        shiny::showModal(
          shiny::modalDialog(
            title = paste("Selected lipid:", selected_feature),
            size = "l",
            easyClose = TRUE,
            footer = shiny::modalButton(label = "Close"),
            plotly::plotlyOutput(
              outputId = ns("violin_modal"),
              height = "450px"
            )
          ),
          session = session
        )
      })

    #-------------------------------------------------------------- comment ----
    shiny::observeEvent(input$volcanoComments, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Add comments for the volcano plots",
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton(label = "Close"),
          shiny::textAreaInput(
            label = "Comment before:",
            inputId = ns("volcanoCommentBefore"),
            value = analysis_settings$volcano$comments$comment_before,
            width = "100%"
          ),
          shiny::textAreaInput(
            label = "Comment after:",
            inputId = ns("volcanoCommentAfter"),
            value = analysis_settings$volcano$comments$comment_after,
            width = "100%"
          )
        ),
        session = session
      )
    })


    shiny::observeEvent(c(input$volcanoCommentBefore), {
      analysis_settings$volcano$comments$comment_before <- input$volcanoCommentBefore
    })


    shiny::observeEvent(c(input$volcanoCommentAfter), {
      analysis_settings$volcano$comments$comment_after <- input$volcanoCommentAfter
    })

    #--------------------------------------------------------------- export ----
    export <- function() {
      res <- list(
        plot = exportplot$plot,
        settings = analysis_settings$volcano$settings,
        comments = analysis_settings$volcano$comments
      )

      return(res)
    }

    return(list(export = export))
  })
}
