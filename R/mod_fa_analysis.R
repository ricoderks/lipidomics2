#' fa_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card layout_sidebar
#'
mod_fa_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::uiOutput(
            outputId = ns("settingsFa")
          ),
          shiny::actionButton(
            inputId = ns("faComments"),
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
          outputId = ns("faPlot")
        )
      )
    )
  )
}

#' fa_analysis Server Functions
#'
#' @noRd
mod_fa_analysis_server <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    print("Fatty acid analysis server started")

    analysis_settings <- shiny::reactiveValues(
      faAnalysis = list(
        settings = list(
          table = "raw",
          lipid_class = "all",
          view = "faClass",
          group = r$columns$groups[1]
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

    output$settingsFa <- shiny::renderUI({
      selection <- c(
        "Raw data" = "raw",
        "Total area normalization" = "totNorm",
        "PQN normalization" = "pqnNorm",
        "Protein normalization" = "protNorm"
      )

      selected <- names(unlist(r$analysis$normalization)[unlist(r$analysis$normalization)])
      selected <- c("raw", selected)

      # for the lipid class selection, only show lipid class which have at least
      # one lipid with a lipid tail annotated.
      my_ids <- unique(
        r$tables$analysis_data$my_id[r$tables$analysis$keep == TRUE &
                                       r$tables$analysis$class_keep == TRUE &
                                       r$tables$analysis$sample_name %in% r$index$selected_samples]
      )

      feature_data <- r$tables$feature_data[r$tables$feature_data$my_id %in% my_ids, ]
      lipid_classes <- unique(as.character(droplevels(feature_data[feature_data$carbon_1 != 0, "Class"])))
      names(lipid_classes) <- lipid_classes
      lipid_classes <- c("All (incl. TG)" = "all",
                         "All (excl. TG)" = "all_noTG",
                         lipid_classes)

      shiny::tagList(
        shiny::div(
          shiny::selectInput(
            inputId = ns("faSelectTable"),
            label = "Select data table:",
            choices = selection[selection %in% selected],
            selected = shiny::isolate(analysis_settings$faAnalysis$settings$table)
          ),
          # select input for total or fa tails
          shiny::selectInput(
            inputId = ns("faSelectView"),
            label = "Select view:",
            choices = c("FA overview per class" = "faClass",
                        "Class overview per FA" = "classFa"),
            selected = shiny::isolate(analysis_settings$faAnalysis$settings$view)
          ),
          # select input for lipid class
          shiny::selectInput(
            inputId = ns("faSelectClass"),
            label = "Select lipid class:",
            choices = lipid_classes,
            selected = shiny::isolate(analysis_settings$faAnalysis$settings$lipid_class)
          ),
          # select input for fa tails
          # select input for group
          shiny::selectInput(
            inputId = ns("faSelectGroup"),
            label = "Group:",
            choices = c("none", r$columns$groups),
            selected = shiny::isolate(analysis_settings$faAnalysis$settings$group)
          ),
          style = "font-size:75%"
        ) # end div
      ) # end tagList
    })


    shiny::observeEvent(
      c(
        input$faSelectTable,
        input$faSelectClass,
        input$faSelectView,
        input$faSelectGroup
      ), {
        analysis_settings$faAnalysis$settings$table <- input$faSelectTable
        analysis_settings$faAnalysis$settings$lipid_class <- input$faSelectClass
        analysis_settings$faAnalysis$settings$view <- input$faSelectView
        analysis_settings$faAnalysis$settings$group <- input$faSelectGroup
      }
    )


    output$faPlot <- plotly::renderPlotly({
      shiny::req(
        input$faSelectTable,
        input$faSelectClass,
        input$faSelectView,
        input$faSelectGroup != "none"
      )

      area_column <- switch(
        input$faSelectTable,
        "raw" = "area",
        "totNorm" = "totNormArea",
        "pqnNorm" = "pqnNormArea",
        "protNorm" = "protNormArea"
      )

      fa_data <- r$tables$analysis_data[r$tables$analysis$keep == TRUE &
                                          r$tables$analysis$class_keep == TRUE &
                                          r$tables$analysis$sample_name %in% r$index$selected_samples, ]

      keep_ids <- unique(fa_data$my_id)

      res <- fa_analysis_calc(data = fa_data,
                              feature_data = r$tables$feature_data[r$tables$feature_data$my_id %in% keep_ids, ],
                              area_column = area_column,
                              group_column = input$faSelectGroup,
                              selected_lipidclass = input$faSelectClass)

      ply <- show_fa_plot(data = res$plot_data,
                          title = input$faSelectClass,
                          subtitle = res$features,
                          y_title = input$faSelectTable)

      return(ply)
    })

    #-------------------------------------------------------------- comment ----
    shiny::observeEvent(input$faComments, {
      shiny::showModal(
        shiny::modalDialog(
          title = "Add comments for Fatty acid analysis",
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton(label = "Close"),
          shiny::textAreaInput(
            label = "Comment before:",
            inputId = ns("faCommentBefore"),
            value = analysis_settings$faAnalysis$comments$comment_before,
            width = "100%"
          ),
          shiny::textAreaInput(
            label = "Comment after:",
            inputId = ns("faCommentAfter"),
            value = analysis_settings$faAnalysis$comments$comment_after,
            width = "100%"
          )
        ),
        session = session
      )
    })

    shiny::observeEvent(c(input$faCommentBefore), {
      analysis_settings$faAnalysis$comments$comment_before <- input$faCommentBefore
    })

    shiny::observeEvent(c(input$faCommentAfter), {
      analysis_settings$faAnalysis$comments$comment_after <- input$faCommentAfter
    })


    #--------------------------------------------------------------- export ----
    export <- function() {
      res <- list(
        plot = exportplot$plot,
        settings = analysis_settings$faAnalysis$settings,
        comments = analysis_settings$faAnalysis$comments
      )

      return(res)
    }

    return(list(export = export))
  })
}
