#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer
#' @importFrom bslib navset_card_tab nav_panel card page_sidebar sidebar card_body
#'
mod_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::uiOutput(
          outputId = ns("analysis_sidebar_ui")
        ),
        width = 325
      ),
      bslib::navset_card_tab(
        id = ns("analysisTabs"),
        bslib::nav_panel(
          title = "Settings",
          value = "analysisSettingsPanel",
          bslib::card(
            shiny::uiOutput(
              outputId = ns("analysisSettings")
            )
          )
        )
      )
    )
  )
}

#' analysis Server Functions
#'
#' @noRd
#'
mod_analysis_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    rv <- reactiveValues(
      next_id = 0L,
      type_counts = setNames(as.list(rep(0L, 3L)), c("heatmap", "pca", "volcano")),
      modules = list(),   # id -> list(type, label, export)
      labels  = list()    # id -> label
    )

    add_analysis_tab <- function(type = NULL) {
      rv$next_id <- rv$next_id + 1L
      rv$type_counts[[type]] <- rv$type_counts[[type]] + 1L
      tabId <- paste0(tolower(gsub("[^a-zA-Z0-9]+", "_", type)), "_", rv$next_id)
      label <- paste0(type, " #", rv$type_counts[[type]])

      ui_content <- switch(
        type,
        "heatmap" = mod_heatmap_ui(id = ns(tabId)),
        "pca"     = mod_pca_ui(id = ns(tabId)),
        "volcano"  = mod_volcano_ui(id = ns(tabId)),
        div("Unknown analysis type.")
      )

      # do not put anything else in between ui_content and mod!
      bslib::nav_insert(
        id = "analysisTabs",
        nav = bslib::nav_panel(
          title = label,
          value = tabId,
          ui_content
        ),
        session = session,
        select = TRUE
      )

      mod <- switch(
        type,
        "heatmap" = mod_heatmap_server(id = tabId, r = r),
        "pca"     = mod_pca_server(id = tabId, r = r),
        "volcano"  = mod_volcano_server(id = tabId, r = r),
        NULL
      )

      rv$modules[[tabId]] <- list(type = type, label = label, export = NA)
      rv$labels[[tabId]]  <- label

      shiny::observeEvent(input[[paste0(tabId, "-remove")]], {
        bslib::nav_remove(
          id = "analysisTabs",
          target = tabId
        )
        rv$modules[[tabId]] <- NULL
        rv$labels[[tabId]]  <- NULL
      },
      once = TRUE,
      ignoreInit = TRUE)
    }


    output$analysis_sidebar_ui <- shiny::renderUI({
      # The selectInput will be dynamically in the future depending
      # on if it is metabolomics or lipidomics project.

      shiny::tagList(
        shiny::h4("Analysis"),
        shiny::selectInput(
          inputId = ns("selectAnalysisMethod"),
          label = "Select an analysis:",
          choices = list(
            "Heatmap" = "heatmap",
            "PCA" = "pca",
            "Volcano plot" = "volcano"
          )
        ),
        shiny::actionButton(
          inputId = ns("addAnalysisTab"),
          label = "Add analysis",
          width = "75%"
        )
      )
    })


    shiny::observeEvent(input$addAnalysisTab, {
      shiny::req(input$selectAnalysisMethod)

      add_analysis_tab(type = input$selectAnalysisMethod)
    })


    output$analysisSettings <- shiny::renderUI({
      shiny::req(!is.null(r$analysis$normalization))

      normSelected <- names(unlist(r$analysis$normalization)[unlist(r$analysis$normalization)])

      shiny::tagList(
        shiny::h4("Trend correction"),
        shiny::hr(),
        shiny::h4("Normalization"),
        shiny::checkboxGroupInput(
          inputId = ns("selectNormalization"),
          label = "Select normalization",
          choices = c(
            "Total area normalization" = "totNorm",
            "PQN normalization" = "pqnNorm"
          ),
          selected = normSelected
        ),
        shiny::htmlOutput(
          outputId = ns("statusNormalization")
        ),
        shiny::actionButton(
          inputId = ns("applyNormalization"),
          label = "Apply normalization",
          width = "225px"
        ),
        shiny::hr()
      )
    })


    shiny::observeEvent(input$applyNormalization, {
      # shiny::req(r$tables$analysis_data)
      shiny::req(!is.null(r$analysis$normalization))

      # write.csv(x = r$tables$analysis_data,
      #           file = "test.csv")

      print("Apply normalization.")
      selection <- input$selectNormalization
      r$analysis$normalization[names(r$analysis$normalization)] <-
        ifelse(names(r$analysis$normalization) %in% selection, TRUE, FALSE)

      res <- switch(
        selection,
        "totNorm" = total_area_norm(data = r$tables$analysis_data)
      )

      # write.csv(x = res,
      #           file = "test_norm.csv")
      r$tables$analysis_data <- res

    })


    output$statusNormalization <- shiny::renderUI({
      shiny::req(!is.null(r$analysis$normalization))
      print("Update status normalization")

      if(sum(unlist(r$analysis$normalization)) == 0) {
        statusText <- "No normalization applied"
      } else {
        statusText <- c()
        selection <- names(unlist(r$analysis$normalization)[unlist(r$analysis$normalization)])
        for(selected in selection) {
          tmp <- switch(
            selected,
            "totNorm" = "Total area normalization",
            "pqnNorm" = "PQN normalization"
          )
          statusText <- c(statusText, tmp)
        }
        statusText <- paste(statusText, collapse = ", ")
      }
      shiny::p(statusText)
    })

  })
}
