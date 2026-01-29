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
      bslib::navset_tab(
        id = ns("analysisTabs")
      )
      # shiny::uiOutput(
      #   outputId = ns("analysis_main_ui")
      # )
      # mod_bubbleplot_ui(id = ns("bubble"))
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
      modules = list(),   # id -> list(type, label, remove_requested, export)
      labels  = list()    # id -> label
    )

    add_analysis_tab <- function(type = NULL) {
      rv$next_id <- rv$next_id + 1L
      rv$type_counts[[type]] <- rv$type_counts[[type]] + 1L
      tabId <- paste0(tolower(gsub("[^a-zA-Z0-9]+", "_", type)), "_", rv$next_id)
      label <- paste0(type, " #", rv$type_counts[[type]])

      ui_content <- switch(
        type,
        "heatmap" = mod_heatmap_ui(id = tabId),
        "pca"     = mod_pca_ui(id = tabId),
        "volcano"  = mod_volcano_ui(id = tabId),
        div("Unknown analysis type.")
      )

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
        "heatmap" = mod_heatmap_server(id = id, r = r),
        "pca"     = mod_pca_server(id = id, r = r),
        "volcano"  = mod_volcano_server(id = id, r = r),
        NULL
      )

      rv$modules[[tabId]] <- list(type = type, label = label, export = NA)
      rv$labels[[tabId]]  <- label

      print(tabId)
      print(names(input))
      print(mod$remove_requested())

      shiny::observeEvent(mod$remove_requested, {
        # clicking the button is not observed
        print("Debugging: remove tab")

        bslib::nav_remove(
          id = "analysisTabs",
          target = tabId,
          session = session
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
        shiny::p("This is the sidebar."),
        shiny::selectInput(
          inputId = ns("selectAnalysisMethod"),
          label = "Select analysis",
          choices = list(
            "Heatmap" = "heatmap",
            "PCA" = "pca",
            "Volcano plot" = "volcano"
          )
        ),
        shiny::actionButton(
          inputId = ns("addAnalysisTab"),
          label = "Add tab"
        )
      )
    })


    shiny::observeEvent(input$addAnalysisTab, {
      shiny::req(input$selectAnalysisMethod)

      add_analysis_tab(type = input$selectAnalysisMethod)
    })


  })
}
