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
    bslib::navset_card_tab(
      id = "Analysis",
      bslib::nav_panel(
        title = "Heatmap",
        value = "analysis_heatmap",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              shiny::p("Settings")
            )
          ),
          bslib::card_body(
            shiny::p("Show heatmap")
          )
        )
      )
      # ,
      # bslib::nav_panel(
      #   title = "Compare samples",
      #   value = "analysis_compare",
      #   bslib::card(
      #     bslib::page_sidebar(
      #       sidebar = bslib::sidebar(
      #         shiny::p("Settings")
      #       )
      #     ),
      #     bslib::card_body(
      #       shiny::p("Show volcano plot")
      #     )
      #   )
      # ),
      # bslib::nav_panel(
      #   title = "PCA",
      #   value = "analysis_pca",
      #   bslib::card(
      #     bslib::page_sidebar(
      #       sidebar = bslib::sidebar(
      #         shiny::p("Settings")
      #       )
      #     ),
      #     bslib::card_body(
      #       shiny::p("Show PCA")
      #     )
      #   )
      # )
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

  })
}
