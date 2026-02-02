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
    bslib::card(
      bslib::page_sidebar(
        sidebar = bslib::sidebar(
          shiny::actionButton(
            inputId = ns("remove"),
            label = "",
            icon = icon("trash"),
            class = "btn-danger",
            width = "50%"
          )
        ),
        plotly::plotlyOutput(
          outputId = ns("heatmap")
        ),
        shiny::plotOutput(
          outputId = ns("testplot")
        )
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

    output$heatmap <- plotly::renderPlotly({
      # shiny::req(r$tables$analysis_data)
      print("Show heatmap here")
      print(head(r$tables$clean_data))

      # return(NULL)
    })

    output$testplot <- shiny::renderPlot({
      plot_data <- data.frame(
        x = 1:100,
        y = rnorm(n = 100)
      )

      p <- plot_data |>
        ggplot2::ggplot(ggplot2::aes(x = x,
                                     y = y)) +
        ggplot2::geom_point() +
        ggplot2::theme_minimal()

      return(p)
    })
  })
}
