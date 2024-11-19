#' bubbleplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer plotOutput renderPlot
#' @importFrom ggplot2 ggplot aes .data geom_point scale_size geom_line
#'     geom_text facet_grid labs guides coord_cartesian theme element_rect
#'
mod_bubbleplot_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # shiny::plotOutput(
    plotly::plotlyOutput(
      outputId = ns("bubble_plot")
    )
  )
}

#' bubbleplot Server Functions
#'
#' @noRd
#'
mod_bubbleplot_server <- function(id, r, class_pattern){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    # output$bubble_plot <- shiny::renderPlot({
    output$bubble_plot <- plotly::renderPlotly({
      shiny::req(r$tables$analysis_data,
                 r$index$selected_pools,
                 class_pattern)

      plot_data <- r$tables$analysis_data[r$tables$analysis_data$sample_name == r$index$selected_pools[1], ]

      plot_data <- plot_data[grepl(x = plot_data$Class,
                                   pattern = class_pattern) &
                               plot_data$class_keep == TRUE &
                               plot_data$rsd_keep == TRUE, ]

      if(nrow(plot_data) > 0) {
        p <- plot_data |>
          ggplot2::ggplot(ggplot2::aes(x = .data$AverageRT,
                                       y = .data$AverageMZ,
                                       color = .data$carbons)) +
          ggplot2::geom_point(ggplot2::aes(size = .data$DotProduct),
                              alpha = 0.4) +
          # show lipid which should be discarded as grey
          ggplot2::geom_point(data = plot_data[plot_data$keep == FALSE, ],
                              ggplot2::aes(size = .data$DotProduct),
                              color = "grey",
                              alpha = 1) +
          ggplot2::scale_size(range = c(1, 10),
                              limits = c(0, 100)) +
          ggplot2::geom_line() +
          ggplot2::geom_text(ggplot2::aes(label = .data$carbon_db),
                             size = 3.0,
                             color = "black") +
          ggplot2::facet_grid(.data$Class ~ .data$ion,
                              scales = "free") +
          ggplot2::labs(x = "Retention time [minutes]",
                        y = "m/z") +
          ggplot2::guides(color = "none",
                          size = "none") +
          # ggplot2::coord_cartesian(xlim = ranges$x,
          #                          ylim = ranges$y) +
          ggplot2::theme_minimal() +
          ggplot2::theme(strip.text = ggplot2::element_text(size = 10,
                                                            colour = "white",
                                                            face = "bold"),
                         strip.background = ggplot2::element_rect(fill = "#007cc2",
                                                                  colour = "white"))
      } else {
        print("Nothing to show")
        p <- NULL
      }

      ply <- plotly::ggplotly(p = p)

      return(ply)
    })


  })
}
