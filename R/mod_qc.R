#' qc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer
#' @importFrom ggplot2 .data ggplot aes geom_histogram geom_vline labs
#'     theme_minimal guides guide_legend theme geom_violin geom_jitter
#'     geom_hline element_text scale_fill_gradient geom_tile
#' @importFrom tidyr pivot_wider
#' @importFrom stats cor
#'
mod_qc_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::navset_card_tab(
      #--------------------------------------------------- general settings ----
      bslib::nav_panel(
        title = "QC - overall",
        value = "qc_overall",
        bslib::card(
          shiny::plotOutput(
            outputId = ns("qc_overall_plot")
          )
        )
      ),
      bslib::nav_panel(
        title = "QC - Lipid class",
        value = "qc_lipidclass",
        bslib::card(
          shiny::plotOutput(
            outputId = ns("qc_lipidclass_plot")
          )
        )
      ),
      bslib::nav_panel(
        title = "QC - correlation",
        value = "qc_correlation",
        bslib::card(
          shiny::plotOutput(
            outputId = ns("qc_correlation_plot")
          )
        )
      )
    )

  )
}

#' qc Server Functions
#'
#' @noRd
mod_qc_server <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$qc_overall_plot <- shiny::renderPlot({
      shiny::req(r$tables$qc_data,
                 r$settings$rsd_cutoff)

      p <- r$tables$qc_data |>
        ggplot2::ggplot(ggplot2::aes(x = .data$rsd,
                                     fill = .data$polarity)) +
        ggplot2::geom_vline(xintercept = r$settings$rsd_cutoff,
                            colour = "red",
                            linetype = 2) +
        ggplot2::geom_histogram(binwidth = 0.05,
                                alpha = 0.3) +
        ggplot2::labs(x = "Relative standard deviation") +
        ggplot2::guides(fill = ggplot2::guide_legend(title = "Polarity"),
                        override.aes = list(alpha = 1)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")

      return(p)
    })


    output$qc_lipidclass_plot <- shiny::renderPlot({
      shiny::req(r$tables$qc_data,
                 r$settings$rsd_cutoff)

      p <- r$tables$qc_data |>
        ggplot2::ggplot(ggplot2::aes(x = .data$class,
                                     y = .data$rsd)) +
        ggplot2::geom_violin(scale = "width") +
        ggplot2::geom_jitter(ggplot2::aes(colour = .data$polarity),
                             alpha = 0.5) +
        ggplot2::geom_hline(yintercept = r$settings$rsd_cutoff,
                            colour = "red",
                            linetype = 2) +
        ggplot2::guides(colour = ggplot2::guide_legend(title = "Polarity"),
                        override.aes = list(alpha = 1)) +
        ggplot2::labs(y = "Relative standard deviation",
                      x = "Lipid class",
                      title = "RSD per lipidclass") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom",
                       axis.text.x = ggplot2::element_text(angle = 90,
                                                           hjust = 1))

      return(p)
    })


    output$qc_correlation_plot <- shiny::renderPlot({
      shiny::req(r$tables$analysis_data,
                 r$settings$rsd_cutoff)

      df_m <- r$tables$analysis_data[
        r$tables$analysis_data$sample_name %in%
          c(r$index$selected_pools, r$index$selected_samples),
      ] |>
        tidyr::pivot_wider(id_cols = .data$my_id,
                           names_from = .data$sample_name,
                           values_from = .data$area)

      df_m <- df_m[, -1]
      df_m[df_m == 0] <- 1
      cormat <- as.data.frame(stats::cor(log10(df_m)))
      cormat$x <- rownames(cormat)

      cormat_long <- cormat |>
        tidyr::pivot_longer(
          cols = colnames(cormat)[-ncol(cormat)],
          names_to = "y",
          values_to = "area"
        )

      p <- cormat_long |>
        ggplot2::ggplot(ggplot2::aes(x = .data$x,
                                     y = .data$y,
                                     fill = .data$area)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_continuous(limits = c(-1, 1)) +
        ggplot2::scale_fill_gradient(low = "blue",
                                     high = "red") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                           hjust = 1))

      return(p)
    })
  })
}
