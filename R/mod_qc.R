#' qc UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer
#' @importFrom plotly plotlyOutput renderPlotly ggplotly
#'
mod_qc_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::navset_card_tab(
      #------------------------------------------------------------- QC RSD ----
      bslib::nav_panel(
        title = "QC - RSD",
        value = "qc_rsd",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              shiny::selectInput(
                inputId = ns("qc_select_rsd_type"),
                label = "RSD plot",
                choices = c("Overall" = "overall",
                            "Per batch" = "batch")
              )
            ),
            shiny::plotOutput(
              outputId = ns("qc_rsd_plot")
            )
          )
        )
      ),
      #------------------------------------------------------- QC RSD class ----
      bslib::nav_panel(
        title = "QC - RSD Class",
        value = "qc_class",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              shiny::selectInput(
                inputId = ns("qc_select_rsd_class_type"),
                label = "RSD Class plot",
                choices = c("Overall" = "overall",
                            "Per batch" = "batch")
              )
            ),
            shiny::plotOutput(
              outputId = ns("qc_class_plot")
            )
          )
        )
      ),
      #----------------------------------------------------- QC Correlation ----
      bslib::nav_panel(
        title = "QC - correlation ",
        value = "qc_correlation",
        bslib::card(
          plotly::plotlyOutput(
            outputId = ns("qc_correlation_plot")
          )
        )
      ),#--------------------------------------------------------- QC Trend ----
      bslib::nav_panel(
        title = "QC - Trend",
        value = "qc_trend",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              shiny::selectInput(
                inputId = ns("qc_select_trend_type"),
                label = "Trend plot",
                choices = c("Overall" = "overall",
                            "Per batch" = "batch")
              )
            ),
            shiny::plotOutput(
              outputId = ns("qc_trend_plot")
            )
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

    output$qc_rsd_plot <- shiny::renderPlot({
      shiny::req(r$tables$rsd_data_overall,
                 r$tables$rsd_data_batch,
                 r$settings$rsd_cutoff,
                 input$qc_select_rsd_type)

      p <- switch(
        input$qc_select_rsd_type,
        "batch" = {
          show_batch_hist(data = r$tables$rsd_data_batch,
                          rsd_cutoff = ifelse(r$settings$apply_rsd_cutoff,
                                              r$settings$rsd_cutoff,
                                              NA))
        },
        "overall" = {
          show_overall_hist(data = r$tables$rsd_data_overall,
                            rsd_cutoff = ifelse(r$settings$apply_rsd_cutoff,
                                                r$settings$rsd_cutoff,
                                                NA))
        }
      )

      return(p)
    })


    output$qc_class_plot <- shiny::renderPlot({
      shiny::req(r$tables$rsd_data_overall,
                 r$tables$rsd_data_batch,
                 r$settings$rsd_cutoff,
                 input$qc_select_rsd_class_type)

      p <- switch(
        input$qc_select_rsd_class_type,
        "batch" = {
          show_class_batch_violin(data = r$tables$rsd_data_batch,
                                  rsd_cutoff = ifelse(r$settings$apply_rsd_cutoff,
                                                      r$settings$rsd_cutoff,
                                                      NA))
        },
        "overall" = {
          show_class_overall_violin(data = r$tables$rsd_data_overall,
                                    rsd_cutoff = ifelse(r$settings$apply_rsd_cutoff,
                                                        r$settings$rsd_cutoff,
                                                        NA))
        }
      )

      return(p)
    })


    output$qc_correlation_plot <- plotly::renderPlotly({
      shiny::req(r$tables$analysis_data)

      cor_df <- calc_cor(data = r$tables$analysis_data,
                         idx_pools = r$index$selected_pools,
                         idx_samples = r$index$selected_samples)

      p <- qc_cor_plot(data = cor_df)

      ply <- plotly::ggplotly(p = p)

      return(ply)
    })


    output$qc_trend_plot <- shiny::renderPlot({
      shiny::req(r$tables$trend_data,
                 input$qc_select_trend_type)

      if(!is.null(r$tables$trend_data)) {
        p <- trend_plot(trend_data = r$tables$trend_data,
                        type = input$qc_select_trend_type)
      } else {
        p <- NULL
      }
      return(p)
    })

  })
}
