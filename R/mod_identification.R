#' identification UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer selectizeInput uiOutput htmlOutput
#'     renderText
#' @importFrom bslib layout_sidebar sidebar card card_header
#' @importFrom plotly event_data event_register plotlyOutput ggplotly renderPlotly
#' @importFrom ggplot2 ggplot aes theme_minimal theme geom_point geom_line
#'     geom_text facet_grid labs guides element_rect element_text
#'
mod_identification_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::uiOutput(
          outputId = ns("id_sidebar_ui")
        ),
        width = 325
      ),
      shiny::uiOutput(
        outputId = ns("id_main_ui")
      )
      # mod_bubbleplot_ui(id = ns("bubble"))
    )
  )
}

#' identification Server Functions
#'
#' @noRd
#'
mod_identification_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    w <- waiter::Waiter$new(
      html = shiny::tagList(
        waiter::spin_loaders(id = 8,
                             color = "black")
      ),
      color = "rgba(255, 255, 255, 0.5)"
    )

    output$id_sidebar_ui <- shiny::renderUI({
      shiny::req(r$omics)

      class_choices <- switch(
        r$omics,
        "lip" = lip_class_choices(r$defaults$lipid_classes)
      )

      shiny::tagList(
        shiny::selectizeInput(
          inputId = ns("id_select_class"),
          label = "Select a class:",
          choices = c("None", class_choices),
          multiple = FALSE
        ),
        shiny::hr(),
        shiny::htmlOutput(
          outputId = ns("reason")
        ),
        shiny::htmlOutput(
          outputId = ns("id_info")
        )
      )
    })


    output$id_main_ui <- shiny::renderUI({
      shiny::req(input$id_select_class)

      r$tables$analysis_data <- r$tables$analysis_data

      if(input$id_select_class != "None") {
        main_title <- shiny::h2(input$id_select_class)
      } else {
        main_title <- NULL
      }

      shiny::tagList(
        bslib::card(
          bslib::card_header(main_title),
          # mod_bubbleplot_ui(id = ns("bubble")),
          plotly::plotlyOutput(
            outputId = ns("bubble_plot")
          ),
          height = "100%"
        )
      )
    })


    output$bubble_plot <- plotly::renderPlotly({
      shiny::req(r$tables$analysis_data,
                 r$index$selected_pools,
                 input$id_select_class != "None")
      w$show()

      class_pattern <- get_class_pattern(classes = r$defaults$lipid_classes,
                                         class_name = input$id_select_class)

      plot_data <- r$tables$analysis_data[r$tables$analysis_data$sample_name == r$index$selected_pools[1], ]

      plot_data <- plot_data[grepl(x = plot_data$Class,
                                   pattern = class_pattern) &
                               plot_data$class_keep == TRUE &
                               plot_data$rsd_keep == TRUE, ]

      plot_height <- length(unique(plot_data$class_ion)) * 350

      if(nrow(plot_data) > 0) {
        p <- plot_data |>
          ggplot2::ggplot(ggplot2::aes(x = .data$AverageRT,
                                       y = .data$AverageMZ,
                                       color = .data$carbons,
                                       customdata = .data$my_id)) +
          ggplot2::geom_point(ggplot2::aes(size = .data$DotProduct),
                              alpha = 0.4) +
          ggplot2::geom_line() +
          # show lipid which should be discarded as grey
          ggplot2::geom_point(data = plot_data[plot_data$keep == FALSE, ],
                              ggplot2::aes(size = .data$DotProduct),
                              color = "grey",
                              alpha = 1) +
          ggplot2::scale_size(range = c(1, 10),
                              limits = c(0, 100)) +
          ggplot2::geom_line(ggplot2::aes(group = .data$carbons)) +
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

        ply <- plotly::ggplotly(p = p,
                                source = "bubbleplot_click",
                                height = plot_height) |>
          plotly::config(modeBarButtonsToRemove = c("toImage", "select2d",
                                                    "lasso2d",
                                                    "hoverClosestCartesian",
                                                    "hoverCompareCartesian")) |>
          plotly::event_register(event = "plotly_click")
      } else {
        print("Nothing to show")
        ply <- NULL
      }

      w$hide()

      return(ply)
    })


    output$id_info <- shiny::renderText({
      shiny::req(r$tables$analysis_data,
                 r$index$selected_pools,
                 input$id_select_class != "None")

      # if there is no plot this will give a warning, is there a way to check this
      d <- plotly::event_data(event = "plotly_click",
                              source = "bubbleplot_click")

      if(!is.null(d)) {
        info_df <- r$tables$analysis_data[r$tables$analysis_data$my_id == d$customdata, ][1, ]

        if(nrow(info_df) == 1) {
          # make sure if another class is selected, the info output is cleared
          class_pattern <- get_class_pattern(classes = r$defaults$lipid_classes,
                                             class_name = input$id_select_class)
          if(grepl(x = info_df$Class,
                   pattern = class_pattern)) {
            shiny::HTML("<table style=\"width:100%\">",
                        "<tr><td>ID :</td><td>", info_df$my_id, "</td></tr>",
                        "<tr><td>Average RT :</td><td>", info_df$AverageRT, "</td></tr>",
                        "<tr><td>Average <i>m/z</i> :</td><td>", info_df$AverageMZ, "</td></tr>",
                        "<tr><td>Ion :</td><td>", as.character(info_df$ion), "</td></tr>",
                        "<tr><td>Short lipid name :</td><td>", info_df$ShortLipidName, "</td></tr>",
                        "<tr><td>Long lipid name :</td><td>", info_df$LongLipidName, "</td></tr>",
                        "<tr><td>Lipid class :</td><td>", as.character(info_df$Class), "</td></tr>",
                        "<tr><td>Dot product :</td><td>", info_df$DotProduct, "</td></tr>",
                        "<tr><td>Reverse dot product :</td><td>", info_df$RevDotProduct, "</td></tr>",
                        "</table>")
          } else {
            shiny::HTML("")
          }
        } # end if is there only 1 point selected
      } # end if is something found after click
    })


    output$reason <- shiny::renderUI({
      shiny::req(r$tables$analysis_data,
                 r$index$selected_pools,
                 input$id_select_class != "None")

      # if there is no plot this will give a warning, is there a way to check this
      d <- plotly::event_data(event = "plotly_click",
                              source = "bubbleplot_click")

      if(!is.null(d)) {
        info_df <- r$tables$analysis_data[r$tables$analysis_data$my_id == d$customdata, ][1, ]

        lipid_status <- info_df$comment

        # make sure if another class is selected, the info output is cleared
        class_pattern <- get_class_pattern(classes = r$defaults$lipid_classes,
                                           class_name = input$id_select_class)
        if(grepl(x = info_df$Class,
                 pattern = class_pattern)) {
          shiny::tagList(
            shiny::selectInput(inputId = session$ns("select_reason"),
                               label = "Keep :",
                               choices = c("Keep" = "keep",
                                           "No convincing match" = "no_match",
                                           "Incorrect ret. time" = "wrong_rt",
                                           "High background" = "high_bg"),
                               selected = lipid_status)
          )
        }
      }

    })


    shiny::observeEvent(input$select_reason, {
      shiny::req(r$tables$analysis_data)

      # if there is no plot this will give a warning, is there a way to check this
      d <- plotly::event_data(event = "plotly_click",
                              source = "bubbleplot_click")

      if(!is.null(d)) {
        info_df <- r$tables$analysis_data[r$tables$analysis_data$my_id == d$customdata, ][1, ]

        if(input$select_reason != info_df$comment) {
          r$tables$analysis_data$comment[r$tables$analysis_data$my_id == info_df$my_id] <- input$select_reason

          switch(
            input$select_reason,
            "keep" = {
              r$tables$analysis_data$match_keep[r$tables$analysis_data$my_id == info_df$my_id] <- TRUE
              r$tables$analysis_data$rt_keep[r$tables$analysis_data$my_id == info_df$my_id] <- TRUE
              r$tables$analysis_data$background_keep[r$tables$analysis_data$my_id == info_df$my_id] <- TRUE
            },
            "no_match" = r$tables$analysis_data$match_keep[r$tables$analysis_data$my_id == info_df$my_id] <- FALSE,
            "wrong_rt" = r$tables$analysis_data$rt_keep[r$tables$analysis_data$my_id == info_df$my_id] <- FALSE,
            "high_bg" = r$tables$analysis_data$background_keep[r$tables$analysis_data$my_id == info_df$my_id] <- FALSE
          )

          if(input$select_reason == "keep") {
            r$tables$analysis_data$keep[r$tables$analysis_data$my_id == info_df$my_id] <- TRUE
          } else {
            r$tables$analysis_data$keep[r$tables$analysis_data$my_id == info_df$my_id] <- FALSE
          }
        } # end if check reason changed
      } # end is something found after click


    })

  })
}
