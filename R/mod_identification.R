#' identification UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer
#' @importFrom bslib layout_sidebar sidebar card card_header
#'
mod_identification_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::uiOutput(
          outputId = ns("id_sidebar_ui")
        )
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
mod_identification_server <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$id_sidebar_ui <- shiny::renderUI({
      shiny::req(r$omics)

      class_choices <- switch(
        r$omics,
        "lip" = lip_class_choices(r$defaults$lipid_classes)
      )

      shiny::tagList(
        shiny::selectInput(
          inputId = ns("id_select_class"),
          label = "Select a class:",
          choices = c("None", class_choices),
          multiple = FALSE
        )
      )
    })


    output$id_main_ui <- shiny::renderUI({
      shiny::req(input$id_select_class)

      if(input$id_select_class != "None") {
        pattern <- get_class_pattern(classes = r$defaults$lipid_classes,
                                     class_name = input$id_select_class)

        main_title <- shiny::h2(input$id_select_class)
        mod_bubbleplot_server(id = "bubble",
                              r = r,
                              class_pattern = pattern)
      } else {
        main_title <- NULL
      }

      shiny::tagList(
        bslib::card(
          bslib::card_header(main_title),
          mod_bubbleplot_ui(id = ns("bubble")),
          height = "100%"
        )
      )
    })

  })
}
