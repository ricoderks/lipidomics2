#' report_text UI Function
#'
#' @description A shiny Module to add free text chapters to the analysis
#'     report.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer actionButton div p
#'
mod_report_text_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Report text"),
    shiny::p("Add chapters with your own text to the analysis report. The
             chapters will show up between the chapters 'Settings' and
             'Analysis'. The text is interpreted as markdown, use '##' for
             sub-headings and an empty line between paragraphs."),
    shiny::actionButton(
      inputId = ns("addChapter"),
      label = "Add chapter",
      width = "225px"
    ),
    shiny::div(
      id = ns("chapterContainer")
    )
  )
}

#' report_text Server Functions
#'
#' @importFrom shiny insertUI removeUI textInput textAreaInput observe
#' @importFrom bslib card card_body
#'
#' @noRd
#'
mod_report_text_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    rv <- shiny::reactiveValues(
      next_id = 0L,
      ids = character()
    )

    shiny::observeEvent(input$addChapter, {
      rv$next_id <- rv$next_id + 1L
      chId <- paste0("chapter_", rv$next_id)

      shiny::insertUI(
        selector = paste0("#", ns("chapterContainer")),
        where = "beforeEnd",
        ui = bslib::card(
          id = ns(chId),
          bslib::card_body(
            shiny::textInput(
              inputId = ns(paste0(chId, "_title")),
              label = "Chapter title:",
              width = "100%"
            ),
            shiny::textAreaInput(
              inputId = ns(paste0(chId, "_text")),
              label = "Text (markdown):",
              rows = 10,
              width = "100%"
            ),
            shiny::actionButton(
              inputId = ns(paste0(chId, "_remove")),
              label = "Remove chapter",
              icon = shiny::icon("trash"),
              class = "btn-danger",
              width = "225px"
            )
          )
        ),
        session = session
      )

      rv$ids <- c(rv$ids, chId)

      shiny::observeEvent(input[[paste0(chId, "_remove")]], {
        shiny::removeUI(
          selector = paste0("#", ns(chId)),
          session = session
        )
        rv$ids <- setdiff(rv$ids, chId)
      },
      once = TRUE,
      ignoreInit = TRUE)
    })


    shiny::observe({
      r$analysis$report_chapters <- lapply(rv$ids, function(chId) {
        title <- input[[paste0(chId, "_title")]]
        text <- input[[paste0(chId, "_text")]]

        list(
          title = if(is.null(title)) "" else title,
          text = if(is.null(text)) "" else text
        )
      })
    })

  })
}
