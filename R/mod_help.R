#' help UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib navset_card_tab nav_panel
#'
mod_help_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::navset_card_tab(
      bslib::nav_panel(
        title = "File",
        bslib::accordion(
          id = ns("help"),
          open = FALSE,
          bslib::accordion_panel(
            title = shiny::strong("Meta data"),
            value = "help_meta_data",
            shiny::p("In the ", shiny::strong("Meta data"), " tab a meta data file can be loaded. Supported formats are:"),
            shiny::tags$ul(
              shiny::tags$li("comma separated (.csv)"),
              shiny::tags$li("tab separated (.tsv)"),
              shiny::tags$li("Excel file (.xlsx)")
            ),
            shiny::h3("Column selection"),
            shiny::p("In the meta data column several columns are mandatory:"),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("File name:"), " column which contains all the file names."),
              shiny::tags$li(shiny::strong("Sample ID:"), "column which contains all the sample id's."),
              shiny::tags$li(shiny::strong("Sample type:"), "column which contains the sample types, e.g. sample, blank, qc."),
              shiny::tags$li(shiny::strong("Acquisition order:"), "the order of the samples in which they where acquired.
                               If there are multiple batches the counting should continue."),
              shiny::tags$li(shiny::strong("Batch:"), "column with the batch annotation.")
            ),
            shiny::p("Additional columns which can be selected are:"),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("Protein normalization:"), "This column will be used to apply protein normalization on the data."),
              shiny::tags$li(shiny::strong("Blank filtering:"), "This column will be used to apply the an additional blank filtering groupwise."),
              shiny::tags$li(shiny::strong("Groups:"), "Here you can select multiple columns which can later be used in the analysis module.")
            ),
            shiny::h3("Text patterns"),
            shiny::p("For the app to recognize the sample types correctly several text patterns are used. These patterns should be ",
                     shiny::strong("regular expressions"), " and are applied to the column selected in ", shiny::strong("Sample type"), "."),
            shiny::tags$ul(
              shiny::tags$li(shiny::strong("Blanks:"), "regular expression to recognize blanks samples. Default: '^blank'."),
              shiny::tags$li(shiny::strong("Pooled samples:"), "regular expression to recognize pooled samples. Default: '^qcpool'."),
              shiny::tags$li(shiny::strong("Samples:"), "regular expression to recognize the samples. Default: '^sample'.")
            )
          ),
          bslib::accordion_panel(
            title = shiny::strong("Raw data"),
            value = "help_raw_data",
            shiny::p("Raw data")
          ),
          bslib::accordion_panel(
            title = shiny::strong("Rdata"),
            value = "help_rdata",
            shiny::p("Rdata")
          )
        )
      ),
      bslib::nav_panel(
        title = "Data",
        shiny::p("data stuff")
      )
    )
  )
}

#' help Server Functions
#'
#' @noRd
mod_help_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
