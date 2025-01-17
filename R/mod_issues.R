#' issues UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList moduleServer req
#' @importFrom bslib navset_card_tab nav_panel
#' @importFrom DT dataTableOutput renderDataTable
#'
mod_issues_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::navset_card_tab(
      id = "issues",
      bslib::nav_panel(
        title = "Lipid class",
        value = "issues_lipidclass",
        shiny::p("Lipid classes excluded from analysis and export."),
        DT::dataTableOutput(outputId = ns("issues_table_lipidclass"))
      ),
      bslib::nav_panel(
        title = "Lipids",
        value = "issues_features",
        shiny::p("Features excluded from analysis and export."),
        DT::dataTableOutput(outputId = ns("issues_table"))
      )
    )
  )
}

#' issues Server Functions
#'
#' @noRd
#'
mod_issues_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$issues_table_lipidclass <- DT::renderDataTable({
      shiny::req(r$tables$analysis_data)

      table_data <- unique(r$tables$analysis_data[r$tables$analysis_data$class_keep == FALSE, c("Class", "ion")])
    },
    options = list(pageLength = 10,
                   lengthChange = FALSE,
                   dom = "pt",
                   ordering = TRUE),
    selection = "none",
    rownames = FALSE,
    colnames = c("Lipid class", "Ion"))


    output$issues_table <- DT::renderDataTable({
      shiny::req(r$tables$analysis_data)

      table_data <- unique(r$tables$analysis_data[r$tables$analysis_data$keep == FALSE &
                                                    r$tables$analysis_data$class_keep == TRUE, c("my_id", "ion", "FeatureName", "Class", "comment")])

      table_data <- t(apply(table_data, 1, function(x) {
        x["comment"] <- switch(
            x["comment"],
            "no_match" = "Not a convicing match",
            "large_rsd" = "High RSD value",
            "wrong_rt" = "Wrong retention time",
            "high_bg" = "High background"
          )
        return(x)
      }))
    },
    options = list(pageLength = 10,
                   lengthChange = FALSE,
                   dom = "pt",
                   ordering = TRUE),
    selection = "none",
    rownames = FALSE,
    colnames = c("ID", "Ion", "Lipid name", "Lipid class", "Reason"))
  })
}
