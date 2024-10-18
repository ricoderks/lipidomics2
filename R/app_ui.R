#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom utils packageVersion
#' @importFrom bslib page_navbar nav_panel nav_spacer nav_menu
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    bslib::page_navbar(
      title = paste0("CPM - Lipidomics | v", utils::packageVersion("lipidomics2")),
      underline = TRUE,
      bslib::nav_panel(
        title = "Files",
        # mod_data_ui(id = "file")
      ),
      bslib::nav_panel(
        title = "Data",
        # mod_data_ui(id = "data")
      ),
      bslib::nav_spacer(),
      bslib::nav_menu(
        title = "Help",
        bslib::nav_panel(
          title = "Help",
          mod_help_ui(id = "help")
        ),
        "----",
        bslib::nav_panel(
          title = "About",
          mod_about_ui(id = "about")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "lipidomics2"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
