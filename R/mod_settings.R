#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib navset_card_tab nav_panel card tooltip card_header nav_show
#'     nav_hide
#' @importFrom bsicons bs_icon
#' @importFrom DT dataTableOutput
#' @importFrom shinyWidgets progressBar
#' @importFrom shinyjs disabled disable enable
#'
mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    bslib::navset_card_tab(
      id = "settings",
      #--------------------------------------------------- general settings ----
      bslib::nav_panel(
        title = "General settings",
        bslib::card(
          bslib::card_header(bslib::tooltip(
            trigger = list(
              "QC",
              bsicons::bs_icon(name = "info-circle")
            ),
            "Features are immediately tagged with `large_rsd` and will not show up in the bubble plots (identification tab) or in the analysis part."
          )),
          shiny::numericInput(
            inputId = ns("settings_rsd_cutoff"),
            label = "RSD cut off value :",
            value = 0.3,
            min = 0,
            max = 1
          )
        ),
        bslib::card(
          bslib::card_header(
            bslib::tooltip(
              trigger = list(
                "Identification",
                bsicons::bs_icon(name = "info-circle")
              ),
              "Features are immediately tagged with `no_match`. They will show up in the bubble plots (identification tab), but not in the analysis part. Individual features can be added back via the bubble plots (identification part). Keep in mind that when the value of this filter is changed they might be removed again!!"
            )
          ),
          shiny::numericInput(
            inputId = ns("settings_dot_cutoff"),
            label = "Dot product cut off value :",
            value = 50,
            min = 0,
            max = 100
          ),
          shiny::numericInput(
            inputId = ns("settings_revdot_cutoff"),
            label = "Reverse dot product cut off value :",
            value = 50,
            min = 0,
            max = 100
          )
        ),
        bslib::card(
          bslib::card_header(
            bslib::tooltip(
              trigger = list(
                "Sample / blank ratio filter",
                bsicons::bs_icon(name = "info-circle")
              ),
              "For all samples are sample / blank ratio is calculated. In 80% (default threshold) of the samples this value should be higher than the sample / blank ratio cut off. If it is not the feature is removed from the data set (set as high background)."
            )
          ),
          shiny::numericInput(
            inputId = ns("settings_ratio"),
            label = "Sample / average blank ratio",
            value = 5,
            min = 0,
            step = 0.01
          ),
          shiny::sliderInput(
            inputId = ns("settings_threshold"),
            label = "Threshold",
            value = 0.8,
            min = 0,
            max = 1,
            step = 0.01
          )
        )
      ),

      #------------------------------------------------------ lipid classes ----
      bslib::nav_panel(
        title = "Lipid classes",
        value = "lipid_classes",
        bslib::card(
          "Lipid classes"
        )
      ),

      #------------------------------------------------------------ samples ----
      bslib::nav_panel(
        title = "Samples",
        bslib::card(
          "Samples"
        )
      )
    )
  )
}

#' settings Server Functions
#'
#' @noRd
mod_settings_server <- function(id, r){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    shiny::observeEvent(r$omics, {
      print(r$omics)
      switch(
        r$omics,
        "lip" = {
          bslib::nav_show(id = "settings",
                          target = "lipid_classes")
          print("show")
        },
        "met" = {
          bslib::nav_hide(id = "settings",
                          target = "lipid_classes")
          print("hide")
        }
      )
    })

  })
}
