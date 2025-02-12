#' settings UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList isolate req
#' @importFrom bslib navset_card_tab nav_panel card tooltip card_header nav_show
#'     nav_hide
#' @importFrom bsicons bs_icon
#' @importFrom DT dataTableOutput
#' @importFrom shinyWidgets progressBar
#' @importFrom shinyjs disabled disable enable
#' @importFrom waiter Waiter spin_loaders
#'
mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    bslib::navset_card_tab(
      id = "Settings",
      #--------------------------------------------------- general settings ----
      bslib::nav_panel(
        title = "General settings",
        value = "general_settings",
        bslib::card(
          bslib::card_header(bslib::tooltip(
            trigger = list(
              "Quality control",
              bsicons::bs_icon(name = "info-circle")
            ),
            "Features are immediately tagged with `large_rsd` and will not show up in the bubble plots (identification tab) or in the analysis part."
          )),
          shiny::uiOutput(
            outputId = ns("settings_qc_ui")
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
          shiny::uiOutput(
            outputId = ns("settings_id_ui")
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
          shiny::uiOutput(
            outputId = ns("settings_blanksample_ui")
          )
        )
      ),

      #------------------------------------------------------ lipid classes ----
      bslib::nav_panel(
        title = "Lipid classes",
        value = "lipid_classes",
        bslib::card(
          shiny::uiOutput(
            outputId = ns("settings_select_lipidclass")
          )
        )
      ),

      #------------------------------------------------------------ samples ----
      bslib::nav_panel(
        title = "Samples",
        value = "samples",
        bslib::card(
          shiny::p(shiny::strong("NOTE:"), "Removing a file or selecting a file will cause everything to be recalculated!"),
          bslib::layout_column_wrap(
            width = 1 / 3,
            shiny::uiOutput(outputId = ns("settings_blanks_list")),
            shiny::uiOutput(outputId = ns("settings_pools_list")),
            shiny::uiOutput(outputId = ns("settings_samples_list"))
          )
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

    w <- waiter::Waiter$new(
      html = shiny::tagList(
        waiter::spin_loaders(id = 8,
                             color = "black"),
        # shiny::h3("Rendering report....", style = "color:black")
      ),
      color = "rgba(255, 255, 255, 0.5)"
    )

    #----------------------------------------------------- general settings ----
    output$settings_qc_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::numericInput(
          inputId = ns("settings_rsd_cutoff"),
          label = "RSD cut off value :",
          value = r$settings$rsd_cutoff,
          min = 0,
          max = 1,
          step = 0.05
        )
      )
    })


    output$settings_id_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::numericInput(
          inputId = ns("settings_dot_cutoff"),
          label = "Dot product cut off value :",
          value = r$settings$dot_cutoff,
          min = 0,
          max = 100,
          step = 1
        ),
        shiny::numericInput(
          inputId = ns("settings_revdot_cutoff"),
          label = "Reverse dot product cut off value :",
          value = r$settings$revdot_cutoff,
          min = 0,
          max = 100,
          step = 1
        )
      )
    })


    output$settings_blanksample_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::numericInput(
          inputId = ns("settings_ratio"),
          label = "Sample / average blank ratio",
          value = r$settings$blanksample_ratio,
          min = 0,
          step = 0.1
        ),
        shiny::sliderInput(
          inputId = ns("settings_threshold"),
          label = "Threshold",
          value = r$settings$blanksample_threshold,
          min = 0,
          max = 1,
          step = 0.1
        )
      )
    })


    shiny::observeEvent(
      shiny::req(!is.null(input$settings_rsd_cutoff)), {
        print("Changed RSD cutoff.")

        r$settings$rsd_cutoff <- input$settings_rsd_cutoff

        rsd_res <- calc_rsd(data = shiny::isolate(r$tables$analysis_data),
                            pools = r$index$selected_pools,
                            cut_off = input$settings_rsd_cutoff)

        r$index$keep_rsd <- rsd_res$keep
        r$tables$qc_data <- rsd_res$qc_data

        r$tables$analysis_data$rsd_keep <- r$tables$analysis_data$my_id %in% r$index$keep_rsd

        r$tables$analysis_data$comment[!r$tables$analysis_data$rsd_keep] <- "large_rsd"

        r$tables$analysis_data$keep <- mapply(all,
                                              r$tables$analysis_data$rsd_keep,
                                              r$tables$analysis_data$match_keep,
                                              r$tables$analysis_data$background_keep)
      },
      ignoreInit = TRUE
    )


    shiny::observeEvent(
      shiny::req(!is.null(input$settings_dot_cutoff), !is.null(input$settings_revdot_cutoff)), {
        print("Changed ID cut off.")

        r$settings$dot_cutoff <- input$settings_dot_cutoff
        r$settings$revdot_cutoff <- input$settings_revdot_cutoff

        r$index$keep_id <- filter_id(data = shiny::isolate(r$tables$analysis_data),
                                     dot_cutoff = input$settings_dot_cutoff,
                                     revdot_cutoff = input$settings_revdot_cutoff)

        r$tables$analysis_data$match_keep <- r$tables$analysis_data$my_id %in%
          r$index$keep_id

        r$tables$analysis_data$comment[!r$tables$analysis_data$match_keep] <- "no_match"
        r$tables$analysis_data$comment[!r$tables$analysis_data$rsd_keep] <- "large_rsd"

        r$tables$analysis_data$keep <- mapply(all,
                                              r$tables$analysis_data$rsd_keep,
                                              r$tables$analysis_data$match_keep,
                                              r$tables$analysis_data$background_keep)
      },
      ignoreInit = TRUE
    )


    shiny::observeEvent(
      shiny::req(!is.null(input$settings_ratio), !is.null(input$settings_threshold)), {
        print("Changed blank / sample ratio.")

        r$settings$blanksample_ratio <- input$settings_ratio
        r$settings$blanksample_threshold <- input$settings_threshold

        r$index$keep_blankratio <- calc_blank_ratio(data = shiny::isolate(r$tables$clean_data),
                                                    blanks = r$index$selected_blanks,
                                                    samples = r$index$selected_samples,
                                                    ratio = r$settings$blanksample_ratio,
                                                    threshold = r$settings$blanksample_threshold)

        r$tables$analysis_data$background_keep <- r$tables$analysis_data$my_id %in%
          r$index$keep_blankratio

        r$tables$analysis_data$comment[!r$tables$analysis_data$background_keep] <- "high_bg"
        r$tables$analysis_data$comment[!r$tables$analysis_data$rsd_keep] <- "large_rsd"

        r$tables$analysis_data$keep <- mapply(all,
                                              r$tables$analysis_data$rsd_keep,
                                              r$tables$analysis_data$match_keep,
                                              r$tables$analysis_data$background_keep)
      },
      ignoreInit = TRUE
    )

    #-------------------------------------------------------------- samples ----
    output$settings_blanks_list <- shiny::renderUI({
      shiny::req(r$index$blanks,
                 r$index$selected_blanks)

      shiny::tagList(
        checkboxGroupInput(inputId = ns("settings_select_blanks"),
                           label = "(De-)select blanks:",
                           choices = r$index$blanks,
                           selected = r$index$selected_blanks)
      )
    })

    output$settings_pools_list <- shiny::renderUI({
      shiny::req(r$index$pools,
                 r$index$selected_pools)

      shiny::tagList(
        checkboxGroupInput(inputId = ns("settings_select_pools"),
                           label = "(De-)select pooled samples:",
                           choices = r$index$pools,
                           selected = r$index$selected_pools)
      )
    })

    output$settings_samples_list <- shiny::renderUI({
      shiny::req(r$index$samples,
                 r$index$selected_samples)

      shiny::tagList(
        checkboxGroupInput(inputId = ns("settings_select_samples"),
                           label = "(De-)select samples:",
                           choices = r$index$samples,
                           selected = r$index$selected_samples)
      )
    })


    shiny::observeEvent(
      c(input$settings_select_blanks,
        input$settings_select_pools,
        input$settings_select_samples),
      {
        shiny::req(input$settings_select_blanks,
                   input$settings_select_pools,
                   input$settings_select_samples)

        w$show()

        print("(De-) select samples")

        selected_samples <- c(input$settings_select_blanks,
                              input$settings_select_pools,
                              input$settings_select_samples)

        r$index$selected_blanks <- input$settings_select_blanks
        r$index$selected_pools <- input$settings_select_pools
        r$index$selected_samples <- input$settings_select_samples

        r$tables$analysis_data <- r$tables$clean_data[
          r$tables$clean_data$sample_name %in% selected_samples, ]

        # RSD filtering
        rsd_res <- calc_rsd(data = r$tables$clean_data,
                            pools = r$index$selected_pools,
                            cut_off = r$settings$rsd_cutoff)
        r$index$keep_rsd <- rsd_res$keep
        r$tables$qc_data <- rsd_res$qc_data

        # ID filtering
        r$index$keep_id <- filter_id(data = r$tables$clean_data,
                                     dot_cutoff = r$settings$dot_cutoff,
                                     revdot_cutoff = r$settings$revdot_cutoff)
        # Blank filtering
        r$index$keep_blankratio <- calc_blank_ratio(data = r$tables$clean_data,
                                                    blanks = r$index$selected_blanks,
                                                    samples = r$index$selected_samples,
                                                    ratio = r$settings$blanksample_ratio,
                                                    threshold = r$settings$blanksample_threshold)

        r$tables$analysis_data$rsd_keep <- r$tables$analysis_data$my_id %in%
          r$index$keep_rsd
        r$tables$analysis_data$match_keep <- r$tables$analysis_data$my_id %in%
          r$index$keep_id
        r$tables$analysis_data$background_keep <- r$tables$analysis_data$my_id %in%
          r$index$keep_blankratio
        r$tables$analysis_data$keep <- mapply(all,
                                              r$tables$analysis_data$rsd_keep,
                                              r$tables$analysis_data$match_keep,
                                              r$tables$analysis_data$background_keep)

        r$tables$analysis_data$comment <- "keep"
        r$tables$analysis_data$comment[!r$tables$analysis_data$background_keep] <- "high_bg"
        r$tables$analysis_data$comment[!r$tables$analysis_data$match_keep] <- "no_match"
        r$tables$analysis_data$comment[!r$tables$analysis_data$rsd_keep] <- "large_rsd"

        w$hide()
      },
      # everything is still recalculated the first time you visit Settings - Samples
      ignoreInit = TRUE
    )


    #-------------------------------------------- some lipid class settings ----
    output$settings_select_lipidclass <- shiny::renderUI({
      # req(r$tables$clean_data)

      shiny::tagList(
        shiny::p("Select the lipid classes you want to keep."),
        bslib::layout_column_wrap(
          width = 1 / 4,
          shiny::uiOutput(
            outputId = ns("settings_select_lipidclass_col1")
          ),
          shiny::uiOutput(
            outputId = ns("settings_select_lipidclass_col2")
          ),
          shiny::uiOutput(
            outputId = ns("settings_select_lipidclass_col3")
          ),
          shiny::uiOutput(
            outputId = ns("settings_select_lipidclass_col4")
          )
        )
      )
    })


    output$settings_select_lipidclass_col1 <- shiny::renderUI({
      shiny::tagList(
        checkboxGroupInput(inputId = ns("select_PL_class"),
                           label = "Glycerophospholipids:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$PL)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$PL)])
      )
    })


    output$settings_select_lipidclass_col2 <- shiny::renderUI({
      shiny::tagList(
        checkboxGroupInput(inputId = ns("select_Cer_class"),
                           label = "Ceramides:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$Cer)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$Cer)]),
        checkboxGroupInput(inputId = ns("select_HexCer_class"),
                           label = "Neutral glycosphingolipids:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$HexCer)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$HexCer)])
      )
    })


    output$settings_select_lipidclass_col3 <- shiny::renderUI({
      shiny::tagList(
        checkboxGroupInput(inputId = ns("select_FA_class"),
                           label = "Fatty acyls:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$FA)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$FA)]),
        checkboxGroupInput(inputId = ns("select_PSL_class"),
                           label = "Phosphosphingolipids:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$PSL)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$PSL)]),
        checkboxGroupInput(inputId = ns("select_SB_class"),
                           label = "Sphingoid bases:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$SB)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$SB)]),
        checkboxGroupInput(inputId = ns("select_SA_class"),
                           label = "Acidic glycosphingolipids:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$SA)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$SA)]),
        checkboxGroupInput(inputId = ns("select_GL_class"),
                           label = "Glycerolipids:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$GL)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$GL)]),
        checkboxGroupInput(inputId = ns("select_CL_class"),
                           label = "Cardiolipins:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$CL)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$CL)])
      )
    })

    output$settings_select_lipidclass_col4 <- shiny::renderUI({
      shiny::tagList(
        checkboxGroupInput(inputId = ns("select_STL_class"),
                           label = "Sterol lipids:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$STL)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$STL)]),
        checkboxGroupInput(inputId = ns("select_ACPIM_class"),
                           label = "Glycerophosphoinositolglycans:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$ACPIM)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$ACPIM)]),
        checkboxGroupInput(inputId = ns("select_PRL_class"),
                           label = "Prenol lipids:",
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$PRL)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$PRL)])
      )
    })


    shiny::observeEvent(c(
      input$select_PL_class,
      input$select_GL_class,
      input$select_Cer_class,
      input$select_HexCer_class,
      input$select_FA_class,
      input$select_PSL_class,
      input$select_SB_class,
      input$select_CL_class,
      input$select_ACPIM_class,
      input$select_PRL_class,
      input$select_SA_class,
      input$select_STL_class
    ), {
      # get all the selected classes
      class_ion_selected <- c(input$select_PL_class,
                              input$select_GL_class,
                              input$select_Cer_class,
                              input$select_HexCer_class,
                              input$select_FA_class,
                              input$select_PSL_class,
                              input$select_SB_class,
                              input$select_CL_class,
                              input$select_ACPIM_class,
                              input$select_PRL_class,
                              input$select_SA_class,
                              input$select_STL_class)

      r$settings$selected_feature_class <- class_ion_selected

      r$tables$analysis_data$class_keep <- r$tables$analysis_data$class_ion %in% class_ion_selected
    },
    ignoreInit = TRUE)

    # so far not working
    # shiny::observeEvent(r$omics, {
    #   print(r$omics)
    #   switch(
    #     r$omics,
    #     "lip" = {
    #       bslib::nav_show(id = "Settings",
    #                       target = "lipid_classes",
    #                       session = session)
    #       print("show")
    #     },
    #     "met" = {
    #       bslib::nav_hide(id = "Settings",
    #                       target = "lipid_classes",
    #                       session = session)
    #       print("hide")
    #     }
    #   )
    # })

  })
}
