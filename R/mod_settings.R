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
    # shinyjs::useShinyjs(),
    bslib::navset_card_tab(
      id = "Settings",
      #--------------------------------------------------- general settings ----
      bslib::nav_panel(
        title = "General settings",
        value = "general_settings",
        bslib::accordion(
          id = ns("general_settings_accordion"),
          bslib::accordion_panel(
            title = "Quality Control",
            value = "settings_rsd",
            shiny::div(
              bslib::layout_sidebar(
                fillable = TRUE,
                sidebar = bslib::sidebar(
                  shiny::checkboxInput(
                    inputId = ns("settings_apply_rsd_cutoff"),
                    label = "Apply RSD filtering",
                    value = TRUE
                  ),
                  open = "always"
                ),
                shiny::numericInput(
                  inputId = ns("settings_rsd_cutoff"),
                  label = "RSD cut off value :",
                  value = 0.3,
                  min = 0,
                  max = 1,
                  step = 0.05
                )
              ),
              style = "font-size:75%;"
            )
          ), # end accordion_panel Quality control
          bslib::accordion_panel(
            title = "Identification",
            value = "settings_identification",
            shiny::div(
              bslib::layout_sidebar(
                fillable = TRUE,
                sidebar = bslib::sidebar(
                  shiny::checkboxInput(
                    inputId = ns("settings_apply_id_filtering"),
                    label = "Apply ID filtering",
                    value = TRUE
                  ),
                  open = "always"
                ),
                bslib::layout_column_wrap(
                  width = 1 / 2,
                  shiny::numericInput(
                    inputId = ns("settings_dot_cutoff"),
                    label = "Dot product cut off value :",
                    value = 50,
                    min = 0,
                    max = 100,
                    step = 1
                  ),
                  shiny::numericInput(
                    inputId = ns("settings_revdot_cutoff"),
                    label = "Reverse dot product cut off value :",
                    value = 50,
                    min = 0,
                    max = 100,
                    step = 1
                  ),
                  fill = FALSE,
                  fillable = FALSE
                )
              ),
              style = "font-size:75%;"
            )
          ), # end accordion_panel Identification
          bslib::accordion_panel(
            title = "Sample / blank ratio filter",
            value = "settings_sampleblank_filter",
            shiny::div(
              bslib::layout_sidebar(
                fillable = TRUE,
                sidebar = bslib::sidebar(
                  shiny::checkboxInput(
                    inputId = ns("settings_apply_blank_filtering"),
                    label = "Apply blank filtering",
                    value = TRUE
                  ),
                  open = "always"
                ),
                bslib::layout_column_wrap(
                  width = 1 / 2,
                  shiny::numericInput(
                    inputId = ns("settings_ratio"),
                    label = "Sample / average blank ratio",
                    value = 5,
                    min = 0,
                    step = 0.1
                  ),
                  shiny::sliderInput(
                    inputId = ns("settings_threshold"),
                    label = "Threshold",
                    value = 0.8,
                    min = 0,
                    max = 1,
                    step = 0.1
                  )
                )
              ),
              style = "font-size:75%;"
            )
          ), # end accordion_panel sample/blank filter
          bslib::accordion_panel(
            title = "Trend correction",
            value = "settings_trend_correction",
            shiny::div(
              bslib::layout_sidebar(
                fillable = TRUE,
                sidebar = bslib::sidebar(
                  shiny::checkboxInput(
                    inputId = ns("settings_apply_trend_correction"),
                    label = "Apply trend correction",
                    value = FALSE
                  ),
                  open = "always"
                ),
                shiny::selectInput(
                  inputId = ns("settings_select_trend"),
                  label = "Select trend correction:",
                  choices = c("LOESS" = "loess")
                )
              ),
              style = "font-size:75%"
            )
          ) # end accordion_panel trend correction
        ) # end accordion
      ), # end nav_panel general settings

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


    # making sure that enable/disable of the settings work
    shiny::observeEvent(TRUE, {
      print("Run once!")
      # RSD filtering
      shiny::updateCheckboxInput(
        inputId = "settings_apply_rsd_cutoff",
        value = r$settings$apply_rsd_cutoff
      )
      shiny::updateNumericInput(
        inputId = "settings_rsd_cutoff",
        value = r$settings$rsd_cutoff
      )
      shinyjs::toggleState(id = "settings_rsd_cutoff",
                           condition = isTRUE(r$settings$apply_rsd_cutoff))

      # ID filtering
      shiny::updateCheckboxInput(
        inputId = "settings_apply_id_filtering",
        value = r$settings$apply_id_filtering
      )
      shiny::updateNumericInput(
        inputId = "settings_dot_cutoff",
        value = r$settings$dot_cutoff
      )
      shiny::updateNumericInput(
        inputId = "settings_revdot_cutoff",
        value = r$settings$revdot_cutoff
      )
      shinyjs::toggleState(id = "settings_dot_cutoff",
                           condition = isTRUE(r$settings$apply_id_filtering))
      shinyjs::toggleState(id = "settings_revdot_cutoff",
                           condition = isTRUE(r$settings$apply_id_filtering))

      # Blank filtering
      shiny::updateCheckboxInput(
        inputId = "settings_apply_blank_filtering",
        value = r$settings$apply_blank_filtering
      )
      shiny::updateNumericInput(
        inputId = "settings_ratio",
        value = r$settings$blanksample_ratio
      )
      shiny::updateSliderInput(
        inputId = "settings_threshold",
        value = r$settings$blanksample_threshold
      )
      shinyjs::toggleState(id = "settings_ratio",
                           condition = isTRUE(r$settings$apply_blank_filtering))
      shinyjs::toggleState(id = "settings_threshold",
                           condition = isTRUE(r$settings$apply_blank_filtering))

      # trend correction
      shiny::updateCheckboxInput(
        inputId = "settings_apply_trend_correction",
        value = r$settings$apply_trend_correction
      )
      shiny::updateSelectInput(
        inputId = "settings_select_trend",
        selected = r$settings$trend_correction_method
      )
      shinyjs::toggleState(id = "settings_select_trend",
                           condition = isTRUE(r$settings$apply_trend_correction))
    },
    once = TRUE)


    #-------------------------------------------------------- loading rdata ----
    shiny::observeEvent(shiny::req(r$rdata), {
      print("Run once RDATA!")
      # delay to update r$rdata
      # only works if one of the settings is not the same as the defaults.
      onceReset <- shiny::observe({
        r$rdata <- FALSE
        onceReset$destroy()
      },
      suspended = TRUE)

      # RSD filtering
      shiny::updateCheckboxInput(
        inputId = "settings_apply_rsd_cutoff",
        value = r$settings$apply_rsd_cutoff
      )
      shiny::updateNumericInput(
        inputId = "settings_rsd_cutoff",
        value = r$settings$rsd_cutoff
      )
      shinyjs::toggleState(id = "settings_rsd_cutoff",
                           condition = isTRUE(r$settings$apply_rsd_cutoff))

      # ID filtering
      shiny::updateCheckboxInput(
        inputId = "settings_apply_id_filtering",
        value = r$settings$apply_id_filtering
      )
      shiny::updateNumericInput(
        inputId = "settings_dot_cutoff",
        value = r$settings$dot_cutoff
      )
      shiny::updateNumericInput(
        inputId = "settings_revdot_cutoff",
        value = r$settings$revdot_cutoff
      )
      shinyjs::toggleState(id = "settings_dot_cutoff",
                           condition = isTRUE(r$settings$apply_id_filtering))
      shinyjs::toggleState(id = "settings_revdot_cutoff",
                           condition = isTRUE(r$settings$apply_id_filtering))

      # Blank filtering
      shiny::updateCheckboxInput(
        inputId = "settings_apply_blank_filtering",
        value = r$settings$apply_blank_filtering
      )
      shiny::updateNumericInput(
        inputId = "settings_ratio",
        value = r$settings$blanksample_ratio
      )
      shiny::updateSliderInput(
        inputId = "settings_threshold",
        value = r$settings$blanksample_threshold
      )
      shinyjs::toggleState(id = "settings_ratio",
                           condition = isTRUE(r$settings$apply_blank_filtering))
      shinyjs::toggleState(id = "settings_threshold",
                           condition = isTRUE(r$settings$apply_blank_filtering))

      # Trend correction
      shiny::updateCheckboxInput(
        inputId = "settings_apply_trend_correction",
        value = r$settings$apply_trend_correction
      )
      shiny::updateSelectInput(
        inputId = "settings_select_trend",
        selected = r$settings$trend_correction_method
      )
      shinyjs::toggleState(id = "settings_select_trend",
                           condition = isTRUE(r$settings$apply_trend_correction))

      # make sure r$rdata gets not updated to early
      # make sure the order is the same as above
      # not the most beautiful solution
      check_inputs <- list(
        "settings_apply_rsd_cutoff" = input$settings_apply_rsd_cutoff,
        "settings_rsd_cutoff" = input$settings_rsd_cutoff,
        "settings_apply_id_filtering" = input$settings_apply_id_filtering,
        "settings_dot_cutoff" = input$settings_dot_cutoff,
        "settings_revdot_cutoff" = input$settings_revdot_cutoff,
        "settings_apply_blank_filtering" = input$settings_apply_blank_filtering,
        "settings_ratio" = input$settings_ratio,
        "settings_threshold" = input$settings_threshold,
        "settings_apply_trend_correction" = input$settings_apply_trend_correction,
        "settings_select_trend" = input$settings_select_trend
      )
      check_defaults <- r$settings[c("apply_rsd_cutoff",
                                     "rsd_cutoff",
                                     "apply_id_filtering",
                                     "dot_cutoff",
                                     "revdot_cutoff",
                                     "apply_blank_filtering",
                                     "blanksample_ratio",
                                     "blanksample_threshold",
                                     "apply_trend_correction",
                                     "trend_correction_method")]

      changed <- mapply(
        function(current_value, default_value) {
          !isTRUE(all.equal(current_value, default_value))
        },
        check_inputs,
        check_defaults
      )

      if(length(which(changed)) > 0) {
        changed <- names(changed)[max(which(changed))]

        onceReset1 <- shiny::observeEvent(input[[changed]], {
          r$rdata <- FALSE
          onceReset1$destroy()   # remove this one-off observer
        },
        ignoreInit = TRUE,
        once = TRUE)
      } else {
        onceReset$resume()
      }

    })


    #----------------------------------------------------------------- rsd -----
    shiny::observeEvent(
      shiny::req(!is.null(input$settings_rsd_cutoff),
                 !is.null(input$settings_apply_rsd_cutoff)), {

                   r$settings$apply_rsd_cutoff <- input$settings_apply_rsd_cutoff
                   if(!is.null(r$tables$analysis_data) & isFALSE(r$rdata)) {
                     if(r$settings$apply_rsd_cutoff) {
                       print("Changed RSD cutoff.")
                       shinyjs::enable(id = "settings_rsd_cutoff")

                       r$settings$rsd_cutoff <- input$settings_rsd_cutoff

                       rsd_res <- calc_rsd(data = shiny::isolate(r$tables$analysis_data),
                                           pools = r$index$selected_pools,
                                           cut_off = input$settings_rsd_cutoff)

                       r$index$keep_rsd <- rsd_res$keep
                       r$tables$analysis_data$rsd_keep <- r$tables$analysis_data$my_id %in% r$index$keep_rsd
                     } else {
                       print("Disabled RSD cutoff filtering!")
                       shinyjs::disable(id = "settings_rsd_cutoff")

                       r$index$keep_rsd <- unique(r$tables$analysis_data$my_id)
                       r$tables$analysis_data$rsd_keep <- TRUE
                     }
                     r$tables$analysis_data$keep <- mapply(all,
                                                           r$tables$analysis_data$rsd_keep,
                                                           r$tables$analysis_data$match_keep,
                                                           r$tables$analysis_data$background_keep)

                     r$tables$analysis_data$comment <- "keep"
                     r$tables$analysis_data$comment[!r$tables$analysis_data$background_keep] <- "high_bg"
                     r$tables$analysis_data$comment[!r$tables$analysis_data$match_keep] <- "no_match"
                     r$tables$analysis_data$comment[!r$tables$analysis_data$rsd_keep] <- "large_rsd"
                   }
                 },
      ignoreInit = TRUE
    )


    #------------------------------------------------------------------ id -----
    shiny::observeEvent(
      shiny::req(!is.null(input$settings_dot_cutoff),
                 !is.null(input$settings_revdot_cutoff),
                 !is.null(input$settings_apply_id_filtering)), {

                   if(!is.null(r$tables$analysis_data) & isFALSE(r$rdata)) {
                     r$settings$apply_id_filtering <- input$settings_apply_id_filtering

                     if(r$settings$apply_id_filtering){
                       print("Changed ID cut off.")
                       shinyjs::enable(id = "settings_dot_cutoff")
                       shinyjs::enable(id = "settings_revdot_cutoff")

                       r$settings$dot_cutoff <- input$settings_dot_cutoff
                       r$settings$revdot_cutoff <- input$settings_revdot_cutoff

                       r$index$keep_id <- filter_id(data = shiny::isolate(r$tables$analysis_data),
                                                    dot_cutoff = input$settings_dot_cutoff,
                                                    revdot_cutoff = input$settings_revdot_cutoff)

                       r$tables$analysis_data$match_keep <- r$tables$analysis_data$my_id %in%
                         r$index$keep_id
                     } else {
                       print("Disabled ID filtering!")
                       shinyjs::disable(id = "settings_dot_cutoff")
                       shinyjs::disable(id = "settings_revdot_cutoff")

                       r$index$keep_id <- unique(r$tables$analysis_data$my_id)
                       r$tables$analysis_data$match_keep <- TRUE
                     }
                     r$tables$analysis_data$keep <- mapply(all,
                                                           r$tables$analysis_data$rsd_keep,
                                                           r$tables$analysis_data$match_keep,
                                                           r$tables$analysis_data$background_keep)
                     r$tables$analysis_data$comment <- "keep"
                     r$tables$analysis_data$comment[!r$tables$analysis_data$background_keep] <- "high_bg"
                     r$tables$analysis_data$comment[!r$tables$analysis_data$match_keep] <- "no_match"
                     r$tables$analysis_data$comment[!r$tables$analysis_data$rsd_keep] <- "large_rsd"
                   }
                 },
      ignoreInit = TRUE
    )


    #--------------------------------------------------------------- blank -----
    shiny::observeEvent(
      shiny::req(!is.null(input$settings_ratio),
                 !is.null(input$settings_threshold),
                 !is.null(input$settings_apply_blank_filtering)), {

                   if(!is.null(r$tables$analysis_data) & isFALSE(r$rdata)) {
                     r$settings$apply_blank_filtering <- input$settings_apply_blank_filtering

                     if(r$settings$apply_blank_filtering) {
                       print("Changed blank / sample ratio.")
                       shinyjs::enable(id = "settings_ratio")
                       shinyjs::enable(id = "settings_threshold")

                       r$settings$blanksample_ratio <- input$settings_ratio
                       r$settings$blanksample_threshold <- input$settings_threshold

                       r$index$keep_blankratio <- calc_blank_ratio(data = shiny::isolate(r$tables$clean_data),
                                                                   blanks = r$index$selected_blanks,
                                                                   samples = r$index$selected_samples,
                                                                   ratio = r$settings$blanksample_ratio,
                                                                   threshold = r$settings$blanksample_threshold)

                       r$tables$analysis_data$background_keep <- r$tables$analysis_data$my_id %in%
                         r$index$keep_blankratio
                     } else {
                       print("Disabled blank filtering!")
                       shinyjs::disable(id = "settings_ratio")
                       shinyjs::disable(id = "settings_threshold")

                       r$index$keep_blankratio <- unique(r$tables$analysis_data$my_id)
                       r$tables$analysis_data$background_keep <- TRUE
                     }

                     r$tables$analysis_data$keep <- mapply(all,
                                                           r$tables$analysis_data$rsd_keep,
                                                           r$tables$analysis_data$match_keep,
                                                           r$tables$analysis_data$background_keep)
                     r$tables$analysis_data$comment <- "keep"
                     r$tables$analysis_data$comment[!r$tables$analysis_data$background_keep] <- "high_bg"
                     r$tables$analysis_data$comment[!r$tables$analysis_data$match_keep] <- "no_match"
                     r$tables$analysis_data$comment[!r$tables$analysis_data$rsd_keep] <- "large_rsd"
                   }
                 },
      ignoreInit = TRUE
    )

    #----------------------------------------------------- trend correction ----
    shiny::observeEvent(
      shiny::req(!is.null(input$settings_apply_trend_correction)), {
        if(!is.null(r$tables$analysis_data) & isFALSE(r$rdata)) {
          r$settings$apply_trend_correction <- input$settings_apply_trend_correction
          if(r$settings$apply_trend_correction) {
            print("Trend correction!")
            shinyjs::enable(id = "settings_select_trend")
            r$settings$trend_correction_method <- input$settings_select_trend

            w$show()

            # apply trend correction here
            res <- do_trend_correction(
              data = r$tables$analysis_data,
              method = input$settings_select_trend,
              columns = r$columns,
              index = r$index
            )

            r$tables$clean_data <- res

            # after trend correction everything needs to be recalculated
            print("Recalculate everything!")

            r$tables$analysis_data <- r$tables$clean_data

            # Trend calculation
            qcpool_data <- r$tables$clean_data[r$tables$clean_data$sample_name %in% r$index$selected_pools, ]
            r$tables$trend_data <- calc_trend(pool_data = qcpool_data,
                                              order_column = r$columns$acqorder)

            # RSD filtering
            if(r$settings$apply_rsd_cutoff) {
              rsd_res <- calc_rsd(data = r$tables$clean_data,
                                  pools = r$index$selected_pools,
                                  cut_off = r$settings$rsd_cutoff)
              r$index$keep_rsd <- rsd_res$keep
              r$tables$qc_data <- rsd_res$qc_data
              r$tables$rsd_data_overall <- rsd_res$rsd_data_overall
              r$tables$rsd_data_batch <- rsd_res$rsd_data_batch

              r$tables$analysis_data$rsd_keep <- r$tables$analysis_data$my_id %in%
                r$index$keep_rsd
            }

            # update what to keep
            r$tables$analysis_data$keep <- mapply(all,
                                                  r$tables$analysis_data$rsd_keep,
                                                  r$tables$analysis_data$match_keep,
                                                  r$tables$analysis_data$background_keep)

            r$tables$analysis_data$comment <- "keep"
            r$tables$analysis_data$comment[!r$tables$analysis_data$background_keep] <- "high_bg"
            r$tables$analysis_data$comment[!r$tables$analysis_data$match_keep] <- "no_match"
            r$tables$analysis_data$comment[!r$tables$analysis_data$rsd_keep] <- "large_rsd"

            w$hide()

          } else {
            print("Disabled trend correction!")
            shinyjs::disable(id = "settings_select_trend")
            w$show()
            print("Recalculate everything!")

            # put the uncorrected data back
            r$tables$clean_data$area <- r$tables$clean_data$areaOriginal
            r$tables$analysis_data <- r$tables$clean_data

            # Trend calculation
            qcpool_data <- r$tables$clean_data[r$tables$clean_data$sample_name %in% r$index$selected_pools, ]
            r$tables$trend_data <- calc_trend(pool_data = qcpool_data,
                                              order_column = r$columns$acqorder)

            # RSD filtering
            if(r$settings$apply_rsd_cutoff) {
              rsd_res <- calc_rsd(data = r$tables$clean_data,
                                  pools = r$index$selected_pools,
                                  cut_off = r$settings$rsd_cutoff)
              r$index$keep_rsd <- rsd_res$keep
              r$tables$qc_data <- rsd_res$qc_data
              r$tables$rsd_data_overall <- rsd_res$rsd_data_overall
              r$tables$rsd_data_batch <- rsd_res$rsd_data_batch

              r$tables$analysis_data$rsd_keep <- r$tables$analysis_data$my_id %in%
                r$index$keep_rsd
            }

            # update what to keep
            r$tables$analysis_data$keep <- mapply(all,
                                                  r$tables$analysis_data$rsd_keep,
                                                  r$tables$analysis_data$match_keep,
                                                  r$tables$analysis_data$background_keep)

            r$tables$analysis_data$comment <- "keep"
            r$tables$analysis_data$comment[!r$tables$analysis_data$background_keep] <- "high_bg"
            r$tables$analysis_data$comment[!r$tables$analysis_data$match_keep] <- "no_match"
            r$tables$analysis_data$comment[!r$tables$analysis_data$rsd_keep] <- "large_rsd"

            w$hide()
          }
        }
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
                           choices = r$settings$feature_class[grepl(x = r$settings$feature_class, pattern = r$defaults$patterns$PL, perl = TRUE)],
                           selected = r$settings$selected_feature_class[grepl(x = r$settings$selected_feature_class, pattern = r$defaults$patterns$PL, perl = TRUE)])
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
