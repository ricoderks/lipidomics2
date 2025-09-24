#' file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList Progress
#' @importFrom bslib navset_card_tab nav_panel card page_sidebar sidebar tooltip
#' @importFrom bsicons bs_icon
#' @importFrom DT dataTableOutput
#' @importFrom shinyWidgets progressBar
#' @importFrom shinyjs disabled disable enable
#' @importFrom utils head
#'
mod_file_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      #---------------------------------------------------------- meta data ----
      bslib::nav_panel(
        title = "Meta data",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              shiny::div(
                shiny::h4("Column selection"),
                shiny::selectInput(
                  inputId = ns("metadata_select_filename"),
                  label = "File name",
                  choices = NULL
                ),
                shiny::selectInput(
                  inputId = ns("metadata_select_sampleid"),
                  label = "Sample ID",
                  choices = NULL
                ),
                shiny::selectInput(
                  inputId = ns("metadata_select_sampletype"),
                  label = "Sample type",
                  choices = NULL
                ),
                shiny::selectInput(
                  inputId = ns("metadata_select_acqorder"),
                  label = "Acquisition order",
                  choices = NULL
                ),
                shiny::selectInput(
                  inputId = ns("metadata_select_batch"),
                  label = "Batch",
                  choices = NULL
                ),
                shiny::selectInput(
                  inputId = ns("metadata_select_groups"),
                  label = bslib::tooltip(
                    trigger = list(
                      "Groups",
                      bsicons::bs_icon(name = "info-circle")
                    ),
                    "Select \"grouping\" columns which can be later used in the analysis. These columns will also be exported."
                  ),
                  multiple = TRUE,
                  choices = NULL
                ),
                shiny::h4("Text patterns"),
                shiny::textInput(
                  inputId = ns("metadata_blank_pattern"),
                  label = bslib::tooltip(
                    trigger = list(
                      "Blanks",
                      bsicons::bs_icon(name = "info-circle")
                    ),
                    "Regular expression to recognize blank samples."
                  ),
                  value = "^blank",
                  width = "100%"
                ),
                shiny::textInput(
                  inputId = ns("metadata_qc_pattern"),
                  label = bslib::tooltip(
                    trigger = list(
                      "Pooled samples",
                      bsicons::bs_icon(name = "info-circle")
                    ),
                    "Regular expression to recognize pooled samples."
                  ),
                  value = "^qcpool",
                  width = "100%"
                ),
                shiny::textInput(
                  inputId = ns("metadata_sample_pattern"),
                  label = bslib::tooltip(
                    trigger = list(
                      "Samples",
                      bsicons::bs_icon(name = "info-circle")
                    ),
                    "Regular expression to recognize samples."
                  ),
                  value = "^sample",
                  width = "100%"
                ),
                style = "font-size:75%"
              )

            ),
            shiny::div(
              bslib::card_body(
                shiny::fileInput(
                  inputId = ns("metadata_file"),
                  label = "Data file:",
                  multiple = FALSE,
                  accept = c(".csv", ".tsv", ".txt", ".xlsx")
                ),
                min_height = "125px",
                height = "125px",
                fill = FALSE,
                gap = 0
              ),
              style = "font-size:75%"
            ),
            bslib::card_body(
              shiny::div(
                DT::dataTableOutput(
                  outputId = ns("metadata_preview_table")
                ),
                style = "font-size:75%;"
              ),
              height = "40%"
            ),
            bslib::card_body(
              height = "60%",
              shiny::plotOutput(
                outputId = ns("metadata_sampletype_plot")
              )
            )
          )
        )
      ), # end navpanel meta data
      #----------------------------------------------------------- raw data ----
      bslib::nav_panel(
        title = "Raw data",
        bslib::card(
          bslib::page_sidebar(
            sidebar = bslib::sidebar(
              title = "Raw data",
              open = TRUE,
              shiny::radioButtons(
                inputId = ns("raw_select_omics"),
                label = "Select omics :",
                choices = c("Lipidomics" = "lip",
                            "Metabolomics" = "met"),
                selected = "lip"
              ),
              shiny::checkboxGroupInput(
                inputId = ns("raw_which_files"),
                label = bslib::tooltip(
                  trigger = list(
                    "Upload files",
                    bsicons::bs_icon(name = "info-circle")
                  ),
                  "Which result files from MS-DIAL to upload!"
                ),
                choices = c("Positive" = "pos", "Negative" = "neg"),
                selected = c("pos", "neg")
              ),
              shiny::selectInput(
                inputId = ns("raw_select_table"),
                label = "Select table",
                choices = c("Raw table" = "raw_data",
                            "Filtered table" = "clean_data_wide"),
                selected = "clean_data"
              )
            ),
            bslib::card_body(
              height = "15%",
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::fileInput(
                  inputId = ns("rawdata_pos_file"),
                  label = "Data file positive:",
                  multiple = FALSE,
                  accept = c(".txt")
                ),
                shiny::fileInput(
                  inputId = ns("rawdata_neg_file"),
                  label = "Data file negative:",
                  multiple = FALSE,
                  accept = c(".txt")
                ),
                shiny::textOutput(
                  outputId = ns("rawdata_status")
                )
              )
            ),
            bslib::card_body(
              height = "60%",
              shiny::div(
                DT::dataTableOutput(
                  outputId = ns("rawdata_preview_table")
                ),
                style = "font-size:75%;"
              )
            ),
            bslib::card_body(
              bslib::layout_column_wrap(
                width = 1 / 2,
                height = "15%",
                shinyWidgets::progressBar(
                  id = ns("sample_count_bar"),
                  title = "Sample count",
                  value = 0,
                  total = 100,
                  unit_mark = "%"
                ),
                shinyWidgets::progressBar(
                  id = ns("feature_count_bar"),
                  title = "Feature count",
                  value = 0,
                  total = 100,
                  unit_mark = "%"
                )
              )
            )
          )
        )
      ), # end navpanel raw data
      bslib::nav_panel(
        title = "Rdata",
        bslib::card(
          p("Load a .Rdata file here. NOT working yet!"),
          shiny::fileInput(inputId = ns("load_rdata"),
                           label = "Load Rdata file",
                           multiple = FALSE,
                           accept = c(".Rdata", ".RData"))
        )
      )
    )
  )
}

#' file Server Functions
#'
#' @noRd
mod_file_server <- function(id, r){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #----------------------------------------------------------- read files ----
    shiny::observeEvent(
      input$raw_which_files,
      {
        if("pos" %in% input$raw_which_files) {
          shinyjs::enable(id = "rawdata_pos_file")
        } else {
          shinyjs::disable(id = "rawdata_pos_file")
        }

        if("neg" %in% input$raw_which_files) {
          shinyjs::enable(id = "rawdata_neg_file")
        } else {
          shinyjs::disable(id = "rawdata_neg_file")
        }
      },
      ignoreNULL = FALSE
    )


    shiny::observeEvent(
      input$rawdata_pos_file,
      {
        shiny::req(input$rawdata_pos_file)

        print("Raw data read - pos file")
        output$rawdata_status <- shiny::renderText({
          ""
        })
        r$files$data_file_pos <- input$rawdata_pos_file
        tmp <- read_msdial(filename = input$rawdata_pos_file$datapath)
        if(any(tmp$`Adduct type` == "[M+H]+")) {
          r$tables$raw_data_pos <- data.frame("polarity" = "pos",
                                              tmp,
                                              check.names = FALSE)
        } else {
          shiny::showNotification(
            ui = "Error: This data doesn't appear to contain any positive mode data!",
            type = "error"
          )
          r$tables$raw_data_pos <- NULL
          r$tables$clean_data <- NULL
          r$tables$raw_data <- NULL

          shinyWidgets::updateProgressBar(
            session = session,
            id = "feature_count_bar",
            title = "Feature count",
            value = 0,
            total = 100
          )

          shinyWidgets::updateProgressBar(
            session = session,
            id = "sample_count_bar",
            title = "Sample count",
            value = 0,
            total = 100
          )
        }

      })


    shiny::observeEvent(
      input$rawdata_neg_file,
      {
        shiny::req(input$rawdata_neg_file)

        print("Raw data read - neg file")
        output$rawdata_status <- shiny::renderText({
          ""
        })
        r$files$data_file_neg <- input$rawdata_neg_file
        tmp <- read_msdial(filename = input$rawdata_neg_file$datapath)

        if(any(tmp$`Adduct type` == "[M-H]-")) {
          r$tables$raw_data_neg <- data.frame("polarity" = "neg",
                                              tmp,
                                              check.names = FALSE)
        } else {
          shiny::showNotification(
            ui = "Error: This data doesn't appear to contain any negative mode data!",
            type = "error"
          )
          r$tables$raw_data_neg <- NULL
          r$tables$clean_data <- NULL
          r$tables$raw_data <- NULL

          shinyWidgets::updateProgressBar(
            session = session,
            id = "feature_count_bar",
            title = "Feature count",
            value = 0,
            total = 100
          )

          shinyWidgets::updateProgressBar(
            session = session,
            id = "sample_count_bar",
            title = "Sample count",
            value = 0,
            total = 100
          )
        }

      })


    #---------------------------------------------------- process meta data ----
    shiny::observeEvent(
      input$metadata_file,
      {
        req(input$metadata_file)

        r$files$meta_file <- input$metadata_file$name
        file_path <- input$metadata_file$datapath
        data_table <- read_data(file_path = file_path)

        r$tables$meta_data <- data_table
        print("Meta data read")

        # update column names
        column_names <- colnames(r$tables$meta_data)
        shiny::updateSelectInput(
          inputId = "metadata_select_filename",
          choices = sort(column_names),
          selected = ifelse(any(grepl(x = column_names,
                                      pattern = ".*filename*",
                                      ignore.case = TRUE)),
                            grep(x = column_names,
                                 pattern = ".*filename.*",
                                 ignore.case = TRUE,
                                 value = TRUE)[1],
                            column_names[1])
        )
        shiny::updateSelectInput(
          inputId = "metadata_select_sampleid",
          choices = sort(column_names),
          selected = ifelse(any(grepl(x = column_names,
                                      pattern = ".*sampleid*",
                                      ignore.case = TRUE)),
                            grep(x = column_names,
                                 pattern = ".*sampleid*",
                                 ignore.case = TRUE,
                                 value = TRUE)[1],
                            column_names[1])
        )
        shiny::updateSelectInput(
          inputId = "metadata_select_sampletype",
          choices = sort(column_names),
          selected = ifelse(any(grepl(x = column_names,
                                      pattern = ".*type.*",
                                      ignore.case = TRUE)),
                            grep(x = column_names,
                                 pattern = ".*type.*",
                                 ignore.case = TRUE,
                                 value = TRUE)[1],
                            column_names[1])
        )
        shiny::updateSelectInput(
          inputId = "metadata_select_acqorder",
          choices = sort(column_names),
          selected = ifelse(any(grepl(x = column_names,
                                      pattern = ".*order.*",
                                      ignore.case = TRUE)),
                            grep(x = column_names,
                                 pattern = ".*order.*",
                                 ignore.case = TRUE,
                                 value = TRUE)[1],
                            column_names[1])
        )
        shiny::updateSelectInput(
          inputId = "metadata_select_batch",
          choices = sort(column_names),
          selected = ifelse(any(grepl(x = column_names,
                                      pattern = ".*batch.*",
                                      ignore.case = TRUE)),
                            grep(x = column_names,
                                 pattern = ".*batch.*",
                                 ignore.case = TRUE,
                                 value = TRUE)[1],
                            column_names[1])
        )
        shiny::updateSelectInput(
          inputId = "metadata_select_groups",
          choices = sort(column_names)
        )

        # r$bc_applied <- "none"
        # r$tables$bc_data <- NULL

        # shinyjs::enable(id = "rawdata_file")
      })


    output$metadata_preview_table = DT::renderDataTable({
      shiny::req(r$tables$meta_data)

      data_table <- r$tables$meta_data

      DT::datatable(data = data_table,
                    rownames = FALSE,
                    options = list(dom = "t",
                                   pageLength = -1))
    })


    shiny::observeEvent(
      c(input$metadata_select_filename,
        input$metadata_select_sampletype,
        input$metadata_select_acqorder),
      {
        r$columns$sampleid <- input$metadata_select_sampleid
        r$columns$filename <- input$metadata_select_filename
        r$columns$sampletype <- input$metadata_select_sampletype
        r$columns$acqorder <- input$metadata_select_acqorder
        r$columns$batch <- input$metadata_select_batch
      }
    )


    output$metadata_sampletype_plot <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 input$metadata_select_filename,
                 input$metadata_select_sampletype,
                 input$metadata_blank_pattern,
                 input$metadata_qc_pattern,
                 input$metadata_sample_pattern)

      # original data
      data_table <- r$tables$meta_data
      freq_table1 <- data.frame(table(base::factor(data_table[, input$metadata_select_sampletype])))
      names(freq_table1) <- c("value", "count")

      # sample selection based on patterns
      r$text_patterns$blanks <- input$metadata_blank_pattern
      r$text_patterns$pools <- input$metadata_qc_pattern
      r$text_patterns$samples <- input$metadata_sample_pattern
      blank_table <- r$tables$meta_data[grepl(x = r$tables$meta_data[, input$metadata_select_sampletype],
                                              pattern = input$metadata_blank_pattern,
                                              ignore.case = TRUE), ]
      qcpool_table <- r$tables$meta_data[grepl(x = r$tables$meta_data[, input$metadata_select_sampletype],
                                               pattern = input$metadata_qc_pattern,
                                               ignore.case = TRUE), ]
      sample_table <- r$tables$meta_data[grepl(x = r$tables$meta_data[, input$metadata_select_sampletype],
                                               pattern = input$metadata_sample_pattern,
                                               ignore.case = TRUE), ]

      # store id's
      r$index$blanks <- blank_table[, input$metadata_select_filename]
      r$index$pools <- qcpool_table[, input$metadata_select_filename]
      r$index$samples <- sample_table[, input$metadata_select_filename]
      r$index$selected_blanks <- r$index$blanks
      r$index$selected_pools <- r$index$pools
      r$index$selected_samples <- r$index$samples

      data_table <- rbind(
        blank_table,
        qcpool_table,
        sample_table
      )

      freq_table2 <- data.frame(table(base::factor(data_table[, input$metadata_select_sampletype])))
      names(freq_table2) <- c("value", "count")

      p1 <- distribution_plot(data = freq_table1,
                              title = "Type distribution, original")
      p2 <- distribution_plot(data = freq_table2,
                              title = "Type distribution, patterns applied ")

      patchwork::wrap_plots(p1, p2,
                            ncol = 2)
    })


    #----------------------------------------------------- process raw data ----
    shiny::observeEvent(
      c(input$rawdata_pos_file,
        input$rawdata_neg_file),
      {
        shiny::req(r$columns$filename, #r$columns$sampleid,
                   r$index$blanks,
                   r$index$pools,
                   r$index$samples,
                   r$omics)

        # is it neg or pos or both data
        if(!is.null(r$tables$raw_data_pos) & !is.null(r$tables$raw_data_neg)) {
          print("Combine data")
          r$tables$raw_data <- rbind.data.frame(r$tables$raw_data_pos,
                                                r$tables$raw_data_neg)
        }
        if(length(input$raw_which_files) == 1) {
          if(!is.null(r$tables$raw_data_pos)) {
            r$tables$raw_data <- r$tables$raw_data_pos
          }
          if(!is.null(r$tables$raw_data_neg)) {
            r$tables$raw_data <- r$tables$raw_data_neg
          }
        }

        if(!is.null(r$tables$raw_data)) {
          progress <- shiny::Progress$new(session = session,
                                          min = 0,
                                          max = 100)
          on.exit(progress$close())

          print("Clean up")
          clean_data_wide <- clean_up(raw_data = r$tables$raw_data,
                                      blanks = r$index$blanks,
                                      pools = r$index$pools,
                                      samples = r$index$samples)

          progress$set(value = 15,
                       message = "Processing...",
                       detail = NULL)

          r$tables$clean_data_wide <- select_identified(data = clean_data_wide,
                                                        omics = r$omics)

          progress$set(value = 30,
                       message = "Processing...",
                       detail = NULL)

          r$tables$clean_data <- make_tidy(data = r$tables$clean_data_wide,
                                           blanks = r$index$blanks,
                                           pools = r$index$pools,
                                           samples = r$index$samples,
                                           omics = r$omics)

          progress$set(value = 45,
                       message = "Processing...",
                       detail = NULL)

          # merge with meta data
          r$tables$clean_data <- merge(
            x = r$tables$clean_data,
            y = r$tables$meta_data,
            by.x = "sample_name",
            by.y = r$columns$filename
          )
          r$tables$clean_data$batch <- r$tables$clean_data[, r$columns$batch]

          progress$set(value = 55,
                       message = "Processing...",
                       detail = NULL)

          r$settings$feature_class <- sort(unique(r$tables$clean_data$class_ion))

          # trend calculation
          qcpool_data <- r$tables$clean_data[r$tables$clean_data$sample_name %in% r$index$selected_pools, ]
          qcpool_meta <- r$tables$meta_data[r$tables$meta_data[, r$columns$filename] %in% r$index$selected_pools, ]

          r$tables$trend_data <- calc_trend(pool_data = qcpool_data,
                                            order_column = r$columns$acqorder)

          # apply all filtering here (rsd, id, blank/sample, etc.)
          # RSD filtering
          rsd_res <- calc_rsd(data = r$tables$clean_data,
                              pools = r$index$selected_pools,
                              cut_off = r$settings$rsd_cutoff)
          r$index$keep_rsd <- rsd_res$keep
          r$tables$rsd_data_overall <- rsd_res$rsd_data_overall
          r$tables$rsd_data_batch <- rsd_res$rsd_data_batch

          progress$set(value = 65,
                       message = "Processing...",
                       detail = NULL)

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

          progress$set(value = 75,
                       message = "Processing...",
                       detail = NULL)

          # what to keep
          r$tables$analysis_data <- r$tables$clean_data
          r$tables$analysis_data$class_keep <- switch(
            r$omics,
            "lip" = r$tables$analysis_data$class_ion %in% r$defaults$lipidclass_ion,
            "met" = r$tables$analysis_data$class_ion %in% r$defaults$metclass_ion
          )
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

          total_features <- sum(c(length(unique(r$tables$raw_data_pos$`Alignment ID`)),
                                  length(unique(r$tables$raw_data_neg$`Alignment ID`))), na.rm = TRUE)
          total_samples <- sum(c(length(r$index$blanks),
                                 length(r$index$pools),
                                 length(r$index$samples)),
                               na.rm = TRUE)

          progress$set(value = 100,
                       message = "Processing...",
                       detail = NULL)

          shinyWidgets::updateProgressBar(
            session = session,
            id = "feature_count_bar",
            title = "Feature count",
            value = length(unique(r$tables$clean_data$my_id)),
            total = total_features
          )

          shinyWidgets::updateProgressBar(
            session = session,
            id = "sample_count_bar",
            title = "Sample count",
            value = length(unique(r$tables$clean_data$sample_name)),
            total = total_samples
          )
        }

      })


    output$rawdata_preview_table <- DT::renderDataTable({
      shiny::req(r$tables$raw_data,
                 input$raw_select_table)

      data_table <- r$tables[[input$raw_select_table]]

      DT::datatable(data = utils::head(data_table, n = 10),
                    rownames = FALSE,
                    options = list(dom = "t",
                                   pageLength = -1))
    })


    shiny::observeEvent(input$raw_select_omics, {
      r$omics <- input$raw_select_omics

      r$settings$selected_feature_class <- switch(
        r$omics,
        "lip" = r$defaults$lipidclass_ion,
        "met" = r$defaults$metclass_ion
      )
      print(r$omics)
    })


    #----------------------------------------------------------------- Rdata----
    shiny::observeEvent(input$load_rdata, {
      print("Loading Rdata file.")

      progress <- shiny::Progress$new(session = session,
                                      min = 0,
                                      max = 100)
      on.exit(progress$close())

      progress$set(value = 50,
                   message = "Processing...",
                   detail = NULL)

      import_env <- load_to_env(RData = input$load_rdata$datapath)
      #---------------------------------------------------------- Settings -----
      r$name <- import_env$r$name
      r$omics <- import_env$r$omics
      shiny::updateRadioButtons(
        inputId = "raw_select_omics",
        selected = r$omics
      )
      print(r$omics)

      r$defaults$lipidclass_ion <- import_env$r$defaults$lipidclass_ion
      r$defaults$metclass_ion <- import_env$r$defaults$metclass_ion
      r$defaults$lipid_classes <- import_env$r$defaults$lipid_classes

      progress$set(value = 60,
                   message = "Processing...",
                   detail = NULL)

      r$settings$rsd_cutoff <- import_env$r$settings$rsd_cutoff
      r$settings$dot_cutoff <- import_env$r$settings$dot_cutoff
      r$settings$revdot_cutoff <- import_env$r$settings$revdot_cutoff
      r$settings$blanksample_ratio <- import_env$r$settings$blanksample_ratio
      r$settings$blanksample_threshold <- import_env$r$settings$blanksample_threshold
      r$settings$feature_class <- import_env$r$settings$feature_class
      r$settings$selected_feature_class <- import_env$r$settings$selected_feature_class
      shiny::updateNumericInput(
        inputId = "settings_rsd_cutoff",
        value = r$settings$rsd_cutoff
      )
      shiny::updateNumericInput(
        inputId = "settings_dot_cutoff",
        value = r$settings$dot_cutoff
      )
      shiny::updateNumericInput(
        inputId = "settings_revdot_cutoff",
        value = r$settings$revdot_cutoff
      )
      shiny::updateNumericInput(
        inputId = "settings_ratio",
        value = r$settings$blanksample_ratio
      )
      shiny::updateSliderInput(
        inputId = "settings_threshold",
        value = r$settings$blanksample_threshold
      )

      progress$set(value = 70,
                   message = "Processing...",
                   detail = NULL)
      #------------------------------------------------------------- Files -----
      r$files$meta_file <- import_env$r$files$meta_file
      r$files$data_file_pos <- import_env$r$files$data_file_pos
      r$files$data_file_neg <- import_env$r$files$data_file_neg
      r$files$rda_file <- input$load_rdata$datapath
      selected_files <- ""
      if(!is.null(r$files$data_file_pos)) {
        selected_files <- c(selected_files, "pos")
      }
      if(!is.null(r$files$data_file_neg)) {
        selected_files <- c(selected_files, "neg")
      }
      shiny::updateCheckboxGroupInput(
        inputId = "raw_which_files",
        selected = selected_files
      )

      progress$set(value = 75,
                   message = "Processing...",
                   detail = NULL)

      #--------------------------------------------------------- Meta data -----
      r$tables$meta_data <- import_env$r$tables$meta_data
      r$columns$filename <- import_env$r$columns$filename
      r$columns$sampleid <- import_env$r$columns$sampleid
      r$columns$sampletype <- import_env$r$columns$sampletype
      r$columns$acqorder <- import_env$r$columns$acqorder
      r$columns$batch <- import_env$r$columns$batch
      r$columns$groups <- import_env$r$columns$groups
      r$text_patterns$blanks <- import_env$r$text_patterns$blanks
      r$text_patterns$pools <- import_env$r$text_patterns$pools
      r$text_patterns$samples <- import_env$r$text_patterns$samples

      column_names <- colnames(r$tables$meta_data)
      shiny::updateSelectInput(
        inputId = "metadata_select_filename",
        choices = sort(column_names),
        selected = r$columns$filename
      )
      shiny::updateSelectInput(
        inputId = "metadata_select_sampleid",
        choices = sort(column_names),
        selected = r$columns$sampleid
      )
      shiny::updateSelectInput(
        inputId = "metadata_select_sampletype",
        choices = sort(column_names),
        selected = r$columns$sampletype
      )
      shiny::updateSelectInput(
        inputId = "metadata_select_acqorder",
        choices = sort(column_names),
        selected = r$columns$acqorder
      )
      shiny::updateSelectInput(
        inputId = "metadata_select_batch",
        choices = sort(column_names),
        selected = r$columns$batch
      )
      shiny::updateSelectInput(
        inputId = "metadata_select_groups",
        choices = sort(column_names),
        selected = r$columns$groups
      )
      shiny::updateTextInput(
        inputId = "metadata_blank_pattern",
        value = r$text_patterns$blanks
      )
      shiny::updateTextInput(
        inputId = "metadata_qc_pattern",
        value = r$text_patterns$pools
      )
      shiny::updateTextInput(
        inputId = "metadata_samples_pattern",
        value = r$text_patterns$samples
      )
      progress$set(value = 80,
                   message = "Processing...",
                   detail = NULL)
      #------------------------------------------------------------ Indexes ----
      r$index$blanks <- import_env$r$index$blanks
      r$index$pools <- import_env$r$index$pools
      r$index$samples <- import_env$r$index$samples
      r$index$selected_blanks <- import_env$r$index$selected_blanks
      r$index$selected_pools <- import_env$r$index$selected_pools
      r$index$selected_samples <- import_env$r$index$selected_samples
      r$index$keep_rsd <- import_env$r$index$keep_rsd
      r$index$keep_blankratio <- import_env$r$index$keep_blankratio
      r$index$keep_id <- import_env$r$index$keep_id

      progress$set(value = 85,
                   message = "Processing...",
                   detail = NULL)

      #---------------------------------------------------------- Raw data -----
      r$tables$raw_data_pos <- import_env$r$tables$raw_data_pos
      r$tables$raw_data_neg <- import_env$r$tables$raw_data_neg
      r$tables$raw_data <- import_env$r$tables$raw_data
      r$tables$clean_data <- import_env$r$tables$clean_data
      r$tables$clean_data_wide <- import_env$r$tables$clean_data_wide
      r$tables$blank_data <- import_env$r$tables$blank_data
      r$tables$qc_data <- import_env$r$tables$qc_data
      r$tables$rsd_data_overall <- import_env$r$tables$rsd_data_overall
      r$tables$rsd_data_batch <- import_env$r$tables$rsd_data_batch
      r$tables$analysis_data <- import_env$r$tables$analysis_data
      r$tables$trend_data <- import_env$r$tables$trend_data

      total_features <- sum(c(length(unique(r$tables$raw_data_pos$`Alignment ID`)),
                              length(unique(r$tables$raw_data_neg$`Alignment ID`))), na.rm = TRUE)
      total_samples <- sum(c(length(r$index$blanks),
                             length(r$index$pools),
                             length(r$index$samples)),
                           na.rm = TRUE)

      shinyWidgets::updateProgressBar(
        session = session,
        id = "feature_count_bar",
        title = "Feature count",
        value = length(unique(r$tables$clean_data$my_id)),
        total = total_features
      )

      shinyWidgets::updateProgressBar(
        session = session,
        id = "sample_count_bar",
        title = "Sample count",
        value = length(unique(r$tables$clean_data$sample_name)),
        total = total_samples
      )

      progress$set(value = 90,
                   message = "Processing...",
                   detail = NULL)

      #---------------------------------------------------- class selection ----
      r$defaults$patterns$PL <- import_env$r$defaults$patterns$PL
      r$defaults$patterns$GL <- import_env$r$defaults$patterns$GL
      r$defaults$patterns$Cer <- import_env$r$defaults$patterns$Cer
      r$defaults$patterns$HexCer <- import_env$r$defaults$patterns$HexCer
      r$defaults$patterns$FA <- import_env$r$defaults$patterns$FA
      r$defaults$patterns$PSL <- import_env$r$defaults$patterns$PSL
      r$defaults$patterns$SB <- import_env$r$defaults$patterns$SB
      r$defaults$patterns$SA <- import_env$r$defaults$patterns$SA
      r$defaults$patterns$CL <- import_env$r$defaults$patterns$CL
      r$defaults$patterns$ACPIM <- import_env$r$defaults$patterns$ACPIM
      r$defaults$patterns$STL <- import_env$r$defaults$patterns$STL
      r$defaults$patterns$PRL <- import_env$r$defaults$patterns$PRL
      shiny::updateCheckboxGroupInput(
        inputId = "select_PL_class",
        selected = r$defaults$patterns$PL
      )
      shiny::updateCheckboxGroupInput(
        inputId = "select_Cer_class",
        selected = r$defaults$patterns$Cer
      )
      shiny::updateCheckboxGroupInput(
        inputId = "select_HexCer_class",
        selected = r$defaults$patterns$HexCer
      )
      shiny::updateCheckboxGroupInput(
        inputId = "select_FA_class",
        selected = r$defaults$patterns$FA
      )
      shiny::updateCheckboxGroupInput(
        inputId = "select_PSL_class",
        selected = r$defaults$patterns$PSL
      )
      shiny::updateCheckboxGroupInput(
        inputId = "select_SB_class",
        selected = r$defaults$patterns$SB
      )
      shiny::updateCheckboxGroupInput(
        inputId = "select_SA_class",
        selected = r$defaults$patterns$SA
      )
      shiny::updateCheckboxGroupInput(
        inputId = "select_GL_class",
        selected = r$defaults$patterns$GL
      )
      shiny::updateCheckboxGroupInput(
        inputId = "select_CL_class",
        selected = r$defaults$patterns$CL
      )
      shiny::updateCheckboxGroupInput(
        inputId = "select_STL_class",
        selected = r$defaults$patterns$STL
      )
      shiny::updateCheckboxGroupInput(
        inputId = "select_ACPIM_class",
        selected = r$defaults$patterns$ACPIM
      )
      shiny::updateCheckboxGroupInput(
        inputId = "select_PRL_class",
        selected = r$defaults$patterns$PRL
      )

      progress$set(value = 100,
                   message = "Processing...",
                   detail = NULL)

    })

  })
}
