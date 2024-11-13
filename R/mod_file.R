#' file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib navset_card_tab nav_panel card page_sidebar sidebar tooltip
#' @importFrom bsicons bs_icon
#' @importFrom DT dataTableOutput
#' @importFrom shinyWidgets progressBar
#' @importFrom shinyjs disabled disable enable
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
                  value = "^pool",
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
                style = "font-size:85%"
              )

            ),
            shiny::fileInput(
              inputId = ns("metadata_file"),
              label = "Data file:",
              multiple = FALSE,
              accept = c(".csv", ".tsv", ".txt", ".xlsx")
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
                  "Which result files from MS-DIAl to upload!"
                ),
                choices = c("Positive" = "pos", "Negative" = "neg"),
                selected = c("pos", "neg")
              )
            ),
            bslib::card_body(
              height = "35%",
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
                shiny::selectInput(
                  inputId = ns("raw_select_table"),
                  label = "Select table",
                  choices = c("Raw table" = "raw_data",
                              "Filtered table" = "clean_data"),
                  selected = "clean_data"
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
      ) # end navpanel raw data
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

        # need to add a polarity check here

        print("Raw data read - pos file")
        r$files$data_file_pos <- input$rawdata_pos_file
        tmp <- read_msdial(filename = input$rawdata_pos_file$datapath)
        r$tables$raw_data_pos <- data.frame("polarity" = "pos",
                                            tmp,
                                            check.names = FALSE)
      })


    shiny::observeEvent(
      input$rawdata_neg_file,
      {
        shiny::req(input$rawdata_neg_file)

        # need to add a polarity check here

        print("Raw data read - neg file")
        r$files$data_file_neg <- input$rawdata_neg_file
        tmp <- read_msdial(filename = input$rawdata_neg_file$datapath)
        r$tables$raw_data_neg <- data.frame("polarity" = "neg",
                                            tmp,
                                            check.names = FALSE)

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
          inputId = "metadata_select_sampleid",
          choices = sort(column_names),
          selected = ifelse(any(grepl(x = column_names,
                                      pattern = ".*sampleid.*",
                                      ignore.case = TRUE)),
                            grep(x = column_names,
                                 pattern = ".*sampleid.*",
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
      c(input$metadata_select_sampleid,
        input$metadata_select_sampletype,
        input$metadata_select_acqorder),
      {
        r$columns$sampleid <- input$metadata_select_sampleid
        r$columns$sampletype <- input$metadata_select_sampletype
        r$columns$acqorder <- input$metadata_select_acqorder
      }
    )


    output$metadata_sampletype_plot <- shiny::renderPlot({
      shiny::req(r$tables$meta_data,
                 input$metadata_select_sampleid,
                 input$metadata_select_sampletype,
                 input$metadata_blank_pattern,
                 input$metadata_qc_pattern,
                 input$metadata_sample_pattern)

      # original data
      data_table <- r$tables$meta_data
      freq_table1 <- data.frame(table(base::factor(data_table[, input$metadata_select_sampletype])))
      names(freq_table1) <- c("value", "count")

      # sample selection based on patterns
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
      r$index$blanks <- blank_table[, input$metadata_select_sampleid]
      r$index$pools <- qcpool_table[, input$metadata_select_sampleid]
      r$index$samples <- sample_table[, input$metadata_select_sampleid]

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
    # shiny::observe({
    shiny::observeEvent(c(input$rawdata_pos_file,
                          input$rawdata_neg_file), {
      shiny::req(r$columns$sampleid,
                 r$index$blanks,
                 r$index$pools,
                 r$index$samples,
                 input$raw_select_omics)

      if(!(is.null(r$tables$raw_data_pos) & is.null(r$tables$raw_data_neg))) {
        print("Combine data")
        r$tables$raw_data <- rbind.data.frame(r$tables$raw_data_pos,
                                              r$tables$raw_data_neg)

        if(!is.null(r$tables$raw_data)) {
          print("Clean up")
          clean_data <- clean_up(raw_data = r$tables$raw_data,
                                 blanks = r$index$blanks,
                                 pools = r$index$pools,
                                 samples = r$index$samples)

          clean_data <- select_identified(data = clean_data,
                                          omics = input$raw_select_omics)

          clean_data <- make_tidy(data = clean_data,
                                  blanks = r$index$blanks,
                                  pools = r$index$pools,
                                  samples = r$index$samples,
                                  omics = input$raw_select_omics)

          r$tables$clean_data <- clean_data

          total_features <- sum(c(length(unique(r$tables$raw_data_pos$`Alignment ID`)),
            length(unique(r$tables$raw_data_pos$`Alignment ID`))), na.rm = TRUE)
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
        }
      }
    })


    output$rawdata_preview_table = DT::renderDataTable({
      shiny::req(r$tables$raw_data,
                 input$raw_select_table)

      data_table <- r$tables[[input$raw_select_table]]

      DT::datatable(data = head(data_table, n = 10),
                    rownames = FALSE,
                    options = list(dom = "t",
                                   pageLength = -1))
    })

  })
}
