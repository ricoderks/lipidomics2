#' file
#'
#' @title Read MS-DIAl files
#'
#' @description Read the result files from MS-DIAL.
#'
#' @param filename The filename of the MS-DIAL file.
#'
#' @return Returns a tibble
#'
#' @importFrom readr read_delim cols col_double col_character col_integer
#'
#' @author Rico Derks
#'
read_msdial <- function(filename = NULL) {
  # determine which version is loaded, read only the column names
  column_names <- colnames(readr::read_delim(file = filename,
                                             delim ="\t",
                                             na = c("", "NA", "null"),
                                             n_max = 1,
                                             show_col_types = FALSE,
                                             skip = 4))

  if("Simple dot product" %in% column_names) {
    # version > 5.1
    res <- readr::read_delim(file = filename,
                             delim ="\t",
                             na = c("", "NA", "null"),
                             show_col_types = FALSE,
                             col_types = readr::cols(`Alignment ID` = readr::col_integer(),
                                                     `Average Rt(min)` = readr::col_double(),
                                                     `Average Mz` = readr::col_double(),
                                                     `Metabolite name` = readr::col_character(),
                                                     `Adduct type` = readr::col_character(),
                                                     `Post curation result` = readr::col_character(),
                                                     `Fill %` = readr::col_double(),
                                                     `MS/MS assigned` = readr::col_character(),
                                                     `Reference RT` = readr::col_character(),
                                                     `Reference m/z` = readr::col_character(),
                                                     Formula = readr::col_character(),
                                                     Ontology = readr::col_character(),
                                                     INCHIKEY = readr::col_character(),
                                                     SMILES = readr::col_character(),
                                                     `Annotation tag (VS1.0)` = readr::col_character(),
                                                     `RT matched` = readr::col_character(),
                                                     `m/z matched` = readr::col_character(),
                                                     `MS/MS matched` = readr::col_character(),
                                                     Comment = readr::col_character(),
                                                     `Manually modified for quantification` = readr::col_character(),
                                                     `Manually modified for annotation` = readr::col_character(),
                                                     `Isotope tracking parent ID` = readr::col_character(),
                                                     `Isotope tracking weight number` = readr::col_character(),
                                                     `Total score` = readr::col_double(),
                                                     `RT similarity` = readr::col_double(),
                                                     `Simple dot product` = readr::col_double(),
                                                     `Reverse dot product` = readr::col_double(),
                                                     `Weighted dot product` = readr::col_double(),
                                                     `Matched peaks percentage` = readr::col_double(),
                                                     `S/N average` = readr::col_double(),
                                                     `Spectrum reference file name` = readr::col_character(),
                                                     `MS1 isotopic spectrum` = readr::col_character(),
                                                     `MS/MS spectrum` = readr::col_character()),
                             skip = 4)
  } else {
    # version 4.x
    res <- readr::read_delim(file = filename,
                             delim ="\t",
                             na = c("", "NA", "null"),
                             show_col_types = FALSE,
                             col_types = readr::cols(`Alignment ID` = readr::col_integer(),
                                                     `Average Rt(min)` = readr::col_double(),
                                                     `Average Mz` = readr::col_double(),
                                                     `Metabolite name` = readr::col_character(),
                                                     `Adduct type` = readr::col_character(),
                                                     `Post curation result` = readr::col_character(),
                                                     `Fill %` = readr::col_double(),
                                                     `MS/MS assigned` = readr::col_character(),
                                                     `Reference RT` = readr::col_character(),
                                                     `Reference m/z` = readr::col_character(),
                                                     Formula = readr::col_character(),
                                                     Ontology = readr::col_character(),
                                                     INCHIKEY = readr::col_character(),
                                                     SMILES = readr::col_character(),
                                                     `Annotation tag (VS1.0)` = readr::col_character(),
                                                     `RT matched` = readr::col_character(),
                                                     `m/z matched` = readr::col_character(),
                                                     `MS/MS matched` = readr::col_character(),
                                                     Comment = readr::col_character(),
                                                     `Manually modified for quantification` = readr::col_character(),
                                                     `Manually modified for annotation` = readr::col_character(),
                                                     `Isotope tracking parent ID` = readr::col_character(),
                                                     `Isotope tracking weight number` = readr::col_character(),
                                                     `Total score` = readr::col_double(),
                                                     `RT similarity` = readr::col_double(),
                                                     `Dot product` = readr::col_double(),
                                                     `Reverse dot product` = readr::col_double(),
                                                     `Fragment presence %` = readr::col_double(),
                                                     `S/N average` = readr::col_double(),
                                                     `Spectrum reference file name` = readr::col_character(),
                                                     `MS1 isotopic spectrum` = readr::col_character(),
                                                     `MS/MS spectrum` = readr::col_character()),
                             skip = 4)
  }



  return(res)
}


#' @title Clean up tibble from MS-DIAL
#'
#' @description Clean up the columns and column names of the tibble after
#'     reading the MS-DIAL result files.
#'
#' @param raw_data The tibble.
#' @param blanks character() vector with the name of the blanks to keep.
#' @param pools character() vector with the name of the pooled samples to keep.
#' @param samples character() vector with the name of the samples to keep.
#'
#' @return Returns a tibble
#'
#' @importFrom dplyr rename mutate select
#' @importFrom tidyselect contains
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#'
#' @author Rico Derks
#'
clean_up <- function(raw_data = NULL,
                     blanks = NULL,
                     pools = NULL,
                     samples = NULL) {
  # remove spaces at the front or at the back of the column name
  colnames(raw_data) <- trimws(colnames(raw_data))
  column_names <- colnames(raw_data)

  if("Simple dot product" %in% column_names) {
    # version > 5.1
    # rename some columns in the data frame for ease of access later on.
    raw_data <- raw_data |>
      dplyr::rename(AlignmentID = .data$`Alignment ID`,
                    AverageRT = .data$`Average Rt(min)`,
                    AverageMZ = .data$`Average Mz`,
                    ion = .data$`Adduct type`,
                    FeatureName = .data$`Metabolite name`,
                    Class = .data$Ontology,
                    DotProduct = .data$`Simple dot product`,
                    RevDotProduct = .data$`Reverse dot product`,
                    TotalScore = .data$`Total score`,
                    FragPresence = .data$`Matched peaks percentage`,
                    RefFile = .data$`Spectrum reference file name`,
                    MSMSspectrum = .data$`MS/MS spectrum`) |>
      dplyr::mutate(DotProduct = .data$DotProduct * 100,
                    RevDotProduct = .data$RevDotProduct * 100,
                    scale_DotProduct = .data$DotProduct / 10,
                    scale_RevDotProduct = .data$RevDotProduct / 10,
                    my_id = paste(.data$polarity, "_", .data$AlignmentID, sep = ""),
                    ion = factor(.data$ion),
                    polarity = factor(.data$polarity)) |>
      dplyr::select(.data$my_id, .data$AlignmentID, .data$AverageRT, .data$AverageMZ, .data$ion, .data$FeatureName, .data$Class,
                    .data$DotProduct, .data$scale_DotProduct, .data$RevDotProduct, .data$scale_RevDotProduct,
                    .data$FragPresence, .data$TotalScore, .data$polarity, .data$MSMSspectrum,
                    tidyselect::contains(c(blanks, pools, samples)))
  } else {
    # version 4.x
    # rename some columns in the data frame for ease of access later on.
    raw_data <- raw_data |>
      dplyr::rename(AlignmentID = .data$`Alignment ID`,
                    AverageRT = .data$`Average Rt(min)`,
                    AverageMZ = .data$`Average Mz`,
                    ion = .data$`Adduct type`,
                    FeatureName = .data$`Metabolite name`,
                    Class = .data$Ontology,
                    DotProduct = .data$`Dot product`,
                    RevDotProduct = .data$`Reverse dot product`,
                    TotalScore = .data$`Total score`,
                    FragPresence = .data$`Fragment presence %`,
                    RefFile = .data$`Spectrum reference file name`,
                    MSMSspectrum = .data$`MS/MS spectrum`) |>
      dplyr::mutate(scale_DotProduct = .data$DotProduct / 10,
                    scale_RevDotProduct = .data$RevDotProduct / 10,
                    my_id = paste(.data$polarity, "_", .data$AlignmentID, sep = ""),
                    ion = factor(.data$ion),
                    polarity = factor(.data$polarity)) |>
      dplyr::select(.data$my_id, .data$AlignmentID, .data$AverageRT, .data$AverageMZ, .data$ion, .data$FeatureName, .data$Class,
                    .data$DotProduct, .data$scale_DotProduct, .data$RevDotProduct, .data$scale_RevDotProduct,
                    .data$FragPresence, .data$TotalScore, .data$polarity, .data$MSMSspectrum,
                    tidyselect::contains(c(blanks, pools, samples)))
  }
  return(raw_data)
}


#' @title Keep only the identified lipids
#'
#' @description Filter the tibble to keep only the identified lipids.
#'
#' @param data The tibble.
#' @param omics character(1), lipidomics or metabolomics data
#'
#' @return Returns a tibble
#'
#' @importFrom dplyr filter arrange mutate
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#'
#' @author Rico Derks
#'
select_identified <- function(data = NULL,
                              omics = c("lip", "met")) {
  # rename some columns in the data frame for ease of access later on.
  data <- data |>
    dplyr::filter(.data$FeatureName != "Unknown",
                  .data$Class != "Others",
                  # remove annotated peaks without library result
                  !grepl(x = .data$FeatureName,
                         pattern = "w/o .*"),
                  !grepl(x = .data$FeatureName,
                         pattern = "RIKEN"),
                  !grepl(x = .data$FeatureName,
                         pattern = "(no MS2.*|low score.*)")) |>
    dplyr::arrange(.data$Class, .data$FeatureName, .data$polarity) |>
    # add some extra columns
    dplyr::mutate(
      # make Class a factor
      Class = factor(.data$Class,
                     levels = sort(unique(.data$Class)),
                     labels = sort(unique(.data$Class))),
      # general keep
      keep = TRUE,
      comment = "",
      # if rsd is too high
      rsd_keep = TRUE,
      # if quality MSMS is too high
      match_keep = TRUE,
      # if retention time is wrong
      rt_keep = TRUE,
      # if the background is too high
      background_keep = TRUE,
      # if class is discarded
      class_keep = TRUE,
      # clean up some levels which might not be present anymore
      ion = droplevels(.data$ion),
      polarity = droplevels(.data$polarity),
      Class = droplevels(.data$Class))

  data <- switch(
    omics,
    "lip" = {
      data |>
        dplyr::mutate(
          # get the short lipid name
          ShortLipidName = stringr::str_extract(string = .data$FeatureName,
                                                pattern = "[A-Za-z- 0-9:;/\\(\\)]+"),
          # make a copy of the ShortLipidName
          orgShortLipidName = .data$ShortLipidName,
          # get the long lipid name
          LongLipidName = stringr::str_replace(string = .data$FeatureName,
                                               pattern = "([A-Za-z-_ 0-9:;/]*)([|])([A-Za-z-_ 0-9:;]*)",
                                               replacement = "\\3"),
          # correct for empty long lipid names
          LongLipidName = ifelse(.data$LongLipidName == "" | is.na(.data$LongLipidName),
                                 .data$ShortLipidName,
                                 .data$LongLipidName),
          class_ion = paste(.data$Class, .data$ion,
                            sep = " - "),
        )
    }
  )

  return(data)
}



#' @title Make the tibble in tidy format
#'
#' @description Filter the tibble to keep only the identified lipids.
#'
#' @param data The tibble.
#' @param blanks character() vector with the name of the blanks to keep.
#' @param pools character() vector with the name of the pooled samples to keep.
#' @param samples character() vector with the name of the samples to keep.
#' @param omics character(1), lipidomics or metabolomics data
#'
#' @details After making the tibble in long format also some additional columns ared added.
#'
#' @return Returns a tibble in tidy (long) format
#'
#' @importFrom dplyr mutate n group_by ungroup arrange if_else
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect matches
#' @importFrom rlang .data
#' @importFrom stringr str_extract str_replace
#'
#' @author Rico Derks
#'
make_tidy <- function(data = NULL,
                      blanks = NULL,
                      pools = NULL,
                      samples = NULL,
                      omics = c('lip", "met')) {
  # create long table
  df_long <- data |>
    tidyr::pivot_longer(cols = tidyselect::contains(c(blanks, pools, samples)),
                        names_to = "sample_name",
                        values_to = "area")

  df_long <- switch(
    omics,
    "lip" = {
      df_long |>
        dplyr::mutate(
          # a column with number of carbons and double bonds is needed for the bubble plots
          carbons = factor(stringr::str_extract(string = .data$ShortLipidName,
                                                pattern = "[0-9]{1,2}")),
          carbon_db = stringr::str_extract(string = .data$ShortLipidName,
                                           pattern = "[0-9]{1,2}:[0-9]{1,2}")) |>
        # sample_type = factor(tolower(stringr::str_extract(string = .data$sample_name,
        #                                                   pattern = "([bB]lank|[qQ][cC]pool|[sS]ample)"))))

        ### rename duplicate lipids
        # df_long <- df_long |>
        # determine what are the duplicates
        dplyr::group_by(.data$ShortLipidName, .data$sample_name) |>
        dplyr::arrange(.data$AverageRT) |>
        dplyr::mutate(count_duplicates = dplyr::n(),
                      append_name = paste0("_", 1:dplyr::n())) |>
        dplyr::ungroup() |>
        # rename them
        dplyr::mutate(ShortLipidName = dplyr::if_else(.data$count_duplicates > 1,
                                                      paste0(.data$ShortLipidName, .data$append_name),
                                                      .data$ShortLipidName)) |>
        # sort back
        dplyr::arrange(.data$Class, .data$ShortLipidName) |>
        dplyr::select(-.data$count_duplicates, -.data$append_name)
    }
  )

  return(df_long)
}


read_data <- function(file_path = NULL,
                      sep = NULL,
                      first_column_as_index = FALSE) {
  if(is.null(file_path)) {
    stop("No file path specified!")
  } else {
    if(!file.exists(file_path)) {
      stop("File path does not exist!")
    }
  }

  # if (is.na(sep)) {
  #   if (stringr::str_sub(file_path, -4, -1) == ".tsv") {
  #     sep <- '\t'
  #   }
  # }

  if (first_column_as_index) {
    index <- 1
  } else {
    index <- NULL
  }

  if (stringr::str_sub(file_path, -5, -1) == ".xlsx") {
    data_table <- as.data.frame(readxl::read_xlsx(path = file_path))
  } else {
    if (is.null(sep)) {
      sep <- find_delim(file_path = file_path)
      data_table <- utils::read.table(file_path,
                                      header = TRUE,
                                      sep = sep,
                                      check.names = FALSE)
    } else {
      data_table <- utils::read.table(file_path,
                                      header = TRUE,
                                      sep = sep,
                                      check.names = FALSE)
    }
  }

  # if (!is.null(index)) {
  #   duplicates <- duplicated(data_table[, index])
  #   if (sum(duplicates) > 0) {
  #     print(paste0('Removed ', sum(duplicates), ' duplicated features'))
  #     data_table <- data_table[!duplicates, ]
  #     rownames(data_table) <- data_table[, 1]
  #     data_table[, 1] <- NULL
  #   }
  # }

  original_count <- ncol(data_table)
  if (original_count > 1) {
    data_table <- data_table[, !duplicated(colnames(data_table))]
    final_count <- ncol(data_table)
    if(original_count != final_count) {
      print(paste0("Removed ", original_count - final_count, " duplicated columns"))
    }
  }

  return(data_table)
}


#' @title Find the delimiter of a file
#'
#' @description
#' Find the delimiter of a file by reading the first 10 lines.
#'
#' @param file_path character(1) path of the file.
#'
#' @return character(1), the delimiter found.
#'
#' @author Damien Olivier
#' @author Rico Derks
#'
find_delim = function(file_path = NULL) {
  if(is.null(file_path)) {
    stop("No file path specified!")
  } else {
    if(!file.exists(file_path)) {
      stop("File path does not exist!")
    }
  }

  probe <- paste(readLines(con = file_path,
                           n = 10),
                 collapse = "")
  sep <- c(
    "\t" = lengths(regmatches(x = probe,
                              m = gregexpr(pattern = "\t",
                                           text = probe))),
    "," = lengths(regmatches(x = probe,
                             m = gregexpr(pattern = ",",
                                          text = probe))),
    ";" = lengths(regmatches(x = probe,
                             m = gregexpr(pattern = ";",
                                          text = probe)))
  )

  return(names(which.max(sep)))
}


#' @title Distribution plot
#'
#' @description
#' Distribution plot of the sample types
#'
#' @param data data.frame with all the data.
#' @param title character(1), title of the plot.
#'
#' @return ggplot2 object
#'
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes .data geom_bar geom_text labs theme_minimal
#'
#' @noRd
distribution_plot <- function(data = NULL,
                              title = NULL) {
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$value,
                                 y = .data$count)) +
    ggplot2::geom_bar(stat = "identity",
                      fill = "lightblue") +
    ggplot2::geom_text(ggplot2::aes(label = .data$count),
                       vjust = -0.5,
                       hjust = 0.5,
                       size = 4) +
    ggplot2::labs(x = NULL,
                  y = NULL,
                  title = title) +
    ggplot2::theme_minimal()

  return(p)
}
