#' export
#' @title Create the export data.frame
#'
#' @description Create the export data.frame.
#'
#' @param meta_data data.frame with meta data.
#' @param analysis_data data.frame with the analysis data to export.
#' @param filename_column column name of the filename column in the meta data.
#' @param normalization character(), which normalization to export additionally.
#' @param blanks character() with file names of the blanks.
#' @param qcpools character() with the file names of the pooled samples.
#' @param samples character() with the file names of the samples.
#'
#' @return list with data.frames with export data.
#'
#' @noRd
#'
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @author Rico Derks
#'
prepare_export_data <- function(meta_data = NULL,
                                analysis_data = NULL,
                                filename_column = NULL,
                                normalization = NULL,
                                blanks = NULL,
                                qcpools = NULL,
                                samples = NULL) {

  normalization <- names(unlist(normalization)[unlist(normalization)])
  normalization <- c("raw", normalization)

  # meta data
  export_meta <- meta_data[meta_data[, filename_column] %in% c(blanks, qcpools, samples), ]
  export_meta <- as.data.frame(lapply(export_meta, as.character))
  export_meta <- export_meta[sort(export_meta[, filename_column],
                                  index.return = TRUE)$ix, ]

  export_meta <- export_meta |>
    tidyr::pivot_longer(cols = -filename_column) |>
    tidyr::pivot_wider(names_from = filename_column,
                       values_from = "value")

  meta_cols <- colnames(export_meta)
  export_meta <- data.frame(export_meta[, meta_cols[1]],
                            "ShortLipidName" = NA,
                            "LongLipidName" = NA,
                            "Class" = NA,
                            "ion" = NA,
                            export_meta[, meta_cols[-1]],
                            check.names = FALSE)
  colnames(export_meta)[1] <- "my_id"

  # analysis data
  export <- vector(mode = "list", length = length(normalization))
  names(export) <- normalization
  for(norm in normalization) {
    values_column <- switch(
      norm,
      "raw" = "area",
      "totNorm" = "totNormArea",
      "pqnNorm" = "pqnNormArea",
      "protNorm" = "protNormArea"
    )
    export_data <- analysis_data[analysis_data$keep == TRUE &
                                   analysis_data$class_keep == TRUE, ] |>
      tidyr::pivot_wider(
        id_cols = c("my_id", "ShortLipidName",  "LongLipidName", "Class", "ion"),
        names_from = "sample_name",
        values_from = values_column
      )

    export[[norm]] <- rbind.data.frame(export_meta,
                                       export_data)
  }

  return(export)
}
