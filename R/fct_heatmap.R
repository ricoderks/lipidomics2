#' heatmap
#'
#' @title Heatmap
#'
#' @description Heatmap.
#'
#' @param data data.frame.
#' @param area_column character(1), which column contains the peak area information.
#' @param column_annotation character(), vector with column names for sample annotation.
#' @param row_annotation character(), vector with column names for feature annotation.
#' @param row_clustering logical(1), should the rows be clustered.
#' @param col_clustering logical(1), should the rows be clustered.
#'
#' @return Heatmap, plotly object.
#'
#' @author Rico Derks
#'
#' @importFrom heatmaply heatmaply
#' @importFrom plotly plotly_build
#' @importFrom tidyr pivot_wider all_of
#'
#' @noRd
show_heatmap <- function(data = NULL,
                         area_column = "area",
                         column_annotation = NULL,
                         row_annotation = NULL,
                         row_clustering = FALSE,
                         col_clustering = FALSE) {

  columns <- c("my_id", "sample_name", "LongLipidName",
               "ShortLipidName", "Class")

  data <- data[order(data$Class, data$LongLipidName), ]

  data_wide <- data[, c(columns, area_column)] |>
    tidyr::pivot_wider(
      names_from = .data[["sample_name"]],
      values_from = .data[[area_column]]
    ) |>
    as.data.frame()
  rownames(data_wide) <- data_wide$my_id

  if(!is.null(column_annotation)) {
    sample_annotation <- unique(data[, c("sample_name", column_annotation)])
    rownames(sample_annotation) <- sample_annotation$sample_name
    sample_annotation$sample_name <- NULL
  } else {
    sample_annotation <- NULL
  }

  if(!is.null(row_annotation)) {
    feature_annotation <- unique(data[, c("my_id", row_annotation)])
    rownames(feature_annotation) <- feature_annotation$my_id
    feature_annotation$my_id <- NULL
  } else {
    feature_annotation <- NULL
  }

  ply <- heatmaply::heatmaply(
    x = data_wide[, !(colnames(data_wide) %in% columns)],
    Rowv = row_clustering,
    Colv = col_clustering,
    plot_method = "ggplot",
    key.title = "z-score",
    # needs to be unique
    labRow = data_wide$ShortLipidName,
    col_side_colors = sample_annotation,
    row_side_colors = feature_annotation,
    scale = "row"
  )

  if(!is.null(feature_annotation)) {
    # remove feature annotation legends
    ply <- plotly::plotly_build(p = ply)

    for (i in seq_along(ply$x$data)) {
      tr <- ply$x$data[[i]]
      if(!is.null(tr$text)) {
        tr$showlegend <- FALSE
      }
      ply$x$data[[i]] <- tr
    }
  }

  return(ply)
}
