#' heatmap
#'
#' @title Heatmap
#'
#' @description Heatmap.
#'
#' @param data data.frame.
#' @param area_column character(1), which column contains the peak area information.
#' @param group_columns character(), vector with column names for sample annotation.
#' @param row_clustering logical(1), should the rows be clustered.
#' @param col_clustering logical(1), should the rows be clustered.
#'
#' @return Heatmap, plotly object.
#'
#' @author Rico Derks
#'
#' @importFrom heatmaply heatmaply
#' @importFrom tidyr pivot_wider all_of
#'
#' @noRd
show_heatmap <- function(data = NULL,
                         area_column = "area",
                         group_columns = NULL,
                         row_clustering = FALSE,
                         col_clustering = FALSE) {

  columns <- c("my_id", "sample_name", "LongLipidName",
               "ShortLipidName", "Class", group_columns)
  print(columns)
  print(str(columns))

  data_wide <- data[, c(columns, area_column)] |>
    tidyr::pivot_wider(
      # this doesn't work
      id_cols = tidyr::all_of(columns),
      names_from = .data[["sample_name"]],
      values_from = .data[[area_column]]
    ) |>
    as.data.frame()

  print(dim(data_wide))
  print(head(data_wide))

  rownames(data_wide) <- data_wide$my_id

  ply <- heatmaply::heatmaply(
    x = data_wide[, !(colnames(data_wide) %in% columns)],
    Rowv = row_clustering,
    Colv = col_clustering,
    # needs to be unique
    labRow = data_wide$ShortLipidName,
    scale = "row"
  )

  return(ply)
}
