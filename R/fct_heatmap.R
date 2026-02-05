#' heatmap
#'
#' @title Heatmap
#'
#' @description Heatmap.
#'
#' @param data data.frame.
#' @param area_column character(1), which column contains the peak area information.
#'
#' @return Heatmap, plotly object.
#'
#' @author Rico Derks
#'
#' @importFrom heatmaply heatmaply
#' @importFrom tidyr pivot_wider
#'
#' @noRd
show_heatmap <- function(data = NULL,
                         area_column = "area") {

  data_wide <- data[, c("my_id", "sample_name", "LongLipidName",
                        "ShortLipidName", "Class", area_column)] |>
    tidyr::pivot_wider(
      id_cols = c(my_id:Class),
      names_from = .data[["sample_name"]],
      values_from = .data[[area_column]]
    ) |>
    as.data.frame()

  rownames(data_wide) <- data_wide$my_id

  ply <- heatmaply::heatmaply(
    x = data_wide[, -c(1:4)],
    Rowv = NA,
    Colv = NA,
    scale = "row"
  )

  return(ply)
}
