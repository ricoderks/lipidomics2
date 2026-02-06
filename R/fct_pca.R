#' pca
#'
#' @title Do a PCA analysis
#'
#' @description Do a PCA analysis.
#'
#' @param data data.frame.
#' @param area_column character(1), which column contains the peak area information.
#'
#' @return pcaRes object
#'
#' @author Rico Derks
#'
#' @importFrom tidyr pivot_wider
#'
#' @noRd
do_pca <- function(data = NULL,
                   area_column = "area") {
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
}
