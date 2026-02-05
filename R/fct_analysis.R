#' analysis
#'
#' @title Total area normalization
#'
#' @description Total area normalization
#'
#' @param data data.frame in long format.
#'
#' @return The return value, if any, from executing the function.
#'
#' @author Rico Derks
#'
#' @noRd
total_area_norm <- function(data = NULL) {
  # First keep only the lipids selected
  data_filtered <- data[data$keep == TRUE & data$class_keep == TRUE, ]

  tot_sum_data <- tapply(
    X = data_filtered$area,
    INDEX = data_filtered$sample_name,
    FUN = sum,
    simplify = FALSE,
    na.rm = TRUE
  )
  tot_sum_data <- as.data.frame(do.call("rbind", tot_sum_data))
  colnames(tot_sum_data)[1] <- "totalArea"
  tot_sum_data$sample_name <- rownames(tot_sum_data)

  data <- merge(
    x = data,
    y = tot_sum_data,
    by = "sample_name"
  )

  data$totNormArea <- data$area / data$totalArea

  return(data)
}
