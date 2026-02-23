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
#' @param height numeric(1), height of the plotly plot in pixels.
#'
#' @return Heatmap, plotly object.
#'
#' @author Rico Derks
#'
#' @importFrom heatmaply heatmaply RdBu
#' @importFrom plotly plotly_build
#' @importFrom tidyr pivot_wider
#'
#' @noRd
show_heatmap <- function(data = NULL,
                         area_column = "area",
                         column_annotation = NULL,
                         row_annotation = NULL,
                         row_clustering = FALSE,
                         col_clustering = FALSE,
                         height = NULL) {

  columns <- c("my_id", "sample_name", "LongLipidName",
               "ShortLipidName", "Class")

  colors <- c("#F81626", "#32E322", "#1C0DFC", "#EDB8C7", "#FF22EC", "#0DD7FD",
              "#D7C500", "#006535", "#E97D35", "#5D3287", "#CD0071", "#845C16",
              "#F99FFE", "#C400FF", "#00DDBE", "#759EFD", "#8A1C63", "#7390A3",
              "#99D576", "#F28293", "#B3BE9F", "#FE68CB", "#5F4549", "#BE1C16",
              "#AC00B6", "#C6BAFB", "#BB7DFD", "#FAB980", "#0069A1", "#FF0079",
              "#808722", "#1CDE86", "#930D2A", "#2E38B2", "#9F75A0", "#FBB900",
              "#E98AC3", "#974F51", "#79D4D8", "#008200", "#8DD700", "#76C495",
              "#FF16B8", "#555626", "#0D79FC", "#8000DC", "#D9C580", "#963B96",
              "#BD6D40", "#FC6280")

  classes <- sort(unique(data$Class))
  colors <- colors[1:length(classes)]
  names(colors) <- classes

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
    colors = rev(heatmaply::RdBu(n = 100)),
    Rowv = row_clustering,
    Colv = col_clustering,
    plot_method = "ggplot",
    key.title = "z-score",
    # needs to be unique
    labRow = data_wide$ShortLipidName,
    col_side_colors = sample_annotation,
    row_side_colors = feature_annotation,
    row_side_palette = colors,
    scale = "row",
    height = height
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
