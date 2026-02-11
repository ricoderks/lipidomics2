#' pca
#'
#' @title Do a PCA analysis
#'
#' @description Do a PCA analysis.
#'
#' @param data data.frame.
#' @param nPcs integer(1) maximum number of PC to calculate.
#' @param area_column character(1), which column contains the peak area information.
#' @param scaling character(1), what scaling to use.
#' @param transformation character(1), what transformation to use.
#' @param group_columns character(), column(s) with extra sample information.
#' @param feature_annotation character(1), column with extra feature information.
#'
#' @returns list with 3 data.frames: summary of fit, scores and loadings.
#'
#' @author Rico Derks
#'
#' @importFrom tidyr pivot_wider pivot_longer
#'
#' @noRd
do_pca <- function(data = NULL,
                   nPcs = 2,
                   area_column = "area",
                   scaling = c("none", "uv", "pareto"),
                   transformation = c("none", "log10", "log1p"),
                   group_columns = NULL,
                   feature_annotation = NULL) {
  scaling <- match.arg(arg = scaling,
                       choices = c("none", "uv", "pareto"),
                       several.ok = FALSE)
  transformation <- match.arg(arg = transformation,
                              choices = c("none", "log10", "log1p"),
                              several.ok = FALSE)


  if(group_columns == "none") {
    group_columns <- NULL
  }
  if(feature_annotation == "none") {
    feature_annotation <- NULL
  }

  columns <- c("my_id", "sample_name")

  data_wide <- data[, c(columns, area_column)] |>
    tidyr::pivot_wider(
      names_from = .data[["my_id"]],
      values_from = .data[[area_column]]
    ) |>
    as.data.frame()
  rownames(data_wide) <- data_wide$sample_name
  pca_data <- data_wide[, !(colnames(data_wide) %in% columns)]

  pca_data <- switch(
    transformation,
    "none" = pca_data,
    "log10" = log10(pca_data),
    "log1p" = log1p(pca_data)
  )

  m <- pcaMethods::pca(
    object = pca_data,
    nPcs = nPcs,
    center = TRUE,
    scale = scaling,
    cv = "q2"
  )

  summary_fit <- data.frame(
    "PC" = 1:m@nPcs,
    "R2cum" = m@R2cum,
    "Q2cum" = m@cvstat
  ) |>
    tidyr::pivot_longer(
      cols = c("R2cum", "Q2cum"),
      names_to = "variable",
      values_to = "value"
    )

  # scores data
  scores <- data.frame(
    "sample_name" = rownames(pca_data),
    m@scores
  )
  if(!is.null(group_columns)) {
    scores <- merge(
      x = scores,
      y = unique(data[, c("sample_name", group_columns)]),
      by = "sample_name"
    )
  }

  # loadings data
  loadings <- data.frame(
    "my_id" = colnames(pca_data),
    m@loadings
  )
  if(!is.null(feature_annotation)) {
    loadings <- merge(
      x = loadings,
      y = unique(data[, c("my_id", feature_annotation)]),
      by = "my_id"
    )
  }

  res <- list(
    "summary_fit" = summary_fit,
    "scores" = scores,
    "loadings" = loadings
  )

  return(res)
}


#' @title Show a plot of PCA analysis
#'
#' @description
#' Show a plot of the PCA analysis.
#'
#' @param data list with 3 data.frames summary of fit, scores and loadings.
#' @param x character(1), what to show on the x-axis.
#' @param y character(1), what to show on the y-axis.
#' @param sample_annotation character(1), what to color in the scores plot.
#' @param feature_annotation character(1), what to color in the loadings plot.
#'
#' @returns The selected plot as a plotly object.
#'
#' @author Rico Derks
#'
#' @noRd
#'
show_pca <- function(data = NULL,
                     x = "PC1",
                     y = "PC2",
                     sample_annotation = NULL,
                     feature_annotation = NULL) {

  plys <- list(
    "scores" = NULL,
    "loadings" = NULL,
    "summary_fit" = NULL
  )

  plys[["scores"]] <- scores_plot(data = data[["scores"]],
                                  sample_annotation = sample_annotation,
                                  x = x,
                                  y = y)
  plys[["loadings"]] <- loadings_plot(data = data[["loadings"]],
                                      feature_annotation = feature_annotation,
                                      x = x,
                                      y = y)
  plys[["summary_fit"]] <- summary_fit(data = data[["summary_fit"]])

  return(plys)
}


#' @title PCA scores plot
#'
#' @description
#' PCA scores plot.
#'
#' @param data data.frame.
#' @param x character(1), what to show on the x-axis.
#' @param y character(1), what to show on the y-axis.
#' @param sample_annotation character(1), what to color in the plot.
#'
#' @returns Scores plot as plotly object.
#'
#' @author Rico Derks
#'
#' @importFrom plotly plot_ly layout add_trace
#' @importFrom stats as.formula
#'
#' @noRd
#'
scores_plot <- function(data = NULL,
                        x = "PC1",
                        y = "PC2",
                        sample_annotation = NULL) {
  if(sample_annotation == "none") {
    color_arg <- NULL
    customdata <- rep("", nrow(data))
    hovertemplate <- paste0(
      "<b>%{text}</b><br><br>",
      "%{xaxis.title.text}: %{x:.3}<br>",
      "%{yaxis.title.text}: %{y:.3}<br>",
      "<extra></extra>"
    )
  } else {
    color_arg <- stats::as.formula(paste0("~", sample_annotation))
    customdata <- stats::as.formula(paste0("~", sample_annotation))
    hovertemplate <- paste0(
      "<b>%{text}</b><br><br>",
      "%{xaxis.title.text}: %{x:.3}<br>",
      "%{yaxis.title.text}: %{y:.3}<br>",
      "<b>%{customdata}</b><br>",
      "<extra></extra>"
    )
  }

  # get the data points of the ellipse
  t2 <- simple_ellipse(
    x = data[[x]],
    y = data[[y]]
  )

  ply <- plotly::plot_ly(
    data = data,
    x = ~.data[[x]],
    y = ~.data[[y]],
    color = color_arg,
    customdata = customdata,
    text = ~paste0(
      "Sample: ", sample_name
    ),
    hovertemplate = hovertemplate,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = 8
    )
  ) |>
    plotly::add_trace(
      data = t2,
      x = ~x,
      y = ~y,
      type = "scatter",
      mode = "lines",
      line = list(
        color = "lightgrey"
      ),
      text = NULL,
      inherit = FALSE,
      showlegend = FALSE
    ) |>
    plotly::layout(
      title = list(text = "Scores plot"),
      xaxis = list(title = x),
      yaxis = list(title = y)
    )

  return(ply)
}


#' @title PCA loadings plot
#'
#' @description
#' PCA loadings plot.
#'
#' @param data data.frame.
#' @param x character(1), what to show on the x-axis.
#' @param y character(1), what to show on the y-axis.
#' @param feature_annotation character(1), what to color in the plot.
#'
#' @returns Scores plot as plotly object.
#'
#' @author Rico Derks
#'
#' @importFrom plotly plot_ly layout
#' @importFrom stats as.formula
#'
#' @noRd
#'
loadings_plot <- function(data = NULL,
                          x = "PC1",
                          y = "PC2",
                          feature_annotation = NULL) {
  if(feature_annotation == "none") {
    color_arg <- NULL
  } else {
    color_arg <- stats::as.formula(paste0("~", feature_annotation))
  }

  ply <- plotly::plot_ly(
    data = data,
    x = ~.data[[x]],
    y = ~.data[[y]],
    color = color_arg,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = 4
    )
  ) |>
    plotly::layout(
      title = list(text = "Loadings plot"),
      xaxis = list(title = x),
      yaxis = list(title = y)
    )

  return(ply)
}


#' @title show summary of fit of a PCA model
#'
#' @description Create a plot which shows a summary of the fit your PCA model.
#'
#' @param data data frame with all data. See details for more information.
#'
#' @returns A summary of fit plot as plotly object.
#'
#' @details The data frame \code{data} should contain 3 columns. \code{PC},
#' \code{variable} and \code{value}.
#'
#' @importFrom plotly plot_ly layout
#'
#' @author Rico Derks
#'
#' @noRd
#'
summary_fit <- function(data = NULL) {
  ply <- plotly::plot_ly(
    data = data,
    x = ~PC,
    y = ~value,
    color = ~variable,
    type = "bar"
  ) |>
    plotly::layout(
      title = list(text = "Summary of fit")
    )

  return(ply)
}


#' @title Create a Hotelling T2 ellipse for a PCA score plot
#'
#' @description This function can be used to create a confidence (Hotelling T2) interval for a
#' PCA score plot.
#'
#' @param x x vector
#' @param y y vector
#' @param alpha confidence interval
#' @param len number of points to create the ellipse
#'
#' @returns A data frame is returned with the points to create the ellipse.
#'
#' @details This is a helper function which is used to create a confidence (Hotelling T2) interval for a
#' PCA score plot.
#'
#' @importFrom stats var qf
#'
#' @author Rico Derks
#'
#' @noRd
simple_ellipse <- function(x, y, alpha = 0.95, len = 200) {
  N <- length(x)
  mypi <- seq(0, 2 * pi, length = len)

  r1 <- sqrt(stats::var(x) * stats::qf(alpha, 2, N - 2) * (2 * (N^2 - 1) / (N * (N - 2))))
  r2 <- sqrt(stats::var(y) * stats::qf(alpha, 2, N - 2) * (2 * (N^2 - 1) / (N * (N - 2))))

  result <- data.frame(
    x = (r1 * cos(mypi) + mean(x)),
    y = (r2 * sin(mypi) + mean(y))
  )

  return(result)
}
