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
#' @importFrom tidyr pivot_wider pivot_longer all_of
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

  lipids <- unique(data[, c("my_id", "ShortLipidName", "LongLipidName", "Class")])

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

  # calculate the mean centered value for the variable plot
  var_data <- as.data.frame(apply(pca_data, 2, scale, scale = FALSE))
  var_data$sample_name <- rownames(pca_data)
  var_data <- var_data |>
    tidyr::pivot_longer(
      cols = !tidyr::all_of("sample_name"),
      names_to = "my_id",
      values_to = "value"
    )
  var_data <- merge(
    x = var_data,
    y = lipids,
    by = "my_id",
    all.x = TRUE
  )

  # do the PCA
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
  loadings <- merge(
    x = loadings,
    y = unique(data[, c("my_id", "ShortLipidName", "LongLipidName", "Class")]),
    by = "my_id"
  )

  res <- list(
    "summary_fit" = summary_fit,
    "scores" = scores,
    "loadings" = loadings,
    "var_data" = var_data
  )

  return(res)
}


#' @title Show a plot of PCA analysis
#'
#' @description
#' Show a plot of the PCA analysis.
#'
#' @param data list with 3 data.frames summary of fit, scores and loadings.
#' @param name character(1), name of the plot. Used in source.
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
                     name = NULL,
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
                                  name = name,
                                  sample_annotation = sample_annotation,
                                  x = x,
                                  y = y)
  plys[["loadings"]] <- loadings_plot(data = data[["loadings"]],
                                      name = name,
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
#' @param name character(1), name of the plot. Used in source.
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
                        name = NULL,
                        x = "PC1",
                        y = "PC2",
                        sample_annotation = NULL) {
  if(sample_annotation == "none") {
    color_arg <- NULL
    #   customdata <- rep("", nrow(data))
    hovertemplate <- paste0(
      "<b>%{text}</b><br><br>",
      "%{xaxis.title.text}: %{x:.3}<br>",
      "%{yaxis.title.text}: %{y:.3}<br>",
      "<extra></extra>"
    )
  } else {
    color_arg <- stats::as.formula(paste0("~", sample_annotation))
    #   customdata <- stats::as.formula(paste0("~", sample_annotation))
    hovertemplate <- paste0(
      "<b>%{text}</b><br><br>",
      "%{xaxis.title.text}: %{x:.3}<br>",
      "%{yaxis.title.text}: %{y:.3}<br>",
      # "<b>%{customdata}</b><br>",
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
    source = paste0(name, "_scores"),
    x = ~.data[[x]],
    y = ~.data[[y]],
    color = color_arg,
    customdata = ~sample_name,
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
#' @param name character(1), name of the plot. Used in source.
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
                          name = NULL,
                          x = "PC1",
                          y = "PC2",
                          feature_annotation = NULL) {
  if(feature_annotation == "none") {
    color_arg <- NULL
  } else {
    color_arg <- stats::as.formula(paste0("~", feature_annotation))
  }

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

  ply <- plotly::plot_ly(
    data = data,
    source = paste0(name, "_loadings"),
    x = ~.data[[x]],
    y = ~.data[[y]],
    color = color_arg,
    colors = colors,
    customdata = ~my_id,
    text = ~paste0(
      "Class: ", Class, "<br>",
      "Lipid (short): ", ShortLipidName, "<br>",
      "Lipid: ", LongLipidName
    ),
    hovertemplate = paste0(
      "<b>%{text}</b><br><br>",
      "%{xaxis.title.text}: %{x:.3}<br>",
      "%{yaxis.title.text}: %{y:.3}<br>",
      "<extra></extra>"
    ),
    type = "scatter",
    mode = "markers",
    marker = list(
      size = 6
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
    customdata = ~variable,
    hovertemplate = paste0(
      "<b>PC%{x}</b><br>",
      "%{customdata}: %{y:.3f}",
      "<extra></extra>"
    ),
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


#' @title Variable plot
#'
#' @description
#' Variable plot.
#'
#' @param plot_data data.frame.
#' @param feature_annotation character(1), what to color in the plot.
#'
#' @details
#' Show the difference to the mean.
#'
#' @returns Variable plot as plotly object.
#'
#' @author Rico Derks
#'
#' @importFrom plotly plot_ly layout add_bars
#' @importFrom stats as.formula
#'
#' @noRd
#'
show_var_plot <- function(plot_data = NULL,
                          feature_annotation = NULL) {

  if(feature_annotation == "none") {
    color_arg <- NULL
  } else {
    color_arg <- stats::as.formula(paste0("~", feature_annotation))
  }

  colors <- c("#F81626", "#32E322", "#1C0DFC", "#EDB8C7", "#FF22EC", "#0DD7FD",
              "#D7C500", "#006535", "#E97D35", "#5D3287", "#CD0071", "#845C16",
              "#F99FFE", "#C400FF", "#00DDBE", "#759EFD", "#8A1C63", "#7390A3",
              "#99D576", "#F28293", "#B3BE9F", "#FE68CB", "#5F4549", "#BE1C16",
              "#AC00B6", "#C6BAFB", "#BB7DFD", "#FAB980", "#0069A1", "#FF0079",
              "#808722", "#1CDE86", "#930D2A", "#2E38B2", "#9F75A0", "#FBB900",
              "#E98AC3", "#974F51", "#79D4D8", "#008200", "#8DD700", "#76C495",
              "#FF16B8", "#555626", "#0D79FC", "#8000DC", "#D9C580", "#963B96",
              "#BD6D40", "#FC6280")
  # order the x-axis according to lipid class and then lipid
  plot_data$order_x <- paste(plot_data$Class, plot_data$ShortLipidName, sep = "_")
  plot_data <- plot_data[order(plot_data$order_x), ]

  # create list for the hovertemplate
  custom_list <- plot_data[, c("ShortLipidName", "LongLipidName", "Class")]
  custom_list <- split(custom_list, seq_len(nrow(custom_list)))

  ply <- plot_data |>
    plotly::plot_ly(
      x = ~ShortLipidName,
      y = ~value,
      color = color_arg,
      colors = colors,
      customdata = custom_list,
      hovertemplate = paste0(
        "Short: %{customdata.ShortLipidName}<br>",
        "Long: %{customdata.LongLipidName}<br>",
        "Lipid class: %{customdata.Class}<br>",
        "<extra></extra>"
      )
    ) |>
    plotly::add_bars(
      # order the x-axis according to lipid class and then lipid
      xaxis = list(
        type = "category",
        categoryorder = "array",
        categoryarray =  ~order_x
      )
    ) |>
    plotly::layout(
      yaxis = list(tickformat = ".2e"),
      xaxis = list(title = "Lipid")
    ) |>
    plotly::hide_legend()

  return(ply)
}


#' @title Observation plot
#'
#' @description
#' Observation plot.
#'
#' @param plot_data data.frame.
#' @param observation_annotation character(1), what to color in the plot.
#'
#' @details
#' Show the difference to the mean.
#'
#' @returns Observation plot as plotly object.
#'
#' @author Rico Derks
#'
#' @importFrom plotly plot_ly layout add_bars
#' @importFrom stats as.formula
#'
#' @noRd
#'
show_obs_plot <- function(plot_data = NULL,
                          observation_annotation = NULL) {
  if(observation_annotation == "none") {
    color_arg <- NULL
  } else {
    color_arg <- stats::as.formula(paste0("~", observation_annotation))
  }

  ply <- plot_data |>
    plotly::plot_ly(
      x = ~sample_name,
      y = ~value,
      color = color_arg
    ) |>
    plotly::add_bars() |>
    plotly::layout(
      yaxis = list(tickformat = ".2e"),
      xaxis = list(title = "Sample name")
    ) |>
    plotly::hide_legend()

  return(ply)
}
