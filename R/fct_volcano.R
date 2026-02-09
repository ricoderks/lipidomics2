#' volcano
#'
#' @title Do a statistical test
#'
#' @description Do a statistical test.
#'
#' @param data data.frame with the data.
#' @param area_column character(1), which column contains the peak area information.
#' @param transformation character(1), what transformation to use.
#' @param test character(1) which test to do.
#' @param group character(1) name of the grouping column.
#' @param group1 character(1) what value of the group is x.
#' @param group2 character(1) what value of the group is y.
#'
#' @returns list with several data.frames with data.
#'
#' @author Rico Derks
#'
#' @noRd
do_test <- function(data = NULL,
                    area_column = c("area", "totNormArea", "pqnNormArea"),
                    transformation = c("none", "log10", "log1p"),
                    test = c("ttest", "mw"),
                    group = NULL,
                    group1 = NULL,
                    group2 = NULL) {
  area_column <- match.arg(arg = area_column,
                           choices = c("area", "totNormArea", "pqnNormArea"))
  transformation <- match.arg(arg = transformation,
                              choices = c("none", "log10", "log1p"),
                              several.ok = FALSE)
  test <- match.arg(arg = test,
                    choices = c("ttest", "mw"))

  feature_data <- unique(data[, c("my_id", "ShortLipidName", "LongLipidName", "Class")])

  res <- switch(
    test,
    "ttest" = do_test.ttest(data = data,
                            area_column = area_column,
                            transformation = transformation,
                            group = group,
                            group1 = group1,
                            group2 = group2),
    "mw" = do_test.mw(data = data,
                      area_column = area_column,
                      group = group,
                      group1 = group1,
                      group2 = group2)
  )

  res <- merge(
    x = res,
    y = feature_data,
    by = "my_id"
  )

  return(res)
}


#' @title Do a t-test
#'
#' @description Do a t-test.
#'
#' @param data data.frame with the data.
#' @param area_column character(1), which column contains the peak area information.
#' @param transformation character(1), what transformation to use.
#' @param group character(1) name of the grouping column.
#' @param group1 character(1) what value of the group is x.
#' @param group2 character(1) what value of the group is y.
#'
#' @returns list with several data.frames with data.
#'
#' @author Rico Derks
#'
#' @noRd
do_test.ttest <- function(data = NULL,
                          area_column = c("area", "totNormArea", "pqnNormArea"),
                          transformation = c("none", "log10", "log1p"),
                          group = NULL,
                          group1 = NULL,
                          group2 = NULL) {
  area_column <- match.arg(arg = area_column,
                           choices = c("area", "totNormArea", "pqnNormArea"))
  transformation <- match.arg(arg = transformation,
                              choices = c("none", "log10", "log1p"),
                              several.ok = FALSE)

  columns <- c("my_id", "sample_name")

  test_data <- data[, c(columns, group, area_column)]
  test_data[[area_column]] <- switch(
    transformation,
    "none" = test_data[[area_column]],
    "log10" = log10(test_data[[area_column]]),
    "log1p" = log1p(test_data[[area_column]])
  )

  res <- tapply(test_data, list(test_data$my_id), function(x) {
    res <- t.test(x = x[x[[group]] == group1, area_column],
                  y = x[x[[group]] == group2, area_column])
    data.frame("pvalue" = res$p.value,
               # y / x = group2 / group1
               "fold_change" = res$estimate[2] / res$estimate[1])
  })
  res <- do.call("rbind", res)
  res$my_id <- rownames(res)
  res$log10p <- -log10(res$pvalue)
  res$log2fc <- log2(res$fold_change)

  return(res)
}


#' @title Do a Mann-Whitney test
#'
#' @description Do a Mann-Whitney test.
#'
#' @param data data.frame with the data.
#' @param area_column character(1), which column contains the peak area information.
#' @param transformation character(1), what transformation to use.
#' @param group character(1) name of the grouping column.
#' @param group1 character(1) what value of the group is x.
#' @param group2 character(1) what value of the group is y.
#'
#' @returns list with several data.frames with data.
#'
#' @author Rico Derks
#'
#' @noRd
do_test.mw <- function(data = NULL,
                       area_column = c("area", "totNormArea", "pqnNormArea"),
                       transformation = c("none", "log10", "log1p"),
                       group = NULL,
                       group1 = NULL,
                       group2 = NULL) {
  area_column <- match.arg(arg = area_column,
                           choices = c("area", "totNormArea", "pqnNormArea"))
  transformation <- match.arg(arg = transformation,
                              choices = c("none", "log10", "log1p"),
                              several.ok = FALSE)

  columns <- c("my_id", "sample_name")

  test_data <- data[, c(columns, group, area_column)]
  test_data[[area_column]] <- switch(
    transformation,
    "none" = test_data[[area_column]],
    "log10" = log10(test_data[[area_column]]),
    "log1p" = log1p(test_data[[area_column]])
  )

  res <- tapply(test_data, list(test_data$my_id), function(x) {
    res <- wilcox.test(x = x[x[[group]] == group1, area_column],
                       y = x[x[[group]] == group2, area_column])
    data.frame(
      "pvalue" = res$p.value,
      # y / x = group2 / group1
      "fold_change" = mean(x[x[[group]] == group2, area_column], na.rm = TRUE) /
        mean(x[x[[group]] == group1, area_column], na.rm = TRUE)
    )
  })
  res <- do.call("rbind", res)
  res$my_id <- rownames(res)
  res$log10p <- -log10(res$pvalue)
  res$log2fc <- log2(res$fold_change)

  return(res)
}


#' @title Show volcano plot
#'
#' @description
#' Show volcano plot.
#'
#' @param data  data.frame().
#' @param fc_threshold numeric(1) threshold for the fold change threshold lines.
#' @param pvalue_threshold numeric(1) threshold for the p-value threshold line.
#'
#' @returns volcano plot as plotly object.
#'
#' @author Rico Derks
#'
#' @importFrom plotly plot_ly layout
#' @importFrom scales rescale
#'
#' @noRd
#'
show_volcano <- function(data = NULL,
                         fc_threshold = 2,
                         pvalue_threshold = 0.05) {
  # todo:
  # * min / max not working due to Inf/-Inf values
  # * highlight only significant values (opacity in marker list)
  # * color feature annotation
  # * name of feature in the popup
  # * violin/box plot on popup
  # print(min(data$log2fc))
  # print(max(data$log2fc))

  data$signif <- ifelse(data$pvalue <= pvalue_threshold &
                          (data$fold_change >= fc_threshold |
                             data$fold_change <= 1 / fc_threshold),
                        1,
                        0.5)

  ply <- plotly::plot_ly(
    data = data,
    x = ~log2fc,
    y = ~log10p,
    type = "scatter",
    mode = "markers",
    marker = list(
      size = 10,
      opacity = ~signif
    )
  ) |> plotly::layout(
    xaxis = list(title = "log2(fold change)"),
    yaxis = list(title = "-log10(p-value)"),
    shapes = list(
      list(
        type = "line",
        x0 = -log2(fc_threshold),
        x1 = -log2(fc_threshold),
        y0 = 0,
        y1 = 1.2 * max(c(data$log10p, -log10(pvalue_threshold))),
        line = list(color = "black", width = 1, dash = "dot")
      ),
      list(
        type = "line",
        x0 = log2(fc_threshold),
        x1 = log2(fc_threshold),
        y0 = 0,
        y1 = 1.2 * max(c(data$log10p, -log10(pvalue_threshold))),
        line = list(color = "black", width = 1, dash = "dot")
      ),
      list(
        type = "line",
        x0 = 1.2 * round(min(data$log2fc, na.rm = TRUE)),
        x1 = 1.2 * round(max(data$log2fc, na.rm = TRUE)),
        y0 = -log10(pvalue_threshold),
        y1 = -log10(pvalue_threshold),
        line = list(color = "black", width = 1, dash = "dot")
      )
    )
  )

  return(ply)
}
