#' fa_analysis
#'
#' @title Calculate the fatty acid analysis
#'
#' @description
#' Calculate the fatty acid analysis.
#'
#' @param data data.frame with the actual data.
#' @param feature_data data.frame with all the feature data information.
#' @param area_column character(1) name of the column with the area.
#' @param group_column character(1) name of the grouping column.
#' @param selected_lipidclass character(1) selected lipid class.
#'
#' @returns A list with the fatty acid analysis data as data.frame and character(1)
#' with how many of the lipids of the selected lipid class contained fatty acid
#' tail information.
#'
#' @importFrom tidyr pivot_longer pivot_wider all_of any_of
#'
#' @author Rico Derks
#'
#' @noRd
#'
fa_analysis_calc <- function(data = NULL,
                             feature_data = NULL,
                             area_column = NULL,
                             group_column = NULL,
                             selected_lipidclass = NULL) {
  write.csv(x = feature_data,
            file = "./features_function.csv")
  id_cols <- c("sample_name", group_column)
  data <- data |>
    tidyr::pivot_wider(
      id_cols = tidyr::all_of(id_cols),
      values_from = tidyr::all_of(area_column),
      names_from = tidyr::all_of("my_id")
    ) |>
    as.data.frame()

  # get the species from the selected lipid classes
  if(selected_lipidclass == "all") {
    sel_feat_idx <- feature_data$my_id
  } else if(selected_lipidclass == "all_noTG") {
    sel_feat_idx <- feature_data$my_id[!(feature_data$Class %in% c("TG"))]
  } else {
    sel_feat_idx <- feature_data$my_id[feature_data$Class %in% selected_lipidclass]
  }
  sel_feature_data <- feature_data[feature_data$my_id %in% sel_feat_idx, ]

  ## Data
  # select the correct data
  sel_raw_data <- data[, sel_feat_idx, drop = FALSE]

  # get the unique chain lengths and unsaturation
  uniq_carbon <- sort(unique(c(sel_feature_data$carbon_1,
                               sel_feature_data$carbon_2,
                               sel_feature_data$carbon_3,
                               sel_feature_data$carbon_4)))
  uniq_carbon <- uniq_carbon[uniq_carbon != 0]
  uniq_unsat <- sort(unique(c(sel_feature_data$db_1,
                              sel_feature_data$db_2,
                              sel_feature_data$db_3,
                              sel_feature_data$db_4)))

  # Initialize results data.frame
  fa_chains <- expand.grid(uniq_unsat, uniq_carbon)
  fa_chains <- paste(fa_chains[, 2], fa_chains[, 1], sep = ":")
  res <- as.data.frame(matrix(ncol = length(fa_chains),
                              nrow = nrow(sel_raw_data)))
  colnames(res) <- fa_chains
  rownames(res) <- rownames(sel_raw_data)

  # do the calculations
  for(a in uniq_carbon) {
    for(b in uniq_unsat) {
      sel_fa_chain <- paste(a, b, sep = ":")
      sel_lipids <- sel_feature_data$my_id[(sel_feature_data$carbon_1 == a &
                                              sel_feature_data$db_1 == b) |
                                             (sel_feature_data$carbon_2 == a &
                                                sel_feature_data$db_2 == b) |
                                             (sel_feature_data$carbon_3 == a &
                                                sel_feature_data$db_3 == b) |
                                             (sel_feature_data$carbon_4 == a &
                                                sel_feature_data$db_4 == b)]
      sel_lipids_double <- sel_feature_data$my_id[(sel_feature_data$carbon_1 == a &
                                                     sel_feature_data$db_1 == b) &
                                                    (sel_feature_data$carbon_2 == a &
                                                       sel_feature_data$db_2 == b)]
      sel_lipids_triple <- sel_feature_data$my_id[(sel_feature_data$carbon_1 == a &
                                                     sel_feature_data$db_1 == b) &
                                                    (sel_feature_data$carbon_2 == a &
                                                       sel_feature_data$db_2 == b) &
                                                    (sel_feature_data$carbon_3 == a &
                                                       sel_feature_data$db_3 == b)]
      sel_lipids_quad <- sel_feature_data$my_id[(sel_feature_data$carbon_1 == a &
                                                   sel_feature_data$db_1 == b) &
                                                  (sel_feature_data$carbon_2 == a &
                                                     sel_feature_data$db_2 == b) &
                                                  (sel_feature_data$carbon_3 == a &
                                                     sel_feature_data$db_3 == b) &
                                                  (sel_feature_data$carbon_4 == a &
                                                     sel_feature_data$db_4 == b)]

      res[, sel_fa_chain] <-
        rowSums(sel_raw_data[, sel_lipids, drop = FALSE], na.rm = TRUE) +
        rowSums(sel_raw_data[, sel_lipids_double, drop = FALSE], na.rm = TRUE) +
        rowSums(sel_raw_data[, sel_lipids_triple, drop = FALSE], na.rm = TRUE) +
        rowSums(sel_raw_data[, sel_lipids_quad, drop = FALSE], na.rm = TRUE)
    }
  }

  # remove empty columns
  empty_idx <- apply(res, 2, function(x) {
    all(x == 0)
  })
  res <- res[, !empty_idx, drop = FALSE]

  res <- cbind(sample_name = data$sample_name,
               group = data[[group_column]],
               res)

  res <- res |>
    tidyr::pivot_longer(cols = -c("sample_name", "group"),
                        names_to = "fa_tails",
                        values_to = "value")

  plot_data <- tapply(as.data.frame(res), list(res$group, res$fa_tails), function(x) {
    avg <- mean(x[, "value"], na.rm = TRUE)
    stdev <- stats::sd(x[, "value"], na.rm = TRUE)

    return(list(avg = avg,
                stdev = stdev,
                fa_tails = x[1, "fa_tails"],
                group = x[1, "group"]))
  })

  plot_data <- do.call(rbind.data.frame, plot_data)
  plot_data$carbon <- as.numeric(gsub(x = plot_data$fa_tails,
                                      pattern = "^([0-9]{1,2}).*",
                                      replacement = "\\1"))
  plot_data$db <- as.numeric(gsub(x = plot_data$fa_tails,
                                  pattern = ".*([0-9]{1,2})$",
                                  replacement = "\\1"))
  plot_data$fa_tails <- factor(plot_data$fa_tails,
                               labels = plot_data$fa_tails[order(plot_data$carbon, plot_data$db)],
                               levels = plot_data$fa_tails[order(plot_data$carbon, plot_data$db)])

  features <- sprintf(
    "%s out of %s lipid species contain fatty acid tail information.",
    length(sel_feature_data$my_id[sel_feature_data$carbon_1 != 0]),
    length(sel_feature_data$my_id))

  return(
    list(
      plot_data = plot_data,
      features = features
    )
  )
}


#' @title Show the fatty acid analysis plot
#'
#' @description
#' Show the fatty acid analysis plot.
#'
#' @param data data.frame from fa_analysis_calc().
#' @param title character(1) title of the plot.
#' @param subtitle character(1) subtitle of the plot.
#' @param y_title character(1) title of the y-axis.
#'
#' @returns Fatty acid analysis plot as plotly object.
#'
#' @importFrom plotly plot_ly add_bars layout
#'
#' @author Rico Derks
#'
#' @noRd
#'
show_fa_plot <- function(data = NULL,
                         title = NULL,
                         subtitle = NULL,
                         y_title = NULL) {
  if(!is.null(title)) {
    plot_title <- switch(
      title,
      "all" = "Lipid class: All (incl. TG)",
      "all_noTG" = "Lipid class: All (excl. TG)",
      paste0("Lipid class: ", title)
    )
  } else {
    plot_title <- NULL
  }

  plot_title <- paste0(
    plot_title, "<br>",
    "<sup>", subtitle, "</sup>"
  )

  if(!is.null(y_title)) {
    y_title <- switch(
      y_title,
      "raw" = "Average value",
      "totNorm" = "Average totala area normalized value",
      "pqnNorm" = "Average PQN normalized value",
      "protNorm" = "Average protein normalized value"
    )
  } else {
    y_title = NULL
  }

  ply <- data |>
    plotly::plot_ly(
      x = ~fa_tails,
      y = ~avg,
      color = ~group
    ) |>
    plotly::add_bars(
      error_y = ~list(
        array = stdev,
        color = "#000000"
      )
    ) |>
    plotly::layout(
      title = list(
        text = plot_title,
        x = 0,
        y = 1.2,
        xref = "paper",
        yref = "paper"
        # this is not working
        # https://plotly.com/r/reference/layout/
        # subtitle = list(
        #   text = "This is the subtitle"
        # )
      ),
      xaxis = list(
        title = "Fatty acid tail"
      ),
      yaxis = list(
        title = y_title,
        tickformat = ".2e"
      )
    )

  return(ply)
}
