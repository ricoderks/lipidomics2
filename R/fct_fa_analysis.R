#' fa_analysis
#'
#' @title Calculate the fatty acid analysis
#'
#' @description
#' Calculate the fatty acid analysis.
#'
#' @param data data.frame with the actual data.
#' @param feature_data data.frame with all the feature data information.
#' @param selected_lipidclass character(1) selected lipid class.
#'
#' @returns The fatty acid analysis data as data.frame
#'
#' @importFrom tidyr pivot_longer
#'
#' @author Rico Derks
#'
#' @noRd
#'
fa_analysis_calc <- function(data = NULL,
                             feature_data = NULL,
                             selected_lipidclass = NULL) {

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
  sel_raw_data <- raw_data[, sel_feat_idx, drop = FALSE]

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

  res <- cbind(sampleName = sat_data$sampleName,
               Group = sat_data$Group,
               res)

  res <- res |>
    tidyr::pivot_longer(cols = -c("sampleName", "Group"),
                        names_to = "fa_tails",
                        values_to = "value")

  plot_data <- tapply(as.data.frame(res), list(res$Group, res$fa_tails), function(x) {
    avg <- mean(x[, "value"], na.rm = TRUE)
    stdev <- stats::sd(x[, "value"], na.rm = TRUE)

    return(list(avg = avg,
                stdev = stdev,
                fa_tails = x[1, "fa_tails"],
                group = x[1, "Group"]))
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

  return(plot_data)
}
