#' qc
#'
#' @title Create RSD histogram
#'
#' @description Create RSD histogram.
#'
#' @param data data.frame with the RSD data.
#' @param rsd_cutoff numeric(1), the RSD cut off value.
#'
#' @details data should contain the columns RSD and polarity.
#'
#' @return ggplot2 object, histogram of the RSD values.
#'
#' @importFrom ggplot2 ggplot aes .data geom_vline geom_histogram labs guides
#'     guide_legend theme_minimal theme
#'
#' @noRd
#'
#' @author Rico Derks
#'
show_overall_hist <- function(data = NULL,
                              rsd_cutoff = 0.3) {
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$rsd,
                                 fill = .data$polarity)) +
    ggplot2::geom_vline(xintercept = rsd_cutoff,
                        colour = "red",
                        linetype = 2) +
    ggplot2::geom_histogram(binwidth = 0.05,
                            alpha = 0.4) +
    ggplot2::labs(x = "Relative standard deviation") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Polarity",
                                                 override.aes = list(alpha = 1))) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  return(p)
}


#' @title Create RSD violin plot
#'
#' @description Create RSD violin plot per class.
#'
#' @param data data.frame with the RSD data.
#' @param rsd_cutoff numeric(1), the RSD cut off value.
#'
#' @details data should contain the columns RSD, class and polarity.
#'
#' @return ggplot2 object, violin plot of the RSD values per class.
#'
#' @importFrom ggplot2 ggplot aes .data geom_violin geom_hline labs guides
#'     guide_legend theme_minimal theme
#'
#' @noRd
#'
#' @author Rico Derks
#'
show_class_violin <- function(data = NULL,
                              rsd_cutoff = 0.3) {
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$class,
                                 y = .data$rsd)) +
    ggplot2::geom_violin(scale = "width") +
    ggplot2::geom_jitter(ggplot2::aes(colour = .data$polarity),
                         alpha = 0.7) +
    ggplot2::geom_hline(yintercept = rsd_cutoff,
                        colour = "red",
                        linetype = 2) +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "Polarity",
                                                   override.aes = list(alpha = 1,
                                                                       size = 3))) +
    ggplot2::labs(y = "Relative standard deviation",
                  x = "Lipid class",
                  title = "RSD per lipidclass") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 90,
                                                       hjust = 1))

  return(p)
}


#' @title Correlation heatmap
#'
#' @description Correlation heatmap of all samples and pooled samples.
#'
#' @param data data.frame with all the data.
#'
#' @return ggplot2 object, histogram of the RSD values
#'
#' @importFrom ggplot2 ggplot aes .data geom_tile scale_fill_gradient
#'     theme_minimal theme element_text element_blank guides guide_colourbar
#'
#' @noRd
#'
#' @author Rico Derks
#'
qc_cor_plot <- function(data = NULL) {
  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = .data$x,
                                 y = .data$y)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$cor),
                       color = "white",
                       lwd = 0.5,
                       linetype = 1) +
    ggplot2::scale_fill_gradient(limits = c(-1, 1),
                                 low = "blue",
                                 high = "red") +
    ggplot2::guides(fill = ggplot2::guide_colourbar(title = "Pearson corr.")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       hjust = 1),
                   axis.title = ggplot2::element_blank())


  return(p)
}


#' @title Calculate correlation between all samples and pooled samples
#'
#' @description
#' Calculate correlation between all samples and pooled samples.
#'
#' @param data data.frame with all the data.
#' @param idx_samples character(), with all the sample names.
#' @param idx_pools character(), with all the pooled sample names.
#'
#' @return data.frame in long format for `qc_cor_plot()`.
#'
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stats cor
#'
#' @noRd
#'
#' @author Rico Derks
#'
calc_cor <- function(data = NULL,
                     idx_samples = NULL,
                     idx_pools = NULL) {
  df_m <- data[data$sample_name %in% c(idx_pools, idx_samples), ] |>
    tidyr::pivot_wider(id_cols = "my_id",
                       names_from = "sample_name",
                       values_from = "area")

  df_m <- df_m[, -1]
  df_m[df_m == 0] <- 1
  cormat <- as.data.frame(stats::cor(log10(df_m)))
  cormat$x <- rownames(cormat)

  cormat_long <- cormat |>
    tidyr::pivot_longer(
      cols = colnames(cormat)[-ncol(cormat)],
      names_to = "y",
      values_to = "cor"
    )

  cormat_long$tooltip <- sprintf("%s</br>%s</br>Pearson corr.:%0.2f",
                                 cormat_long$x,
                                 cormat_long$y,
                                 cormat_long$cor)

  return(cormat_long)
}
