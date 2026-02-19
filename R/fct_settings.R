#' settings
#'
#' @title Do trend correction
#'
#' @description
#' Do trend correction.
#'
#' @param data data.frame with the data.
#' @param method character(1) which method to use.
#' @param columns list() with column names.
#' @param index list() with the sample names.
#'
#' @details
#' When method is 'loess' make sure the samples are sorted in order of measurement!
#'
#' @returns Trend corrected data in long format again.
#'
#' @author Rico Derks
#'
#' @noRd
#'
do_trend_correction <- function(data = NULL,
                                method = c("loess"),
                                columns = NULL,
                                index = NULL) {

  data$areaOriginal <- data$area

  res <- switch(
    method,
    "loess" = do_loess(data = data,
                       columns = columns,
                       index = index)
  )

  return(res)
}


#' @title Do the loess correction
#'
#' @description
#' Do the loess correction.
#'
#' @param data data.frame() with the data in long format.
#' @param columns list() with column names.
#' @param index list() with the sample names.
#'
#' @details
#' When method is 'loess' make sure the samples are sorted in order of measurement!
#' The correction is done batch wise.
#'
#' @returns Trend corrected data
#'
#' @importFrom tidyr pivot_wider pivot_longer all_of
#'
#' @author Rico Derks
#'
#' @noRd
#'
do_loess <- function(data = NULL,
                     columns = NULL,
                     index = NULL) {
  inj_order_col <- columns$acqorder
  batch_col <- columns$batch
  samples <- c(index$selected_pools, index$selected_samples)
  keep_cols <- c("sample_name", inj_order_col, batch_col, "my_id", "area")
  id_cols <- c("sample_name", inj_order_col, batch_col)

  # make data wide
  data_wide <- data[data$sample_name %in% samples, keep_cols] |>
    tidyr::pivot_wider(
      id_cols = tidyr::all_of(id_cols),
      names_from = "my_id",
      values_from = "area"
    ) |>
    as.data.frame()

  # sort by measurement order
  data_wide <- data_wide[order(data_wide[, inj_order_col]), ]
  rownames(data_wide) <- data_wide$sample_name

  batches <- unique(data_wide[, batch_col])

  res <- data_wide
  for(batch in batches) {
    tmp <- data_wide[data_wide[, batch_col] == batch, -c(1:3)]
    sample_type <- ifelse(data_wide$sample_name %in% index$selected_pools, 1, 2)

    res[res[, batch_col] == batch, -c(1:3)] <- qc_rlsc(
      tab = tmp,
      colv = sample_type,
      or = 1:nrow(tmp),
      verbose = FALSE
    )
  }

  # add the trend corrected column to the original data.frame
  res <- res |>
    tidyr::pivot_longer(
      cols = !tidyr::all_of(id_cols),
      names_to = "my_id",
      values_to = "area"
    ) |>
    as.data.frame()

  data$area <- NULL
  data <- merge(
    x = data,
    y = res[, c("my_id", "sample_name", "area")],
    by = c("sample_name", "my_id"),
    all.x = TRUE
  )

  return(data)
}


#' @title Perform QC-RLSC for batch correction of chromatographic signal
#'
#' @description Perform QC-RLSC for batch correction of chromatographic signal.
#'
#' @param tab table N*K (row * column) with N samples and K variables.
#' @param colv integer(), vector N of numbers: 1 for QC samples and 2 for other samples.
#' @param or integer(), vector of measuring order (see details).
#' @param verbose print which variable has been corrected to monitor the process (default = FALSE).
#'
#' @return corrected table N*K
#'
#' @details Make sure that everything is sorted in measurement order!!!
#'
#' @importFrom stats loess approx
#'
#' @author E. Nevedomskaya
#' @author Rico Derks
#'
#' @references Dunn et al. Nature Protocols 6, 1060-1083 (2011)
#'
#' @noRd
#'
qc_rlsc <- function(tab = NULL,
                    colv = NULL,
                    or = NULL,
                    verbose = FALSE) {
  # create table of the same size as initial
  tab_corr <- tab

  # For each variable (columns) in the initial table
  for (i in 1:ncol(tab)) {
    # fit loess curve to the QCs
    ll <- stats::loess(tab[which(colv == 1), i] ~ or[which(colv == 1)])

    # approximate the curve for all the samples
    aa <- stats::approx(x = or[which(colv == 1)],
                        y = ll$fitted,
                        xout = or)

    # correct the variable according to the curve for all the samples
    tab_corr[, i] <- tab[, i] / aa$y

    # print which variable has been corrected in order to monitor the progress
    if(verbose == TRUE) {
      print(i)
    }

  }

  return(tab_corr)
}
