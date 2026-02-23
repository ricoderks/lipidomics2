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
  print("Do total area normalization.")
  if("totNormArea" %in% colnames(data)) {
    data$totNormArea <- NULL
    data$totalArea <- NULL
  }

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


#' @title PQN normalization
#'
#' @description PQN normalization
#'
#' @param data data.frame in long format.
#' @param QC character(), vector of sample names to specify which samples
#'     to use as reference.
#'
#' @details
#' PQN normalization needs to be done on total area normalized data.
#'
#' @return The return value, if any, from executing the function.
#'
#' @author Rico Derks
#'
#' @importFrom tidyr pivot_wider pivot_longer
#'
#' @noRd
pqn_norm <- function(data = NULL,
                     QC = NULL) {
  print("Do pqn normalization.")
  if("pqnNormArea" %in% colnames(data)) {
    data$pqnNormArea <- NULL
  }

  tot_data <- total_area_norm(data = data)

  tot_data_wide <- tot_data[, c("sample_name", "my_id", "totNormArea")] |>
    tidyr::pivot_wider(
      id_cols =  "sample_name",
      names_from = "my_id",
      values_from = "totNormArea"
    ) |>
    as.data.frame()

  tot_data_wide[, -1] <- pqn(X = tot_data_wide[, -1],
                             QC = which(tot_data_wide$sample_name %in% QC))

  tot_data_long <- tot_data_wide |>
    tidyr::pivot_longer(
      cols = !c("sample_name"),
      names_to = "my_id",
      values_to = "pqnNormArea"
    )

  data <- merge(
    x = tot_data,
    y = tot_data_long[, c("my_id", "sample_name", "pqnNormArea")],
    by = c("my_id", "sample_name"),
    all.x = TRUE
  )

  return(data)
}


#' @title Perform Probabilistic Quotient Normalization
#'
#' @description
#' Perform Probabilistic Quotient Normalization.
#'
#' @param X matrix or data.frame in wide format: samples x variables (rows x columns).
#' @param n character(1) normalization reference: "mean" for using the overall average of variables as reference
#' or "median" (default) for using the overall median of variables as reference.
#' @param QC integer(), vector of number(s) to specify samples which average to use as reference.
#'
#' @details First a total area normalization should be done before PQN is applied.
#'
#' @returns matrix with PQN normalized values
#'
#' @importFrom stats median
#'
#' @author E. Nevedomskaya
#' @author Rico Derks
#'
#' @references Dieterle, F., Ross, A., Schlotterbeck, G. & Senn, H. Probabilistic Quotient
#' Normalization as Robust Method to Account for Dilution of Complex Biological Mixtures.
#' Application in H1 NMR Metabonomics. Anal. Chem. 78, 4281-4290 (2006).
#'
#' @noRd
pqn <- function(X = NULL,
                n = c("median", "mean"),
                QC = NULL) {
  n <- match.arg(arg = n,
                 choices = c("median", "mean"),
                 several.ok = FALSE)

  X.norm <- matrix(nrow = nrow(X), ncol = ncol(X))
  colnames(X.norm) <- colnames(X)
  rownames(X.norm) <- rownames(X)

  if (!is.null(QC)) {
    # if QC vector exists, use this as reference spectrum
    if (length(QC) == 1) {
      # only 1 reference sample given
      mX <- as.numeric(X[QC, ])
    } else {
      if (n == "mean") {
        mX <- as.numeric(colMeans(X[QC, ], na.rm = TRUE))
      }
      if (n == "median") {
        mX <- as.numeric(apply(X[QC, ], 2, stats::median, na.rm = TRUE))
      }
    }
  } else {
    # otherwise use the mean or median of all samples as reference sample
    if (n == "mean") {
      mX <- as.numeric(colMeans(X, na.rm = TRUE))
    }
    if (n == "median") {
      mX <- as.numeric(apply(X, 2, stats::median, na.rm = TRUE))
    }
  }

  # do the actual normalisation
  for (a in 1:nrow(X)) {
    X.norm[a, ] <- as.numeric(X[a, ] / stats::median(as.numeric(X[a, ] / mX), na.rm = TRUE))
  }

  return(X.norm)
}


#' @title Protein normalization
#'
#' @description Protein normalization
#'
#' @param data data.frame in long format.
#' @param column character(1), column containing the protein amount/concentration.
#'
#' @return The return value, if any, from executing the function.
#'
#' @author Rico Derks
#'
#' @noRd
prot_norm <- function(data = NULL,
                     column = NULL) {
  print("Do protein normalization.")
  if("protNormArea" %in% colnames(data)) {
    data$protNormArea <- NULL
  }

  data$protNormArea <- data$area / data[[column]]

  return(data)
}
