#' identification
#'
#' @title Get lipid class list for `selectInput`
#'
#' @description Get lipid class list for `selectInput`
#'
#' @param lipid_classes list(), with all information about the lipid classes.
#'
#' @return The return value, if any, from executing the function.
#'
#' @importFrom stats setNames
#'
#' @noRd
#'
#' @author Rico Derks
#'
lip_class_choices <- function(lipid_classes = NULL) {
  res <- vector(mode = "list",
                length = length(lipid_classes))
  names(res) <- names(lipid_classes)

  for(a in 1:length(lipid_classes)) {
    res[[a]] <- lapply(lipid_classes[[a]], function(x) {
      x$name
    })
    res[[a]] <- stats::setNames(res[[a]], NULL)
  }

  return(res)
}
