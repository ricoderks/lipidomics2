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


#' @title Get the regex pattern for a lipid class
#'
#' @description
#' Get the regex pattern for a lipid class.
#'
#' @param classes list(), with all class information.
#' @param class_name character(1), with the class name.
#'
#' @return character(1) with the pattern for these classes.
#'
#' @noRd
#'
#' @author Rico Derks
#'
get_class_pattern <- function(classes = NULL,
                              class_name = NULL) {
  res <- sapply(classes, function(x) {
    which(sapply(x, function(y) {
      class_name %in% y$name
    }))
  })

  res <- res[sapply(res, length) > 0]

  pattern <- classes[[names(res)]][[res[[1]]]]$pattern

  return(pattern)
}
