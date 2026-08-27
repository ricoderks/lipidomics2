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


#' @title Update the stored axes ranges of the bubble plot
#'
#' @description
#' Update the stored axes ranges (i.e. the zoom) of the bubble plot with the
#' information from a `plotly_relayout` event.
#'
#' @param current list(), with the axes ranges stored so far. Each element is
#'     named after the axis (e.g. `xaxis`, `yaxis2`) and contains a numeric(2)
#'     with the range of that axis.
#' @param relayout list(), the event data of a `plotly_relayout` event.
#'
#' @return list() with the updated axes ranges. Axes which are (back) on
#'     autorange are removed from the list.
#'
#' @details
#' A `plotly_relayout` event is also fired for things which have nothing to do
#' with zooming (e.g. resizing the window). In that case the ranges stored so
#' far are returned unchanged.
#'
#' @noRd
#'
#' @author Rico Derks
#'
update_zoom_ranges <- function(current = NULL,
                               relayout = NULL) {
  axes_names <- names(relayout)

  if(is.null(axes_names)) {
    return(current)
  }

  range_idx <- grepl(x = axes_names,
                     pattern = "^[xy]axis[0-9]*\\.range(\\[[01]\\])?$")
  auto_idx <- grepl(x = axes_names,
                    pattern = "^[xy]axis[0-9]*\\.autorange$")

  if(!any(range_idx) & !any(auto_idx)) {
    return(current)
  }

  if(is.null(current)) {
    current <- list()
  }

  for(axis_name in axes_names[range_idx]) {
    axis <- sub(x = axis_name,
                pattern = "\\.range(\\[[01]\\])?$",
                replacement = "")
    value <- unlist(relayout[[axis_name]])

    if(!is.numeric(value)) {
      next
    }

    if(grepl(x = axis_name,
             pattern = "\\.range$")) {
      # the complete range is send at once
      if(length(value) == 2) {
        current[[axis]] <- value
      }
    } else {
      # only one of the two limits is send
      limit <- as.numeric(sub(x = axis_name,
                              pattern = "^.*\\[([01])\\]$",
                              replacement = "\\1")) + 1

      if(is.null(current[[axis]])) {
        current[[axis]] <- c(NA_real_, NA_real_)
      }

      current[[axis]][limit] <- value
    }
  }

  # autoscale / double click means no zoom for that axis anymore
  for(axis_name in axes_names[auto_idx]) {
    if(isTRUE(relayout[[axis_name]])) {
      axis <- sub(x = axis_name,
                  pattern = "\\.autorange$",
                  replacement = "")

      current[[axis]] <- NULL
    }
  }

  # incomplete ranges can not be used
  current <- current[vapply(current, function(x) {
    length(x) == 2 && all(is.finite(x))
  }, FUN.VALUE = logical(1))]

  return(current)
}


#' @title Restore the axes ranges of the bubble plot
#'
#' @description
#' Restore the axes ranges (i.e. the zoom) of the bubble plot, so the zoom is
#' kept when the plot is redrawn.
#'
#' @param p plotly object.
#' @param ranges list(), with the axes ranges as created by
#'     `update_zoom_ranges()`.
#'
#' @return plotly object with the axes ranges set.
#'
#' @details
#' Plotly.js remembers the ranges of the first draw as the ranges to go back to
#' when clicking the 'Reset axes' button, which means it would go back to the
#' restored zoom instead of showing everything. That is why the plot is
#' configured with `doubleClick = "autosize"` and without the 'Reset axes'
#' button, see `mod_identification_server()`.
#'
#' @importFrom plotly layout
#'
#' @noRd
#'
#' @author Rico Derks
#'
apply_zoom_ranges <- function(p = NULL,
                              ranges = NULL) {
  if(is.null(p) | length(ranges) == 0) {
    return(p)
  }

  for(axis in names(ranges)) {
    axis_layout <- list(list(range = ranges[[axis]],
                             autorange = FALSE))
    names(axis_layout) <- axis

    p <- do.call(what = plotly::layout,
                 args = c(list(p = p),
                          axis_layout))
  }

  return(p)
}
