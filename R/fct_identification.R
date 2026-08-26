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
#' @return plotly object which restores the axes ranges after it is drawn.
#'
#' @details
#' The ranges are not set in the layout of the plot, but applied with
#' `Plotly.relayout()` after the plot is drawn. Plotly.js stores the ranges of
#' the very first draw as the ranges to go back to when double clicking or
#' clicking the 'Reset axes' button, but only for axes which are not on
#' autorange. So by drawing the plot on autorange first and zooming in
#' afterwards, double clicking / resetting the axes shows everything again
#' instead of going back to the restored zoom.
#'
#' @importFrom htmlwidgets onRender
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

  # relayout wants something like {"xaxis.range": [1, 2], "yaxis2.range": [3, 4]}
  relayout_json <- paste0("\"", names(ranges), ".range\": [",
                          sapply(ranges, function(x) {
                            paste(formatC(x = x,
                                          digits = 15,
                                          width = 1,
                                          format = "g"),
                                  collapse = ", ")
                          }),
                          "]",
                          collapse = ", ")

  js_code <- paste0("function(el) {\n",
                    "  var ranges = {", relayout_json, "};\n",
                    "  var restore = function(tries) {\n",
                    "    if(el._fullLayout) {\n",
                    "      Plotly.relayout(el, ranges);\n",
                    "    } else if(tries < 20) {\n",
                    "      setTimeout(function() { restore(tries + 1); }, 50);\n",
                    "    }\n",
                    "  };\n",
                    "  restore(0);\n",
                    "}")

  p <- htmlwidgets::onRender(x = p,
                             jsCode = js_code)

  return(p)
}
