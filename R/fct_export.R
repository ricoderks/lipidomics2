#' export
#' @title Create the export data.frame
#'
#' @description Create the export data.frame. The lipids are sorted by lipid
#'     class and long lipid name.
#'
#' @param meta_data data.frame with meta data.
#' @param analysis_data data.frame with the analysis data to export.
#' @param filename_column column name of the filename column in the meta data.
#' @param normalization character(), which normalization to export additionally.
#' @param blanks character() with file names of the blanks.
#' @param qcpools character() with the file names of the pooled samples.
#' @param samples character() with the file names of the samples.
#'
#' @return list with data.frames with export data.
#'
#' @noRd
#'
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @author Rico Derks
#'
prepare_export_data <- function(meta_data = NULL,
                                analysis_data = NULL,
                                filename_column = NULL,
                                normalization = NULL,
                                blanks = NULL,
                                qcpools = NULL,
                                samples = NULL) {

  normalization <- names(unlist(normalization)[unlist(normalization)])
  normalization <- c("raw", normalization)

  # meta data
  export_meta <- meta_data[meta_data[, filename_column] %in% c(blanks, qcpools, samples), ]
  export_meta <- as.data.frame(lapply(export_meta, as.character))
  export_meta <- export_meta[sort(export_meta[, filename_column],
                                  index.return = TRUE)$ix, ]

  export_meta <- export_meta |>
    tidyr::pivot_longer(cols = -filename_column) |>
    tidyr::pivot_wider(names_from = filename_column,
                       values_from = "value")

  meta_cols <- colnames(export_meta)
  export_meta <- data.frame(export_meta[, meta_cols[1]],
                            "ShortLipidName" = NA,
                            "LongLipidName" = NA,
                            "Class" = NA,
                            "ion" = NA,
                            export_meta[, meta_cols[-1]],
                            check.names = FALSE)
  colnames(export_meta)[1] <- "my_id"

  # analysis data
  export <- vector(mode = "list", length = length(normalization))
  names(export) <- normalization
  for(norm in normalization) {
    values_column <- switch(
      norm,
      "raw" = "area",
      "totNorm" = "totNormArea",
      "pqnNorm" = "pqnNormArea",
      "protNorm" = "protNormArea",
      "cellNorm" = "cellNormArea"
    )
    export_data <- analysis_data[analysis_data$keep == TRUE &
                                   analysis_data$class_keep == TRUE, ] |>
      tidyr::pivot_wider(
        id_cols = c("my_id", "ShortLipidName",  "LongLipidName", "Class", "ion"),
        names_from = "sample_name",
        values_from = values_column
      )
    # sort the lipids by class and long lipid name
    export_data <- export_data[order(export_data$Class,
                                     export_data$LongLipidName), ]

    export[[norm]] <- rbind.data.frame(export_meta,
                                       export_data)
  }

  return(export)
}


#' @title Prepare the user written report chapters
#'
#' @description Prepare the user written chapters for the analysis report. The
#'     title is escaped so pandoc shows it as literal text and the text is
#'     converted from markdown to sanitized html. Chapters without title and
#'     text are removed.
#'
#' @param chapters list with chapters, each a list with a title and text.
#'
#' @return list with chapters, each a list with a title (escaped markdown) and
#'     html (character).
#'
#' @noRd
#'
#' @author Rico Derks
#'
prepare_report_chapters <- function(chapters = NULL) {
  res <- lapply(chapters, function(chapter) {
    title <- if(is.null(chapter$title)) "" else trimws(chapter$title)
    text <- if(is.null(chapter$text)) "" else chapter$text

    if(title == "" && trimws(text) == "") {
      return(NULL)
    }

    list(
      title = escape_markdown_text(title),
      html = sanitize_markdown_html(text)
    )
  })

  Filter(Negate(is.null), res)
}


#' @title Convert markdown to sanitized html
#'
#' @description Convert user written markdown to html. Raw html in the text is
#'     escaped (shown as plain text) and only http(s), mailto, in-page and
#'     relative links are allowed, so no javascript can end up in the report.
#'
#' @param text character(1), markdown text.
#'
#' @return character(1) with html.
#'
#' @noRd
#'
#' @importFrom commonmark markdown_html
#'
#' @author Rico Derks
#'
sanitize_markdown_html <- function(text = NULL) {
  if(is.null(text) || length(text) == 0) {
    return("")
  }
  text <- paste(text, collapse = "\n")

  # escape raw html, without '<' no html tag can be opened
  text <- gsub(pattern = "&", replacement = "&amp;", x = text, fixed = TRUE)
  text <- gsub(pattern = "<", replacement = "&lt;", x = text, fixed = TRUE)

  html <- commonmark::markdown_html(text = text,
                                    extensions = TRUE)

  # only allow safe urls in links and images
  matches <- gregexpr(pattern = '(href|src)="[^"]*"',
                      text = html,
                      perl = TRUE)
  regmatches(html, matches) <- lapply(regmatches(html, matches), function(attrs) {
    vapply(attrs, function(attr) {
      attr_name <- sub(pattern = "=.*$", replacement = "", x = attr)
      url <- sub(pattern = '^[a-z]+="(.*)"$', replacement = "\\1", x = attr)
      if(is_safe_url(url)) {
        attr
      } else {
        paste0(attr_name, '="#"')
      }
    }, character(1), USE.NAMES = FALSE)
  })

  html
}


#' @title Check if an url is safe
#'
#' @description Only http(s), mailto, in-page and relative urls (without a
#'     scheme) are considered safe.
#'
#' @param url character(1), the url.
#'
#' @return logical(1).
#'
#' @noRd
#'
#' @author Rico Derks
#'
is_safe_url <- function(url = NULL) {
  grepl(pattern = "^(https?://|mailto:|#)",
        x = url,
        ignore.case = TRUE) ||
    !grepl(pattern = ":",
           x = url,
           fixed = TRUE)
}


#' @title Escape markdown text
#'
#' @description Replace all ASCII punctuation characters by numeric html
#'     entities so pandoc shows the text as literal text (no html, attributes,
#'     math or formatting). Backslash escaping is not used, because pandoc
#'     interprets for example '\(' as the start of math. Newlines are replaced
#'     by a space.
#'
#' @param x character(1), text to escape.
#'
#' @return character(1), escaped text.
#'
#' @noRd
#'
#' @author Rico Derks
#'
escape_markdown_text <- function(x = NULL) {
  x <- gsub(pattern = "[\r\n]+", replacement = " ", x = x)
  chars <- strsplit(x = x, split = "")[[1]]
  punct <- grepl(pattern = "^[!-/:-@\\[-`{-~]$",
                 x = chars,
                 perl = TRUE)
  chars[punct] <- paste0("&#", vapply(chars[punct], utf8ToInt, integer(1)), ";")

  paste(chars, collapse = "")
}


#' @title Sanitize the report title
#'
#' @description Only keep letters, digits, spaces and a few safe punctuation
#'     characters, so the title is safe to use in the yaml header of the
#'     report.
#'
#' @param x character(1), the title.
#'
#' @return character(1), the sanitized title.
#'
#' @noRd
#'
#' @author Rico Derks
#'
sanitize_report_title <- function(x = NULL) {
  if(is.null(x) || length(x) == 0) {
    return("")
  }
  x <- gsub(pattern = "[^\\p{L}\\p{N} .,:;()+/'-]",
            replacement = "",
            x = x,
            perl = TRUE)
  trimws(x)
}
