
#' Match Regular Expressions with a Nicer 'API'
#'
#' A small wrapper on 'regexpr' to extract the matches and captured
#' groups from the match of a regular expression to a character vector.
#' See \code{\link{re_match}}.
#'
#' @docType package
#' @name rematch
NULL

#' Match a regular expression to a character vector
#'
#' This function is a small wrapper on the \code{\link[base]{regexpr}}
#' base R function, to provide an API that is easier to use.
#'
#' Currently only the first occurence of the pattern is used.
#'
#' @param pattern Regular expression, defaults to be a PCRE
#'   expression. See \code{\link[base]{regex}} for more about
#'   regular expressions.
#' @param text Character vector.
#' @param ... Additional arguments to pass to
#'   \code{\link[base]{regexpr}}.
#' @return A character matrix of the matched (sub)strings.
#'   The first column is always the full match. This column is
#'   named \code{.match}. The result of the columns are capture groups,
#'   with appropriate column names, if the groups are named.
#'
#' @export
#' @examples
#' dates <- c("2016-04-20", "1977-08-08", "not a date", "2016",
#'   "76-03-02", "2012-06-30", "2015-01-21 19:58")
#' isodate <- "([0-9]{4})-([0-1][0-9])-([0-3][0-9])"
#' re_match(text = dates, pattern = isodate)
#'
#' # The same with named groups
#' isodaten <- "(?<year>[0-9]{4})-(?<month>[0-1][0-9])-(?<day>[0-3][0-9])"
#' re_match(text = dates, pattern = isodaten)

re_match <- function(pattern, text, ...) {

  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = TRUE, ...)

  ## Full matches
  res <- cbind(as.character(
    ifelse(
      match == -1,
      NA_character_,
      substr(text, match, match + attr(match, "match.length") - 1)
    )
  ))

  if (!is.null(attr(match, "capture.start"))) {

    res <- cbind(
      res,
      rbind(vapply(
        seq_len(NCOL(attr(match, "capture.start"))),
        function(i) {
          start <- attr(match, "capture.start")[,i]
          len <- attr(match, "capture.length")[,i]
          end <- start + len - 1
          res <- substr(text, start, end)
          res[ start == -1 ] <- NA_character_
          res
        },
        character(length(match))
      ))
    )
  }

  colnames(res) <- c(".match", auto_name(match))

  tibble::as_data_frame(res)
}

#' Extract all matches of a regular expression
#'
#' This function is a thin wrapper on the \code{\link[base]{gregexpr}}
#' base R function, to provide an API that is easier to use. It is
#' similar to \code{\link{re_match}}, but extracts all matches, including
#' potentially named capture groups.
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[base]{regexpr}}.
#' @inheritParams re_match
#' @return A list of character matrices. Each list element contains the
#'   matches of one string in the input character vector. Each matrix
#'   has a \code{.match} column that contains the matching part of the
#'   string. Additional columns are added for capture groups. For named
#'   capture groups, the columns are named.
#'
#' @export

re_match_all <- function(pattern, text, ...) {

  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern), length(text) > 0)
  text <- as.character(text)

  match <- gregexpr(pattern, text, perl = TRUE, ...)

  for (i in seq_along(match)) {
    match_len <- attr(match[[i]], "match.length")
    capt_start <- attr(match[[i]], "capture.start")
    capt_len <- attr(match[[i]], "capture.length")
    capt_nms <- attr(match[[i]], "capture.names")

    # Setup columns if the first match
    if (i == 1) {
      if (is.null(capt_start)) {
        res <- list(list())
      } else {
        res <- rep(list(list()), length = ncol(capt_start) + 1L)
      }
      names(res) <- c(".match", auto_name(match[[i]]))
    }

    # Add match results
    m <- as.vector(match[[i]])
    if (m[[1L]] == -1) {
      res[[1L]][[i]] <- NA_character_
    } else {
      res[[1L]][[i]] <- substring(text[[i]], m, m + match_len - 1)
    }

    # Add captures
    if (!is.null(capt_start)) {
      capt_str <- substring(text[[i]], capt_start, capt_start + capt_len - 1)
      for (j in seq_len(NCOL(capt_start))) {
        res[[j + 1L]][[i]] <- capt_str[seq(j, length(capt_str), NCOL(capt_start))]
      }
    }
  }

  tibble::as_data_frame(res)
}

auto_name <- function(x) {
  nms <- attr(x, "capture.names")
  if (is.null(nms)) {
    paste0("V", length(match))
  } else {
    empty <- nzchar(nms)
    nms[!empty] <- paste0("V", which(!empty))
  }
  nms
}
