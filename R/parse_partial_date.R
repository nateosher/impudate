#' Parse a partially missing date string
#'
#' Internal helper that extracts year, month, and day from strings like
#' "YYYY", "YYYY-MM", or "YYYY-MM-DD". Non-matching values return NA.
#' @param x A vector to parse.
#' @param invalid How to handle invalid month/day values. One of "allow",
#'   "warn", or "error".
#' @return A list with elements year, month, day (integer vectors).
#' @keywords internal
parse_partial_date <- function(x, invalid = c("allow", "warn", "error")) {
  invalid <- match.arg(invalid)
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(list(
      year = as.integer(format(x, "%Y")),
      month = as.integer(format(x, "%m")),
      day = as.integer(format(x, "%d"))
    ))
  }

  x_chr <- as.character(x)
  x_chr <- stringr::str_trim(x_chr)

  # Accept: YYYY, YYYY-MM, YYYY/MM, YYYY-MM-DD, YYYY/MM/DD
  pattern <- "^(\\d{4})(?:[-/](\\d{1,2}))?(?:[-/](\\d{1,2}))?$"
  matches <- stringr::str_match(x_chr, pattern)

  year <- suppressWarnings(as.integer(matches[, 2]))
  month <- suppressWarnings(as.integer(matches[, 3]))
  day <- suppressWarnings(as.integer(matches[, 4]))

  invalid_month <- !is.na(month) & (month < 1L | month > 12L)
  invalid_day <- !is.na(day) & (day < 1L | day > 31L)
  has_invalid <- any(invalid_month | invalid_day)

  if (has_invalid) {
    msg <- "Invalid month/day values found while parsing partial dates."
    if (invalid == "warn") {
      warning(msg, call. = FALSE)
    } else if (invalid == "error") {
      stop(msg, call. = FALSE)
    }
  }

  if (invalid != "allow") {
    month <- dplyr::if_else(invalid_month, NA_integer_, month)
    day <- dplyr::if_else(invalid_day, NA_integer_, day)
  }

  list(year = year, month = month, day = day)
}
