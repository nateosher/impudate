#' Add year, month, and day columns for partially missing dates
#'
#' @param data A data frame or tibble.
#' @param col Column containing a partial date (e.g., "YYYY", "YYYY-MM", or
#'   "YYYY-MM-DD"). Accepts bare or quoted column names.
#' @param prefix Optional prefix for new columns. Defaults to the column name.
#' @param invalid How to handle invalid month/day values. One of "allow",
#'   "warn", or "error".
#' @return The input data with three additional columns.
#' @examples
#' library(dplyr)
#' tibble(date = c("2024", "2024-07", "2024-07-31", NA)) |>
#'   add_date_parts(date)
#' @export
add_date_parts <- function(data, col, prefix = NULL, invalid = c("allow", "warn", "error")) {
  col_sym <- rlang::ensym(col)
  col_name <- rlang::as_string(col_sym)
  if (is.null(prefix) || !nzchar(prefix)) {
    prefix <- col_name
  }

  parsed <- parse_partial_date(data[[col_name]], invalid = invalid)

  dplyr::mutate(
    data,
    '{prefix}_year' := parsed$year,
    '{prefix}_month' := parsed$month,
    '{prefix}_day' := parsed$day
  )
}
