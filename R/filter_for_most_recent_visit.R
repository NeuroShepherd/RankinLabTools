

#' Filter for Most Recent Visit by ID
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param id identifying and/or grouping variable
#' @param date date variable. Should be of class date, but can be any numeric value.
#'
#' @return
#' @export
#'
#' @examples
#'
#'
filter_for_most_recent_visit <- function(.data, id, date) {

  if ( (!lubridate::is.Date(dplyr::pull(.data,{{date}}))) &&
       (is.numeric(dplyr::pull(.data,{{date}}))) ) {
    warning("The {date} variable is not of class date, but is of class numeric. The function will operate, but results may be unexpected.")
  }

  .data %>%
    dplyr::group_by( {{id}} ) %>%
    dplyr::filter( {{date}} == max({{date}}, na.rm=T) )

}
