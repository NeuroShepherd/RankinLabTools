

#' Title
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param id grouping ID variable
#' @param date date variable
#'
#' @return
#' @export
#'
#' @section Examples:
#'
#'
#' ```{r, message=F, rows.print=5}
#'
#' ```
#'
filter_for_first_visit <- function(.data, id, date) {

  .data %>%
    group_by( {{id}} ) %>%
    dplyr::filter( {{date}} == min({{date}}) )

}
