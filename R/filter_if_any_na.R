

#' Title
#'
#' @description
#' This function filters out rows where any variables specified in the `...` argument are NA.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... 	One or more unquoted expressions separated by commas. Variable names can be used as if they were positions in the data frame, so expressions like x:y can be used to select a range of variables.
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
filter_if_any_na <- function(.data, ...){

  .data %<>%
    dplyr::filter( dplyr::if_all(c(...), ~!is.na(.)) )

  return(.data)

}
