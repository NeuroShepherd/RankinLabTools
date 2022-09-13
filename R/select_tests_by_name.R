
#' Title
#'
#' @description
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param test_names character vector of all names of tests that one would like to select from .data; assumes that the tests share a common string (e.g. "_DART" or "_BISBAS")
#' @param ... any additional columns to be selected from .data
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
select_by_test_name <- function(.data, test_names, ...){

  test_names <- test_names %>%
    stringr::str_trim() %>%
    paste0(.,collapse="|")

  .data %<>%
    dplyr::select(...,dplyr::matches(test_names))

  return(.data)

}

