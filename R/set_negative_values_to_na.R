

#' Negative Values to NA
#'
#' @description
#' Allows for the user to define any numeric variables they would like to change to NA if the value of that variable is less than 0
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... any numeric variables where negative values should be set to NA
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
negative_values_to_na <- function(.data, ...){

  non_numeric_vars <- select(.data, ...) %>%
    map(~is.numeric(.x)) %>%
    keep(. == FALSE) %>% names %>%
    toString()

  if (length(non_numeric_vars) > 0) {
    stop(glue::glue("Cannot operate on non-numeric variables: {non_numeric_vars}"))
  }

  .data %<>%
    dplyr::mutate(dplyr::across(c(...), ~dplyr::case_when(. >= 0 ~ .,
                                                          TRUE ~ NA_real_)
                                )
                  )

  return(.data)

}

