

#' Calculate difference between vectors of columns
#'
#' @description This function accepts any number of 2 sets of equal length
#' columns, and calculates the difference (that is, past-current = difference).
#' Intended to be used in conjunction with the combine_current_and_past_observations() function.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param final vector of final value columns from dataframe
#' @param initial vector of initial value columns from dataframe
#'
#' @return dataframe
#' @export
#'
#' @section Examples:
#'
#' The examples below highlight three different approaches to using this function.
#' 1. The difference between two columns
#' 1. The difference between a series of columns, using the `:` notation from `{dplyr}`
#' 1. The difference between columns using a vector of column names
#'
#' ```{r, message=F, rows.print=5, comment=""}
#'
#' data(mtcars)
#' mtcars <- tibble(mtcars) %>%
#'    select(mpg, cyl, disp, hp)
#' calculate_current_past_difference(mtcars, mpg, hp) %>%
#'    head(5)
#' calculate_current_past_difference(mtcars, mpg:cyl, disp:hp) %>%
#'    head(5)
#' calculate_current_past_difference(mtcars, c(mpg,cyl), c(disp,hp)) %>%
#'    head(5)
#'
#' ```
#'
calculate_current_past_difference <- function(.data, final, initial) {

  if( length(select(.data,{{final}})) != length(select(.data,{{initial}})) ) {
    stop("Final and initial arguments are not of the same length. Calculations must be of an equal number of variables.")
  }

  difference_names <- paste0(dplyr::select(.data,{{final}}) %>% names(),
                             "-",
                             dplyr::select(.data,{{initial}}) %>% names())

  .data %<>%
    purrr::map2(.x = dplyr::select(.,{{final}}),
                .y = dplyr::select(.,{{initial}}),
                .f = ~{.x - .y}) %>% # calculate difference between selected columns; FINAL - INITIAL = DIFFERENCE
    set_names(difference_names) %>%
    dplyr::bind_cols(.data, .)

  return(.data)
}

