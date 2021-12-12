
#' Reverse Scoring
#'
#' @description
#'
#' This function wraps `dplyr::mutate()` to reverse score columns given in the
#' `...` argument. This procedure is frequently used for e.g. Likert scale
#' questions whereby different questions within the same questionnaire have
#' positive or negative valence. Note that you can supply an arbitrary number
#' of variables to the `...` argument; this usage is recommended only if
#' all of the variables have the same maximum value (e.g. again, a common
#' situation for Likert questionnaires.)
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... variables to have their score reversed
#' @param max_score maximum attainable score on the variables selected; needs to be the same maximum for all vars
#'
#' @return
#' @export
#'
#' @section Examples:
#'
#' ```{r, message=F, rows.print=5}
#'
#' data <- tibble(a = c(3,4,5,3), b=c(1,2,1,1))
#'
#' data
#' # Say that column a is reverse scored with a max value of 5.
#' data %>%
#'    reverse_scoring(a, max_score = 5)
#'
#' ```
#'
reverse_scoring <- function(.data, ..., max_score) {

  .data %<>%
    dplyr::mutate( across(c(...), ~{max_score+1 - .x}) )

  return(.data)

}


