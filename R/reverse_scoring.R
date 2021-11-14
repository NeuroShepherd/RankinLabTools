
#' Reverse Scoring
#'
#' @description
#'
#' This function wraps `dplyr::mutate()` to reverse score columns given in the `...` argument. This procedure is frequently used for e.g. Likert scale questions whereby different questions within the same questionnaire have positive or negative valence.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... variables to have their score reversed
#' @param max_score maximum attainable score on the variables selected; needs to be the same maximum for all vars
#'
#' @return
#' @export
#'
#' @examples
reverse_scoring <- function(.data, ..., max_score) {

  .data %<>%
    dplyr::mutate( across(c(...), ~{max_score+1 - .x}) )

  return(.data)

}


