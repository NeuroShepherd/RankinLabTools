
#' Reverse Scoring
#' Allows for reverse-scoring of any specified columns
#'
#' @param dataframe dataframe object
#' @param reversed_columns variables to have their score reversed
#' @param extremum_max maximum attainable score on the variables selected; needs to be the same maximum for all vars
#'
#' @return
#' @export
#'
#' @examples
reverse_scoring <- function(dataframe, reversed_columns, extremum_max) {
  reversed_columns <- dplyr::enquo(reversed_columns)

  if (missing(extremum_min)) {
    dataframe %<>%
      dplyr::mutate_at(dplyr::vars(!!reversed_columns), ~(extremum_max+1 - .))
  }

  return(dataframe)
}


