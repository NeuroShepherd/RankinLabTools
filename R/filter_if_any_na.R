

#' Title
#'
#' @description
#' This function filters out rows where any variables specified in the `...` argument are NA.
#'
#' @param dataframe
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
filter_if_any_na <- function(dataframe, ...){

  dataframe %<>%
    dplyr::filter( dplyr::if_all(c(...), ~!is.na(.)) )

  return(dataframe)

}
