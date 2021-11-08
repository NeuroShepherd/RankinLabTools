


#' Remove Rows with Multiple NAs
#'
#' @description
#' This function filters out rows where all variables specified in the `...` argument are NA.
#'
#' @param ...
#' @param dataframe dataframe object
#'
#' @return
#' @export
#'
#' @examples
filter_if_all_na <- function(dataframe, ...){

  dataframe %<>%
    dplyr::filter( dplyr::if_any(c(...), ~!is.na(.)) )

  return(dataframe)

}


