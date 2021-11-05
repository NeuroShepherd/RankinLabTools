


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
  # This function allows you to remove rows from a dataframe given ALL variables specified in the *columns*
  # argument are NA.

  # Should rename this function to drop_if_all_NA()
  # Should create another version of the function where a row is dropped if any variables in the list are
  # missing, and this would be drop_if_any_NA(). It's counterintuitive, but I think I would just need to
  # change the any_vars() call to all_vars() in the function to implement this change.
}


