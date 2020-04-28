


#' Remove Rows with Multipls NAs
#' This function allows you to remove rows if ALL specified columns are NA
#'
#' @param dataframe dataframe object
#' @param columns variables to check for missingness
#'
#' @return
#' @export
#'
#' @examples
remove_rows_with_mult_NAs <- function(dataframe, columns){
  columns <- enquo(columns)

  dataframe %<>%
    dplyr::filter_at(dplyr::vars(!!columns), dplyr::any_vars(!is.na(.)))

  return(dataframe)
  # This function allows you to remove rows from a dataframe given ALL variables specified in the *columns*
  # argument are NA.

  # Should rename this function to drop_if_all_NA()
  # Should create another version of the function where a row is dropped if any variables in the list are
  # missing, and this would be drop_if_any_NA(). It's counterintuitive, but I think I would just need to
  # change the any_vars() call to all_vars() in the function to implement this change.
}


