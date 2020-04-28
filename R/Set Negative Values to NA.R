

#' Negative Values to NA
#' Allows for the user to define any numeric variables they would like to change to NA if the value of that variable is less than 0
#'
#' @param dataframe dataframe object
#' @param vars_to_clean any numeric variables where negative values should be set to NA
#'
#' @return
#' @export
#'
#' @examples
negative_values_to_na <- function(dataframe, vars_to_clean){

  vars_to_clean <- enquo(vars_to_clean)

  dataframe %<>%
    dplyr::mutate_at(dplyr::vars(!!vars_to_clean), ~dplyr::case_when(. >= 0 ~ .))
  return(dataframe)

}


