

#' Add Test Name to Variables
#'
#' @description Adds a name to all variables in the dataframe; can specify if the name appears at the beginning or end of the variables
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param test_name name to be added to all variables; must be quoted (e.g. "name")
#' @param location where to append the test_name; 0 = end of variables, 1 = beginning of variables; defaults to 0
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
add_test_name_to_vars <- function(.data, test_name, location = 0){

  # Create a vector of columns that *do not* already contain the test name
  rename_me <- .data %>%
    colnames() %>%
    stringr::str_subset(test_name, negate=TRUE)

  if (location == 1){
    .data %<>%
      dplyr::rename_at(dplyr::vars(all_of(rename_me)), ~paste(test_name,.,sep="_"))
  }
  # Use the rename_me vector to rename columns only where needed

  if (location == 0){
    .data %<>%
      dplyr::rename_at(dplyr::vars(all_of(rename_me)), ~paste(sep="_",.,test_name))
  }

  return(.data)

}

