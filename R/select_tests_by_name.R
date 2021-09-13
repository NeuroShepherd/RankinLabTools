
#' Title
#'
#' @param dataframe dataframe object
#' @param test_names character vector of all names of tests that one would like to select from the dataframe; assumes that the tests share a common string (e.g. "_DART" or "_BISBAS")
#' @param ... any additional columns to be selected from the dataframe
#'
#' @return
#' @export
#'
#' @examples
select_by_test_name <- function(dataframe, test_names, ...){
  test_names <- paste0(test_names,collapse="|")

  column_list <- dataframe %>%
    colnames() %>%
    grep(test_names,.,ignore.case = TRUE, value = TRUE)

  dataframe %>%
    dplyr::select(...,column_list)

}

