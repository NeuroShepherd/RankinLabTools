

#' Read CSV By String Search
#'
#' @description This convenience function searches either the default directory or a specified directory for a (1) .csv file that at least partially matches the string_search variable. The file is then read-in to R.
#'
#' @param string_search string or regular expression to search for in the directory
#' @param directory string of the directory in which to search for CSV file; defaults to the current working directory using here::here()
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
read_csv_by_string_search <- function(string_search, directory = here::here()) {

  pathway <- fs::dir_info(directory) %>%
    dplyr::filter(dplyr::across(path, ~stringr::str_detect(.x,pattern=glue::glue("{string_search}")))) %>%
    dplyr::pull(path)

  if (length(pathway) < 1) {
    warning(glue::glue("Error! No object with partial name = '{string_search}' found in directory {directory}. Specify a different file location using the directory argument"))
  } else if (length(pathway) > 1){
      warning(glue::glue("Error!", length(pathway),"objects with partial name = '{string_search}' found in this directory. Check folder contents for duplicate files, and ensure your string_search argument is specific."))
    }

  return(readr::read_csv(paste0(pathway)))

}
