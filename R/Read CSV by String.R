

#' Read CSV By String Search
#' This convenience function searches either the default directory or a specified directory for a (1) .csv file that at least partially matches the string_search variable. The file is then read-in to R.
#'
#' @param string_search string to search for in the directory
#' @param file_parent can specify the parent directory of the file if needed
#'
#' @return
#' @export
#'
#' @examples
read_csv_by_string_search <- function(string_search, file_parent) {
  if(rlang::is_missing(file_parent)){
    destination <- here::here()
  } else {
    destination <- here::here(file_parent)
  }


  pathway <- fs::dir_info(destination) %>%
    dplyr::filter_at(vars(path), dplyr::all_vars(stringr::str_detect(.,pattern=glue::glue("{string_search}")))) %>%
    dplyr::pull(path)
  if (length(pathway) < 1){print(glue::glue("Error! No object with partial name = '{string_search}' found in this directory. Specify a different folder within this project using the file_parent argument"))}
  if (length(pathway) > 1){print(glue::glue("Error!", length(pathway),"objects with partial name = '{string_search}' found in this directory. Check folder contents for duplicate files, and ensure your string_search argument is specific."))}

  return(readr::read_csv(paste0(pathway)))

}
