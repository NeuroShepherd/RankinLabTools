

#' Title
#'
#' @param dataframe
#' @param id
#' @param date
#'
#' @return
#' @export
#'
#' @examples
filter_for_first_visit <- function(dataframe, id, date) {

  dataframe %>%
    group_by( {{id}} ) %>%
    dplyr::filter( {{date}} == min({{date}}) )

}
