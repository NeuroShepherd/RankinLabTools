

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
#'
#'
filter_for_most_recent_visit <- function(dataframe, id, date) {

  dataframe %>%
    group_by( {{id}} ) %>%
    dplyr::filter( {{date}} == max({{date}}) )

}
