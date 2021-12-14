

#' Title
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param two_point1
#' @param two_point2
#' @param one_point
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
qualtrics_sdr_item_score <- function(.data, two_point1, two_point2 = NA, one_point) {

  var <- .data %>%
    select({{two_point1}}) %>%
    names() %>%
    str_remove_all(., "_[:digit:].*") %>%
    paste0("sdr_score_",.)


  .data %>%
    mutate(
      {{var}} := case_when( {{two_point1}} == "On" | {{two_point2}} == "On" ~ 2,
                            {{one_point}} == "On" ~ 1,
                            TRUE ~ 0)
    )

}
