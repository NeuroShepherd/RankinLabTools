

#' Title
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param var
#' @param value
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
qualtrics_dart_item_score <- function(.data,var,value) {

  .data %>%
    select({{var}}) %>%
    mutate(
      "score_{{var}}" := if_else( {{var}} == {{value}}, 1, 0)
    ) %>%
    select(-{{var}})

}
