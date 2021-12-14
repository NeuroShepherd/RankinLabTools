

#' Title
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param var
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
qualtrics_hot_spot_score <- function(.data, var) {

  .data %>%
    select({{var}}) %>%
    mutate(
      "score_{{var}}" := if_else( {{var}} == "On", 1, 0)
    ) %>%
    select(-{{var}})

}
