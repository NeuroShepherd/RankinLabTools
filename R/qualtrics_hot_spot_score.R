

#' Title
#'
#' @param data
#' @param var
#'
#' @return
#' @export
#'
#' @examples
qualtrics_hot_spot_score <- function(data,var) {

  data %>%
    select({{var}}) %>%
    mutate(
      "score_{{var}}" := if_else( {{var}} == "On", 1, 0)
    ) %>%
    select(-{{var}})

}
