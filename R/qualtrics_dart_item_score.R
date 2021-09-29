

qualtrics_dart_item_score <- function(data,var,value) {

  data %>%
    select({{var}}) %>%
    mutate(
      "score_{{var}}" := if_else( {{var}} == {{value}}, 1, 0)
    ) %>%
    select(-{{var}})

}
