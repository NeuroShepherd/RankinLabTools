

#' Title
#'
#' @param df
#' @param variable
#'
#' @return
#' @export
#'
#' @examples
qualtrics_sdr_item_response <- function(df, variable) {

  var_names <- df %>%
    select(matches({{root_variable}})) %>%
    names() %>%
    sort()

  result <- df %>%
    mutate("response_{variable}" := case_when(.data[[ var_names[[1]] ]] %in% c("On","Like") ~ 1,
                                              .data[[ var_names[[2]] ]] %in% c("On","Like") ~ 2,
                                              .data[[ var_names[[3]] ]] %in% c("On","Like") ~ 3,
                                              .data[[ var_names[[4]] ]] %in% c("On","Like") ~ 4,
                                              .data[[ var_names[[5]] ]] %in% c("On","Like") ~ 5)
    )

  return(result)

}
