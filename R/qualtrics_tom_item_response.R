

#' Title
#'
#' @param df
#' @param root_variable
#'
#' @return
#' @export
#'
#' @examples
qualtrics_tom_item_response <- function(df, root_variable) {

  var_names <- df %>%
    select(matches({{root_variable}})) %>%
    names() %>%
    sort()

  result <- df %>%
    summarize("response_{root_variable}" := case_when(.data[[ var_names[[1]] ]] %in% c("On","Like") ~ 1,
                                                      .data[[ var_names[[2]] ]] %in% c("On","Like") ~ 2,
                                                      .data[[ var_names[[3]] ]] %in% c("On","Like") ~ 3)
    )

  return(result)

}
