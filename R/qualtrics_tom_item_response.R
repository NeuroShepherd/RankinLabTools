

#' Title
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param root_variable
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
qualtrics_tom_item_response <- function(.data, root_variable) {

  var_names <- .data %>%
    select(matches({{root_variable}})) %>%
    names() %>%
    sort()

  result <- .data %>%
    summarize("response_{root_variable}" := case_when(.data[[ var_names[[1]] ]] %in% c("On","Like") ~ 1,
                                                      .data[[ var_names[[2]] ]] %in% c("On","Like") ~ 2,
                                                      .data[[ var_names[[3]] ]] %in% c("On","Like") ~ 3)
    )

  return(.data)

}
