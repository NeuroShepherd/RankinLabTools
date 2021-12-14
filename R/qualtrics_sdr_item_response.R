

#' Title
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param variable
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
qualtrics_sdr_item_response <- function(.data, variable) {

  var_names <- .data %>%
    select(matches({{root_variable}})) %>%
    names() %>%
    sort()

  result <- .data %>%
    mutate("response_{variable}" := case_when(.data[[ var_names[[1]] ]] %in% c("On","Like") ~ 1,
                                              .data[[ var_names[[2]] ]] %in% c("On","Like") ~ 2,
                                              .data[[ var_names[[3]] ]] %in% c("On","Like") ~ 3,
                                              .data[[ var_names[[4]] ]] %in% c("On","Like") ~ 4,
                                              .data[[ var_names[[5]] ]] %in% c("On","Like") ~ 5)
    )

  return(result)

}
