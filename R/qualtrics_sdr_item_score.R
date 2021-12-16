

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
qualtrics_sdr_item_score <- function(.data, .metadata, two_point1, two_point2 = NA, one_point) {

  # https://stackoverflow.com/questions/57136322/what-does-the-operator-mean-in-r-particularly-in-the-context-symx

  two_point1_sym <- pull(.metadata, {{two_point1}}) %>%
    map( ~{ if (is.na(.x)) { NA } else { sym(.x) } } )
  two_point2_sym <- pull(.metadata, {{two_point2}}) %>%
    map( ~{ if (is.na(.x)) { NA } else { sym(.x) } } )
  one_point_sym <- pull(.metadata, {{one_point}}) %>%
    map( ~{ if (is.na(.x)) { NA } else { sym(.x) } } )

  names <- pull( .metadata, {{two_point1}} ) %>%
    map( ~str_remove_all(.x, "_[:digit:].*") %>% paste0("sdr_score_",.) )

  meta_vars <- list(two_point1_sym, two_point2_sym, one_point_sym, names)

  pmap(meta_vars,
       function(two_point1_sym, two_point2_sym, one_point_sym, names) {
         .data %>%
           mutate({{names}} := case_when( !!two_point1_sym == "On" | !!two_point2_sym == "On" ~ 2,
                                          !!one_point_sym == "On" ~ 1,
                                          TRUE ~ 0) ) %>%
           select(last_col())
       }
  ) %>%
    bind_cols()


}
