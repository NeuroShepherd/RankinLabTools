

#' Impute for Missing Values
#' This function calculates imputed scores where item-level data is missing, and allows for the threshold of proportion of missing questions to be adjusted
#'
#' @param dataframe dataframe object
#' @param vars_to_impute vector of variables from which a score will be calculated/imputed
#' @param scale_name output name of the scale being calculated
#' @param missing_threshold maximum proportion of the scale questions allowed to be missing (i.e. the scale will not be calculated if the number of missing questions is greater than this value)
#' @param toggle_warning
#'
#' @return
#' @export
#'
#' @examples
impute_missing_values <- function(dataframe, vars_to_impute, scale_name, missing_threshold = 0.2, toggle_warning = TRUE){
  if(missing_threshold != 0.2 & toggle_warning){print("Warning! Default threshold is 0.2 or 20% in LAVA. Do you have a compelling reason to change this? (You can turn this warning off; toggle_warning = FALSE)")}

  vars_to_impute <- rlang::enquo(vars_to_impute)
  length_imputed_columns <- dataframe %>%
    dplyr::select(!!vars_to_impute) %>%
    length()
  quantity_missing <- dplyr::quo_name(scale_name) %>%
    stringr::str_replace(., ":","_") %>%
    paste0(.,"_quantity_missing")
  prop_missing_Q <- dplyr::quo_name(scale_name) %>%
    stringr::str_replace(.,":","_") %>%
    paste0(.,"_prop_missing_Q")
  row_Avg <- dplyr::quo_name(scale_name) %>%
    stringr::str_replace(.,":","_") %>%
    paste0(.,"_avg")
  row_Sum <- dplyr::quo_name(scale_name) %>%
    stringr::str_replace(.,":","_") %>%
    paste0(.,"_imputed_sum")

  dataframe %<>%
    dplyr::mutate_at(dplyr::vars(!!vars_to_impute), ~dplyr::case_when(. >= 0 ~ .)) %>%
    dplyr::mutate(!!row_Avg := dplyr::select(.,!!vars_to_impute) %>% rowMeans(.,na.rm=TRUE),
                  !!quantity_missing := dplyr::select(.,!!vars_to_impute) %>% is.na() %>% rowSums(),
                  !!prop_missing_Q := !!dplyr::sym(quantity_missing)/length_imputed_columns,
                  !!row_Sum := dplyr::case_when(!!dplyr::sym(prop_missing_Q) <= missing_threshold ~ round(!!dplyr::sym(row_Avg)*length_imputed_columns))
                  )
#   Lines below imputed averages to the empty columns then calculated the sum of the rows for
#   the selected columns, but this item-level replacment should not be done!!
#    dplyr::mutate_at(dplyr::vars(!!vars_to_impute), ~dplyr::case_when(!!dplyr::sym(prop_missing_Q) <= missing_threshold & is.na(.) ~ !!dplyr::sym(row_Avg), TRUE ~ .)) %>%
#    dplyr::mutate(!!row_sum := dplyr::select(.,!!vars_to_impute) %>% rowSums(.))

  return(dataframe)

  # When imputing values, item-level values should not be replaced!!!
  # To calculate new sub-scale scores: take the average of all existing values in the sub-scale, and multiply this
  # average by the total number of items in the sub-scale (that is, in R terms, the length() of the columns used
  # in the sub-scale)
}



