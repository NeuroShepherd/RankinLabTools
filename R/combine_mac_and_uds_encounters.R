
#' Combine MAC and UDS Encounters
#' This function allows you to combine MAC and UDS encounters for a particular test where the
#' encounters have been separated into different rows, but really occurred around the same time period.
#' You can determine what range of days qualifies as a "single visit" using the day_range argument.
#'
#' @param dataframe dataframe object
#' @param questions_to_combine select the variables that should be combined. This will be at least the item-level variables, and potentially your summary scores
#' @param day_range range of days over which MAC and UDS visits can be considered the "same" visit; default of 30 days
#' @param DCDate date identifier; defaults to DCDate variable
#' @param PIDN person identifier; defaults to PIDN
#'
#' @return dataframe
#' @export
#'
#' @examples
combine_mac_uds_encounters <- function(dataframe, questions_to_combine, day_range = 30, DCDate = DCDate, PIDN = PIDN){
  questions_to_combine <- enquo(questions_to_combine)
  DCDate <- enquo(DCDate)
  PIDN <- enquo(PIDN)

  dataframe %<>%
    dplyr::mutate_at(dplyr::vars(!!questions_to_combine), ~dplyr::na_if(.,-6)) %>%
    dplyr::mutate(DCDate = lubridate::as_date(!!DCDate)) %>%
    dplyr::arrange(!!PIDN,DCDate) %>%
    dplyr::group_by(!!PIDN) %>%
    dplyr::mutate(closeness_lag = dplyr::near(DCDate,dplyr::lead(DCDate),tol=day_range)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(newDate = dplyr::case_when(closeness_lag == TRUE ~ dplyr::lead(DCDate),
                                             closeness_lag == FALSE ~ DCDate,
                                             is.na(closeness_lag) ~ DCDate)) %>%
    dplyr::group_by(!!PIDN,newDate) %>%
    dplyr::summarize_all(list(~dplyr::first(stats::na.omit(.)))) %>%
    dplyr::ungroup()

  return(dataframe)

}

