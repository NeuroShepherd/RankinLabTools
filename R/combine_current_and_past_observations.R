

#' Combine Current and Past Observations
#'
#' @param dataframe dataframe object
#' @param CurrentPast variable indicating whether the observation is a "Current" or "Past" record; default column name expected is CurrentPast, but can be replaced with any column with "current" and "past"/"before" characters
#' @param DCDate date column; defaults to DCDate
#' @param PIDN identifier column; defaults to PIDN
#'
#' @return dataframe
#' @export
#'
#' @examples
combine_current_and_past_observations <- function(dataframe, CurrentPast = CurrentPast, DCDate=DCDate, PIDN=PIDN) {
  CurrentPast <- enquo(CurrentPast)
  DCDate <- enquo(DCDate)
  PIDN <- enquo(PIDN)

  current_dataframe <- dataframe %>%
    dplyr::filter(!!CurrentPast %in% c("CURRENT","Current"))

  past_dataframe <- dataframe %>%
    dplyr::filter(!!CurrentPast %in% c("PAST","Past","Before","BEFORE")) %>%
    dplyr::mutate(DCDate = lubridate::as_date(DCDate)) %>%
    dplyr::group_by(PIDN) %>% # These three lines act to select only the earliest instance of completing a PAST test within each PIDN
    dplyr::filter(DCDate == min(DCDate)) %>%
    dplyr::ungroup()

  final_dataframe <- current_dataframe %>%
    dplyr::left_join(past_dataframe, by=c("PIDN"="PIDN"), suffix=c("_current","_past"))

  return(final_dataframe)
}


