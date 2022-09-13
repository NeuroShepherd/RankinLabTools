

#' Combine Current and Past Observations
#'
#' @description
#'
#' This is a specialized function for use with LAVA Query data frames, and can be
#' thought of as a conditional version of `spread()` or `pivot_longer()`. There are
#' a number of questionnaires where data is collected about the "current" time
#' point *and* about a time in the "past," generally 5 years in the past for the
#' questionnaires we use. This "past" data is collected only once while the "current"
#' data is collected at initial and every subsequent visit. Ultimately, researchers
#' will want to calculate change scores from before someone's diagnosis, and this function
#' places the "past/before" data next to every instance of "current" data/
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param CurrentPast variable indicating whether the observation is a "Current" or "Past" record; default column name expected is CurrentPast, but can be replaced with any column with "current" and "past"/"before" characters
#' @param DCDate date column; defaults to DCDate
#' @param PIDN identifier column; defaults to PIDN
#'
#' @return dataframe
#' @export
#'
#' @section Examples:
#'
#'
#' ```{r, message=F, rows.print=5}
#'
#' ```
#'
combine_current_and_past_observations <- function(.data, CurrentPast = CurrentPast, DCDate=DCDate, PIDN=PIDN) {
  CurrentPast <- enquo(CurrentPast)
  DCDate <- enquo(DCDate)
  PIDN <- enquo(PIDN)

  current_dataframe <- .data %>%
    dplyr::filter(!!CurrentPast %in% c("CURRENT","Current"))

  past_dataframe <- .data %>%
    dplyr::filter(!!CurrentPast %in% c("PAST","Past","Before","BEFORE")) %>%
    dplyr::mutate(DCDate = lubridate::as_date(DCDate)) %>%
    dplyr::group_by(PIDN) %>% # These three lines act to select only the earliest instance of completing a PAST test within each PIDN
    dplyr::filter(DCDate == min(DCDate)) %>%
    dplyr::ungroup()

  final_dataframe <- current_dataframe %>%
    dplyr::left_join(past_dataframe, by=c("PIDN"="PIDN"), suffix=c("_current","_past"))

  return(final_dataframe)
}


