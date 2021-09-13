

#' Calculate difference between vectors of columns
#' This function accepts any number of 2 sets of equal length columns, and calculates the difference (that is, past-current = difference).
#' Intended to be used in conjunction with the combine_current_and_past_observations() function.
#'
#' @param dataframe dataframe object
#' @param variables_past variables where the observation is in the past
#' @param variables_current variables where the observation is current
#' @param variables_output_prefix add a meaningful prefix to the name of the difference columns
#' @param PIDN defaults to PIDN; can be any other ID
#' @param DCDate defaults to DCDate; can be any other date variable
#'
#' @return dataframe
#' @export
#'
#' @examples
calculate_current_past_difference <- function(dataframe, variables_past, variables_current, variables_output_prefix, PIDN=PIDN, DCDate = DCDate) {
  variables_current <- dplyr::enquo(variables_current)
  variables_past <- dplyr::enquo(variables_past)
  PIDN <- dplyr::enquo(PIDN)
  DCDate <- dplyr::enquo(DCDate)

  length1 <- dataframe %>%
    dplyr::select(!!variables_past) %>%
    length()
  length2 <- dataframe %>%
    dplyr::select(!!variables_current) %>%
    length()
  if (length1 != length2){print("Stop! Your variable lengths differ; you are not using this function correctly. The map() call within this function will not work!")}

  difference_names <- dataframe %>%
    dplyr::select(!!variables_current) %>%
    dplyr::rename_all(~gsub("current","difference",.)) %>%
    colnames()

  dataframe %<>%
    dplyr::select(!!variables_past,!!variables_current,everything()) %>%
    dplyr::bind_cols(purrr::map2(.x = .[, 1:(length1)], .y = .[, (1+length1):(length1+length2)], .f = ~.x - .y)) %>% # calculate difference between selected columns; PAST - CURRENT = DIFFERENCE
    dplyr::rename_at(dplyr::vars(!colnames(dataframe)), ~paste(difference_names)) %>% # Rename new columns to indicate they're the difference column
    dplyr::select(everything(),!!variables_past,!!variables_current,ends_with("difference")) # arrange new columns


  return(dataframe)
}


