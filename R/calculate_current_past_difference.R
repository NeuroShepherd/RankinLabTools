

#' Calculate difference between vectors of columns
#' This function accepts any number of 2 sets of equal length columns, and calculates the difference (that is, past-current = difference).
#' Intended to be used in conjunction with the combine_current_and_past_observations() function.
#'
#' @param dataframe dataframe object
#' @param final vector of final value columns from dataframe
#' @param initial vector of initial value columns from dataframe
#'
#' @return dataframe
#' @export
#'
#' @examples
calculate_current_past_difference <- function(dataframe, final, initial) {

# TODO: turn the code below into an actual error to prevent people from accidentally recycling vectors
  # length1 <- dataframe %>%
  #   dplyr::select({{final}}) %>%
  #   length()
  # length2 <- dataframe %>%
  #   dplyr::select({{initial}}) %>%
  #   length()
  # if (length1 != length2){print("Stop! Your variable lengths differ; you are not using this function correctly. The map() call within this function will not work!")}

  difference_names <- paste0(dplyr::select(dataframe,{{final}}) %>% names(),
                             "-",
                             dplyr::select(dataframe,{{initial}}) %>% names())

  dataframe %<>%
    purrr::map2(.x = dplyr::select(.,{{final}}),
                .y = dplyr::select(.,{{initial}}),
                .f = ~{.x - .y}) %>% # calculate difference between selected columns; FINAL - INITIAL = DIFFERENCE
    set_names(difference_names) %>%
    dplyr::bind_cols(dataframe, .)

  return(dataframe)
}

