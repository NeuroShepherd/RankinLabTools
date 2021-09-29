

#' Calculate difference between vectors of columns
#' This function accepts any number of 2 sets of equal length columns, and calculates the difference (that is, past-current = difference).
#' Intended to be used in conjunction with the combine_current_and_past_observations() function.
#'
#' @param dataframe dataframe object
#' @param variables_past variables where the observation is in the past
#' @param variables_current variables where the observation is current
#' @param variables_output_prefix add a meaningful prefix to the name of the difference columns
#'
#' @return dataframe
#' @export
#'
#' @examples
calculate_current_past_difference <- function(dataframe, variables_past, variables_current, variables_output_prefix) {


  length1 <- dataframe %>%
    dplyr::select({{variables_past}}) %>%
    length()
  length2 <- dataframe %>%
    dplyr::select({{variables_current}}) %>%
    length()
  if (length1 != length2){print("Stop! Your variable lengths differ; you are not using this function correctly. The map() call within this function will not work!")}

  difference_names <- dataframe %>%
    dplyr::select({{variables_current}}) %>%
    dplyr::rename_all(~gsub("current","difference",.)) %>%
    colnames()

  dataframe %<>%
    dplyr::bind_cols(purrr::map2(.x = dplyr::select(.,{{variables_past}}),
                                 .y = dplyr::select(.,{{variables_current}}),
                                 .f = ~.x - .y)
                     )  # calculate difference between selected columns; PAST - CURRENT = DIFFERENCE
    # dplyr::rename_at(dplyr::vars(!colnames(dataframe)), ~paste(difference_names)) # Rename new columns to indicate they're the difference column
    # bind_cols(dataframe, .) # arrange new columns

  return(dataframe)
}


tibble::tibble(a1 = c(1,2,3,2,1,3,3), a2 = c(1,1,1,1,3,3,3),
               b1 = c(5,5,5,5,5,5,3), b2 = c(4,4,4,5,3,3,3)) %>%
  calculate_current_past_difference(c(a1,b1), c(a2,b2), "oink")



tibble::tibble(a1 = c(1,2,3,2,1,3,3), a2 = c(1,1,1,1,3,3,3),
               b1 = c(5,5,5,5,5,5,3), b2 = c(4,4,4,5,3,3,3)) %>%
  purrr::map2(.x = .[,c("a1","b1")], .y = .[,c("a2","b2")],
       .f = ~ {.x - .y} %>% as.data.frame)
  dplyr::mutate("hello" := purrr::map2(c(a1,b1),c(a2,b2), .f=~{.x - .y}) %>% unlist)


  purrr::map2_df(c(a1,b1), c(a2,b2), .f = ~{select(.,.x) - select(.,.y)})


oink_internal <- function(dataset, initial, final) {
    oinkers <- dataset %>%
      purrr::map2(.x = dplyr::select(.,{{initial}}), .y = dplyr::select(.,{{final}}),
                  .f = ~ {.x - .y})
    return(oinkers)
}

oinkoink <- function(dataset, initial, final) {

  dataset %>%
    purrr::map2(.x = dplyr::select(.,{{initial}}),
         .y = dplyr::select(.,{{final}}),
         .f = ~oink_internal(dataset,all_of(.x),all_of(.y)))

}


tibble::tibble(a1 = c(1,2,3,2,1,3,3), a2 = c(1,1,1,1,3,3,3),
               b1 = c(5,5,5,5,5,5,3), b2 = c(4,4,4,5,3,3,3)) %>%
  oink_internal(a1,a2) %>%
  as.data.frame()

tibble::tibble(a1 = c(1,2,3,2,1,3,3), a2 = c(1,1,1,1,3,3,3),
               b1 = c(5,5,5,5,5,5,3), b2 = c(4,4,4,5,3,3,3)) %>%
  oinkoink(a1,a2)



mewmew <- function(dataframe, variables_past, variables_current) {


  dataframe %<>%
    purrr::map2(.x = dplyr::select(.,{{variables_past}}),
                .y = dplyr::select(.,{{variables_current}}),
                .f = ~{.x - .y})


  return(dataframe)

}

tibble::tibble(a1 = c(1,2,3,2,1,3,3), a2 = c(1,1,1,1,3,3,3),
               b1 = c(5,5,5,5,5,5,3), b2 = c(4,4,4,5,3,3,3)) %>%
  mewmew(c(a1,b1),c(a2,b2))



