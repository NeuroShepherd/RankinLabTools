
#' Title
#'
#' @param df1 first dataframe argument
#' @param df2 second dataframe argument
#' @param mode used to specify if the join should be "left", "right", "full", "semi", or "anti"; defaults to "left"
#' @param interval absolute value of days over which to match visits; default of +/-90 days
#' @param key1
#' @param key2
#' @param date1
#' @param date2
#'
#' @return
#' @export
#'
#' @examples
custom_fuzzy_join <- function(df1, df2, key1, key2, date1, date2, mode = "left", interval = 90) {


  joining_pidn <- magrittr::set_names(rlang::as_name(rlang::enquo(PIDN2)),
                            rlang::as_name(rlang::enquo(PIDN1)) )

  joining_date <- magrittr::set_names(rlang::as_name(rlang::enquo(DCDate2)),
                            rlang::as_name(rlang::enquo(DCDate1)) )


  DCDate1 <- df1 %>% dplyr::select({{DCDate1}}) %>% dplyr::mutate(dplyr::across(~as.date(.)))

  DCDate2 <- df2 %>% dplyr::select({{DCDate2}}) %>% dplyr::mutate(dplyr::across(~as.date(.)))


  fuzzy_joined_data <- df1 %>%
    fuzzyjoin::fuzzy_join(df2,
                          by=c(joining_pidn,joining_date),
                          match_fun = list(`==`, function(x,y)abs(x-y)<interval),
                          mode=mode)

  return(fuzzy_joined_data)

}


# Sub into the fuzzy_join by= call?
rlang::as_name(rlang::enquo(PIDN1)) = rlang::as_name(rlang::enquo(PIDN2))
rlang::as_name(rlang::enquo(DCDate1)) = rlang::as_name(rlang::enquo(DCDate2))

# If the above doesn't work, then at least update the quo_name to as_name and
# put the enquo calls into the set_names stuff
