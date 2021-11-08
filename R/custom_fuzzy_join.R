
#' Fuzzy Join by Days
#'
#' @description
#'
#' @param df1 first dataframe argument
#' @param DCDate1 first date argument
#' @param PIDN1 first ID argument; defaults to PIDN
#' @param df2 second dataframe argument
#' @param DCDate2 second date argument
#' @param PIDN2 second ID argument; defaults to PIDN
#' @param mode used to specify if the join should be "left", "right", "full", "semi", or "anti"; defaults to "left"
#' @param interval absolute value of days over which to match visits; default of +/-90 days
#'
#' @return
#' @export
#'
#' @examples
join_key_date_difference <- function(data1, data2, id1, id2, date1, date2,
                              mode = "left", interval = 90) {


  # Sub into the fuzzy_join by= call?
  # rlang::as_name(rlang::enquo(PIDN1)) = rlang::as_name(rlang::enquo(PIDN2))
  # rlang::as_name(rlang::enquo(DCDate1)) = rlang::as_name(rlang::enquo(DCDate2))

  joining_pidn <- magrittr::set_names(rlang::as_name(rlang::enquo(id2)),
                                      rlang::as_name(rlang::enquo(id1)) )

  joining_date <- magrittr::set_names(rlang::as_name(rlang::enquo(date2)),
                                      rlang::as_name(rlang::enquo(date1)) )


  data1 %>%
    fuzzyjoin::fuzzy_join(data2,
                          by=c(joining_pidn,joining_date),
                          match_fun = list(`==`, function(x,y)abs(x-y)<interval),
                          mode=mode)

}

