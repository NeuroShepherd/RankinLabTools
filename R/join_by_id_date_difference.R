
#' Fuzzy Join by Days
#'
#' @description
#'
#' @param data1 first data frame
#' @param data2 second data frame
#' @param id1 primary key variable from data1
#' @param id2 primary key variable from data2
#' @param date1 date key variable from data1. This column should be of class "Date," but accepts any numeric value
#' @param date2 date key variable from data2. This column should be of class "Date," but accepts any numeric value
#' @param mode specify join type should be "left", "right", "full", "semi", or "anti"; defaults to "left"
#' @param interval absolute value of days over which to match observations by id; default of +/-90 days
#'
#' @return data frame comprised of data1 and data2
#' @export
#'
#' @section Examples:
#'
#'
#' ```{r, message=F, rows.print=5}
#'
#' ```
#'
join_by_id_date_difference <- function(data1, data2, id1, id2, date1, date2, mode = "left", interval = 90) {


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

