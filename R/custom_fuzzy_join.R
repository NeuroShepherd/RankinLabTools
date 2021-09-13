
#' Title
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
custom_fuzzy_join <- function(df1, DCDate1, PIDN1 = PIDN,
                              df2, DCDate2, PIDN2 = PIDN,
                              mode = "left", interval = 90) {

  PIDN1 <- enquo(PIDN1)
  PIDN2 <- enquo(PIDN2)
  DCDate1 <- enquo(DCDate1)
  DCDate2 <- enquo(DCDate2)

  joining_pidn <- set_names(quo_name(PIDN2),quo_name(PIDN1))
  joining_date <- set_names(quo_name(DCDate2),quo_name(DCDate1))

  DCDate1 <- df1 %>%
    select(!!DCDate1) %>%
    mutate_at(vars(!!DCDate1), ~as_date(.))
  DCDate2 <- df2 %>%
    select(!!DCDate2) %>%
    mutate_at(vars(!!DCDate2), ~as_date(.))

  df1 %>%
    fuzzyjoin::fuzzy_join(df2,
                          by=c(joining_pidn,joining_date),
                          match_fun = list(`==`, function(x,y)abs(x-y)<interval),
                          mode=mode)

}

