

#' Calculate Global CDR plus NACC FTLD Rating
#'
#' @param data dataframe object
#' @param MEMORY
#' @param ORIENT
#' @param JUDGMENT
#' @param COMMUN
#' @param HOMEHOBB
#' @param PERSCARE
#' @param COMPORT
#' @param CDRLANG
#'
#' @return dataframe
#' @export
#'
#' @examples
#' calculate_cdr_plus_nacc_ftld(nacc_data)
calculate_cdr_plus_nacc_ftld <- function(data, MEMORY = MEMORY, ORIENT = ORIENT, JUDGMENT = JUDGMENT, COMMUN = COMMUN,
                                         HOMEHOBB = HOMEHOBB, PERSCARE = PERSCARE, COMPORT = COMPORT, CDRLANG = CDRLANG) {


  cdr_variables <- enexprs(MEMORY,ORIENT,JUDGMENT,COMMUN,HOMEHOBB,PERSCARE,COMPORT,CDRLANG) %>% as.character()

  MEMORY <- enquo(MEMORY); ORIENT <- enquo(ORIENT); JUDGMENT <- enquo(JUDGMENT); COMMUN <- enquo(COMMUN)
  HOMEHOBB <- enquo(HOMEHOBB); PERSCARE <- enquo(PERSCARE); COMPORT <- enquo(COMPORT); CDRLANG <- enquo(CDRLANG)


  data %<>%
    mutate(cdr_plus_nacc_ftld = case_when(
      # 1) If all domains are 0 then the global CDR plus NACC FTLD is 0
      .data[[MEMORY]]==0 & .data[[ORIENT]]==0 & .data[[JUDGMENT]]==0 & .data[[COMMUN]]==0 &
      .data[[HOMEHOBB]]==0 & .data[[PERSCARE]]==0 & .data[[COMPORT]]==0 & .data[[CDRLANG]]==0 ~ 0,


      # 2) If the maximum domain score is 0.5, the CDR plus NACC FTLD is 0.5
      pmax( !!!dplyr::select(.,!!cdr_variables) ) == 0.5 ~ 0.5,


      # 3) If the maximum domain score is above 0.5 in any domain then...
      # A) Maximum domain score is 1 and all other domains are 0 then CDR plus NACC FTLD is 0.5
      (pmax( !!!dplyr::select(.,!!cdr_variables) ) == 1)
      &
        (rowSums(dplyr::select(.,!!cdr_variables)) == pmax( !!!dplyr::select(.,!!cdr_variables) )) ~ 0.5,


      # B) Maximum domain score is 2 or 3 and all other domains are 0 then CDR + NACC is 1
      (pmax( !!!dplyr::select(.,!!cdr_variables) ) %in% c(2,3))
      &
        (rowSums(dplyr::select(.,!!cdr_variables)) == pmax( !!!dplyr::select(.,!!cdr_variables) )) ~ 1,


      # C) Maximum domain score occurs only once and there is another rating besides zero,
      # the global CDR + NACC FTLD score is one level lower than the level corresponding to the
      # maximum impairment
        # First section addresses possible final scores of 1 and 2
      (rowSums(dplyr::select(.,!!cdr_variables) == pmax( !!!dplyr::select(.,!!cdr_variables) )) == 1)
      &
        (rowSums(dplyr::select(.,!!cdr_variables)) > pmax( !!!dplyr::select(.,!!cdr_variables) ))
      &
        pmax( !!!dplyr::select(.,!!cdr_variables) ) %in% c(2,3) ~
        pmax( !!!dplyr::select(.,!!cdr_variables) ) - 1,

        # Second section addresses possible final score of 0.5. Had to separate out because e.g. 1-1 in the above
        # equation would mathematicallly give 0, but the math is really (CDR 1 - one level) which is a drop down to 0.5
      (rowSums(dplyr::select(.,!!cdr_variables) == pmax( !!!dplyr::select(.,!!cdr_variables) )) == 1)
      &
        (rowSums(dplyr::select(.,!!cdr_variables)) > pmax( !!!dplyr::select(.,!!cdr_variables) ))
      &
        pmax( !!!dplyr::select(.,!!cdr_variables) ) %in% c(1) ~
        0.5,


      # D) Maximum domain score occurs more than once than the global CDR + NACC FTLD is that domain score
      (rowSums(dplyr::select(.,!!cdr_variables) == pmax( !!!dplyr::select(.,!!cdr_variables) )) > 1) ~
        pmax( !!!dplyr::select(.,!!cdr_variables) )

    )
    )

  return(data)
}



