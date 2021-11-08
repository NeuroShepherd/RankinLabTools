

#' Calculate Global CDR`r sprintf("\U00AE")` plus NACC FTLD Rating
#'
#' @description `calculate_cdr_plus_nacc_ftld()` calculates the global CDR`r sprintf("\U00AE")` plus NACC FTLD score
#' as described by Miyagawa et al. (2020). The default arguments expect CDR variable names as defined by the NACC,
#' but custom variable names can be assigned to each of these arguments.
#'
#' @param data dataframe object
#' @param MEMORY CDR memory score
#' @param ORIENT CDR orientation score
#' @param JUDGMENT CDR judgement score
#' @param COMMUN CDR community? score
#' @param HOMEHOBB CDR home acitivites and hobbies score
#' @param PERSCARE CDR personal care score
#' @param COMPORT CDR behavior score
#' @param CDRLANG CDR language score
#'
#' @return
#'
#' Object the same type as the `data` argument which should be a data frame variant.
#'  * The returned data frame includes a new variable labeled `cdr_plus_nacc_ftld`
#'
#' @export
#'
#' @examples
#'
#'
calculate_cdr_plus_nacc_ftld <- function(data, MEMORY = MEMORY, ORIENT = ORIENT, JUDGMENT = JUDGMENT, COMMUN = COMMUN,
                                         HOMEHOBB = HOMEHOBB, PERSCARE = PERSCARE, COMPORT = COMPORT, CDRLANG = CDRLANG) {


  cdr_variables <- rlang::enexprs(MEMORY,ORIENT,JUDGMENT,COMMUN,HOMEHOBB,PERSCARE,COMPORT,CDRLANG) %>% as.character()

  MEMORY <- rlang::enquo(MEMORY); ORIENT <- rlang::enquo(ORIENT); JUDGMENT <- rlang::enquo(JUDGMENT); COMMUN <- rlang::enquo(COMMUN)
  HOMEHOBB <- rlang::enquo(HOMEHOBB); PERSCARE <- rlang::enquo(PERSCARE); COMPORT <- rlang::enquo(COMPORT); CDRLANG <- rlang::enquo(CDRLANG)


  data %<>%
    dplyr::mutate(cdr_plus_nacc_ftld = dplyr::case_when(
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



