

#' Calculate Global CDR`r sprintf("\U00AE")` plus NACC FTLD Rating
#'
#'
#' @description `calculate_cdr_plus_nacc_ftld()` calculates the global CDR`r sprintf("\U00AE")` plus NACC FTLD score
#' as described by Miyagawa et al. (2020). The default arguments expect CDR variable names as defined by the NACC,
#' but custom variable names can be assigned to each of these arguments.
#'
#' See [Form B4: CDR`r sprintf("\U00AE")` Dementia Staging Instrument](https://files.alz.washington.edu/documentation/uds3-ivp-b4.pdf) and pages 91-92 of the [UDS3 Coding Guidebook](https://files.alz.washington.edu/documentation/uds3-ivp-ded.pdf)
#'
#' @param .data dataframe object
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
#' An object of the same type as `.data`. The output has the following properties:
#'  * Rows are not affected.
#'  * Data frame attributes are preserved.
#'  * Groups are maintained; you can't select off grouping variables.
#'  * The returned data frame includes a new variable labeled `cdr_plus_nacc_ftld`.
#'
#'
#'
#' @section Examples:
#'
#' ```{r, message=F, rows.print=5}
#' nacc_cdr_data_simulated %>%
#'    select(-NACCID,-VISITDATE) %>% # limit columns in final output for readability purposes
#'    calculate_cdr_plus_nacc_ftld()
#'
#'
#' ```
#'
#' @export
calculate_cdr_plus_nacc_ftld <- function(.data, .memory = MEMORY, .orient = ORIENT, .judgment = JUDGMENT, .commun = COMMUN,
                                         .homehobb = HOMEHOBB, .perscare = PERSCARE, .comport = COMPORT, .cdrlang = CDRLANG) {


  cdr_variables <- rlang::enexprs(.memory,.orient,.judgment,.commun,.homehobb,.perscare,.comport,CDRLANG) %>% as.character()

  .memory <- rlang::enquo(.memory); .orient <- rlang::enquo(.orient); .judgment <- rlang::enquo(.judgment); .commun <- rlang::enquo(.commun)
  .homehobb <- rlang::enquo(.homehobb); .perscare <- rlang::enquo(.perscare); .comport <- rlang::enquo(.comport); .cdrlang <- rlang::enquo(.cdrlang)


  .data %<>%
    dplyr::mutate(cdr_plus_nacc_ftld = dplyr::case_when(
      # 1) If all domains are 0 then the global CDR plus NACC FTLD is 0
      .data[[.memory]]==0 & .data[[.orient]]==0 & .data[[.judgment]]==0 & .data[[.commun]]==0 &
      .data[[.homehobb]]==0 & .data[[.perscare]]==0 & .data[[.comport]]==0 & .data[[.cdrlang]]==0 ~ 0,


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

  return(.data)
}



