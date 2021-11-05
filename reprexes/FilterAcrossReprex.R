
suppressPackageStartupMessages({
library(tidyverse)
library(rlang)
library(magrittr)
})

example_table <- tibble::tibble(a = c(1,1,NA,1), b=c(NA,NA,NA,1), c=c(2,NA,NA,2))

example_table

# Old version of dplyr (<1.0.0). This function works.
function_old_dplyr <- function(dataframe, columns){
  columns <- rlang::enquo(columns)

  dataframe %<>%
    dplyr::filter_at(dplyr::vars(!!columns), dplyr::any_vars(!is.na(.)))

  return(dataframe)

}

# This works
function_old_dplyr(example_table, c(a,b))



# Attempt with new dplyr (>1.0.0). I
function_new_dplyr <- function(dataframe, ...){
  columns <- rlang::enquos(...)

  dataframe %<>%
    dplyr::filter( dplyr::across(dplyr::vars(!!!columns), ~dplyr::any_vars(!is.na(.))) )

  return(dataframe)

}

# This doesn't
function_new_dplyr(example_table, a, b)

