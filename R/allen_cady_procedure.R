

#' Allen-Cady Linear Model Feature Selection
#'
#' @description
#' This function is an implementation of the Allen-Cady procedure for backwards selection of variables to include in a linear model.
#'
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param .outcome_var The outcome (dependent) variable for the linear model; must be a column in the data frame.
#' @param p_threshold The threshold significance value. Predictors with p-values above the threshold are iteratively removed from the model.
#'
#' @return A list of *i* linear model outputs (e.g. a list of the results from `base::lm()`) where *i* is the number of iterations to create a model where all features are below the p-value threshold.
#' @export
#'
#' @examples
#' allen_cady(mtcars, mpg)
#'
allen_cady <- function(.data, .outcome_var, p_threshold = 0.2) {

  .outcome_var <- substitute(.outcome_var)
  remaining_independent_vars <- length(.data)
  i <- 0

  while (remaining_independent_vars > 0) {

    i = i + 1

    lm_formula <- .data %>% tibble::tibble() %>% names() %>%
      dplyr::setdiff(c(.outcome_var,"(Intercept)")) %>%
      purrr::reduce(paste, sep=" + ") %>% paste(.outcome_var, " ~ ", .) %>%
      as.formula()

    if (i == 1) {lm_results <- list()}
    lm_results[[i]] <- lm(lm_formula, data = .data)

    temp_results <- lm_results %>%
      magrittr::extract2(i) %>%
      summary() %>%
      magrittr::extract2(4) %>%
      as.data.frame() %>%
      purrr::set_names(c("Estimate","StdError","t_value","p_value")) %>%
      tibble::rownames_to_column() %>%
      tibble()

    .data <- temp_results %>%
      dplyr::filter(p_value < p_threshold) %>%
      dplyr::pull(rowname) %>%
      stringr::str_remove("\\..*|\\^.*") %>%
      unique() %>%
      dplyr::setdiff("(Intercept)") %>%
      dplyr::select(.data, {{.outcome_var}}, .)


    remaining_independent_vars <- temp_results %>%
      dplyr::filter(p_value > p_threshold) %>%
      nrow()

  }

  return(lm_results)

}

