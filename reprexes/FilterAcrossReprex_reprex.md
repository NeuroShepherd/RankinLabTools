``` r
suppressPackageStartupMessages({
  library(tidyverse)
  library(rlang)
  library(magrittr)
})

example_table <- tibble::tibble(a = c(1, 1, NA, 1), b = c(NA, NA, NA, 1), c = c(2, NA, NA, 2))

example_table
#> # A tibble: 4 × 3
#>       a     b     c
#>   <dbl> <dbl> <dbl>
#> 1     1    NA     2
#> 2     1    NA    NA
#> 3    NA    NA    NA
#> 4     1     1     2

# Old version of dplyr (<1.0.0). This function works.
function_old_dplyr <- function(dataframe, columns) {
  columns <- rlang::enquo(columns)

  dataframe %<>%
    dplyr::filter_at(dplyr::vars(!!columns), dplyr::any_vars(!is.na(.)))

  return(dataframe)
}

# This works
function_old_dplyr(example_table, c(a, b))
#> # A tibble: 3 × 3
#>       a     b     c
#>   <dbl> <dbl> <dbl>
#> 1     1    NA     2
#> 2     1    NA    NA
#> 3     1     1     2



# Attempt with new dplyr (>1.0.0). I
function_new_dplyr <- function(dataframe, ...) {
  columns <- rlang::enquos(...)

  dataframe %<>%
    dplyr::filter(dplyr::across(dplyr::vars(!!!columns), ~ dplyr::any_vars(!is.na(.))))

  return(dataframe)
}

# This doesn't
function_new_dplyr(example_table, a, b)
#> Error: Problem with `filter()` input `..1`.
#> ℹ Input `..1` is `dplyr::across(dplyr::vars(a, b), ~dplyr::any_vars(!is.na(.)))`.
#> x Must subset columns with a valid subscript vector.
#> x Subscript has the wrong type `quosures`.
#> ℹ It must be numeric or character.
```

<sup>Created on 2021-11-05 by the [reprex package](https://reprex.tidyverse.org) (v2.0.0)</sup>

<details style="margin-bottom:10px;">

<summary>Standard output and standard error</summary>

``` sh
Error: Can't combine <quosures> and <logical>.
Backtrace:
     █
  1. ├─global::function_new_dplyr(example_table, a, b)
  2. │ └─`%<>%`(...)
  3. ├─dplyr::filter(., dplyr::across(dplyr::vars(!!!columns), ~dplyr::any_vars(!is.na(.))))
  4. ├─dplyr:::filter.data.frame(...)
  5. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
  6. │   ├─base::withCallingHandlers(...)
  7. │   └─mask$eval_all_filter(dots, env_filter)
  8. ├─dplyr::across(dplyr::vars(a, b), ~dplyr::any_vars(!is.na(.)))
  9. │ └─dplyr:::across_setup(...)
 10. │   └─tidyselect::eval_select(cols, data = across_cols)
 11. │     └─tidyselect:::eval_select_impl(...)
 12. │       ├─tidyselect:::with_subscript_errors(...)
 13. │       │ ├─base::tryCatch(...)
 14. │       │ │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
 15. │       │ │   └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
 16. │       │ │     └─base:::doTryCatch(return(expr), name, parentenv, handler)
 17. │       │ └─tidyselect:::instrument_base_errors(expr)
 18. │       │   └─base::withCallingHandlers(...)
 19. │       └─tidyselect:::vars_select_eval(...)
 20. │         └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
 21. │           └─tidyselect:::as_indices_sel_impl(...)
 22. │             └─tidyselect:::as_indices_impl(x, vars, strict = strict)
 23. │               └─vctrs::vec_as_subscript(x, logical = "error")
 24. └─vctrs:::try_catch_impl(...)
 25.   ├─base::tryCatch(try_catch_callback(data, NULL), ...)
 26.   │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
 27.   │   └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
 28.   │     └─base:::doTryCatch(return(expr), name, parentenv, handler)
 29.   └─vctrs:::try_catch_callback(data, NULL)
 30.     └─(function () ...
 31.       └─vctrs::vec_default_ptype2(...)
 32.         └─vctrs::stop_incompatible_type(...)
 33.           └─vctrs:::stop_incompatible(...)
 34.             └─vctrs:::stop_vctrs(...)
Error: Can't combine <quosures> and <integer>.
Backtrace:
     █
  1. ├─global::function_new_dplyr(example_table, a, b)
  2. │ └─`%<>%`(...)
  3. ├─dplyr::filter(., dplyr::across(dplyr::vars(!!!columns), ~dplyr::any_vars(!is.na(.))))
  4. ├─dplyr:::filter.data.frame(...)
  5. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
  6. │   ├─base::withCallingHandlers(...)
  7. │   └─mask$eval_all_filter(dots, env_filter)
  8. ├─dplyr::across(dplyr::vars(a, b), ~dplyr::any_vars(!is.na(.)))
  9. │ └─dplyr:::across_setup(...)
 10. │   └─tidyselect::eval_select(cols, data = across_cols)
 11. │     └─tidyselect:::eval_select_impl(...)
 12. │       ├─tidyselect:::with_subscript_errors(...)
 13. │       │ ├─base::tryCatch(...)
 14. │       │ │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
 15. │       │ │   └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
 16. │       │ │     └─base:::doTryCatch(return(expr), name, parentenv, handler)
 17. │       │ └─tidyselect:::instrument_base_errors(expr)
 18. │       │   └─base::withCallingHandlers(...)
 19. │       └─tidyselect:::vars_select_eval(...)
 20. │         └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
 21. │           └─tidyselect:::as_indices_sel_impl(...)
 22. │             └─tidyselect:::as_indices_impl(x, vars, strict = strict)
 23. │               └─vctrs::vec_as_subscript(x, logical = "error")
 24. └─vctrs:::try_catch_impl(...)
 25.   ├─base::tryCatch(try_catch_callback(data, NULL), ...)
 26.   │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
 27.   │   └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
 28.   │     └─base:::doTryCatch(return(expr), name, parentenv, handler)
 29.   └─vctrs:::try_catch_callback(data, NULL)
 30.     └─(function () ...
 31.       └─vctrs::vec_default_ptype2(...)
 32.         └─vctrs::stop_incompatible_type(...)
 33.           └─vctrs:::stop_incompatible(...)
 34.             └─vctrs:::stop_vctrs(...)
Error: Can't combine <quosures> and <character>.
Backtrace:
     █
  1. ├─global::function_new_dplyr(example_table, a, b)
  2. │ └─`%<>%`(...)
  3. ├─dplyr::filter(., dplyr::across(dplyr::vars(!!!columns), ~dplyr::any_vars(!is.na(.))))
  4. ├─dplyr:::filter.data.frame(...)
  5. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
  6. │   ├─base::withCallingHandlers(...)
  7. │   └─mask$eval_all_filter(dots, env_filter)
  8. ├─dplyr::across(dplyr::vars(a, b), ~dplyr::any_vars(!is.na(.)))
  9. │ └─dplyr:::across_setup(...)
 10. │   └─tidyselect::eval_select(cols, data = across_cols)
 11. │     └─tidyselect:::eval_select_impl(...)
 12. │       ├─tidyselect:::with_subscript_errors(...)
 13. │       │ ├─base::tryCatch(...)
 14. │       │ │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
 15. │       │ │   └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
 16. │       │ │     └─base:::doTryCatch(return(expr), name, parentenv, handler)
 17. │       │ └─tidyselect:::instrument_base_errors(expr)
 18. │       │   └─base::withCallingHandlers(...)
 19. │       └─tidyselect:::vars_select_eval(...)
 20. │         └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
 21. │           └─tidyselect:::as_indices_sel_impl(...)
 22. │             └─tidyselect:::as_indices_impl(x, vars, strict = strict)
 23. │               └─vctrs::vec_as_subscript(x, logical = "error")
 24. └─vctrs:::try_catch_impl(...)
 25.   ├─base::tryCatch(try_catch_callback(data, NULL), ...)
 26.   │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
 27.   │   └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
 28.   │     └─base:::doTryCatch(return(expr), name, parentenv, handler)
 29.   └─vctrs:::try_catch_callback(data, NULL)
 30.     └─(function () ...
 31.       └─vctrs::vec_default_ptype2(...)
 32.         └─vctrs::stop_incompatible_type(...)
 33.           └─vctrs:::stop_incompatible(...)
 34.             └─vctrs:::stop_vctrs(...)
Error: Must subset elements with a valid subscript vector.
x Subscript has the wrong type `quosures`.
ℹ It must be numeric or character.
Backtrace:
     █
  1. ├─global::function_new_dplyr(example_table, a, b)
  2. │ └─`%<>%`(...)
  3. ├─dplyr::filter(., dplyr::across(dplyr::vars(!!!columns), ~dplyr::any_vars(!is.na(.))))
  4. ├─dplyr:::filter.data.frame(...)
  5. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
  6. │   ├─base::withCallingHandlers(...)
  7. │   └─mask$eval_all_filter(dots, env_filter)
  8. └─dplyr::across(dplyr::vars(a, b), ~dplyr::any_vars(!is.na(.)))
  9.   └─dplyr:::across_setup(...)
 10.     └─tidyselect::eval_select(cols, data = across_cols)
 11.       └─tidyselect:::eval_select_impl(...)
 12.         ├─tidyselect:::with_subscript_errors(...)
 13.         │ ├─base::tryCatch(...)
 14.         │ │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
 15.         │ │   └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
 16.         │ │     └─base:::doTryCatch(return(expr), name, parentenv, handler)
 17.         │ └─tidyselect:::instrument_base_errors(expr)
 18.         │   └─base::withCallingHandlers(...)
 19.         └─tidyselect:::vars_select_eval(...)
 20.           └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
 21.             └─tidyselect:::as_indices_sel_impl(...)
 22.               └─tidyselect:::as_indices_impl(x, vars, strict = strict)
 23.                 └─vctrs::vec_as_subscript(x, logical = "error")
Error: Must subset columns with a valid subscript vector.
x Subscript has the wrong type `quosures`.
ℹ It must be numeric or character.
Backtrace:
     █
  1. ├─global::function_new_dplyr(example_table, a, b)
  2. │ └─`%<>%`(...)
  3. ├─dplyr::filter(., dplyr::across(dplyr::vars(!!!columns), ~dplyr::any_vars(!is.na(.))))
  4. ├─dplyr:::filter.data.frame(...)
  5. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
  6. │   ├─base::withCallingHandlers(...)
  7. │   └─mask$eval_all_filter(dots, env_filter)
  8. └─dplyr::across(dplyr::vars(a, b), ~dplyr::any_vars(!is.na(.)))
  9.   └─dplyr:::across_setup(...)
 10.     └─tidyselect::eval_select(cols, data = across_cols)
 11.       └─tidyselect:::eval_select_impl(...)
 12.         ├─tidyselect:::with_subscript_errors(...)
 13.         │ ├─base::tryCatch(...)
 14.         │ │ └─base:::tryCatchList(expr, classes, parentenv, handlers)
 15.         │ │   └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
 16.         │ │     └─base:::doTryCatch(return(expr), name, parentenv, handler)
 17.         │ └─tidyselect:::instrument_base_errors(expr)
 18.         │   └─base::withCallingHandlers(...)
 19.         └─tidyselect:::vars_select_eval(...)
 20.           └─tidyselect:::walk_data_tree(expr, data_mask, context_mask)
 21.             └─tidyselect:::as_indices_sel_impl(...)
 22.               └─tidyselect:::as_indices_impl(x, vars, strict = strict)
 23.                 └─vctrs::vec_as_subscript(x, logical = "error")
Error: Problem with `filter()` input `..1`.
ℹ Input `..1` is `dplyr::across(dplyr::vars(a, b), ~dplyr::any_vars(!is.na(.)))`.
x Must subset columns with a valid subscript vector.
x Subscript has the wrong type `quosures`.
ℹ It must be numeric or character.
Backtrace:
     █
  1. ├─global::function_new_dplyr(example_table, a, b)
  2. │ └─`%<>%`(...)
  3. ├─dplyr::filter(., dplyr::across(dplyr::vars(!!!columns), ~dplyr::any_vars(!is.na(.))))
  4. ├─dplyr:::filter.data.frame(...)
  5. │ └─dplyr:::filter_rows(.data, ..., caller_env = caller_env())
  6. │   ├─base::withCallingHandlers(...)
  7. │   └─mask$eval_all_filter(dots, env_filter)
  8. ├─dplyr::across(dplyr::vars(a, b), ~dplyr::any_vars(!is.na(.)))
  9. │ └─dplyr:::across_setup(...)
 10. │   └─tidyselect::eval_select(cols, data = across_cols)
 11. │     └─tidyselect:::eval_select_impl(...)
 12. │       └─tidyselect:::with_subscript_errors(...)
 13. │         └─base::tryCatch(...)
 14. │           └─base:::tryCatchList(expr, classes, parentenv, handlers)
 15. │             └─base:::tryCatchOne(expr, names, parentenv, handlers[[1L]])
 16. │               └─value[[3L]](cond)
 17. │                 └─rlang::cnd_signal(cnd)
 18. ├─rlang:::signal_abort(x)
 19. │ └─base::stop(fallback)
 20. └─(function (e) ...
```


</details>
