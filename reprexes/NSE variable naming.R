

name <- sprintf("CDR\U00AE_plus_NACC_FTLD")

iris %>% tibble() %>% mutate(!!name := (Sepal.Length + Sepal.Width))

