#' Tidy cohen.d output
#'
#' This function creates a tidy dataframe from effsize::cohend.d
#' @export
tidy_cohen <- function(cohendat) {
  cohens_stuff %>%
    unlist() %>%
    enframe() %>%
    t() %>%
    as_tibble() %>%
    set_names(.[1,]) %>%
    .[-1,] %>% janitor::clean_names()
}

