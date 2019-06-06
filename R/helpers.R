#' Tidy cohen.d output
#'
#' This function creates a tidy dataframe from effsize::cohend.d
#' @param cohendat results from Cohen's D
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


#' Install newest openmindR version from GitHub
#'
#' This function installs the newest openmindR version from GitHub
#' @param ... arguments to install_github
#' @export
install_openmindR <- function(...) {

  devtools::install_github("openmindplatform/openmindR", ...)

  library(openmindR)

}


