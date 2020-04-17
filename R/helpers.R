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

#' @export
check_for_pkg <- function(pkg) {
  n_package <- tibble::as_tibble(installed.packages()) %>%
    dplyr::filter(Package == pkg) %>%
    nrow

  result <- F
  if(n_package == 1){
    result <- T
  }

  return(result)
}



#' SQL Database Append
#'
#' This is a helper function that will write a dataframe to a SQL database
#'
#' @param path path to SQL database
#' @param tbl name of the table in SQL database
#' @param data the object dataframe that goes into the SQL database
#' @export
db_append <- function(path, tbl, data) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  if(!is.null(DBI::dbListTables(con))) {
    DBI::dbWriteTable(con, tbl, data, append = T)
  } else {
    DBI::dbWriteTable(con, tbl, data)
  }
  DBI::dbDisconnect(con)

}


#' SQL Database Retrieve
#'
#' This is a helper function that will retreive a dataframe to a SQL database
#'
#' @param tbl_dat which table from the SQL database do you want to retrieve
#' @param path path to database
#' @export
db_get_data <- function(tbl_dat, path = "sql_data/omdata.db") {
  # con <- dbConnect(RSQLite::SQLite(), "../om_metrics_report/sql_data/omdata.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  out <- con %>%
    dplyr::tbl(tbl_dat) %>%
    dplyr::collect()

  DBI::dbDisconnect(con)

  return(out)
}

#' Specify a decimal
#'
#' @param x a number to be rounded
#' @param k round to which position after the comma
#' @export
specify_decimal <- function(x, k) {
  trimmed <- trimws(format(round(x, k), nsmall=k))

  return(trimmed)
}


#' SQL Database Remove
#'
#' This is a helper function that will remove a dataframe from a SQL database
#'
#' @param path path to database
#' @param datasets which table from the SQL database do you want to remove
#' @param remove_cleaned_data boolean remove all datasets that are created through the cleaning script
#' @export
db_remove <- function(path, datasets = NULL, remove_cleaned_data = T) {

  # path <- "sql_data/omdata.db"

  con <- DBI::dbConnect(RSQLite::SQLite(), path)

  if (remove_cleaned_data) {

    remove_em <- DBI::dbListTables(con) %>% purrr::discard(~stringr::str_detect(.x, "dat\\."))

    datasets <- remove_em
  }

  datasets %>%
    purrr::map(~DBI::dbRemoveTable(con, name = .x))

  DBI::dbDisconnect(con)

}


#' Get MailChimp Member List
#'
#' This function wraps the python module "mailchimp3" to get all members of a list through the Mailchimp API
#'
#' @param list_id unique id that denotes MailChimp list
#' @export
mailchimp_member_list <- function(list_id = "c7e9a42a8f") {

  mc <- reticulate::import("mailchimp3")

  client <- mc$MailChimp(Sys.getenv("MAILCHIMP_KEY"), 'OpenMind')

  members_list <- client$lists$members$all(list_id, get_all = T, fields = "members.email_address,members.merge_fields,members.id")

  return(members_list)

}
