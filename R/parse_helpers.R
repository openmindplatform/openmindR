#' Diagnostic Dataset
#'
#' Get diagnostic data from parser
#' @param parsed_data Parsed dataset that is an output from `safely`
#' @export
get_error_diag <- function(parsed_data) {
  parsed_data %>%
    map("error") %>%
    map(~ifelse(is.null(.x), "No Error", .x)) %>%
    unlist() %>%
    tibble(errors = .) %>%
    bind_cols(base_dat) %>%
    mutate(fake = ifelse(errors == "No Error", 0, 1)) %>%
    mutate(UserType = as.factor(UserType))
}


#' This function allows you to print a message in a pipe chain
#'
#' @param data Dataset pipethrough
#' @param message What message should be printed?
#' @export
pipe_print <- function(data, message) {print(message); data}

#' This function allows you to split a string and keep the delimeter
#'
#' @param x String
strsplit <- function(x,
                     split,
                     type = "remove",
                     perl = FALSE,
                     ...) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl, ...)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE,
                          ...)
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE,
                          ...)
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}



