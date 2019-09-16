#' Diagnostic Dataset
#'
#' Get diagnostic data from parser
#' @param parsed_data Parsed dataset that is an output from `safely`
#' @export
get_error_diag <- function(parsed_data) {
  parsed_data %>%
    purrr::map("error") %>%
    purrr::map(~ifelse(is.null(.x), "No Error", .x)) %>%
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
#' @param x a string
#' @export
om_strsplit <- function(x,
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

#' This function counts NAs
#'
#' Can be used with mutate_all
#'
#'
#' @param x Variable
#' @export
count_na <- function(x) sum(is.na(x), na.rm = T)


#' This function removes characters in AirTable and turns them to date
#'
#' @param date date
#' @export
chr_to_date <- function(date) {
  stringr::str_remove_all(date, "th,|rd,|st,|nd,") %>% as.Date(format = "%B %d %Y")
}

#' This function assigns colnames
#'
#' This function is necessary because variable names have to be guessed
#' Becomes obsolute in OpenMind version 2.911
#'
#' @param col_length how long is the column
#' @param b1q logical, was was B1 found?
#' @param b2q logical, was was B2 found?
#' @param b3q logical, was was B3 found?
#' @export
get_colnames <- function(col_length, b2q, b1q, b3q) {


  if (col_length == 27) {

    return(col_names)

  }

  if (b1q) {

    if (col_length == 25) {

      col_names <- col_names %>%
        discard(~stringr::str_detect(.x, "D6")) %>%
        discard(~stringr::str_detect(.x, "B1"))
      return(col_names)

    }

    if (col_length == 26) {

      col_names <- col_names %>%
        discard(~stringr::str_detect(.x, "B1"))
      return(col_names)

    }

  }



  if (b2q) {
    if (col_length == 26) {

      return(col_names %>% discard(~stringr::str_detect(.x, "D6")))

    }
  }



  if (b3q) {

    if (col_length == 25) {

      col_names <- col_names %>%
        discard(~stringr::str_detect(.x, "D6")) %>%
        discard(~stringr::str_detect(.x, "B3"))
      return(col_names)

    }

    if (col_length == 26) {

      col_names <- col_names %>%
        discard(~stringr::str_detect(.x, "B3"))
      return(col_names)

    }
  }

  ## For V4
  # if (col_length == 22) {
  #   col_names <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12",
  #                  "C1", "C2", "C3", "D1", "D2", "D3", "D4", "D5",
  #                  "Date", "B1")
  #
  #   return(col_names)
  # }



  if(all(c(!b1q, !b2q, !b3q))){
    return(col_names %>% discard(~stringr::str_detect(.x, "D6")))
  }

}


#' Parses Assessment String
#'
#'
#'  Debugging Examples
#'
#'    data <- om_user_data %>%
#' filter(AssessmentVersion >= 5) %>%
#' # filter(PostAssessmentComplete == 1) %>%
#' pull(AssessmentAnswers) %>% .[[2000]]
#'
#' error_dat %>%
#'   filter(errors != "No Error") -> ss
#'
#' error_dat %>%
#'   filter(errors != "No Error")  %>%
#'   pull(OMID) %>% .[[3]]
#'
#' data <- error_dat %>%
#' filter(errors != "No Error")  %>%
#' pull(AssessmentAnswers) %>% .[[3]]
#'
#'
#' data <- om_user_data %>%
#' filter(OMID %in% error_ids)  %>%
#'   mutate(OMID = factor(OMID, levels = error_ids)) %>%
#'   arrange(OMID) %>%
#' pull(AssessmentAnswers) %>%
#'   .[1]
#' #
#' assessment_parser(data)
#'
#'  base_dat$FeedbackAnswers %>%
#'   .[[13]]
#'
#'
#'@param data assessment string
#'@param labels labels string (starts from OpenMind version 2.911)
#'@param verbose enable verbose printing
#'@export
#'
assessment_parser <- function(data, labels = NA, verbose = F) {


  ## If there is no data in the AssessmentAnswer Variable
  ## Return a tibble with a variable indication "no data"
  if (nchar(data) < 3) {
    return(tibble(no_data = 1))
  }


  ## Cleaning open-ended answers seperately

  ## "\\[\\|.*?\\|\\]," This regex grabs B1
  ## "\\[\\<.*?\\>\\]," This regex grabs B3

  ## Extract B1 and B3 since they are too weird to deal with
  open_answers <- data %>%
    stringr::str_extract_all("\\[\\|.*?\\|\\],|\\[\\<.*?\\>\\],") %>%
    magrittr::extract2(1)

  ## if neither B1 nor B3 then open_answers is NULL
  if (length(open_answers) == 0) {

    open_answers <- NULL

    ## if there are three open answers
  } else if (length(open_answers) == 3) {

    ## only keep unique answers
    open_answers <- unique(open_answers)

  }

  ## Check if B1 was asked
  b1_q <-  data %>%
    stringr::str_remove_all("\\[\\|.*?\\|\\],") %>%
    stringr::str_remove_all("\\[\\<.*?\\>\\],") %>%
    stringr::str_detect("\\bB1\\b")

  ## Check if B2 was asked
  b2_q <-  data %>%
    stringr::str_remove_all("\\[\\|.*?\\|\\],") %>%
    stringr::str_remove_all("\\[\\<.*?\\>\\],") %>%
    stringr::str_detect("\\bB2\\b")

  ## Check if B3 was asked
  b3_q <-  data %>%
    stringr::str_remove_all("\\[\\|.*?\\|\\],") %>%
    stringr::str_remove_all("\\[\\<.*?\\>\\],") %>%
    stringr::str_detect("\\bB3\\b")

  ## if B1 is found
  if (b1_q) {

    ## create columns from the open answers
    open_cols <- open_answers %>%
      map_dfc(tibble)

    ## count columns, will be used to detemine if PRE and POST
    n_open_cols <- open_cols %>% ncol()

    ## Create dataframe that will be appended on the final dataset
    open_answers <- open_cols %>%
      ## Set Column names
      set_names(c("B1Pre", "B1Post")[1:n_open_cols]) %>%
      ## some cleaning
      mutate_all(~stringr::str_remove_all(.x, "\\[|\\],"))

  } else if (b3_q) {

    ## create columns from the open answers
    open_cols <- open_answers %>%
      map_dfc(tibble)

    ## count columns, will be used to detemine if PRE and POST
    n_open_cols <- open_cols %>% ncol()

    ## Create dataframe that will be appended on the final dataset
    open_answers <- open_cols %>%
      ## Set Column names
      set_names(c("B3Pre", "B3Post")[1:n_open_cols]) %>%
      ## some cleaning
      mutate_all(~stringr::str_remove_all(.x, "\\[|\\],"))
  }

  clean_s1_string <- "abortion rights, Black Lives Matter, gun control|banning abortion, All Lives Matter, gun rights"


  row_check <- data %>%
    ## Cleaning up target variable with regex
    stringr::str_replace_all("(?<=\\{[^\\}]{0,100}),", " ")  %>%
    stringr::str_replace_all(clean_s1_string, "") %>%
    ## Remove Behavioural Tasks
    stringr::str_remove_all("\\[\\|.*?\\|\\],") %>%
    stringr::str_remove_all("\\[\\<.*?\\>\\],") %>%
    ## Split by Pre and Post
    stringr::str_split("\\}\\],") %>%
    purrr::map(~stringr::str_split(.x, ", (?=[^\\|])")) %>%
    flatten() %>%
    purrr::map(stringr::str_remove_all, "\\[|\\]") %>%
    purrr::map(stringr::str_trim)

  ## if there are three answers
  if (length(row_check) == 3) {

    ## delete duplicate answer
    row_check <- case_when(
      23 < sum(row_check[[1]] == row_check[[2]]) ~ row_check[-1],
      23 < sum(row_check[[2]] == row_check[[3]]) ~ row_check[-2],
      23 < sum(row_check[[1]] == row_check[[3]]) ~ row_check[-3]
    )
  }

  row_check <- row_check %>%
    reduce(c) %>% tibble(V1 = .) %>%
    ## count characters of responses (possible to find out what answer lies behind it?)
    mutate(row_present = nchar(V1))

  ## count how many columns there are
  n_rows <- nrow(row_check)

  ## if below 28 columns, we assume that the person only was asked once
  ## could probably also be done by inserting the value from the start dataframe
  times <- ifelse(n_rows < 28, 1, 2)

  ## Divide n_rows by two if post-assessment was done
  n_rows <- ifelse(n_rows < 28, n_rows, n_rows/2)

  if (is.na(labels)) {
    ## solution before Variable names were introduced
    ## Get column names
    col_names_fix <- rep(get_colnames(col_length = n_rows, # how long is the column
                                      b1q = b1_q,          # was b1 asked
                                      b2q = b2_q,          # was b2 asked
                                      b3q = b3_q),         # was b3 asked
                         times)               # how many times repeat?
  } else if (!is.na(labels)) {
    ## if labels are there
    col_names_fix <- labels %>%
      stringr::str_split(",") %>%
      unlist() %>%
      stringr::str_trim() %>%
      stringr::str_remove_all("\\[|\\]") %>%
      map_chr(~stringr::str_remove_all(.x, "AssessmentCompleted"))

    if (b1_q) {
      col_names_fix <- col_names_fix %>% discard(~stringr::str_detect(.x, "B1"))
    }
    if (b3_q) {
      col_names_fix <- col_names_fix %>% discard(~stringr::str_detect(.x, "B3"))
    }

  }

  ## applying colnames
  col_check <- row_check %>%
    mutate(name = col_names_fix)

  ## if only Pre then return this
  if (times == 1) {
    final_dat <- col_check %>%
      mutate(suffix = c(rep("Pre", n_rows))) %>%
      mutate(varnames = paste0(name, suffix)) %>%
      mutate(varnames = factor(varnames, levels = varnames)) %>%
      select(varnames, V1) %>%
      ## Wide format
      tidyr::spread(varnames, V1)  %>%
      ## Demographic varibales appear only once
      rename_at(vars(D1Pre:D5Pre),  ~stringr::str_remove(.x, "Pre")) %>%
      ## Add open-ended answers
      bind_cols(open_answers)

    ## if Pre and Post then return this
  } else {
    final_dat <- col_check %>%
      mutate(suffix = c(rep("Pre", n_rows), rep("Post", n_rows))) %>%
      mutate(varnames = paste0(name, suffix)) %>%
      mutate(varnames = factor(varnames, levels = varnames)) %>%
      select(varnames, V1) %>%
      ## Wide format
      tidyr::spread(varnames, V1) %>%
      ## Demographic varibales appear only once
      select(-D1Post:-D5Post) %>%
      rename_at(vars(D1Pre:D5Pre),  ~stringr::str_remove(.x, "Pre")) %>%
      ## Add open-ended answers
      bind_cols(open_answers)
  }

  if (verbose) {

    position <- which(base_dat$AssessmentAnswers == data)



    if ((position %% 100 == 0)[1]) {
      cat(glue::glue("{position} rows parsed..\n{round(position / total * 100)}%\n\n"))
    }

    # cat(paste0(position, "\n\n"))

  }

  return(final_dat)

}


#' Spreads steps
#'
#'@param x steps to be spread
#'@param row_dat dataset
#'@export
spread_it <- function(x, row_dat) {

  selected_dat <- dplyr::select(row_dat, "colnames", x)

  check_it <- selected_dat %>% .[1,2] %>% dplyr::pull(1)


  is_zero <- check_it %>% magrittr::equals("0")

  is_one <- check_it %>% magrittr::equals("1")
  is_two <- check_it %>% magrittr::equals("2")
  is_three <- check_it %>% magrittr::equals("3")
  is_four <- check_it %>% magrittr::equals("4")
  is_five <- check_it %>% magrittr::equals("5")

  if (is_zero) return(NULL)
  if (is_one) selected_dat[,1] <- c("Step1", paste0("Step1_Q", 1:5))
  if (is_two) selected_dat[,1] <- c("Step2", paste0("Step2_Q", 1:5))
  if (is_three) selected_dat[,1] <- c("Step3", paste0("Step3_Q", 1:5))
  if (is_four) selected_dat[,1] <- c("Step4", paste0("Step4_Q", 1:5))
  if (is_five) selected_dat[,1] <- c("Step5", paste0("Step5_Q", 1:5))

  selected_dat %>%
    tidyr::spread("colnames", x)

}

#' Clean Feedback Answers
#'
#'@param x string
#'@param row_dat dataset
#'@export
clean_fa_string <- function(x) {
  stringr::str_trim(x) %>%
    stringr::str_remove(", ") %>%
    stringr::str_remove_all("\\]") %>%
    ifelse(. == ",", NA, .) %>%
    ifelse(. == "\\]", NA, .)
}





#' This function parses feedback from AirTable (dat.par)
#'
#'@param raw_input AirTable data: column FeedbackAnswers
#'@export
parse_feedback_at <- function(raw_input) {

  # raw_input <- "[2, (not asked), 10, (not asked), |How it is interactive|, |Videos|], [2, (not asked), 8, (not asked), |The research about mindsets|, |Videos|], [3, (not asked), 10, (not asked), |The examples|, |Videos|], [4, (not asked), 10, (not asked), |It’s interactive|, |Videos|], [5, (not asked), 10, (not asked), |How it’s interactive|, |Videos|]"
  # if (is.na(raw_input)) return(tibble(Step1 = NA))

  if (is.na(raw_input)) {
    return(dplyr::tibble(no_data = 1))
  }

  raw_input %>%
    openmindR::om_strsplit("],", type = "after") %>%
    purrr::map(~stringr::str_split(.x, ",")) %>%
    magrittr::extract2(1) %>%
    purrr::map(~{
      fixed_answers <- .x[1:4]
      open_answers <- .x[5:length(.x)] %>%
        glue::glue_collapse(",") %>%
        stringr::str_split(", \\|", n = 2)
      # stringr::str_split("\\.,|\\b , \\b")

      return(list(fixed_answers = fixed_answers,
                  open_answers = open_answers))
    }) -> split_string

  clean_fix <- split_string %>%
    purrr::map("fixed_answers") %>%
    dplyr::bind_cols()


  clean_open <- split_string %>%
    purrr::map("open_answers") %>%
    purrr::flatten() %>%
    dplyr::bind_cols()

  row_dat <- dplyr::bind_rows(clean_fix, clean_open) %>%
    dplyr::mutate_all(function(x) ifelse(nchar(x) == 0, NA, x)) %>%
    dplyr::mutate_all(function(x) stringr::str_remove_all(x, "\\[\\[|\\[")) %>%
    purrr::set_names(.[1,] %>% .[1:ncol(.)]) %>%
    janitor::clean_names() %>%
    dplyr::mutate(colnames = c("Step", paste0("X", 1:5))) %>%
    dplyr::mutate_all(stringr::str_trim)

  ## Remove duplicate steps
  row_dat <- row_dat[,!(row_dat[1, ] %>% purrr::transpose() %>% duplicated())]
  row_dat <- row_dat[,!(row_dat %>% slice(1) %>% unlist() %>% as.vector() %>% duplicated())]


  ## Remove steps outside of 1 to 5 and Step
  # row_dat <- row_dat[, row_dat[1, ] %>% transpose() %>% .[,1] %>% is_in(c(1:5, "Step"))]

  final_dat <- row_dat %>%
    colnames() %>%
    purrr::discard(. == "colnames") %>%
    purrr::map_dfc(~spread_it(.x, row_dat)) %>%
    dplyr::as_tibble()

  return(final_dat)

}
