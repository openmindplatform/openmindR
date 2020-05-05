#' Parse Lifehacks
#'
#' Parses Lifehacks from GuidedTrack AND AirTable data
#'
#'@param x LifeHack String
#'@param var How to name the variable
#'@export
parse_lifehacks <- function(x, var) {

  # x <- "0, Acknowledge that your abilities are fluid, Challenge your motivated reasoning, Analyze your own beliefs, Respect and understand other people's elephants"
  # var <- "helo"


  if (!is.na(x)) {
    if (nchar(x) == 0) {
      x <- NA_character_
    }
  }

  if (!is.na(x)) {
    if (nchar(x) == 0) {
      x <- NA_character_
    }
  }

  var_names <- paste0(var, 1:5)

  len_string <- x %>% stringr::str_split(",") %>% unlist %>% length()

  if (is.na(x) | len_string != 5 ) {
    na_dat <- rep(NA_character_, 5) %>%
      tibble::tibble() %>% t() %>%
      tibble::as_tibble() %>%
      magrittr::set_colnames(var_names)

    return(na_dat)
  }

  x %>%
    stringr::str_split(",") %>%
    purrr::map(~stringr::str_remove_all(.x, "\\[|\\]")) %>%
    purrr::map(~stringr::str_remove_all(.x, '\\"|\\"')) %>%
    purrr::map(stringr::str_trim) %>%
    purrr::map(t) %>%
    purrr::map_dfr(tibble::as_tibble) %>%
    magrittr::set_colnames(var_names)
}


#' Count LifeHacks Chosen and LifeHacks completed
#'
#' Happens for each Step.
#'
#'@param lh_data a dataset that has gone through \code{\link{om_parse_lifehacks}}
#'@param lifehack Specify LifeHacks from which step you want to count
#'@param lifehackcomplete Specify LifeHacksComplete from which step you want to count
#'@param step Swhich step are we talking about
lh_chosen_completed <- function(lh_data, lifehack, lifehackcomplete, step) {

  # lh_data <- tibble::tibble(LifeHack1 = 1, LifeHacksComplete1 = 3, What = 3)

  if (!any(colnames(lh_data) %in% c("LifeHack1", "LifeHacksComplete1"))) {
    stop("Parsed LifeHack data not found. You first need to run om_parse_lifehacks.")
  }

  lifehack <- dplyr::enquo(lifehack)
  lifehackcomplete <- dplyr::enquo(lifehackcomplete)
  # lifehackuseful <- enquo(lifehackuseful)

  lh_data %>%
    count(!!lifehack, !!lifehackcomplete) %>%
    dplyr::filter(!(!!lifehack == "No Life Hack" & !!lifehackcomplete == "Yes")) %>%
    dplyr::filter(!(!!lifehack == "No Life Hack" & !!lifehackcomplete == "No")) %>%
    dplyr::filter(!(!!lifehackcomplete == "Non Response")) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(!!lifehack) %>%
    dplyr::mutate(LifeHacksN = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LifeHacksTotal = sum(n)) %>%
    dplyr::mutate(LifeHacksPerc = specify_decimal(LifeHacksN/LifeHacksTotal * 100, 2)) %>%
    dplyr::select(!!lifehack, LifeHacksN, LifeHacksPerc, LifeHacksTotal, everything()) %>%
    dplyr::rename(LifeHacksCompleteN = n) %>%
    dplyr::mutate(LifeHacksCompletePerc = specify_decimal(LifeHacksCompleteN/LifeHacksN * 100, 2)) %>%
    dplyr::mutate(Step = step) %>%
    magrittr::set_colnames(c("LifeHack", "LifeHacksN", "LifeHacksPerc",
                       "LifeHacksTotal", "LifeHacksComplete",
                       "LifeHacksCompleteN", "LifeHacksCompletePerc",
                       "Step")) %>%
    dplyr::mutate(Step = paste("Step ", step)) %>%
    dplyr::select(Step, tidyselect::everything())

}


#' Count LifeHacks Useful
#'
#' Happens for each Step.
#'
#'@param lh_data a dataset that has gone through \code{\link{om_parse_lifehacks}}
#'@param lifehack Specify LifeHacks from which step you want to count
#'@param lifehackcomplete Specify LifeHacksComplete from which step you want to count
#'@param useful Specify LifeHacksUseful from which step you want to count
#'@param step Swhich step are we talking about
lh_useful <- function(lh_data, lifehack, lifehackcomplete, useful, step) {

  if (!any(colnames(lh_data) %in% c("LifeHack1", "LifeHacksComplete1"))) {
    stop("Parsed LifeHack data not found. You first need to run om_parse_lifehacks.")
  }

  lifehack <- dplyr::enquo(lifehack)
  lifehackcomplete <- dplyr::enquo(lifehackcomplete)
  useful <- dplyr::enquo(useful)

  # browser()

  lh_data %>%
    dplyr::count(!!lifehack, !!lifehackcomplete, !!useful, .drop = F) %>%
    tidyr::drop_na(!!lifehack) %>%
    dplyr::filter(!!useful != "Non Response") %>%
    dplyr::filter(!(!!lifehack == "No Life Hack" & !!lifehackcomplete == "Yes")) %>%
    dplyr::filter(!(!!lifehack == "No Life Hack" & !!lifehackcomplete == "No")) %>%
    dplyr::filter(!(!!lifehack == "No Life Hack" & !!useful == "Yes")) %>%
    dplyr::filter(!(!!lifehack == "No Life Hack" & !!useful == "No")) %>%
    dplyr::filter(!(!!lifehackcomplete == "No" & !!useful == "Yes")) %>%
    dplyr::filter(!(!!lifehackcomplete == "No" & !!useful == "No")) %>%
    dplyr::group_by(!!lifehack) %>%
    dplyr::mutate(LifeHackUsefulTotal = sum(n)) %>%
    dplyr::mutate(LifeHackUsefulPerc = specify_decimal(n/LifeHackUsefulTotal * 100, 2)) %>%
    dplyr::ungroup() %>%
    magrittr::set_colnames(c("LifeHack", "LifeHackComplete", "LifeHackUseful", "LifeHackUsefulN",
                "LifeHackUsefulTotal", "LifeHackUsefulPerc")) %>%
    dplyr::select(LifeHack, LifeHackComplete, LifeHackUseful, LifeHackUsefulN, LifeHackUsefulPerc, LifeHackUsefulTotal) %>%
    dplyr::mutate(LifeHackUseful = forcats::fct_relevel(LifeHackUseful, c("Yes", "No"))) %>%
    dplyr::arrange(LifeHackUseful) %>%
    dplyr::mutate(Step = paste("Step ", step)) %>%
    dplyr::select(Step, tidyselect::everything())
}


lh_useful_num <- function(lh_data, lifehack, lifehackuseful, mod) {
  lh_data %>%
    group_by({{ lifehack }}) %>%
    summarize(LifeHacksUsefulMean = mean({{ lifehackuseful }}, na.rm = T)) %>%
    set_names(c("LifeHack", "LifeHacksUsefulMean")) %>%
    filter(LifeHack != "No Life Hack") %>%
    drop_na(LifeHack) %>%
    dplyr::mutate(Step = paste("Step ", mod)) %>%
    dplyr::select(Step, tidyselect::everything())
}
#' Count LifeHacks Reason
#'
#' Happens for each Step.
#'
#'@param lh_data a dataset that has gone through \code{\link{om_parse_lifehacks}}
#'@param lifehack Specify LifeHacks from which step you want to count
#'@param lifehackcomplete Specify LifeHacksComplete from which step you want to count
#'@param reason Specify LifeHacksReason from which step you want to count
#'@param step Swhich step are we talking about
lh_reason <- function(lh_data, lifehack, lifehackcomplete, reason, step) {

  if (!any(colnames(lh_data) %in% c("LifeHack1", "LifeHacksComplete1"))) {
    stop("Parsed LifeHack data not found. You first need to run om_parse_lifehacks.")
  }

  lifehack <- dplyr::enquo(lifehack)
  lifehackcomplete <- dplyr::enquo(lifehackcomplete)
  reason <- dplyr::enquo(reason)

  lh_data %>%
    dplyr::rename(LifeHacksReason = !!reason) %>%
    dplyr::mutate(LifeHacksReason = stringr::str_split(LifeHacksReason, "\\)\\(")) %>%
    tidyr::unnest(LifeHacksReason) %>%
    dplyr::mutate(LifeHacksReason = str_remove_all(LifeHacksReason, "\\(|\\)")) %>%
    dplyr::count(!!lifehack, !!lifehackcomplete, LifeHacksReason, .drop = F) %>%
    tidyr::drop_na(!!lifehack) %>%
    dplyr::filter(LifeHacksReason != "Non Response") %>%
    dplyr::filter(!!lifehack != "No Life Hack") %>%
    dplyr::filter(!(!!lifehack == "No Life Hack" & LifeHacksReason == "Yes")) %>%
    dplyr::filter(!(!!lifehack == "No Life Hack" & LifeHacksReason == "No")) %>%
    dplyr::filter(!(!!lifehack == "No Life Hack" & LifeHacksReason == "Yes")) %>%
    dplyr::filter(!(!!lifehack == "No Life Hack" & LifeHacksReason == "No")) %>%
    dplyr::filter(!(!!lifehackcomplete == "No" & LifeHacksReason == "Yes")) %>%
    dplyr::filter(!(!!lifehackcomplete == "No" & LifeHacksReason == "No")) %>%
    dplyr::filter(!!lifehackcomplete == "No") %>%
    dplyr::group_by(!!lifehack) %>%
    dplyr::mutate(total = sum(n))  %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc = paste0(round((n/total*100)), "%")) %>%
    dplyr::select(LifeHacksReason, tidyselect::everything()) %>%
    magrittr::set_colnames(c("LifeHacksReason", "LifeHack", "LifeHacksComplete", "LifeHacksReasonN", "LifeHacksReasonTotal", "LifeHacksReasonPerc")) %>%
    dplyr::mutate(Step = paste0("Step ", step)) %>%
    dplyr::arrange(LifeHacksReasonTotal) %>%
    dplyr::select(Step, LifeHack, LifeHacksComplete, LifeHacksReason, LifeHacksReasonN, LifeHacksReasonPerc, LifeHacksReasonTotal)
}


#' Parse LifeHacks
#'
#' Parse LifeHacks and make sure that they all make sense by checking them.
#'
#' @param cleaned_dat a dataset which includes LifeHacks to be parsed and checked
#' @examples
#' ## get key
#' key <- read_lines("../../Keys/airtabler.txt")
#'
#' ## download participant progress data
#' pp_dat <- om_download_at(key, tables = "ParticipantProgress")
#'
#'
#' ## this parses all Lifehack data so far
#' parsed_lh <-  om_parse_lifehacks(pp_dat)
#'
#'
#' ## if you just want the newest life hack data you can filter by OpenMind version
#' parsed_lh <-  pp_dat %>%
#'   ## making sure OpenMindVersion is numeric
#'   mutate(OpenMindVersion = as.numeric(OpenMindVersion)) %>%
#'   ## Only include OM Version 3 and above
#'   filter(OpenMindVersion >= 3) %>%
#'   ## parse Life hacks
#'   om_parse_lifehacks()
#'
#' ## just select the OMID and all LifeHack data (remove all other PP variables)
#' parsed_lh %>%
#'   select(OMID, LifeHack1:LifeHacksReason5)
#' @export
om_parse_lifehacks <- function(cleaned_dat, OpenMindVersion = "Before 4.0") {
  lifehacks_dat <- cleaned_dat %>%
    dplyr::arrange(LifeHacksChosen) %>%
    # slice(9856) %>%
    pipe_print("Parsing LifeHacksChosen") %>%
    dplyr::mutate(LifeHacksChosen_data = LifeHacksChosen %>% purrr::map(~parse_lifehacks(.x, "LifeHack"))) %>%
    pipe_print("Parsing LifeHacksComplete") %>%
    dplyr::mutate(LifeHacksComplete_data = LifeHacksComplete %>% purrr::map(~parse_lifehacks(.x, "LifeHacksComplete"))) %>%
    pipe_print("Parsing LifeHacksUseful") %>%
    dplyr::mutate(LifeHacksUseful_data = LifeHacksUseful %>% purrr::map(~parse_lifehacks(.x, "LifeHacksUseful"))) %>%
    pipe_print("Parsing LifeHacksReason") %>%
    dplyr::mutate(LifeHacksReason_data = LifeHacksReason %>% purrr::map(~parse_lifehacks(.x, "LifeHacksReason"))) %>%
    pipe_print("Unnesting..") %>%
    tidyr::unnest(LifeHacksChosen_data) %>%
    tidyr::unnest(LifeHacksComplete_data) %>%
    tidyr::unnest(LifeHacksUseful_data) %>%
    tidyr::unnest(LifeHacksReason_data)

  ## TODO: Make lifehacks so that you can specify in top level functions which ones you want to provide
  if (OpenMindVersion == "Before 4.0") {
    step1_hacks <- c("Understand what's influencing your viewpoint",
                     "Engage in perspective taking",
                     "Seek out diverse perspectives")

    step2_hacks <- c("Acknowledge that your abilities are fluid",
                     "View each mistake as a learning opportunity",
                     "Challenge yourself to do things you haven't already mastered",
                     "Approach disagreements like a detective",
                     "Vanquish your fear of being wrong")

    step3_hacks <- c("Challenge your motivated reasoning",
                     "Challenge your confirmation bias")



    step4_hacks <- c("Analyze your own beliefs",
                     "Dig deeper on disagreement",
                     "Mind travel",
                     "Examine different moral matrices")

    step5_hacks <- c("Respect and understand other people's elephants",
                     "Be intellectually humble",
                     "Appeal to other people's elephants",
                     "Appeal to others people's elephants",
                     "Challenge your assumptions")
  }

  if (OpenMindVersion == "4.0+") {

    step1_hacks <- c("Challenge your motivated reasoning",
                     "Challenge your confirmation bias")

    step2_hacks <- c("Analyze your own beliefs",
                     "Mind travel",
                     "Examine different moral matrices")

    step3_hacks <- c("Vanquish your fear of being wrong",
                     "Acknowledge that your abilities are fluid",
                     "Approach disagreements like a detective")

    step4_hacks <- c("Understand what's influencing your viewpoint",
                     "Engage in perspective taking",
                     "Seek out diverse perspectives")

    step5_hacks <- c("Respect and understand other people's elephants",
                     "Be intellectually humble",
                     "Appeal to other people's elephants",
                     "Appeal to others people's elephants",
                     "Challenge your assumptions")
  }

  lifehacks_dat <- lifehacks_dat %>%
    dplyr::mutate(LifeHack1 = ifelse(LifeHack1 %in% step1_hacks | LifeHack1 == 0, LifeHack1, NA)) %>%
    dplyr::mutate(LifeHack2 = ifelse(LifeHack2 %in% step2_hacks | LifeHack2 == 0, LifeHack2, NA)) %>%
    dplyr::mutate(LifeHack3 = ifelse(LifeHack3 %in% step3_hacks | LifeHack3 == 0, LifeHack3, NA)) %>%
    dplyr::mutate(LifeHack4 = ifelse(LifeHack4 %in% step4_hacks | LifeHack4 == 0, LifeHack4, NA)) %>%
    dplyr::mutate(LifeHack5 = ifelse(LifeHack5 %in% step5_hacks | LifeHack5 == 0, LifeHack5, NA)) %>%
    dplyr::mutate(LifeHack5 = ifelse(LifeHack5 == "Appeal to others people's elephants",
                                     "Appeal to other people's elephants",
                                     LifeHack5))# %>% count(LifeHack5, sort = T)


  lh_data <- lifehacks_dat %>%
    dplyr::mutate_at(dplyr::vars(LifeHack1:LifeHack5), ~ifelse(.x == 0, "No Life Hack", .x)) %>%
    dplyr::mutate_at(dplyr::vars(LifeHacksComplete1:LifeHacksComplete5), ~ifelse(.x == 0, "Non Response", .x)) %>%
    dplyr::mutate_at(dplyr::vars(LifeHacksReason1:LifeHacksReason5), ~ifelse(.x == 0, "Non Response", .x)) %>%
    dplyr::mutate_at(dplyr::vars(LifeHacksUseful1:LifeHacksUseful5), ~ifelse(.x == 0, "Non Response", .x))


  return(lh_data)



}


#' Parse LifeHacks
#'
#' Parse LifeHacks and make sure that they all make sense by checking them.
#'
#' @param cleaned_dat a dataset which includes LifeHacks to be parsed and checked
#' @export
om_lifehacks_summary <- function(lh_data, ut_filter = "All") {

  if (!any(colnames(lh_data) %in% c("LifeHack1", "LifeHacksComplete1"))) {
    stop("Parsed LifeHack data not found. You first need to run om_parse_lifehacks.")
  }


  lh_data <- lh_data %>%
    dplyr::mutate(UserType = as.character(UserType)) %>%
    dplyr::mutate(UserType = ifelse(AccessCode == "IndividualUser", "IndividualUser", UserType)) %>%
    dplyr::mutate(UserType = dplyr::case_when(
      UserType == "college" ~ "College Users",
      UserType == "corp" ~ "Corporate Users",
      UserType == "highschool" ~ "High School Users",
      UserType == "orgadult" ~ "Adult Organization Users",
      UserType == "orgstudent" ~ "College Organization Users",
      UserType == "IndividualUser" ~ "Individual Users",
      T ~ UserType
    ))

  if (ut_filter == "All") {
    lh_data <- lh_data
  } else if(ut_filter != "All") {
    lh_data <- lh_data %>%
      dplyr::filter(UserType == ut_filter)
  }


  # lh_data <- cleaned_dat

  # debugonce(lh_chosen_completed)

  lh_chosen <- dplyr::bind_rows(
    lh_data %>%
      lh_chosen_completed(LifeHack1, LifeHacksComplete1, step = 1),
    lh_data %>%
      lh_chosen_completed(LifeHack2, LifeHacksComplete2, step = 2),
    lh_data %>%
      lh_chosen_completed(LifeHack3, LifeHacksComplete3, step = 3),
    lh_data %>%
      lh_chosen_completed(LifeHack4, LifeHacksComplete4, step = 4)
  )



  lh_useful_data <- dplyr::bind_rows(
    lh_data %>%
      lh_useful(LifeHack1, LifeHacksComplete1, LifeHacksUseful1, step = 1),
    lh_data %>%
      lh_useful(LifeHack2, LifeHacksComplete2, LifeHacksUseful2, step = 2),
    lh_data %>%
      lh_useful(LifeHack3, LifeHacksComplete3, LifeHacksUseful3, step = 3),
    lh_data %>%
      lh_useful(LifeHack4, LifeHacksComplete4, LifeHacksUseful4, step = 4),
  )


  lh_reason_data <- dplyr::bind_rows(

    lh_data %>%
      dplyr::select(-LifeHacksReason) %>%
      lh_reason(LifeHack1, LifeHacksComplete1, LifeHacksReason1, step = 1),

    lh_data %>%
      dplyr::select(-LifeHacksReason) %>%
      lh_reason(LifeHack2, LifeHacksComplete2, LifeHacksReason2, step = 2),

    lh_data %>%
      dplyr::select(-LifeHacksReason) %>%
      lh_reason(LifeHack3, LifeHacksComplete3, LifeHacksReason3, step = 3),

    lh_data %>%
      dplyr::select(-LifeHacksReason) %>%
      lh_reason(LifeHack4, LifeHacksComplete4, LifeHacksReason4, step = 4)

  )




  lifehacks_summary <-  left_join(
    lh_chosen %>%
      dplyr::filter(LifeHacksComplete == "Yes") %>%
      dplyr::select(Step, LifeHack, LifeHacksPerc, LifeHacksCompletePerc),

    lh_useful_data %>%
      dplyr::filter(LifeHackUseful == "Yes") %>%
      dplyr::select(Step, LifeHack, LifeHackUsefulPerc)
  ) %>%
    dplyr::bind_cols(
      lh_reason_data %>%
        dplyr::group_by(Step, LifeHack) %>%
        dplyr::slice(1:3) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(LifeHacksReason = paste0(rep(1:3, 9), ". ", LifeHacksReason, " (", LifeHacksReasonPerc, ")")) %>%
        # dplyr::mutate(LifeHacksReason = )
        dplyr::select(Step, LifeHack, LifeHacksReason) %>%
        dplyr::group_by(Step, LifeHack) %>%
        dplyr::summarise(LifeHacksReason = paste0(LifeHacksReason, collapse = "<br>")) %>% dplyr::select(LifeHacksReason) %>%
        dplyr::ungroup()
    )


  step5_data <- lh_data %>%
    count(LifeHack5) %>%
    dplyr::filter(LifeHack5 != "No Life Hack") %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::mutate(perc = specify_decimal(n/total*100, 2)) %>%
    dplyr::rename(LifeHack = LifeHack5) %>%
    dplyr::mutate(Step = "Step 5") %>%
    dplyr::rename(LifeHacksPerc = perc) %>%
    dplyr::select(Step, LifeHack, LifeHacksPerc)


  lifehacks_summary <- lifehacks_summary %>%
    dplyr::select(-Step1) %>%
    dplyr::bind_rows(
      step5_data
    ) %>%
    dplyr::mutate(Type = ut_filter)


  return(lifehacks_summary)
}



#' Parse LifeHacks
#'
#' Parse LifeHacks and make sure that they all make sense by checking them.
#'
#' @param cleaned_dat a dataset which includes LifeHacks to be parsed and checked
#' @export
om_lifehacks_summary2 <- function(lh_data, ut_filter = "All") {

  if (!any(colnames(lh_data) %in% c("LifeHack1", "LifeHacksComplete1"))) {
    stop("Parsed LifeHack data not found. You first need to run om_parse_lifehacks.")
  }


  # lh_data <- lh_data %>%
  #   dplyr::mutate(UserType = as.character(UserType)) %>%
  #   dplyr::mutate(UserType = ifelse(AccessCode == "IndividualUser", "IndividualUser", UserType)) %>%
  #   dplyr::mutate(UserType = dplyr::case_when(
  #     UserType == "college" ~ "College Users",
  #     UserType == "corp" ~ "Corporate Users",
  #     UserType == "highschool" ~ "High School Users",
  #     UserType == "orgadult" ~ "Adult Organization Users",
  #     UserType == "orgstudent" ~ "College Organization Users",
  #     UserType == "IndividualUser" ~ "Individual Users",
  #     T ~ UserType
  #   ))

  if (ut_filter == "All") {
    lh_data <- lh_data
  } else if(ut_filter != "All") {
    lh_data <- lh_data %>%
      dplyr::filter(UserType == ut_filter)
  }


  # lh_data <- cleaned_dat

  # debugonce(lh_chosen_completed)

  lh_chosen <- dplyr::bind_rows(
    lh_data %>%
      lh_chosen_completed(LifeHack1, LifeHacksComplete1, step = 1),
    lh_data %>%
      lh_chosen_completed(LifeHack2, LifeHacksComplete2, step = 2),
    lh_data %>%
      lh_chosen_completed(LifeHack3, LifeHacksComplete3, step = 3),
    lh_data %>%
      lh_chosen_completed(LifeHack4, LifeHacksComplete4, step = 4)
  )



  lh_useful_data <- bind_rows(
    lh_data %>%
      lh_useful_num(LifeHack1, LifeHacksUseful1, "1"),
    lh_data %>%
      lh_useful_num(LifeHack2, LifeHacksUseful2, "2"),
    lh_data %>%
      lh_useful_num(LifeHack3, LifeHacksUseful3, "3"),
    lh_data %>%
      lh_useful_num(LifeHack4, LifeHacksUseful4, "4")
  )


  lh_reason_data <- dplyr::bind_rows(

    lh_data %>%
      dplyr::select(-LifeHacksReason) %>%
      lh_reason(LifeHack1, LifeHacksComplete1, LifeHacksReason1, step = 1),

    lh_data %>%
      dplyr::select(-LifeHacksReason) %>%
      lh_reason(LifeHack2, LifeHacksComplete2, LifeHacksReason2, step = 2),

    lh_data %>%
      dplyr::select(-LifeHacksReason) %>%
      lh_reason(LifeHack3, LifeHacksComplete3, LifeHacksReason3, step = 3),

    lh_data %>%
      dplyr::select(-LifeHacksReason) %>%
      lh_reason(LifeHack4, LifeHacksComplete4, LifeHacksReason4, step = 4)

  )




  lifehacks_summary <-  left_join(
    lh_chosen %>%
      dplyr::filter(LifeHacksComplete == "Yes") %>%
      dplyr::select(Step, LifeHack, LifeHacksPerc, LifeHacksCompletePerc),

    lh_useful_data %>%
      dplyr::select(Step, LifeHack, LifeHacksUsefulMean)
  ) %>%
    dplyr::bind_cols(
      lh_reason_data %>%
        dplyr::group_by(Step, LifeHack) %>%
        dplyr::slice(1:3) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(LifeHacksReason = paste0(rep(1:3, 9), ". ", LifeHacksReason, " (", LifeHacksReasonPerc, ")")) %>%
        # dplyr::mutate(LifeHacksReason = )
        dplyr::select(Step, LifeHack, LifeHacksReason) %>%
        dplyr::group_by(Step, LifeHack) %>%
        dplyr::summarise(LifeHacksReason = paste0(LifeHacksReason, collapse = "<br>")) %>% dplyr::select(LifeHacksReason) %>%
        dplyr::ungroup()
    )


  step5_data <- lh_data %>%
    count(LifeHack5) %>%
    dplyr::filter(LifeHack5 != "No Life Hack") %>%
    dplyr::mutate(total = sum(n)) %>%
    dplyr::mutate(perc = specify_decimal(n/total*100, 2)) %>%
    dplyr::rename(LifeHack = LifeHack5) %>%
    dplyr::mutate(Step = "Step 5") %>%
    dplyr::rename(LifeHacksPerc = perc) %>%
    dplyr::select(Step, LifeHack, LifeHacksPerc)


  lifehacks_summary <- lifehacks_summary %>%
    dplyr::select(-Step1) %>%
    dplyr::bind_rows(
      step5_data
    ) %>%
    dplyr::mutate(Type = ut_filter)


  return(lifehacks_summary)
}
