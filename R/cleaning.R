#' This function filters down a dataframe by the desired characteristics.
#'
#'
#' @param app.dat Assessment data from AirTable
#' @param n_assessments {AssessmentsDone} Filter down to what asssesments? (Should be 1, 2 and/or 3)
#' @param version {AssessmentVersion} Filter down to what asssesment version?
#' @param accesscode {AccessCode} Filter down to specific AccessCodes
#' @export
om_filter_data <- function(app.dat, n_assessments = NULL,
                       version = NULL, accesscode = NULL) {

  if (!is.null(n_assessments)) {
    app.dat <- app.dat %>%
      mutate(AssessmentsDone = as.numeric(AssessmentsDone)) %>%
      filter(AssessmentsDone %in% n_assessments)
  }
  if (!is.null(version)) {
    app.dat <- app.dat %>%
      # mutate(AssessmentVersion = as.numeric(AssessmentVersion)) %>%
      filter(AssessmentVersion %in% version)
  }
  if (!is.null(accesscode)) {

    ## turn AccessCode and search string to lower
    app.dat <- app.dat %>% mutate(AccessCode2 = str_to_lower(AccessCode))
    accesscode <- str_to_lower(accesscode)

    ## if vector
    if (length(accesscode) >= 2) {
      app.dat <- app.dat %>%
        filter(AccessCode2 %in% accesscode) %>%
        select(-AccessCode2)
    ## if single string
    } else
      app.dat <- app.dat %>%
        filter(str_detect(AccessCode2, accesscode)) %>%
        select(-AccessCode2)
  }

  return(app.dat)

}

#' This calculates the correct score for each step
#'
#' @param StepScomplete (0/1) Was Step completed?
#' @param Stepscores What was the score on the step?
#' @param StepQuestionTotals How many Question were there in total?
#' @export
calc_correct <- function(StepsComplete, StepsScores, StepQuestionTotals) {

  ifelse(StepsComplete == 1, StepsScores/StepQuestionTotals, NA)
}

#' Performs cleaning on ParticipantProgress data from AirTable
#'
#' Creates the following measures
#' \itemize{
#'   \item StepTimes1 to StepTimes5: Duration in minutes to complete a step
#'   \item StepCorrect1 to StepCorrect5: Percentage of correct answers for each step
#'   \item FeedbackAnswers Q1 to Q5
#' }
#'
#' @param dat.par ParticipantProgress data from AirTable
#' @param ... Arguments for select to get more from ParticipantProgress
#' @export
om_clean_par <- function(dat.par, ...) {

  dat.par %>%
    ## seperating StepsComplete brackets
    separate(StepsComplete, into = paste("StepsComplete", 1:5, sep = ""), remove = F) %>%
    ## seperating StepsScores brackets
    separate(StepScores, into = paste("StepsScores", 1:5, sep = "")) %>%
    ## seperating StepsQuestionTotals brackets
    separate(StepQuestionTotals, into = paste("StepQuestionTotals", 1:5, sep = "")) %>%
    ## seperating StepTines brackets
    separate(StepTimes, into = paste("StepTimes", 1:5, sep = ""),
             sep = ",", remove = F) %>%
    ## Clean up seperated vars
    mutate_at(vars(StepTimes1:StepTimes5), ~str_remove_all(.x, "[^[:digit:]. ]") %>% parse_number) %>%
    mutate_at(vars(StepTimes1:StepTimes5), ~ifelse(.x == 0, NA, .x)) %>%
    ## Making columns numeric where they need to be
    mutate_at(vars(StepsComplete1:StepQuestionTotals5, AppRating), as.numeric)  %>%
    # ## Steps Complete
    ## Now calculating scores
    ## percent correct for each step
    mutate(StepCorrect1 = calc_correct(StepsComplete1, StepsScores1, StepQuestionTotals1)) %>%
    mutate(StepCorrect2 = calc_correct(StepsComplete2, StepsScores2, StepQuestionTotals2)) %>%
    mutate(StepCorrect3 = calc_correct(StepsComplete3, StepsScores3, StepQuestionTotals3)) %>%
    mutate(StepCorrect4 = calc_correct(StepsComplete4, StepsScores4, StepQuestionTotals4)) %>%
    mutate(StepCorrect5 = calc_correct(StepsComplete5, StepsScores5, StepQuestionTotals5)) %>%
    ## Parse Feedback Answers
    mutate(data = FeedbackAnswers %>%
             map(~parse_feedback_at(.x))) %>%
    unnest(data)  %>%
    ## Make Step variables to characters (for merging)
    mutate_at(vars(Step1:Step5_Q5), as.character) %>%
    select(OMID, StepTimes, StepsComplete, StepCorrect1:StepCorrect5, StepTimes1:StepTimes5, Step1:Step5_Q5,
           FeedbackAnswers, FeedbackAnswersVariableNames, AppRating, AppRecommend, at_date, ...)
}


#' Creates several measures of Political Orientation
#'
#' Creates the following measures of Political Orientation
#' \itemize{
#'   \item ppol_raw: a variable that merges Assessment V4 and V5.1 spelling of Political Orientation (D4)
#'   \item ppol: a factor variable ordered from "Very Progressive/left" to "Very Conservative/right". Excludes all other categories as NA (classical liberal etc.)
#'   \item ppol_num: numeric variable ranging from 1 "Very Progressive/left" to 7 "Very Conservative/right"
#'   \item ppol_cat: a factor variable which has two categories "Progressive" and "Conservative". The rest is NA.
#' }
#'
#'
#' @param app.dat Assessment data from AirTable
#' @export
om_clean_ppol <- function(app.dat) {

  ## creates ppol variables

  app.dat %>%
    ## should clean characters in numeric variables first
    ## Making columns numeric where they need to be
    mutate_at(vars(matches(var_strings)), as.numeric) %>%
    ## construct raw ppol variable
    mutate(ppol_raw = D4) %>%
    ## fix the names of categories
    mutate(ppol_raw = case_when(
      str_detect(ppol_raw, "Moderate") ~ "Moderate/Middle-of-the-road",
      str_detect(ppol_raw, "Slightly progressive") ~ "Slightly Progressive/left",
      str_detect(ppol_raw, "not political") ~ "Don't know/Not political",
      str_detect(ppol_raw, "Very progressive") ~ "Very Progressive/left",
      str_detect(ppol_raw, "Slightly conservative") ~ "Slightly Conservative/right",
      str_detect(ppol_raw, "classical liberal") ~ "Libertarian/Classical liberal",
      str_detect(ppol_raw, "Very conservative") ~ "Very Conservative/right",
      str_detect(ppol_raw, "progressive") ~ "Progressive/left",
      T ~ ppol_raw
    )) %>%
    #reorder political orientation into sensible continuum
    mutate(ppol = factor(ppol_raw, levels = c("Very Progressive/left",
                                              "Progressive/left",
                                              "Slightly Progressive/left",
                                              "Moderate/Middle-of-the-road",
                                              "Slightly Conservative/right",
                                              "Conservative/right",
                                              "Very Conservative/right"))) %>%
    ## clean politics variable / make it numeric / only use valid cases
    mutate(ppol_num = as.numeric(ppol)) %>%
    # select(ppol, ppol_num, D4) %>%
    mutate(ppol_cat = case_when(
      ppol_num %in% c(1:3) ~ "Progressives",
      ppol_num %in% c(5:7) ~ "Conservatives",
      T ~ NA_character_
    )) %>%
    mutate(ppol_cat = fct_relevel(ppol_cat, c("Progressives",
                                              "Conservatives")))
  # TODO: For future make more ppol variants
}

#' Creates Political Polarization variables
#'
#' This is lower-level function that belongs to om_construct measure. This function is not meant to be used outside of om_construct_measure.
#' Creates the following measures of Political Polarization
#' \itemize{
#'   \item Q14: Affective Polarization
#'   \item Q15: Ingroup
#'   \item Q16: Outgroup
#'   \item Q17: Ingroup vs. Outgroup Affective Polarization
#' }
#'
#'
#' @param final_dat Assessment data from AirTable
#' @param Q1 Q1 variable
#' @param Q2 Q2 variable
#' @export
polar_measures <- function(final_dat, Q1, Q2) {

  ## if ppol_cat is not found then throw error
  if (not(colnames(final_dat) %in% "ppol_cat" %>% any)) {
    stop("Input data is missing column `ppol_cat`. Please make sure to run om_clean_ppol before you run om_construct_measures.\n")
  }

  ## check which wave
  wave <- case_when(
    str_detect(lazyeval::expr_find(Q1), "Pre") ~ "Pre",
    str_detect(lazyeval::expr_find(Q1), "Post") ~ "Post",
    str_detect(lazyeval::expr_find(Q1), "FollowUp") ~ "FollowUp"
  )

  ## lazy evaluation
  Q1 <- enquo(Q1)
  Q2 <- enquo(Q2)


  final_dat <- final_dat %>%
    # make sure vars are numeric
    mutate_at(vars(!!Q1, !!Q2), as.numeric) %>%
    # compute affective polarization
    mutate(Q14 = abs(!!Q1 - !!Q2)) %>%
    # compute liking for ingroup vs. disliking for outgroup
    ## my ingroup
    mutate(Q15 = ifelse(ppol_cat == "Progressives", !!Q1, !!Q2)) %>%
    # my outgroup
    mutate(Q16 = ifelse(ppol_cat == "Progressives", !!Q2, !!Q1)) %>%
    # compute ingroup v outgroup affective polarization
    mutate(Q17 = abs(Q15 - Q16)) %>%
    rename_at(vars(Q14:Q17), ~paste0(.x, wave))

  return(final_dat)
}

#' Creates Intellectual Humility variable
#'
#' This is lower-level function that belongs to om_construct measure. This function is not meant to be used outside of om_construct_measure.
#' Creates the following measure
#' \itemize{
#'   \item Q18: Intellectual Humility
#' }
#' Function automatically accounts for Assessment Version 4 and 5/5.1.
#'
#'
#' @param final_dat Assessment data from AirTable
#' @param wave Specify wave ("Pre", "Post" or "FollowUp")
#' @export
calc_ih <- function(final_dat, wave) {

  ## if AssessmentVersion is not found then throw error
  if (not(colnames(final_dat) %in% "AssessmentVersion" %>% any)) {
    stop("Input data is missing column `AssessmentVersion`. Please make sure to add this column before you run om_construct_measures.\n")
  }

  ## make AssessmentVersion numeric
  final_dat <- final_dat %>% mutate(AssessmentVersion = as.numeric(AssessmentVersion))

  ## intellectual humility for pre
  if (wave == "Pre") {
    final_dat <- final_dat %>%
      mutate(Q18Pre = case_when(
        AssessmentVersion == 4 ~ (Q3Pre + Q6Pre + Q7Pre + Q8Pre)/4,
        AssessmentVersion >= 5 ~ (Q5Pre + Q7Pre + Q8Pre + Q9Pre)/4,
        T ~ NA_real_
      ))
  }

  ## intellectual humility for post
  if (wave == "Post") {
    final_dat <- final_dat %>%
      mutate(Q18Post = case_when(
        AssessmentVersion == 4 ~ (Q3Post + Q6Post + Q7Post + Q8Post)/4,
        AssessmentVersion >= 5 ~ (Q5Post + Q7Post + Q8Post + Q9Post)/4,
        T ~ NA_real_
      ))
  }

  ## intellectual humility for followup
  if (wave == "FollowUp") {
    final_dat <- final_dat %>%
      mutate(Q18FollowUp = case_when(
        AssessmentVersion == 4 ~ (Q3FollowUp + Q6FollowUp + Q7FollowUp + Q8FollowUp)/4,
        AssessmentVersion >= 5 ~ (Q5FollowUp + Q7FollowUp + Q8FollowUp + Q9FollowUp)/4,
        T ~ NA_real_
      ))
  }

  return(final_dat)
}

#' Constructs measures
#'
#' This is a higher-level function that uses both "polar_measures" and "calc_ih" to constuct various measures.
#' Creates the following variables:
#' \itemize{
#'   \item Q14: Affective Polarization
#'   \item Q15: Ingroup
#'   \item Q16: Outgroup
#'   \item Q17: Ingroup vs. Outgroup Affective Polarization
#'   \item Q18: Intellectual Humility
#' }
#' Function automatically accounts for Assessment Version 4 and 5/5.1.
#'
#' @param x Assessment data from AirTable
#' @export
om_construct_measures <- function(x){

  ## construct our measures

  # final_dat <- app.dat %>% om_clean_ppol()

  # cols <- colnames(app.dat) %>% paste0(collapse = "|")

  final_dat <- x

  cols <- colnames(x) %>% paste0(collapse = "|")

  # app.dat %>% select(AssessmentVersion)


  ## If Pre vars are found
  if (str_detect(cols, "Pre")) {
    final_dat <- final_dat %>%
      ## Compute Polarization Measures
      polar_measures(Q1Pre, Q2Pre) %>%
      #compute intellectual humility factor score
      calc_ih("Pre")
  }

  ## If Post vars are found
  if (str_detect(cols, "Post")) {
    final_dat <- final_dat %>%
      ## Compute Polarization Measures
      polar_measures(Q1Post, Q2Post) %>%
      #compute intellectual humility factor score
      calc_ih("Post")
  }

  ## If FollowUp vars are found
  if (str_detect(cols, "FollowUp")) {
    final_dat <- final_dat %>%
      ## Compute Polarization Measures
      polar_measures(Q1FollowUp, Q2FollowUp)  %>%
      #compute intellectual humility factor score
      calc_ih("FollowUp")
  }

  return(final_dat)

}

#' Remove duplicates from AirTable
#'
#'
#' @param cleaned_dat Duplicated data from AirTable
#' @export
remove_dups <- function(cleaned_dat) {

  ## remove duplicates from AirTable

  ## pull duplicated OMIDs
  cleaned_dat %>%
    filter(duplicated(OMID)) %>%
    pull(OMID) -> dups


  ## pull OMIDs that are most complete + latest entries
  removed_airtable_dups <-  cleaned_dat %>%
    mutate(createdTime = as_datetime(createdTime)) %>%
    filter(OMID %in% dups) %>%
    mutate(AssessmentVersion = as.numeric(AssessmentVersion))  %>%
    mutate(count_na = rowSums(is.na(.))) %>%
    arrange(OMID, desc(createdTime), desc(AssessmentVersion), count_na) %>%
    select(OMID, createdTime, AssessmentVersion, AssessmentsDone, count_na, everything()) %>%
    group_by(OMID) %>%
    slice(1)

  message(str_glue("Removing {round(length(dups)/2)} duplicates...\n"))

  ## remove OMIDs that we don't want (older + less complete)
  cleaned_dat %>%
    filter(!(OMID %in% dups)) %>%
    mutate(createdTime = as_datetime(createdTime)) %>%
    mutate(AssessmentVersion = as.numeric(AssessmentVersion))  %>%
    bind_rows(removed_airtable_dups)
}

#' Coalescing joints
#'
#' Source: https://alistaire.rbind.io/blog/coalescing-joins/
#'
#' @param x left hand side data
#' @param y right hand side data
#' @param by key variable
#' @param suffix suffix of data (default: .x and .y)
#' @param join dplyr join function
#' @param ... additional argument to join
#' @export
coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {

  ## awesome function for coalescing joints

  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))

  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]],
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce

  dplyr::bind_cols(joined, coalesced)[cols]
}


#' Gather OpenMind data
#'
#' This function will turn Assessment data into long format and
#' creates the following variables:
#' \itemize{
#'   \item Question: Q1Pre, Q2Pre, Q3Pre etc.
#'   \item Type: Pre, Post, or FollowUp
#'   \item Response: Values of the Question
#'   \item variable_code: Q1, Q2, Q3 etc.
#' }
#' @param .data Assessment data
#' @param which_strings a string indicating which variables should be parsed out
#' @export
om_gather <- function(.data, which_strings) {

  gathered_dat <- .data %>%
    gather(Question, Response, matches(which_strings)) %>%
    ## filter out pre-post and follow as a variable "Type"
    mutate(Type = case_when(
      str_detect(Question, "Pre") ~ "Pre",
      str_detect(Question, "Post") ~ "Post",
      str_detect(Question, "FollowUp") ~ "FollowUp"
    ))  %>%
    mutate(variable_code = str_remove(Question, Type)) %>%
    mutate(Response = as.numeric(Response))

  return(gathered_dat)
}

#' String count (because it will be deprecated from dplyr)
#'
#' This is just a helper function because it will be deprecated from dplyr at some point and we would like to keep it.
#'
#'
#' @export
om_count_ <- function (x, vars, wt = NULL, sort = FALSE) {
  vars <- dplyr:::compat_lazy_dots(vars, rlang::caller_env())
  wt <- wt %||% quo(NULL)
  wt <- dplyr:::compat_lazy(wt, rlang::caller_env())
  dplyr::count(x, !!!vars, wt = !!wt, sort = sort)
}

#' SQL Database Append
#'
#' This is a helper function that will write a dataframe to a SQL database
#'
#'
#' @export
db_append <- function(path, tbl, data) {
  con <- dbConnect(RSQLite::SQLite(), path)

  if(!is.null(dbListTables(con))) {
    dbWriteTable(con, tbl, data, append = T)
  } else {
    dbWriteTable(con, tbl, data)
  }
  dbDisconnect(con)

}
