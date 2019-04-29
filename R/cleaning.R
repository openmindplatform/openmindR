#' This function filters down a dataframe by the desired characteristics.
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
    if (length(accesscode) >= 2) {
      app.dat <- app.dat %>%
        filter(AccessCode %in% accesscode)
    } else
      app.dat <- app.dat %>%
        filter(str_detect(AccessCode, accesscode))
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
#' @param dat.par (0/1) ParticipantProgress data from AirTable
#' @param ... Arguments for select to get more from ParticipantProgress
#' @export
om_clean_par <- function(dat.par, ...) {

  dat.par %>%
    ## seperating brackets and cleaning up
    separate(StepsComplete, into = paste("StepsComplete", 1:5, sep = ""), remove = F) %>%
    separate(StepScores, into = paste("StepsScores", 1:5, sep = "")) %>%
    separate(StepQuestionTotals, into = paste("StepQuestionTotals", 1:5, sep = "")) %>%
    separate(StepTimes, into = paste("StepTimes", 1:5, sep = ""),
             sep = ",", remove = F) %>%
    ## Clean up seperated vars
    mutate_at(vars(StepTimes1:StepTimes5), ~str_remove_all(.x, "[^[:digit:]. ]") %>% parse_number) %>%
    mutate_at(vars(StepTimes1:StepTimes5), ~ifelse(.x == 0, NA, .x)) %>%
    ## Making columns numeric where they need to be
    mutate_at(vars(StepsComplete1:StepQuestionTotals5), as.numeric)  %>%
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
#'   \item ppol: a factor variable ordered from "Very Progressive/left" to "Very Conservative/right". Excludes all other categories as NA (classical liberal etc.)ppol: a factor variable ordered from "Very Progressive/left" to "Very Conservative/right". Excludes all other categories as NA (classical liberal etc.)
#'   \item ppol_num: numeric variable ranging from 1 "Very Progressive/left" to 7 "Very Conservative/right"
#'   \item ppol_cat: a factor variable which has two categories "Progressive" and "Conservative". The rest is NA.
#' }
#'
#'
#' @param app.dat (0/1) Assessment data from AirTable
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


#' String count (because it will be deprecated from dplyr)
#'
#'
#' @export
om_count_ <- function (x, vars, wt = NULL, sort = FALSE) {
  vars <- dplyr:::compat_lazy_dots(vars, rlang::caller_env())
  wt <- wt %||% quo(NULL)
  wt <- dplyr:::compat_lazy(wt, rlang::caller_env())
  dplyr::count(x, !!!vars, wt = !!wt, sort = sort)
}
