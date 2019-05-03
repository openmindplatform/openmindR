#' This function filters down Assessment (or ParticipantProgress) data
#'
#' You can filter data by \code{AssessmentsDone}, \code{AssessmentVersion} and \code{AccessCode} (ParticipantProgress data can only be filtered by \code{AccessCode})
#'
#' Want to match multiple specific Access Codes? Then just specify a vector: \code{c("EddySalemStateUniversityF18", "EddySalemStateUniversityF18k")}.
#'
#' If you want multiple "fuzzy" searches for an AccessCode you would use the argument \code{exact_search = F} (the default) and use \code{"Salem|NYU"} which will give you all Access Codes that include Salem and and NYU (the bar indicates separate searches).
#'
#' Finally, if you want just one specific Access Code then you'd use \code{"EddySalemStateUniversityF18"} with \code{exact_search = T}
#'
#'
#' @param app.dat Assessment data from AirTable
#' @param n_assessments \code{AssessmentsDone} How many assessments do the participants need to have completed? If 1, it will only provide data for people who completed 1 assessment. If 2, it will provide all people who completed exactly 2 assessments. If 3, it will provide all people who completed all 3 assessments. (Should be 1, 2 and/or 3)
#' @param version \code{AssessmentVersion} Filter down to what asssesment version. This argument either takes a single number or vector.
#' @param accesscode \code{AccessCode} Filter down to (several) AccessCode(s)
#' @param exact_search \code{logical} This argument takes TRUE or FALSE. If you want to match AccessCodes exactly set this to TRUE. Default is FALSE. If you want to select multiple AccessCodes by exact name, use an explicit vector instead.
#' @export
om_filter_data <- function(app.dat, n_assessments = NULL,
                       version = NULL, accesscode = NULL, exact_search = F) {

  if (!is.null(n_assessments)) {
    app.dat <- app.dat %>%
      dplyr::mutate(AssessmentsDone = base::as.numeric(AssessmentsDone)) %>%
      dplyr::filter(AssessmentsDone %in% n_assessments)
  }
  if (!is.null(version)) {
    app.dat <- app.dat %>%
      # dplyr::mutate(AssessmentVersion = as.numeric(AssessmentVersion)) %>%
      dplyr::filter(AssessmentVersion %in% version)
  }
  if (!is.null(accesscode)) {

    ## turn AccessCode and search string to lower
    app.dat <- app.dat %>% dplyr::mutate(AccessCode2 = stringr::str_to_lower(AccessCode))
    accesscode <- stringr::str_to_lower(accesscode)

    ## if vector
    if (length(accesscode) >= 2) {
      app.dat <- app.dat %>%
        dplyr::filter(AccessCode2 %in% accesscode) %>%
        dplyr::select(-AccessCode2)
    ## if single string
    }
    if (length(accesscode) == 1) {
      # if strings seperated by |
      if (stringr::str_detect(accesscode, "\\|")) {
        app.dat <- app.dat %>%
          dplyr::filter(stringr::str_detect(AccessCode2, accesscode)) %>%
          dplyr::select(-AccessCode2)
      } # if not seperate strings seperated by | and not exact search
      if (!stringr::str_detect(accesscode, "\\|") & !exact_search) {
        app.dat <- app.dat %>%
          dplyr::filter(stringr::str_detect(AccessCode2, accesscode)) %>%
          dplyr::select(-AccessCode2)
      } # if not seperate strings seperated by | and not exact search
      if (!stringr::str_detect(accesscode, "\\|") & exact_search) {
        app.dat <- app.dat %>%
          dplyr::filter(stringr::str_detect(AccessCode2, stringr::str_glue("\\b{accesscode}\\b"))) %>%
          dplyr::select(-AccessCode2)
      }
    }

  }

  return(dplyr::as_tibble(app.dat))

}

#' This calculates the correct score for each step
#'
#' This is just an internal helper function.
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
#' @param parse_feedback Parse Feedback answers (Q1 to Q5 for Step 1 to 5). Default is \code{FALSE}.
#' @param ... Arguments for select to get additional variables from ParticipantProgress
#' @export
om_clean_par <- function(dat.par, parse_feedback = F, ...) {

  final_dat <- dat.par %>%
    ## seperating StepsComplete brackets
    tidyr::separate(StepsComplete, into = paste("StepsComplete", 1:5, sep = ""), remove = F) %>%
    ## seperating StepsScores brackets
    tidyr::separate(StepScores, into = paste("StepsScores", 1:5, sep = "")) %>%
    ## seperating StepsQuestionTotals brackets
    tidyr::separate(StepQuestionTotals, into = paste("StepQuestionTotals", 1:5, sep = "")) %>%
    ## seperating StepTines brackets
    tidyr::separate(StepTimes, into = paste("StepTimes", 1:5, sep = ""),
             sep = ",", remove = F) %>%
    ## Clean up seperated vars
    dplyr::mutate_at(dplyr::vars(StepTimes1:StepTimes5), ~stringr::str_remove_all(.x, "[^[:digit:]. ]")) %>%
    dplyr::mutate_at(dplyr::vars(StepTimes1:StepTimes5), readr::parse_number) %>%
    dplyr::mutate_at(dplyr::vars(StepTimes1:StepTimes5), ~ifelse(.x == 0, NA, .x)) %>%
    ## Making columns numeric where they need to be
    dplyr::mutate_at(dplyr::vars(StepsComplete1:StepQuestionTotals5, AppRating), readr::parse_number)  %>%
    # ## Steps Complete
    ## Now calculating scores
    ## percent correct for each step
    dplyr::mutate(StepCorrect1 = calc_correct(StepsComplete1, StepsScores1, StepQuestionTotals1)) %>%
    dplyr::mutate(StepCorrect2 = calc_correct(StepsComplete2, StepsScores2, StepQuestionTotals2)) %>%
    dplyr::mutate(StepCorrect3 = calc_correct(StepsComplete3, StepsScores3, StepQuestionTotals3)) %>%
    dplyr::mutate(StepCorrect4 = calc_correct(StepsComplete4, StepsScores4, StepQuestionTotals4)) %>%
    dplyr::mutate(StepCorrect5 = calc_correct(StepsComplete5, StepsScores5, StepQuestionTotals5)) %>%
    dplyr::select(OMID, StepTimes, StepsComplete, StepCorrect1:StepCorrect5, StepTimes1:StepTimes5,
           FeedbackAnswers, FeedbackAnswersVariableNames, AppRating, AppRecommend, at_date, ...)

  if (parse_feedback) {
    ## Parse Feedback Answers
    final_dat <- final_dat %>%
      dplyr::mutate(data = FeedbackAnswers %>%
                                 purrr::map(~parse_feedback_at(.x))) %>%
      tidyr::unnest(data) %>%
      ## Make Step variables to characters (for merging)
      dplyr::mutate_at(dplyr::vars(Step1:Step5_Q5), as.character) %>%
      dplyr::select(OMID, StepTimes, StepsComplete, StepCorrect1:StepCorrect5, StepTimes1:StepTimes5, Step1:Step5_Q5,
                    FeedbackAnswers, FeedbackAnswersVariableNames, AppRating, AppRecommend, at_date, ...)
  }

  return(final_dat)

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
    dplyr::mutate_at(dplyr::vars(dplyr::matches(openmindR::var_strings)), as.numeric) %>%
    ## construct raw ppol variable
    dplyr::mutate(ppol_raw = D4) %>%
    ## fix the names of categories
    dplyr::mutate(ppol_raw = dplyr::case_when(
      stringr::str_detect(ppol_raw, "Moderate") ~ "Moderate/Middle-of-the-road",
      stringr::str_detect(ppol_raw, "Slightly progressive") ~ "Slightly Progressive/left",
      stringr::str_detect(ppol_raw, "not political") ~ "Don't know/Not political",
      stringr::str_detect(ppol_raw, "Very progressive") ~ "Very Progressive/left",
      stringr::str_detect(ppol_raw, "Slightly conservative") ~ "Slightly Conservative/right",
      stringr::str_detect(ppol_raw, "classical liberal") ~ "Libertarian/Classical liberal",
      stringr::str_detect(ppol_raw, "Very conservative") ~ "Very Conservative/right",
      stringr::str_detect(ppol_raw, "progressive") ~ "Progressive/left",
      T ~ ppol_raw
    )) %>%
    #reorder political orientation into sensible continuum
    dplyr::mutate(ppol = factor(ppol_raw, levels = c("Very Progressive/left",
                                              "Progressive/left",
                                              "Slightly Progressive/left",
                                              "Moderate/Middle-of-the-road",
                                              "Slightly Conservative/right",
                                              "Conservative/right",
                                              "Very Conservative/right"))) %>%
    ## clean politics variable / make it numeric / only use valid cases
    dplyr::mutate(ppol_num = as.numeric(ppol)) %>%
    # dplyr::select(ppol, ppol_num, D4) %>%
    dplyr::mutate(ppol_cat = dplyr::case_when(
      ppol_num %in% c(1:3) ~ "Progressives",
      ppol_num %in% c(5:7) ~ "Conservatives",
      T ~ NA_character_
    )) %>%
    dplyr::mutate(ppol_cat = forcats::fct_relevel(ppol_cat, c("Progressives",
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
  if (magrittr::not(colnames(final_dat) %in% "ppol_cat" %>% any)) {
    stop("Input data is missing column `ppol_cat`. Please make sure to run om_clean_ppol before you run om_construct_measures.\n")
  }

  ## check which wave
  wave <- dplyr::case_when(
    stringr::str_detect(lazyeval::expr_find(Q1), "Pre") ~ "Pre",
    stringr::str_detect(lazyeval::expr_find(Q1), "Post") ~ "Post",
    stringr::str_detect(lazyeval::expr_find(Q1), "FollowUp") ~ "FollowUp"
  )

  ## lazy evaluation
  Q1 <- dplyr::enquo(Q1)
  Q2 <- dplyr::enquo(Q2)


  final_dat <- final_dat %>%
    # make sure vars are numeric
    dplyr::mutate_at(dplyr::vars(!!Q1, !!Q2), as.numeric) %>%
    # compute affective polarization
    dplyr::mutate(Q14 = abs(!!Q1 - !!Q2)) %>%
    # compute liking for ingroup vs. disliking for outgroup
    ## my ingroup
    dplyr::mutate(Q15 = ifelse(ppol_cat == "Progressives", !!Q1, !!Q2)) %>%
    # my outgroup
    dplyr::mutate(Q16 = ifelse(ppol_cat == "Progressives", !!Q2, !!Q1)) %>%
    # compute ingroup v outgroup affective polarization
    dplyr::mutate(Q17 = abs(Q15 - Q16)) %>%
    dplyr::rename_at(dplyr::vars(Q14:Q17), ~paste0(.x, wave))

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
  if (magrittr::not(colnames(final_dat) %in% "AssessmentVersion" %>% any)) {
    stop("Input data is missing column `AssessmentVersion`. Please make sure to add this column before you run om_construct_measures.\n")
  }

  ## make AssessmentVersion numeric
  final_dat <- final_dat %>% dplyr::mutate(AssessmentVersion = as.numeric(AssessmentVersion))

  ## intellectual humility for pre
  if (wave == "Pre") {
    final_dat <- final_dat %>%
      dplyr::mutate(Q18Pre = dplyr::case_when(
        AssessmentVersion == 4 ~ (Q3Pre + Q6Pre + Q7Pre + Q8Pre)/4,
        AssessmentVersion >= 5 ~ (Q5Pre + Q7Pre + Q8Pre + Q9Pre)/4,
        T ~ NA_real_
      ))
  }

  ## intellectual humility for post
  if (wave == "Post") {
    final_dat <- final_dat %>%
      dplyr::mutate(Q18Post = dplyr::case_when(
        AssessmentVersion == 4 ~ (Q3Post + Q6Post + Q7Post + Q8Post)/4,
        AssessmentVersion >= 5 ~ (Q5Post + Q7Post + Q8Post + Q9Post)/4,
        T ~ NA_real_
      ))
  }

  ## intellectual humility for followup
  if (wave == "FollowUp") {
    final_dat <- final_dat %>%
      dplyr::mutate(Q18FollowUp = dplyr::case_when(
        AssessmentVersion == 4 ~ (Q3FollowUp + Q6FollowUp + Q7FollowUp + Q8FollowUp)/4,
        AssessmentVersion >= 5 ~ (Q5FollowUp + Q7FollowUp + Q8FollowUp + Q9FollowUp)/4,
        T ~ NA_real_
      ))
  }

  return(final_dat)
}

#' Constructs measures
#'
#' This is a higher-level function that uses both "polar_measures" and "calc_ih" to construct various measures.
#' Creates the following variables:
#' \itemize{
#'   \item Q14: Affective Polarization
#'   \item Q15: Ingroup
#'   \item Q16: Outgroup
#'   \item Q17: Ingroup vs. Outgroup Affective Polarization
#'   \item Q18: Intellectual Humility
#' }
#' Function automatically accounts for Assessment Version 4 and 5/5.1.
#' Future assessment versions likely lead to problems so it needs to be adapted.
#'
#' @param x Assessment data from AirTable
#' @export
om_construct_measures <- function(x){

  ## construct our measures

  # final_dat <- app.dat %>% om_clean_ppol()

  # cols <- colnames(app.dat) %>% paste0(collapse = "|")

  final_dat <- x

  cols <- colnames(x) %>% paste0(collapse = "|")

  # app.dat %>% dplyr::select(AssessmentVersion)


  ## If Pre vars are found
  if (stringr::str_detect(cols, "Pre")) {
    final_dat <- final_dat %>%
      ## Compute Polarization Measures
      openmindR::polar_measures(Q1Pre, Q2Pre) %>%
      #compute intellectual humility factor score
      openmindR::calc_ih("Pre")
  }

  ## If Post vars are found
  if (stringr::str_detect(cols, "Post")) {
    final_dat <- final_dat %>%
      ## Compute Polarization Measures
      openmindR::polar_measures(Q1Post, Q2Post) %>%
      #compute intellectual humility factor score
      openmindR::calc_ih("Post")
  }

  ## If FollowUp vars are found
  if (stringr::str_detect(cols, "FollowUp")) {
    final_dat <- final_dat %>%
      ## Compute Polarization Measures
      openmindR::polar_measures(Q1FollowUp, Q2FollowUp)  %>%
      #compute intellectual humility factor score
      openmindR::calc_ih("FollowUp")
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
    dplyr::filter(duplicated(OMID)) %>%
    dplyr::pull(OMID) -> dups


  ## pull OMIDs that are most complete + latest entries
  removed_airtable_dups <-  cleaned_dat %>%
    dplyr::mutate(createdTime = lubridate::as_datetime(createdTime)) %>%
    dplyr::filter(OMID %in% dups) %>%
    dplyr::mutate(AssessmentVersion = as.numeric(AssessmentVersion))  %>%
    dplyr::mutate(count_na = rowSums(is.na(.))) %>%
    dplyr::arrange(OMID, desc(createdTime), desc(AssessmentVersion), count_na) %>%
    dplyr::select(OMID, createdTime, AssessmentVersion, AssessmentsDone, count_na, dplyr::everything()) %>%
    dplyr::group_by(OMID) %>%
    dplyr::slice(1)

  message(stringr::str_glue("Removing {round(length(dups)/2)} duplicates...\n"))

  ## remove OMIDs that we don't want (older + less complete)
  cleaned_dat %>%
    dplyr::filter(!(OMID %in% dups)) %>%
    dplyr::mutate(createdTime = lubridate::as_datetime(createdTime)) %>%
    dplyr::mutate(AssessmentVersion = as.numeric(AssessmentVersion))  %>%
    dplyr::bind_rows(removed_airtable_dups)
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
  cols <- dplyr::union(names(x), names(y))

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
#' Function takes strings in the form of \code{"Q1P|Q2P"} for Pre- and Post Questions 1 & 2
#' @param .data Assessment data
#' @param which_strings a string indicating which variables should be parsed out (default is \code{openmindR::q_c_strings})
#' @export
om_gather <- function(.data, which_strings = openmindR::q_c_strings) {

  gathered_dat <- .data %>%
    tidyr::gather(Question, Response, dplyr::matches(which_strings)) %>%
    ## filter out pre-post and follow as a variable "Type"
    dplyr::mutate(Type = dplyr::case_when(
      stringr::str_detect(Question, "Pre") ~ "Pre",
      stringr::str_detect(Question, "Post") ~ "Post",
      stringr::str_detect(Question, "FollowUp") ~ "FollowUp"
    ))  %>%
    dplyr::mutate(variable_code = stringr::str_remove(Question, Type)) %>%
    dplyr::mutate(Response = as.numeric(Response))

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

  if(!is.null(DBI::dbListTables(con))) {
    DBI::dbWriteTable(con, tbl, data, append = T)
  } else {
    DBI::dbWriteTable(con, tbl, data)
  }
  DBI::dbDisconnect(con)

}
