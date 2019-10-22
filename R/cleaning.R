#' Download AirTable data
#'
#' This function downloads current AirTable data
#'
#' @md
#' @section Currently allowed input:
#' + `"AssessmentV4"`
#' + `"AssessmentV5"`
#' + `"AccessCodes"`
#' + `"ParticipantProgress"`
#' + `"InstructorSurvey"`
#' + `"TechnicalInquiries"`
#'
#'@param key key for AirTable API
#'@param tables specify which tables you want to download
#'@return a list with (several) dataframe(s)
#'@export
om_download_at <- function(key, tables = c("AssessmentV4", "AssessmentV5","AccessCodes","ParticipantProgress","InstructorSurvey", "TechnicalInquiries"), clean = F, file = NULL) {

  if (any(tables %nin% c("AssessmentV4", "AssessmentV5","AccessCodes","ParticipantProgress","InstructorSurvey", "TechnicalInquiries", "AssessmentV6", "AssessmentV6DiD", "DiDProgress"))) {
    stop("Warning: Should be one of the following: AssessmentV4, AssessmentV5,AccessCodes,ParticipantProgress,InstructorSurvey, TechnicalInquiries, AssessmentV6, AssessmentV6DiD, DiDProgress\n")
  }

  cat("Seting up key\n")

  Sys.setenv(AIRTABLE_API_KEY = key)
  AIRTABLE_API_KEY = key


  dat.ass.1 <- airtabler::airtable(
    base = "appjU7KUyybZ4rGvT",
    tables = tables
  )

  final_list <- list()

  ## downloads full data table
  if ("AssessmentV4" %in% tables) {
    cat("Download AssessmentV4 Data\n")
    final_list$dat.ass4 <- dat.ass.1$AssessmentV4$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. AssessmentV4 Data has ", nrow(final_list$dat.ass4), " rows\n"))
  }

  if ("AssessmentV5" %in% tables) {
    cat("Download AssessmentV5 Data\n")
    final_list$dat.ass5 <- dat.ass.1$AssessmentV5$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. AssessmentV5 Data has ", nrow(final_list$dat.ass5), " rows\n"))
  }

  if ("AssessmentV6" %in% tables) {
    cat("Download AssessmentV6 Data\n")
    final_list$dat.ass6 <- dat.ass.1$AssessmentV6$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. AssessmentV6 Data has ", nrow(final_list$dat.ass6), " rows\n"))

    if (clean) {

      final_list$dat.ass6 <- final_list$dat.ass6 %>%
        dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(minor)"), NA_character_, .x)) %>%
        dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(Opt Out)"), NA_character_, .x)) %>%
        dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(not asked yet)"), NA_character_, .x)) %>%
        dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(not asked)"), NA_character_, .x)) %>%
        dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(not met)"), NA_character_, .x))  %>%
        dplyr::select(sort(tidyselect::peek_vars(), decreasing = T)) %>%
        dplyr::select(id, OMID, AccessCode, AssessmentVersion, AssessmentsDone,
               D1, D2, D3, D4, D5, D6, D7, D8,
               dplyr::everything()) %>%
        # select(contains("Trait"))
        dplyr::mutate_at(dplyr::vars(AssessmentVersion,
                       AssessmentsDone,
                       D1,
                       dplyr::contains("Motivation"),
                       dplyr::contains("GBSS"),
                       dplyr::contains("CIHS_LIO"),
                       dplyr::contains("Temp"),
                       dplyr::contains("C1"),
                       dplyr::contains("C2"),
                       dplyr::contains("C3"),
                       dplyr::contains("C4"),
                       dplyr::contains("C5"),
                       dplyr::contains("C6"),
                       dplyr::contains("HowImportantIssue"),
                       dplyr::contains("SoughtOutDifferent"),
                       dplyr::contains("Preparedness"),
                       dplyr::contains("GrowthMindset"),
                       dplyr::contains("Trait"),
                       dplyr::contains("Upset"),
                       dplyr::contains("HowLeastLikedGroup"),
                       dplyr::contains("PoliticalTolerance"),
                       dplyr::contains("IssueQ"),
                       dplyr::contains("ROV_NE"),
                       dplyr::contains("TalkToDifferent"),
                       dplyr::contains("Trait")
        ), ~as.character(.x) %>% readr::parse_number())



    }


    if (!is.null(file)) {

      readr::write_csv(final_list$dat.ass6, file = file)

    }

  }

  if ("AssessmentV6DiD" %in% tables) {
    cat("Download AssessmentV6DiD Data\n")
    final_list$dat.ass6did <- dat.ass.1$AssessmentV6DiD$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. AssessmentV6DiD Data has ", nrow(final_list$dat.ass6did), " rows\n"))
  }

  if ("DiDProgress" %in% tables) {
    cat("Download DiDProgress Data\n")
    final_list$didprogress <- dat.ass.1$DiDProgress$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. DiDProgress Data has ", nrow(final_list$didprogress), " rows\n"))
  }


  if ("AccessCodes" %in% tables) {
    cat("Download AccessCodes Data\n")
    final_list$dat.acc <- dat.ass.1$AccessCodes$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. AccessCodes Data has ", nrow(final_list$dat.acc), " rows\n"))
  }


  if ("ParticipantProgress" %in% tables) {
    cat("Download Participant Progress Data\n")
    final_list$dat.par <- dat.ass.1$ParticipantProgress$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. Participant Progress Data has ", nrow(final_list$dat.par), " rows\n"))
  }

  if ("InstructorSurvey" %in% tables) {
    cat("Download Instructor Survey Data\n")
    final_list$dat.ins <- dat.ass.1$InstructorSurvey$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. Instructor Survey Data has ", nrow(final_list$dat.ins), " rows\n"))
  }

  if ("TechnicalInquiries" %in% tables) {
    cat("Download Technial Inquiries Data\n")
    final_list$dat.tec <- dat.ass.1$TechnicalInquiries$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. Technical Inquiries Data has ", nrow(final_list$dat.tec), " rows\n"))
  }

  if (length(tables) == 1) final_list <- final_list %>% magrittr::extract2(1) %>% tibble::as_tibble()

  return(final_list)

}


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
#' Usually you would run this function first to filter down your data.
#'
#' @section Workflow:
#' \code{\link{om_filter_data}} -> \code{\link{om_rescale}} -> \code{\link{om_clean_ppol}} -> \code{\link{om_construct_measures}} -> \code{\link{remove_dups}} -> \code{\link{om_gather}}
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
    # dplyr::mutate(createdTime = at_date) %>%
    dplyr::select(OMID, StepTimes, StepsComplete, StepCorrect1:StepCorrect5, StepTimes1:StepTimes5,
           FeedbackAnswers, FeedbackAnswersVariableNames, AppRating, AppRecommend, LifeHacksChosen,
           LifeHacksComplete, LifeHacksUseful, LifeHacksReason,
           # createdTime,
           at_date, ...)

  if (parse_feedback) {
    ## Parse Feedback Answers
    final_dat <- final_dat %>%
      dplyr::mutate(data = FeedbackAnswers %>%
                                 purrr::map(~parse_feedback_at(.x))) %>%
      tidyr::unnest(data) %>%
      ## Make Step variables to characters (for merging)
      dplyr::mutate_at(dplyr::vars(Step1:Step5_Q5), as.character) %>%
      dplyr::select(OMID, StepTimes, StepsComplete, StepCorrect1:StepCorrect5, StepTimes1:StepTimes5, Step1:Step5_Q5,
                    FeedbackAnswers, FeedbackAnswersVariableNames, AppRating, AppRecommend, LifeHacksChosen,
                    LifeHacksComplete, LifeHacksUseful, LifeHacksReason,
                    # createdTime,
                    at_date, ...)
  }

  return(final_dat)


}


#' Rescale Variables
#'
#' This function rescales variables from 0 to 1. Q1 and Q2 is divided by 100 and Q3 - Q12 as well as C1 - C3 (divided by 6).
#'
#'
#'
#' @param .data Assessment data
#' @export
om_rescale <- function(.data) {

  # .data <- dat.ass

  vars_numeric <- .data  %>%
    dplyr::select(dplyr::matches(openmindR::var_strings)) %>%
    dplyr::summarize_all(is.numeric) %>%
    .[1,] %>% unlist() %>% unname() %>% all()


  .data %>%
    do_if(!vars_numeric,
      ~{.x %>%
          dplyr::mutate_at(dplyr::vars(dplyr::matches(openmindR::var_strings)), readr::parse_number)
        }
    ) %>%
    ## Make variables Q1 and Q2 range 0 to 1
    ## ATTENTION.. Match Q1 and ONLY Q1 (Q2)!!!
    dplyr::mutate_at(dplyr::vars(dplyr::matches("Q1P|Q1F|Q2P|Q2F")), function(x) x/100) %>%
    ## Make variables Q3 to C3 range 0 to 1
    dplyr::mutate_at(dplyr::vars(dplyr::matches(openmindR::range01_strings)), function(x) x/6)
}


#' Creates several measures of Political Orientation
#'
#' Creates the following measures of Political Orientation
#' \itemize{
#'   \item ppol_raw: a variable that merges Assessment V4 and V5.1 spelling of Political Orientation (D4)
#'   \item ppol: a factor variable ordered from "Very Progressive/left" to "Very Conservative/right". Excludes all other categories as NA (classical liberal etc.)
#'   \item ppol_num: numeric variable ranging from 1 "Very Progressive/left" to 7 "Very Conservative/right"
#'   \item ppol_cat: a factor variable which has two categories "Progressive" and "Conservative". The rest is NA.
#'   \item ppol_catmod: a factor variable which has three categories "Progressive", "Conservative" and "Moderates". The rest is NA.
#' }
#'
#' @section Workflow:
#' \code{\link{om_filter_data}} -> \code{\link{om_rescale}} -> \code{\link{om_clean_ppol}} -> \code{\link{om_construct_measures}} -> \code{\link{remove_dups}} -> \code{\link{om_gather}}
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
                                                              "Conservatives"))) %>%
    dplyr::mutate(ppol_catmod = dplyr::case_when(
      ppol_num %in% c(1:3) ~ "Progressives",
      ppol_num %in% c(5:7) ~ "Conservatives",
      ppol_num == 4 ~ "Moderates",
      T ~ NA_character_
    )) %>%
    dplyr::mutate(ppol_cat = forcats::fct_relevel(ppol_cat, c("Progressives",
                                                              "Moderates",
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
polar_measures <- function(final_dat, ProgTemp, ConTemp) {

  ## if ppol_cat is not found then throw error
  if (magrittr::not(colnames(final_dat) %in% "ppol_cat" %>% any)) {
    stop("Input data is missing column `ppol_cat`. Please make sure to run om_clean_ppol before you run om_construct_measures.\n")
  }

  ## check which wave
  wave <- dplyr::case_when(
    stringr::str_detect(lazyeval::expr_find(ProgTemp), "Pre") ~ "Pre",
    stringr::str_detect(lazyeval::expr_find(ProgTemp), "Post") ~ "Post",
    stringr::str_detect(lazyeval::expr_find(ProgTemp), "FollowUp") ~ "FollowUp"
  )

  ## lazy evaluation
  ProgTemp <- dplyr::enquo(ProgTemp)
  ConTemp <- dplyr::enquo(ConTemp)
  # Q3 <- dplyr::enquo(Q3)
  # Q4 <- dplyr::enquo(Q4)
  # Q10 <- dplyr::enquo(Q10)


  final_dat <- final_dat %>%
    # make sure vars are numeric
    dplyr::mutate_at(dplyr::vars(!!ProgTemp, !!ConTemp), as.numeric) %>%
    # compute affective polarization
    dplyr::mutate(AffPol1 = abs(!!ProgTemp - !!ConTemp)) %>%
    # compute liking for ingroup vs. disliking for outgroup
    ## my ingroup
    dplyr::mutate(IngroupLiking = case_when(
      ppol_cat == "Progressives" ~ !!ProgTemp,
      ppol_cat == "Conservatives" ~ !!ConTemp
      )) %>%
    # my outgroup
    dplyr::mutate(OutgroupLiking = case_when(
      ppol_cat == "Conservatives" ~ !!ProgTemp,
      ppol_cat == "Progressives" ~ !!ConTemp
    )) %>%
    # compute ingroup v outgroup affective polarization
    dplyr::mutate(AffPol2 = abs(IngroupLiking - OutgroupLiking)) %>%
    dplyr::rename_at(dplyr::vars(AffPol1, AffPol2, IngroupLiking, OutgroupLiking), ~paste0(.x, wave))

  return(final_dat)
}


#' Creates Motivation Atrribution Asymmetry variables
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
motivation_measures <- function(final_dat, MotivationProg1, MotivationCon1, MotivationProg2, MotivationCon2) {

  ## if ppol_cat is not found then throw error
  if (magrittr::not(colnames(final_dat) %in% "ppol_cat" %>% any)) {
    stop("Input data is missing column `ppol_cat`. Please make sure to run om_clean_ppol before you run om_construct_measures.\n")
  }

  ## check which wave
  wave <- dplyr::case_when(
    stringr::str_detect(lazyeval::expr_find(MotivationProg1), "Pre") ~ "Pre",
    stringr::str_detect(lazyeval::expr_find(MotivationProg1), "Post") ~ "Post",
    stringr::str_detect(lazyeval::expr_find(MotivationProg1), "FollowUp") ~ "FollowUp"
  )

  ## lazy evaluation
  MotivationProg1 <- dplyr::enquo(MotivationProg1)
  MotivationCon1 <- dplyr::enquo(MotivationCon1)
  MotivationProg2 <- dplyr::enquo(MotivationProg2)
  MotivationCon2 <- dplyr::enquo(MotivationCon2)


  final_dat <- final_dat %>%
    dplyr::mutate(IngroupMotivation1 = ifelse(ppol_cat == "Progressives",
                                              !!MotivationProg1,
                                              !!MotivationCon1)) %>%
    # my outgroup
    dplyr::mutate(OutgroupMotivation1 = ifelse(ppol_cat == "Progressives",
                                               !!MotivationCon1,
                                               !!MotivationProg1)) %>%
    dplyr::mutate(IngroupMotivation2 = ifelse(ppol_cat == "Progressives",
                                              !!MotivationProg2,
                                              !!MotivationCon2)) %>%
    # my outgroup
    dplyr::mutate(OutgroupMotivation2 = ifelse(ppol_cat == "Progressives",
                                               !!MotivationCon2,
                                               !!MotivationProg2)) %>%
    # compute ingroup v outgroup motivation attribution
    dplyr::mutate(MAA1 = abs(IngroupMotivation1 - OutgroupMotivation1)) %>%
    dplyr::mutate(MAA2 = abs(IngroupMotivation2 - OutgroupMotivation2)) %>%
    dplyr::rename_at(dplyr::vars(IngroupMotivation1:MAA2), ~paste0(.x, wave))

  return(final_dat)
}

#' Creates Target variables
#'
#' This is lower-level function that belongs to om_construct measure. This function is not meant to be used outside of om_construct_measure.
#' Creates the following measures
#' \itemize{
#'   \item Q18: Intellectual Humility
#'   \item Q19: Perspective-Taking
#' }
#' Function automatically accounts for Assessment Version 4 and 5/5.1.
#'
#'
#' @param final_dat Assessment data from AirTable
#' @param wave Specify wave ("Pre", "Post" or "FollowUp")
#' @export
calc_measures <- function(final_dat, wave) {

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
      )) %>%
      ## Perspective-Taking for Pre
      dplyr::mutate(Q19Pre = dplyr::case_when(
        AssessmentVersion == 4 ~ Q4Pre,
        AssessmentVersion >= 5 ~ Q6Pre,
        T ~ NA_real_
      )) #%>%
      # ## Social Distance for Pre
      # dplyr::mutate(Q20Pre = dplyr::case_when(
      #   AssessmentVersion == 4 ~ Q4Pre,
      #   AssessmentVersion >= 5 ~ Q6Pre,
      #   T ~ NA_real_
      # ))


  }

  ## intellectual humility for post
  if (wave == "Post") {
    final_dat <- final_dat %>%
      dplyr::mutate(Q18Post = dplyr::case_when(
        AssessmentVersion == 4 ~ (Q3Post + Q6Post + Q7Post + Q8Post)/4,
        AssessmentVersion >= 5 ~ (Q5Post + Q7Post + Q8Post + Q9Post)/4,
        T ~ NA_real_
      )) %>%
      ## Perspective-Taking for Post
      dplyr::mutate(Q19Post = dplyr::case_when(
        AssessmentVersion == 4 ~ Q4Post,
        AssessmentVersion >= 5 ~ Q6Post,
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
      )) %>%
    ## Perspective-Taking for followup
      dplyr::mutate(Q19FollowUp = dplyr::case_when(
        AssessmentVersion == 4 ~ Q4FollowUp,
        AssessmentVersion >= 5 ~ Q6FollowUp,
        T ~ NA_real_
      ))
  }

  return(final_dat)
}


#' Constructs measures
#'
#'
#' This is a higher-level function that uses both \code{\link{polar_measures}} and \code{\link{calc_measures}} to construct various measures.
#' Creates the following variables:
#' \itemize{
#'   \item Q14: Affective Polarization
#'   \item Q15: Ingroup
#'   \item Q16: Outgroup
#'   \item Q17: Ingroup vs. Outgroup Affective Polarization
#'   \item Q18: Intellectual Humility
#'   \item Q19: Perspective-Taking
#' }
#'
#' Function automatically accounts for Assessment Version 4 and 5/5.1.
#' Future assessment versions likely lead to problems so it needs to be adapted.
#'
#'
#' @section Workflow:
#' \code{\link{om_filter_data}} -> \code{\link{om_rescale}} -> \code{\link{om_clean_ppol}} -> \code{\link{om_construct_measures}} -> \code{\link{remove_dups}} -> \code{\link{om_gather}}
#'
#'
#' @param x Assessment data from AirTable
#' @export
om_construct_measures <- function(x){

  ## construct our measures

  # final_dat <- app.dat %>% om_clean_ppol()

  # cols <- colnames(app.dat) %>% paste0(collapse = "|")

  ## necessary because Assessment Version 4 hast Followup lowercase
  x <- x %>%
    dplyr::rename_at(dplyr::vars(dplyr::matches("Followup")), ~stringr::str_replace(., "Followup", "FollowUp"))

  final_dat <- x

  cols <- colnames(x) %>% paste0(collapse = "|")

  # app.dat %>% dplyr::select(AssessmentVersion)


  ## If Pre vars are found
  if (stringr::str_detect(cols, "Pre")) {
    final_dat <- final_dat %>%
      ## Compute Polarization Measures
      polar_measures(Q1Pre, Q2Pre, Q3Pre, Q4Pre, Q10Pre) %>%
      #compute intellectual humility factor score
      calc_measures("Pre")
  }

  ## If Post vars are found
  if (stringr::str_detect(cols, "Post")) {
    final_dat <- final_dat %>%
      ## Compute Polarization Measures
      polar_measures(Q1Post, Q2Post, Q3Post, Q4Post, Q10Post) %>%
      #compute intellectual humility factor score
      calc_measures("Post")
  }

  ## If FollowUp vars are found
  if (stringr::str_detect(cols, "FollowUp")) {
    final_dat <- final_dat %>%
      ## Compute Polarization Measures
      polar_measures(Q1FollowUp, Q2FollowUp, Q3FollowUp, Q4FollowUp, Q10FollowUp)  %>%
      #compute intellectual humility factor score
      calc_measures("FollowUp")
  }

  return(final_dat)

}

#' Remove duplicates from AirTable
#'
#' @section Workflow:
#' \code{\link{om_filter_data}} -> \code{\link{om_rescale}} -> \code{\link{om_clean_ppol}} -> \code{\link{om_construct_measures}} -> \code{\link{remove_dups}} -> \code{\link{om_gather}}
#'
#' @param cleaned_dat Duplicated data from AirTable
#' @export
remove_dups <- function(cleaned_dat) {

  ## remove duplicates from AirTable

  # cleaned_dat <- dat.ass


  ## pull duplicated OMIDs
  cleaned_dat %>%
    dplyr::filter(duplicated(OMID)) %>%
    dplyr::pull(OMID) -> dups

  # count_na <- function(x) sum(is.na(x), na.rm = T)

  ## pull OMIDs that are most complete + latest entries
  removed_airtable_dups <-  cleaned_dat %>%
    dplyr::mutate(createdTime = lubridate::as_datetime(createdTime)) %>%
    dplyr::filter(OMID %in% dups) %>%
    dplyr::mutate(AssessmentVersion = as.numeric(AssessmentVersion))  %>%
    dplyr::mutate(AssessmentsDone = as.numeric(AssessmentsDone))  %>%
    dplyr::mutate(count_na = rowSums(is.na(.), na.rm = T)) %>%
    dplyr::arrange(OMID, desc(createdTime), desc(AssessmentVersion), desc(AssessmentsDone), count_na) %>%
    dplyr::select(OMID, createdTime, AssessmentVersion, AssessmentsDone, count_na, dplyr::everything()) %>%
    dplyr::group_by(OMID) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    do_if(.,
        condition = is.numeric(cleaned_dat$AssessmentVersion),
        call = ~{.x %>% dplyr::mutate(AssessmentVersion = as.numeric(AssessmentVersion))}
    ) %>%
    do_if(.,
        condition = is.character(cleaned_dat$AssessmentVersion),
        call = ~{.x %>% dplyr::mutate(AssessmentVersion = as.character(AssessmentVersion))}
    ) %>%
    do_if(.,
          condition = is.numeric(cleaned_dat$AssessmentsDone),
          call = ~{.x %>% dplyr::mutate(AssessmentsDone = as.numeric(AssessmentsDone))}
    ) %>%
    do_if(.,
          condition = is.character(cleaned_dat$AssessmentsDone),
          call = ~{.x %>% dplyr::mutate(AssessmentsDone = as.character(AssessmentsDone))}
    ) %>%
    coalesce_join(join = dplyr::left_join,
                  cleaned_dat %>%
                    dplyr::mutate(createdTime = lubridate::as_datetime(createdTime)) %>%
                    dplyr::filter(OMID %in% dups)) %>%
    dplyr::mutate(AssessmentVersion = as.numeric(AssessmentVersion))

  message(stringr::str_glue("Removing {round(length(dups))} duplicates...\n"))

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
#'
#' @section Workflow:
#' \code{\link{om_filter_data}} -> \code{\link{om_rescale}} -> \code{\link{om_clean_ppol}} -> \code{\link{om_construct_measures}} -> \code{\link{remove_dups}} -> \code{\link{om_gather}}
#'
#' @param .data Assessment data
#' @param which_strings a string indicating which variables should be parsed out (default is \code{openmindR::q_c_strings})
#' @export
om_gather <- function(.data, which_strings = openmindR::q_c_strings) {

  # .data <- n3v4constructedremdups

  gathered_dat <- .data  %>%
    dplyr::rename_at(dplyr::vars(dplyr::matches("Followup")), ~stringr::str_replace(., "Followup", "FollowUp")) %>%
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
#' @param x dataseet
#' @param vars which variable to count
#' @param wt weights
#' @param sort Boolean. Sort by count.
#' @export
om_count_ <- function (x, vars, wt = NULL, sort = FALSE) {
  vars <- dplyr:::compat_lazy_dots(vars, rlang::caller_env())
  wt <- wt %||% quo(NULL)
  wt <- dplyr:::compat_lazy(wt, rlang::caller_env())
  dplyr::count(x, !!!vars, wt = !!wt, sort = sort)
}

#' Parse a Character and turn it to Datetime
#'
#'
#' @param date date (chr)
#' @export
chr_to_datetime <- function(date) {
  stringr::str_remove_all(date, "th,|rd,|st,|nd,|,") %>% lubridate::mdy_hm()
}


#' Get Assessment V6.1
#'
#'
#' @export
get_assessmentv6.1 <- function(clean_assessment) {

  pre_vars <- c("ProgTemp", "ConTemp", "MotivationProg1", "MotivationProg2", "MotivationCon1", "MotivationCon2", "CIHS_LIO1", "CIHS_LIO2", "CIHS_LIO3", "CIHS_LIO4", "GrowthMindset", "IssueDisplay", "HowImportantIssue", "IHText", "Preparedness3", "SoughtOutDifferent", "FeedbackAssessment", "GBSS1", "GBSS2", "GBSS3")

  assessment61 <- clean_assessment %>%
    dplyr::select(
      c("id", "OMID", "AccessCode", "AssessmentVersion", "AssessmentsDone"),
      c("D1", "D2", "D3", "D4", "D6",
        paste0(pre_vars, "Pre"),
        paste0(pre_vars, "Post")
      )
    )

  return(assessment61)

}




