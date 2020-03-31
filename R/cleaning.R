#' Download AirTable data
#'
#' This function downloads current AirTable data
#'
#' @md
#' @section Currently allowed input:
#' + `"AssessmentV6"`
#' + `"AssessmentV7"`
#' + `"AccessCodes"`
#' + `"ParticipantProgress"`
#' + `"ParticipantProgress2"`
#' + `"InstructorSurveyV2"`
#' + `"TechnicalInquiries"`
#'
#'@param key key for AirTable API
#'@param tables specify which tables you want to download
#'@param clean clean dataset and construct measures (only works for Assessment V6 and V7)
#'@param file specify path to download to (only works for Assessment V6 and V7)
#'@return a list with (several) dataframe(s)
#'@examples
#'#Here is a code example that will download Assessment V6,
#'#clean it, save it into a folder called "Data" under
#'#Research and filter down to only include V6.1 data.
#'key <- readr::read_lines("../../Research/Projects/Keys/airtabler.txt")
#'
#'assessmentv6 <- om_download_at(key,
#'                               tables = "AssessmentV6",
#'                               clean = TRUE,
#'                               file = "../../../Data/assessmentv6.1.csv",
#'                               v6.1 = TRUE)
#'
#'
#'#Here is another example code for downloading a clean version of Assessment v7:
#'
#'assessmentv7 <- om_download_at(key,
#'                               tables = "AssessmentV7",
#'                               clean = TRUE)
#' @export
om_download_at <- function(key, tables = c("AccessCodes","ParticipantProgress","InstructorSurveyV2", "TechnicalInquiries"), clean = F, file = NULL, v6.1 = F) {

  if (any(tables %nin% c("AccessCodes","ParticipantProgress","ParticipantProgress2","InstructorSurveyV2", "TechnicalInquiries", "AssessmentV6", "AssessmentV7"))) {
    stop("Warning: Should be one of the following: AccessCodes, ParticipantProgress, ParticipantProgress2, InstructorSurveyV2, TechnicalInquiries, AssessmentV6, AssessmentV7\n")
  }

  if (any(tables %in% c("AssessmentV4","AssessmentV5","AssessmentV6DiD", "DiDProgress", "AssessmentV6DiD", "DiDProgress"))) {
    stop("A Table you specified does not live in AirTable anymore!\n")
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

    if (v6.1) {
      final_list$dat.ass6 <- get_assessmentv6.1(final_list$dat.ass6) %>%
        dplyr::filter(AssessmentVersion == "6.1")
    }


    if (clean) {

      final_list$dat.ass6 <- final_list$dat.ass6 %>% clean_assessment6()


    }


    if (!is.null(file)) {

      readr::write_csv(x = final_list$dat.ass6, path = file)

    }

  }


  if ("AssessmentV7" %in% tables) {
    cat("Download AssessmentV7 Data\n")
    final_list$dat.ass7 <- dat.ass.1$AssessmentV7$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. AssessmentV7 Data has ", nrow(final_list$dat.ass7), " rows\n"))

    if (clean) {
      final_list$dat.ass7 <- clean_assessment7(final_list$dat.ass7)
    }


    if (!is.null(file)) {

      readr::write_csv(x = final_list$dat.ass7, path = file)

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

  if ("InstructorSurveyV2" %in% tables) {
    cat("Download Instructor Survey Data\n")
    final_list$dat.ins <- dat.ass.1$InstructorSurveyV2$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. Instructor Survey Data has ", nrow(final_list$dat.ins), " rows\n"))
  }

  if ("TechnicalInquiries" %in% tables) {
    cat("Download Technial Inquiries Data\n")
    final_list$dat.tec <- dat.ass.1$TechnicalInquiries$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. Technical Inquiries Data has ", nrow(final_list$dat.tec), " rows\n"))
  }

  if ("ParticipantProgress2" %in% tables) {
    cat("Download Participant Progress 2 Data\n")
    final_list$dat.par2 <- dat.ass.1$ParticipantProgress2$select_all() %>% tibble::as_tibble()
    cat(paste0("Done. Participant Progress Data 2 has ", nrow(final_list$dat.par2), " rows\n"))
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
#'
#' @param app.dat Assessment data from AirTable
#' @param n_assessments \code{AssessmentsDone} How many assessments do the participants need to have completed? If 1, it will only provide data for people who completed 1 assessment. If 2, it will provide all people who completed exactly 2 assessments. If 3, it will provide all people who completed all 3 assessments. (Should be 1, 2 and/or 3)
#' @param version \code{AssessmentVersion} Filter down to what asssesment version. This argument either takes a single number or vector.
#' @param accesscode \code{AccessCode} Filter down to (several) AccessCode(s)
#' @param exact_search \code{logical} This argument takes TRUE or FALSE. If you want to match AccessCodes exactly set this to TRUE. Default is FALSE. If you want to select multiple AccessCodes by exact name, use an explicit vector instead.
#' @examples
#' assessmentv7 %>%
#'    # specify which number of assessment you want to have
#'    om_filter_data(n_assessments = 1:2,
#'               # assessment version?
#'               version = 7,
#'               # select Accesscode(s)
#'               accesscode = "TuttlePen"
#'               # "TuttlePen" #try this out :)
#'    )
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
#'   \item ppol_num: numeric variable ranging from 1 "Very Progressive/left" to 7 "Very Conservative/right"#'   \item ppol_num: numeric variable ranging from 1 "Very Progressive/left" to 7 "Very Conservative/right"
#'   \item ppol_extreme: numeric variable ranging from 0 "Moderate/Middle-of-the-road" to 3 "Very Conservative/right" or "Very Progressive/left"
#'   \item ppol_cat: a factor variable which has two categories "Progressive" and "Conservative". The rest is NA.
#'   \item ppol_catmod: a factor variable which has three categories "Progressive", "Conservative" and "Moderates". The rest is NA.
#' }
#'
#'
#' @param assessment Assessment data from AirTable
#' @examples
#' assessmentv7 %>%
#'   om_clean_ppol()
#' @export
om_clean_ppol <- function(assessment) {

  ## creates ppol variables

  assessment %>%
    ## should clean characters in numeric variables first
    ## Making columns numeric where they need to be
    # dplyr::mutate_at(dplyr::vars(dplyr::matches(openmindR::var_strings)), as.numeric) %>%
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
    ## political extremes
    dplyr::mutate(ppol_extreme = dplyr::case_when(
      ppol_num == 4 ~ 0,
      ppol_num == 5 ~ 1,
      ppol_num == 6 ~ 2,
      ppol_num == 7 ~ 3,
      T ~ ppol_num
    )) %>%
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
    dplyr::mutate(ppol_cat = forcats::fct_relevel(ppol_catmod, c("Progressives",
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
    dplyr::mutate(IngroupLiking = dplyr::case_when(
      ppol_cat == "Progressives" ~ !!ProgTemp,
      ppol_cat == "Conservatives" ~ !!ConTemp
      )) %>%
    # my outgroup
    dplyr::mutate(OutgroupLiking = dplyr::case_when(
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
#'   \item Time: Pre, Post, or FollowUp
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
#' @examples
#' # Turn AffPol1 variable from v7 into long format
#' assessmentv7 %>%
#'   om_gather(which_strings = "AffPol1")
#'
#' # Turn v7 into long format
#' assessmentv7 %>%
#'   om_gather(which_strings = v7_var_strings)
#' @export
om_gather <- function(.data, which_strings = openmindR::q_c_strings) {

  # .data <- n3v4constructedremdups

  gathered_dat <- .data  %>%
    dplyr::rename_at(dplyr::vars(dplyr::matches("Followup")), ~stringr::str_replace(., "Followup", "FollowUp")) %>%
    tidyr::gather(Question, Response, dplyr::matches(which_strings)) %>%
    ## filter out pre-post and follow as a variable "Time"
    dplyr::mutate(Time = dplyr::case_when(
      stringr::str_detect(Question, "Pre") ~ "Pre",
      stringr::str_detect(Question, "Post") ~ "Post",
      stringr::str_detect(Question, "FollowUp") ~ "FollowUp"
    ))  %>%
    dplyr::mutate(variable_code = stringr::str_remove(Question, Time)) %>%
    dplyr::mutate(Response = as.numeric(Response)) %>%
    dplyr::mutate(Time = dplyr::case_when(
      stringr::str_detect(variable_code, "parednessPre") ~ "Pre",
      stringr::str_detect(variable_code, "parednessPost") ~ "Post",
      stringr::str_detect(variable_code, "parednessFollowUp") ~ "FollowUp",
      T ~ Time
    )) %>%
    dplyr::mutate(variable_code = dplyr::case_when(
      stringr::str_detect(variable_code, "parednessPre") ~ "Preparedness",
      stringr::str_detect(variable_code, "parednessPost") ~ "Preparedness",
      stringr::str_detect(variable_code, "parednessFollowUp") ~ "Preparedness",
      T ~ variable_code
    ))

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
get_assessmentv6.1 <- function(assessment) {

  pre_vars <- c("ProgTemp", "ConTemp", "DateStart", "DateFinish",
                "MotivationProg1", "MotivationProg2",
                "MotivationCon1", "MotivationCon2",
                "CIHS_LIO1", "CIHS_LIO2", "CIHS_LIO3", "CIHS_LIO4",
                "GrowthMindset", "IssueDisplay", "HowImportantIssue",
                "IHText", "Preparedness3", "IntellectualHumility1",
                "IntellectualHumility2", "IntellectualHumility3",
                "SoughtOutDifferent", "FeedbackAssessment",
                "GBSS1", "GBSS2", "GBSS3")

  assessment61 <- assessment %>%
    dplyr::select(
      c("id", "OMID", "AccessCode", "AssessmentVersion", "AssessmentsDone"),
      c("D1", "D2", "D3", "D4", "D6",
        paste0(pre_vars, "Pre"),
        paste0(pre_vars, "Post"),
        paste0(pre_vars, "FollowUp")
      )
    )

  return(assessment61)

}

#' Get Assessment V7
#'
#'
#' @export
clean_assessment7 <- function(assessment) {

  test_acs <- c("TESTcollege", "TESTcorp", "TESTorgadult", "TESTorgstudent", "TESThighschool", "TESTcollegeFUM", "TESTcollegeOMV3", "TEST2020")

  assessment7 <- assessment %>%
    dplyr::filter(AccessCode != "Admin") %>%
    dplyr::select(-MisclickPre, -MisclickPost, -MisclickFollowUp) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(minor)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(Opt Out)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(not asked yet)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(not asked)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(not met)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(Prefer not to say)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(Blank)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, ""), NA_character_, .x)) %>%
    dplyr::mutate(D1 = ifelse(D1 == "99", "991", D1)) %>%
    dplyr::mutate(TargetAge = ifelse(TargetAge == "99", "991", TargetAge)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "99"), NA_character_, .x)) %>%
    dplyr::mutate(D1 = ifelse(D1 == "991", "99", D1)) %>%
    dplyr::mutate(TargetAge = ifelse(TargetAge == "991", "99", TargetAge)) %>%
    dplyr::filter(!(AccessCode %in% test_acs)) %>%
    # dplyr::select(sort(tidyselect::peek_vars(), decreasing = F)) %>%
    dplyr::select(id, OMID, AccessCode, AssessmentVersion, AssessmentsDone,
                  D1, D2, D3, D4, D5, D6,
                  dplyr::everything()) %>%
    dplyr::mutate_all(as.character) %>%
    om_clean_ppol()  %>%
    om_dummy_nonwhite() %>%
    om_dummy_nonstraight() %>%
    dplyr::mutate_at(dplyr::vars(
      dplyr::contains("Date"),
      dplyr::contains("Time")
    ), ~lubridate::as_datetime(.x)) %>%
    dplyr::mutate_at(dplyr::vars(AssessmentVersion,
                                 AssessmentsDone,
                                 D1,
                                 dplyr::contains("Motivation"),
                                 dplyr::contains("GBSS"),
                                 dplyr::contains("Temp"),
                                 dplyr::contains("IH"),
                                 dplyr::contains("IntAnx"),
                                 dplyr::contains("Avoidance"),
                                 dplyr::contains("Tolerance"),
                                 dplyr::contains("Attribution"),
                                 dplyr::contains("SocialDistance"),
                                 dplyr::contains("SE", ignore.case = F),
                                 dplyr::contains("GM"),
                                 dplyr::contains("Anxiety"),
                                 dplyr::contains("Belong"),
                                 dplyr::contains("Dissent"),
                                 dplyr::contains("Age"),
                                 dplyr::contains("NQuestions"),
                                 dplyr::contains("SocialMediaUse")
    ), ~as.character(.x) %>% readr::parse_number()) %>%
    dplyr::mutate(Issue = readr::parse_number(Issue)) %>%
    mutate(SocialMediaUse = 7-SocialMediaUse) %>%
    ## Temperature Questions
    polar_measures(ProgTempPre, ConTempPre) %>%
    polar_measures(ProgTempPost, ConTempPost) %>%
    polar_measures(ProgTempFollowUp, ConTempFollowUp) %>%
    ## Motivation Questions
    ## need to be reversed correctly (first 2 then 1!)
    dplyr::mutate(MotivationProgPre = (MotivationProg2Pre + (8 - MotivationProg1Pre)) / 2) %>%
    dplyr::mutate(MotivationProgPost = (MotivationProg2Post + (8 - MotivationProg1Post)) / 2) %>%
    dplyr::mutate(MotivationProgFollowUp = (MotivationProg2FollowUp + (8 - MotivationProg1FollowUp)) / 2) %>%
    dplyr::mutate(MotivationConPre = (MotivationCon2Pre + (8 - MotivationCon1Pre)) / 2) %>%
    dplyr::mutate(MotivationConPost = (MotivationCon2Post + (8 - MotivationCon1Post)) / 2) %>%
    dplyr::mutate(MotivationConFollowUp = (MotivationCon2FollowUp + (8 - MotivationCon1FollowUp)) / 2) %>%
    dplyr::mutate(IngroupMotivationPre = dplyr::case_when(
      ppol_cat == "Progressives" ~ MotivationProgPre,
      ppol_cat == "Conservatives" ~ MotivationConPre
    )) %>%
    # my outgroup
    dplyr::mutate(OutgroupMotivationPre = dplyr::case_when(
      ppol_cat == "Conservatives" ~ MotivationProgPre,
      ppol_cat == "Progressives" ~ MotivationConPre
    )) %>%
    # my ingroup
    dplyr::mutate(IngroupMotivationPost = dplyr::case_when(
      ppol_cat == "Progressives" ~ MotivationProgPost,
      ppol_cat == "Conservatives" ~ MotivationConPost
    )) %>%
    # my outgroup
    dplyr::mutate(OutgroupMotivationPost = dplyr::case_when(
      ppol_cat == "Conservatives" ~ MotivationProgPost,
      ppol_cat == "Progressives" ~ MotivationConPost
    )) %>%
    # my ingroup
    dplyr::mutate(IngroupMotivationFollowUp = dplyr::case_when(
      ppol_cat == "Progressives" ~ MotivationProgFollowUp,
      ppol_cat == "Conservatives" ~ MotivationConFollowUp
    )) %>%
    # my outgroup
    dplyr::mutate(OutgroupMotivationFollowUp = dplyr::case_when(
      ppol_cat == "Conservatives" ~ MotivationProgFollowUp,
      ppol_cat == "Progressives" ~ MotivationConFollowUp
    )) %>%
    # compute ingroup v outgroup motivation attribution
    dplyr::mutate(MAAPre = abs(IngroupMotivationPre - OutgroupMotivationPre))%>%
    # compute ingroup v outgroup motivation attribution
    dplyr::mutate(MAAPost = abs(IngroupMotivationPost - OutgroupMotivationPost)) %>%
    # compute ingroup v outgroup motivation attribution
    dplyr::mutate(MAAFollowUp = abs(IngroupMotivationFollowUp - OutgroupMotivationFollowUp)) %>%
    # GBSS
    dplyr::mutate(GBSSPre = ((GBSS1Pre+GBSS2Pre+GBSS3Pre)/3) %>% magrittr::subtract(6, .)) %>%
    dplyr::mutate(GBSSPost = ((GBSS1Post+GBSS2Post+GBSS3Post)/3) %>% magrittr::subtract(6, .)) %>%
    dplyr::mutate(GBSSFollowUp = ((GBSS1FollowUp+GBSS2FollowUp+GBSS3FollowUp)/3) %>% magrittr::subtract(6, .)) %>%
    ## Interaction Anxiety
    dplyr::mutate(IntAnxPre = ((IntAnx1Pre+IntAnx2Pre)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(IntAnxPost = ((IntAnx1Post+IntAnx2Post)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(IntAnxFollowUp = ((IntAnx1FollowUp+IntAnx2FollowUp)/2) %>% magrittr::subtract(8, .)) %>%
    ## Avoidance
    dplyr::mutate(AvoidancePre = ((Avoidance1Pre+Avoidance2Pre)/2)  %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(AvoidancePost = ((Avoidance1Post+Avoidance2Post)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(AvoidanceFollowUp = ((Avoidance1FollowUp+Avoidance2FollowUp)/2) %>% magrittr::subtract(8, .)) %>%
    ## Tolerance
    dplyr::mutate(TolerancePre = (Tolerance1Pre+Tolerance2Pre)/2) %>%
    dplyr::mutate(TolerancePost = (Tolerance1Post+Tolerance2Post)/2) %>%
    dplyr::mutate(ToleranceFollowUp = (Tolerance1FollowUp+Tolerance2FollowUp)/2) %>%
    ## Attribution
    dplyr::mutate(AttributionPre = ((Attribution1Pre+Attribution2Pre)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(AttributionPost = ((Attribution1Post+Attribution2Post)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(AttributionFollowUp = ((Attribution1FollowUp+Attribution2FollowUp)/2) %>% magrittr::subtract(8, .)) %>%
    ## SocialDistance
    dplyr::mutate(SocialDistancePre = (SocialDistance1Pre+SocialDistance2Pre)/2) %>%
    dplyr::mutate(SocialDistancePost = (SocialDistance1Post+SocialDistance2Post)/2) %>%
    dplyr::mutate(SocialDistanceFollowUp = (SocialDistance1FollowUp+SocialDistance2FollowUp)/2) %>%
    ## IH - Subscale 1
    dplyr::mutate(IHSub1Pre = ((IH1Pre+IH2Pre)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(IHSub1Post = ((IH1Post+IH2Post)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(IHSub1FollowUp = ((IH1FollowUp+IH2FollowUp)/2) %>% magrittr::subtract(8, .)) %>%
    ## IH - Subscale 2
    dplyr::mutate(IHSub2Pre = (IH3Pre+IH4Pre)/2) %>%
    dplyr::mutate(IHSub2Post = (IH3Post+IH4Post)/2) %>%
    dplyr::mutate(IHSub2FollowUp = (IH3FollowUp+IH4FollowUp)/2) %>%
    ## IH - Subscale 3
    dplyr::mutate(IHSub3Pre = ((IH5Pre+IH6Pre)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(IHSub3Post = ((IH5Post+IH6Post)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(IHSub3FollowUp = ((IH5FollowUp+IH6FollowUp)/2) %>% magrittr::subtract(8, .)) %>%
    # GM
    dplyr::mutate(GMPre = ((8-GM1Pre)+(8-GM2Pre)+GM3Pre)/3) %>%
    dplyr::mutate(GMPost = ((8-GM1Post)+(8-GM2Post)+GM3Post)/3) %>%
    dplyr::mutate(GMFollowUp = ((8-GM1FollowUp)+(8-GM2FollowUp)+GM3FollowUp)/3) %>%
    # Belonging
    dplyr::mutate(BelongPre = ((8-Belong1Pre)+(8-Belong2Pre)+Belong3Pre)/3) %>%
    dplyr::mutate(BelongPost = ((8-Belong1Post)+(8-Belong2Post)+Belong3Post)/3) %>%
    dplyr::mutate(BelongFollowUp = ((8-Belong1FollowUp)+(8-Belong2FollowUp)+Belong3FollowUp)/3) %>%
    # SE
    dplyr::mutate(SEPre = ((8-SE1Pre)+SE2Pre+(8-SE3Pre)+SE4Pre)/4) %>%
    dplyr::mutate(SEPost = ((8-SE1Post)+SE2Post+(8-SE3Post)+SE4Post)/4) %>%
    dplyr::mutate(SEFollowUp = ((8-SE1FollowUp)+SE2FollowUp+(8-SE3FollowUp)+SE4FollowUp)/4) %>%
    # Anxiety
    dplyr::mutate(AnxietyPre = ((Anxiety1Pre+Anxiety2Pre+Anxiety3Pre+Anxiety4Pre)/4) %>% magrittr::subtract(6, .)) %>%
    dplyr::mutate(AnxietyPost = ((Anxiety1Post+Anxiety2Post+Anxiety3Post+Anxiety4Post)/4) %>% magrittr::subtract(6, .)) %>%
    dplyr::mutate(AnxietyFollowUp = ((Anxiety1FollowUp+Anxiety2FollowUp+Anxiety3FollowUp+Anxiety4FollowUp)/4) %>% magrittr::subtract(6, .)) %>%
    # Dissent
    dplyr::mutate(DissentPre = ((Dissent1Pre+Dissent2Pre+Dissent3Pre+Dissent4Pre+(8-Dissent5Pre))/5) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(DissentPost = ((Dissent1Post+Dissent2Post+Dissent3Post+Dissent4Post+(8-Dissent5Post))/5) %>% magrittr::subtract(8, .))  %>%
    dplyr::mutate(DissentFollowUp = ((Dissent1FollowUp+Dissent2FollowUp+Dissent3FollowUp+Dissent4FollowUp+(8-Dissent5FollowUp))/5) %>% magrittr::subtract(8, .))  %>%
    ## IHCulture - Subscale 1
    dplyr::mutate(IHCultureSub1Pre = ((IHCulture1Pre+IHCulture2Pre)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(IHCultureSub1Post = ((IHCulture1Post+IHCulture2Post)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(IHCultureSub1FollowUp = ((IHCulture1FollowUp+IHCulture2FollowUp)/2) %>% magrittr::subtract(8, .)) %>%
    ## IHCulture - Subscale 2
    dplyr::mutate(IHCultureSub2Pre = ((IHCulture3Pre)+(IHCulture4Pre))/2) %>%
    dplyr::mutate(IHCultureSub2Post = ((IHCulture3Post)+(IHCulture4Post))/2) %>%
    dplyr::mutate(IHCultureSub2FollowUp = ((IHCulture3FollowUp)+(IHCulture4FollowUp))/2) %>%
    ## IHCulture - Subscale 3
    dplyr::mutate(IHCultureSub3Pre = ((IHCulture5Pre+IHCulture6Pre)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(IHCultureSub3Post = ((IHCulture5Post+IHCulture6Post)/2) %>% magrittr::subtract(8, .)) %>%
    dplyr::mutate(IHCultureSub3FollowUp = ((IHCulture5FollowUp+IHCulture6FollowUp)/2) %>% magrittr::subtract(8, .))

  return(assessment7)

}

#' Get Assessment V6
#'
#'
#' @export
clean_assessment6 <- function(assessment) {

  test_acs <- c("TESTcollege", "TESTcorp", "TESTorgadult", "TESTorgstudent", "TESThighschool", "TESTcollegeFUM", "TESTcollegeOMV3", "TEST2020", "NA")

  assessment6 <- assessment %>%
    dplyr::filter(AccessCode != "Admin") %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(minor)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(Opt Out)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(not asked yet)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(not asked)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(not met)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(Prefer not to say)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, "(Blank)"), NA_character_, .x)) %>%
    dplyr::mutate_all(~ifelse(magrittr::equals(.x, ""), NA_character_, .x)) %>%
    # dplyr::select(sort(tidyselect::peek_vars(), decreasing = T)) %>%
    dplyr::filter(!(AccessCode %in% test_acs)) %>%
    dplyr::mutate_all(as.character) %>%
    om_clean_ppol()   %>%
    om_dummy_nonwhite() %>%
    om_dummy_nonstraight() %>%
    dplyr::mutate_at(dplyr::vars(
      dplyr::contains("Date")
    ), ~openmindR::chr_to_datetime(.x)) %>%
    dplyr::mutate_at(dplyr::vars(
      dplyr::contains("Time")
    ), ~lubridate::as_datetime(.x)) %>%
    dplyr::select(id, OMID, AccessCode, AssessmentVersion, AssessmentsDone,
                  D1, D2, D3, D4, dplyr::contains("D5"), D6, dplyr::contains("D7"), dplyr::contains("D8"),
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
                                 dplyr::contains("IntellectualHumility"),
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
    ), ~as.character(.x) %>% readr::parse_number()) %>%
    polar_measures(ProgTempPre, ConTempPre) %>%
    polar_measures(ProgTempPost, ConTempPost) %>%
    polar_measures(ProgTempFollowUp, ConTempFollowUp) %>%
    dplyr::mutate(CIHS_LIOPre = (CIHS_LIO1Pre+CIHS_LIO2Pre+CIHS_LIO3Pre+CIHS_LIO4Pre)/4) %>%
    dplyr::mutate(CIHS_LIOPost = (CIHS_LIO1Post+CIHS_LIO2Post+CIHS_LIO3Post+CIHS_LIO4Post)/4) %>%
    dplyr::mutate(CIHS_LIOFollowUp = (CIHS_LIO1FollowUp+CIHS_LIO2FollowUp+CIHS_LIO3FollowUp+CIHS_LIO4FollowUp)/4) %>%
    dplyr::mutate(GBSSPre = (GBSS1Pre+GBSS2Pre+GBSS3Pre)/3) %>%
    dplyr::mutate(GBSSPost = (GBSS1Post+GBSS2Post+GBSS3Post)/3) %>%
    dplyr::mutate(GBSSFollowUp = (GBSS1FollowUp+GBSS2FollowUp+GBSS3FollowUp)/3) %>%
    dplyr::mutate(MotivationProgPre = (MotivationProg1Pre + (8 - MotivationProg2Pre)) / 2) %>%
    dplyr::mutate(MotivationProgPost = (MotivationProg1Post + (8 - MotivationProg2Post)) / 2) %>%
    dplyr::mutate(MotivationProgFollowUp = (MotivationProg1FollowUp + (8 - MotivationProg2FollowUp)) / 2) %>%
    dplyr::mutate(MotivationConPre = (MotivationCon1Pre + (8 - MotivationCon2Pre)) / 2) %>%
    dplyr::mutate(MotivationConPost = (MotivationCon1Post + (8 - MotivationCon2Post)) / 2) %>%
    dplyr::mutate(MotivationConFollowUp = (MotivationCon1FollowUp + (8 - MotivationCon2FollowUp)) / 2) %>%
    dplyr::mutate(PreparednessPre = Preparedness3Pre) %>%
    dplyr::mutate(PreparednessPost = Preparedness3Post) %>%
    dplyr::mutate(PreparednessFollowUp = Preparedness3FollowUp) %>%
    # my ingroup
    dplyr::mutate(IngroupMotivationPre = dplyr::case_when(
      ppol_cat == "Progressives" ~ MotivationProgPre,
      ppol_cat == "Conservatives" ~ MotivationConPre
    )) %>%
    # my outgroup
    dplyr::mutate(OutgroupMotivationPre = dplyr::case_when(
      ppol_cat == "Conservatives" ~ MotivationProgPre,
      ppol_cat == "Progressives" ~ MotivationConPre
    )) %>%
    # my ingroup
    dplyr::mutate(IngroupMotivationPost = dplyr::case_when(
      ppol_cat == "Progressives" ~ MotivationProgPost,
      ppol_cat == "Conservatives" ~ MotivationConPost
    )) %>%
    # my outgroup
    dplyr::mutate(OutgroupMotivationPost = dplyr::case_when(
      ppol_cat == "Conservatives" ~ MotivationProgPost,
      ppol_cat == "Progressives" ~ MotivationConPost
    )) %>%
    # my ingroup
    dplyr::mutate(IngroupMotivationFollowUp = dplyr::case_when(
      ppol_cat == "Progressives" ~ MotivationProgFollowUp,
      ppol_cat == "Conservatives" ~ MotivationConFollowUp
    )) %>%
    # my outgroup
    dplyr::mutate(OutgroupMotivationFollowUp = dplyr::case_when(
      ppol_cat == "Conservatives" ~ MotivationProgFollowUp,
      ppol_cat == "Progressives" ~ MotivationConFollowUp
    )) %>%
    # compute ingroup v outgroup motivation attribution
    dplyr::mutate(MAAPre = abs(IngroupMotivationPre - OutgroupMotivationPre))%>%
    # compute ingroup v outgroup motivation attribution
    dplyr::mutate(MAAPost = abs(IngroupMotivationPost - OutgroupMotivationPost)) %>%
    # compute ingroup v outgroup motivation attribution
    dplyr::mutate(MAAFollowUp = abs(IngroupMotivationFollowUp - OutgroupMotivationFollowUp)) %>%
    dplyr::mutate(IntellectualHumilityPre = (IntellectualHumility1Pre+IntellectualHumility2Pre+IntellectualHumility3Pre+CIHS_LIO3Pre)/4) %>%
    dplyr::mutate(IntellectualHumilityPost = (IntellectualHumility1Post+IntellectualHumility2Post+IntellectualHumility3Post+CIHS_LIO3Post)/4) %>%
    dplyr::mutate(IntellectualHumilityFollowUp = (IntellectualHumility1FollowUp+IntellectualHumility2FollowUp+IntellectualHumility3FollowUp+CIHS_LIO3FollowUp)/4)


  return(assessment6)

}



#' Merge Assessments
#'
#' @param v4 v4 data
#' @param v5 v5 data
#' @param v6 v6 data
#' @examples
#' ## get previous assessment dat
#' v4 <- read.csv("../../../Data/2019-10-29_assessmentv4.csv")
#' v5 <- read.csv("../../../Data/2019-10-29_assessmentv5.csv")
#'
#' ## get key
#' key <- readr::read_lines("../../Keys/airtabler.txt")
#'
#' ## get (clean) assessment v6 data
#' v6 <- om_download_at(key = key, tables = "AssessmentV6", clean = T)
#'
#'
#' ## merge all three datasets and only keep common variables
#' merge_assessments(v4, v5, v6)
#' @export
merge_assessments <- function(v4, v5, v6) {

  suppressMessages(
    suppressWarnings(
      v4_v5 <- v4 %>%
        bind_rows(v5) %>%
        mutate(OMID = as.character(OMID)) %>%
        mutate(AffPol1Pre = Q14Pre) %>%
        mutate(AffPol1Post = Q14Post) %>%
        mutate(OutgroupLikingPre = Q16Pre) %>%
        mutate(OutgroupLikingPost = Q16Post) %>%
        mutate(AffPol2Pre = Q17Pre) %>%
        mutate(AffPol2Post = Q17Post) %>%
        mutate(IHPre = Q18Pre) %>%
        mutate(IHPost = Q18Post) %>%
        mutate(GrowthMindsetPre = Q11Pre) %>%
        mutate(GrowthMindsetPost = Q11Post) %>%
        select(OMID, AssessmentVersion, ppol_cat, AffPol1Pre:IHPost, GrowthMindsetPre, GrowthMindsetPost)
    )
  )

  suppressMessages(
    suppressWarnings(
      v6 <- v6 %>%
        om_clean_ppol() %>%
        polar_measures(ProgTempPre, ConTempPre) %>%
        polar_measures(ProgTempPost, ConTempPost) %>%
        select(-IngroupLikingPre, -IngroupLikingPost) %>%
        mutate(IHPre = (CIHS_LIO1Pre+CIHS_LIO2Pre+CIHS_LIO3Pre+CIHS_LIO4Pre)/4) %>%
        mutate(IHPost = (CIHS_LIO1Post+CIHS_LIO2Post+CIHS_LIO3Post+CIHS_LIO4Post)/4) %>%
        select(OMID, AssessmentVersion, ppol_cat, AffPol1Pre:IHPost, GrowthMindsetPre, GrowthMindsetPost) %>%
        mutate_at(vars(AffPol1Pre:GrowthMindsetPost), ~tidytemplate::range01(.x))
    )
  )

  suppressMessages(
    master_data <- v4_v5 %>%
      bind_rows(v6)
  )

  return(master_data)

}


#' Reverse Code items
#'
#' Reverse codes items and adds them at the end of the dataset with a "_Rev" at the end.
#'
#' @param assessment v7 data
#' @examples
#' assessmentv7 %>%
#'   om_reverse_code()
#' @export
om_reverse_code <- function(assessment) {

  # reverse <- c("IH3", "IH4", "GM3", "SE2", "SE4", "Belong3", "Dissent5", "IHCulture3", "IHCulture4", "SocialDistance1", "SocialDistance2")

  reverse <- c("IH1", "IH2", "IH5", "IH6",
               "GM1", "GM2",
               "SE1", "SE3",
               "Belong1", "Belong2",
               "Dissent1", "Dissent2", "Dissent3", "Dissent4",
               "IHCulture1", "IHCulture2", "IHCulture5", "IHCulture6",
               "IntAnx1", "IntAnx2",
               "Avoidance1", "Avoidance2",
               "Attribution1", "Attribution2", "Attribution3"#,
               # "SocialDistance1", "SocialDistance2"#,
               # "Anxiety1",  "Anxiety2", "Anxiety3", "Anxiety4",
               # "GBSS1", "GBSS2", "GBSS3",
               # "MotivationProg1", "MotivationProg2", "MotivationCon1", "MotivationCon2"
               )


  reversePre <- reverse %>%
    paste0(., "Pre")
  reversePost <- reverse %>%
    paste0(., "Post")
  reverseFollowUp <- reverse %>%
    paste0(., "FollowUp")

  reverse <- c(reversePre, reversePost, reverseFollowUp)

  final <- assessment  %>%
    dplyr::mutate_at(dplyr::vars(reverse),  .funs = list(Rev = ~ 8 - .))

  reverse <- c("Anxiety1", "Anxiety2", "Anxiety3", "Anxiety4", "GBSS1", "GBSS2", "GBSS3")
  reversePre <- reverse %>%
    paste0(., "Pre")
  reversePost <- reverse %>%
    paste0(., "Post")
  reverseFollowUp <- reverse %>%
    paste0(., "FollowUp")

  reverse <- c(reversePre, reversePost, reverseFollowUp)


  final <- final  %>%
    dplyr::mutate_at(dplyr::vars(reverse),  .funs = list(Rev = ~ 6 - .))


  reverse <- c("MotivationProg1", "MotivationCon1")
  reversePre <- reverse %>%
    paste0(., "Pre")
  reversePost <- reverse %>%
    paste0(., "Post")
  reverseFollowUp <- reverse %>%
    paste0(., "FollowUp")

  reverse <- c(reversePre, reversePost, reverseFollowUp)


  final <- final  %>%
    dplyr::mutate_at(dplyr::vars(reverse),  .funs = list(Rev = ~ 7 - .))


  return(final)

}



#' Code a dummy variable for white/nonwhite
#'
#' This function creates a dummy variable from D3 (Race) called \code{race_nonwhite} and codes people who identify only as white as 0 and everyone else as 1.
#'
#' @param assessment assessment data
#' @examples
#' assessmentv7 %>%
#'   om_dummy_nonwhite()
#' @export
om_dummy_nonwhite <- function(assessment) {
  assessment <- assessment %>%
    dplyr::mutate(race_nonwhite = ifelse(!(stringr::str_detect(D3, "\\)\\(")) & stringr::str_detect(D3, "White/Caucasian"), 0, 1))

  return(assessment)
}

#' Code a dummy variable for straight/non-straight
#'
#' This function creates a dummy variable from D5 (Sexuality) called \code{sex_nonstraight} and codes people who identify as heterosexual as 0 and everyone else as 1.
#'
#' @param assessment assessment data
#' @examples
#' assessmentv7 %>%
#'   om_dummy_nonstraight()
#' @export
om_dummy_nonstraight <- function(assessment) {
  assessment <- assessment %>%
    dplyr::mutate(sex_nonstraight = ifelse(stringr::str_detect(D5, "Heterosexual/straight"), 0, 1))

  return(assessment)
}
