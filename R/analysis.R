#' Conducts t-tests and calculates Cohen's d
#'
#' This is lower-level function that belongs to om_summarize_comparisons. This function is not meant to be used outside of om_summarize_comparisons.
#'
#' @param x data
#' @param waves either PrePost or PreFollow
#' @param q14_q17 logical, Q14 and Q17 are coded seperately
#' @export
summarize_comparison <- function(x, waves = "PrePost", q14_q17 = F) {

  ## this function calculates ttests and cohens d

  if (waves == "PrePost") {
    WaveType <- "Post"
  }
  if (waves == "PreFollow") {
    WaveType <- "FollowUp"
  }

  ## This is one direction
  if (q14_q17) {
    final_dat <- x %>% dplyr::summarize(
      cohend = abs(effsize::cohen.d(Response~Type, paired = TRUE, conf.level = 0.95)$estimate),
      cohendCIlow = effsize::cohen.d(Response~Type, paired=TRUE, conf.level = 0.95)$conf.int[1],
      cohendCIhi = effsize::cohen.d(Response~Type, paired=TRUE, conf.level = 0.95)$conf.int[2],
      tstat = abs(t.test(Response~Type, paired=TRUE)$statistic),
      pvalue = t.test(Response~Type, paired=TRUE)$p.value,
      df = t.test(Response~Type, paired=TRUE)$parameter,
      # ttests = list(broom::tidy(t.test(Response~Type, paired=TRUE, data = .))),
      percentimproved = sum((Response[Type == "Pre"] > Response[Type == WaveType])==TRUE)/(df+1)
    )

    return(final_dat)

  }

  ## This is the other direction
  if (magrittr::not(q14_q17)) {

    final_dat <- x %>% dplyr::summarize(
      cohend = abs(effsize::cohen.d(Response~Type, paired = TRUE, conf.level = 0.95)$estimate),
      cohendCIlow = effsize::cohen.d(Response~Type, paired=TRUE, conf.level = 0.95)$conf.int[1],
      cohendCIhi = effsize::cohen.d(Response~Type, paired=TRUE, conf.level = 0.95)$conf.int[2],
      tstat = abs(t.test(Response~Type, paired=TRUE)$statistic),
      pvalue = t.test(Response~Type, paired=TRUE)$p.value,
      df = t.test(Response~Type, paired=TRUE)$parameter,
      # ttests = list(broom::tidy(t.test(Response~Type, paired=TRUE, data = .))),
      percentimproved = sum((Response[Type == "Pre"] < Response[Type == WaveType])==TRUE)/(df+1)
    )

    return(final_dat)
  }
}

#' Conducts t-tests and calculates Cohen's d for Q14 and Q17 seperately
#'
#' This is lower-level function that belongs to om_summarize_comparisons. This function is not meant to be used outside of om_summarize_comparisons.
#'
#' @param .data
#' @param ... other arguments passed to summarize_comparison
#' @export
bind_questions <- function(.data, ...) {

  # .data <- compare_dat_prefollow

  dplyr::bind_rows(
    ## All variables that are not Q14 or Q17
    .data %>%
      dplyr::filter(variable_code %nin% c("Q14", "Q17")) %>%
      dplyr::group_by(variable_code) %>%
      openmindR::summarize_comparison(...,
                           q14_q17 = F),
    ## Just Q14 and Q17
    .data %>%
      dplyr::filter(variable_code %in% c("Q14", "Q17")) %>%
      dplyr::group_by(variable_code) %>%
      openmindR::summarize_comparison(...,
                           q14_q17 = T)
  )
}

#' Pipable if workflow
#'
#' This function allows to create if statements within a pipe workflow
#' @param .x dataset
#' @param if_true logical statement
#' @param pipe_work A function that will be performed on \code{TRUE}
#' @export
if_flow <- function(.x, if_true, pipe_work) {
  {if (if_true) {
    .x %>% pipe_work
    }
  }
}

#' Conduct t-tests and calculate Cohen's d
#'
#' This is a higher-level function that uses both "bind_questions" and "summarize_comparison" to calculate t-tests and Cohen's d on Assessment data.
#' @param gathered_dat Assessment data as long format
#' @param  compare With the `compare` argument you can specify either \code{"PrePost"}, \code{"PreFollow"} or both \code{c("PrePost", "PreFollow")} comparisons (the latter is the default).
#' @export
om_summarize_comparisons <- function(gathered_dat, compare = c("PrePost", "PreFollow")) {

  # gathered_dat <- n3v4long

  ## this is the higher level function which
  ## brings together bind_questions and summarize_comparison

  if ("PrePost" %in% compare) {
    ## PrePost Data
    compare_dat_prepost <- gathered_dat %>%
      dplyr::filter(Type %in% c("Pre", "Post")) %>%
      tidyr::drop_na(Response) %>%
      dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "Post"))) %>%
      ## count OMIDs and PrePost Type
      dplyr::add_count(OMID, variable_code) %>%
      ## only keep cases where Pre and Post exist
      dplyr::filter(n == 2)

    ## Calculate Scores for all data
    moderate_dat_prepost <- compare_dat_prepost %>%
      dplyr::filter(variable_code %nin% c("Q15", "Q16", "Q17")) %>%
      ## PrePost
      openmindR::bind_questions(waves = "PrePost") %>%
      dplyr::mutate(Comparison = "PrePost") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithModerates") %>%
      ## Deal with Culture Vars
      dplyr::filter(!is.nan(cohend)) %>%
      dplyr::mutate(moderates = ifelse(variable_code %in% c("C1", "C2", "C3"), "CultureVars", moderates))


    ## Calculate scores where Moderates need to be excluded
    no_moderate_dat_prepost <- compare_dat_prepost %>%
      dplyr::filter(variable_code %in% c("Q15", "Q16", "Q17")) %>%
      tidyr::drop_na(ppol_cat) %>%
      ## PrePost
      openmindR::bind_questions(waves = "PrePost") %>%
      dplyr::mutate(Comparison = "PrePost") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithoutModerates")

    final_compared_prepost <- moderate_dat_prepost %>% dplyr::bind_rows(no_moderate_dat_prepost)
  }


  if ("PreFollow" %in% compare) {
    ## PreFollow Data
    compare_dat_prefollow <- gathered_dat %>%
      dplyr::filter(Type %in% c("Pre", "FollowUp")) %>%
      tidyr::drop_na(Response) %>%
      dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "FollowUp"))) %>%
      ## count OMIDs and PreFollow Type
      dplyr::add_count(OMID, variable_code) %>%
      ## only keep cases where Pre and Post exist
      dplyr::filter(n == 2)

    ## Calculate Scores for all data
    moderate_dat_prefollow <- compare_dat_prefollow %>%
      dplyr::filter(variable_code %nin% c("Q15", "Q16", "Q17")) %>%
      ## PreFollow
      openmindR::bind_questions(waves = "PreFollow") %>%
      dplyr::mutate(Comparison = "PreFollow") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithModerates") %>%
      ## Deal with Culture Vars
      dplyr::filter(!is.nan(cohend)) %>%
      dplyr::mutate(moderates = ifelse(variable_code %in% c("C1", "C2", "C3"), "CultureVars", moderates))


    ## Calculate scores where Moderates need to be excluded
    no_moderate_dat_prefollow <- compare_dat_prefollow  %>%
      dplyr::filter(variable_code %in% c("Q15", "Q16", "Q17")) %>%
      tidyr::drop_na(ppol_cat) %>%
      ## PreFollow
      openmindR::bind_questions(waves = "PreFollow") %>%
      dplyr::mutate(Comparison = "PreFollow") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithoutModerates")

    final_compared_prefollow <- moderate_dat_prefollow %>% dplyr::bind_rows(no_moderate_dat_prefollow)
  }

  ## if both PrePost and PreFollow are given
  if (all(c("PrePost" ,"PreFollow") %in% compare)) {

    final_compared <- final_compared_prepost %>% dplyr::bind_rows(final_compared_prefollow)

  }

  if (all("PrePost" == compare)) final_compared <- final_compared_prepost
  if (all("PreFollow" == compare)) final_compared <- final_compared_prefollow


  return(final_compared)
}
