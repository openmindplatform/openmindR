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
  if (not(q14_q17)) {

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

  # .data <- compare_dat_prepost

  ## this function is necessary because some variables are coded in different direction

  bind_rows(
    ## All variables that are not Q14 or Q17
    .data %>%
      filter(variable_code %nin% c("Q14", "Q17")) %>%
      dplyr::group_by(variable_code) %>%
      summarize_comparison(...,
                           q14_q17 = F),
    ## Just Q14 and Q17
    .data %>%
      filter(variable_code %in% c("Q14", "Q17")) %>%
      dplyr::group_by(variable_code) %>%
      summarize_comparison(...,
                           q14_q17 = T)
  )
}


#' Conduct t-tests and calculate Cohen's d
#'
#' This is a higher-level function that uses both "bind_questions" and "summarize_comparison" to calculate t-tests and Cohen's d on Assessment data.
#' @param gathered_dat Assessment data as long format
#' @export
om_summarize_comparisons <- function(gathered_dat) {

  # gathered_dat

  ## this is the higher level function which
  ## brings together bind_questions and summarize_comparison


  ## PrePost Data
  compare_dat_prepost <- gathered_dat %>%
    filter(Type %in% c("Pre", "Post")) %>%
    drop_na(Response) %>%
    mutate(Type = fct_relevel(Type, c("Pre", "Post"))) %>%
    ## count OMIDs and PrePost Type
    add_count(OMID, variable_code) %>%
    ## only keep cases where Pre and Post exist
    filter(n == 2)

  ## PreFollow Data
  compare_dat_prefollow <- gathered_dat %>%
    filter(Type %in% c("Pre", "FollowUp")) %>%
    drop_na(Response) %>%
    mutate(Type = fct_relevel(Type, c("Pre", "FollowUp"))) %>%
    ## count OMIDs and PreFollow Type
    add_count(OMID, variable_code) %>%
    ## only keep cases where Pre and Post exist
    filter(n == 2)

  ## Calculate Scores for all data
  moderate_dat <- compare_dat_prepost %>%
    filter(variable_code %nin% c("Q15", "Q16", "Q17")) %>%
    ## PrePost
    bind_questions(#nrows = nrow_prepost,
      waves = "PrePost") %>%
    mutate(Comparison = "PrePost") %>%
    ## PreFollow
    bind_rows(compare_dat_prefollow %>%
                filter(variable_code %nin% c("Q15", "Q16", "Q17")) %>%
                bind_questions(#nrows = nrow_prefollow,
                  waves = "PreFollow") %>%
                mutate(Comparison = "PreFollow")) %>%
    ## add indicator
    mutate(moderates = "WithModerates") %>%
    ## Deal with Culture Vars
    filter(!is.nan(cohend)) %>%
    mutate(moderates = ifelse(variable_code %in% c("C1", "C2", "C3"), "CultureVars", moderates))


  ## Calculate scores where Moderates need to be excluded
  no_moderate_dat <- compare_dat_prepost %>%
    filter(variable_code %in% c("Q15", "Q16", "Q17")) %>%
    drop_na(ppol_cat) %>%
    ## PrePost
    bind_questions(#nrows = nrow_prepost,
      waves = "PrePost") %>%
    mutate(Comparison = "PrePost") %>%
    ## PreFollow
    bind_rows(compare_dat_prefollow  %>%
                filter(variable_code %in% c("Q15", "Q16", "Q17")) %>%
                drop_na(ppol_cat) %>%
                bind_questions(#nrows = nrow_prefollow,
                  waves = "PreFollow") %>%
                mutate(Comparison = "PreFollow")) %>%
    ## add indicator
    mutate(moderates = "WithoutModerates")

  final_compared <- moderate_dat %>% bind_rows(no_moderate_dat)

  return(final_compared)
}
