summary_se <- function(data, measurevar, idvar, betweenvars) {
  require('dplyr')
  require('lazyeval')

  summarized = data %>%
    group_by_(.dots=c(idvar, betweenvars)) %>%
    dplyr::summarise_(.dots = list(
      MeasureVar = interp( ~mean( MEASUREVAR, na.rm=TRUE), MEASUREVAR = as.name(measurevar)  )
    )) %>%
    ungroup() %>%
    group_by_(.dots=betweenvars) %>%
    dplyr::summarise(N    = n(),
                     Mean = mean(MeasureVar, na.rm=TRUE),
                     SD   = sd(MeasureVar, na.rm=TRUE),
                     SE   = SD / sqrt(N)
    ) %>%
    ungroup()

  colnames(summarized)[colnames(summarized)=='Mean'] = measurevar

  return( summarized )

}

summary_se_within <- function(data, measurevar, idvar, withinvars= NULL, betweenvars= NULL) {
  require('dplyr')
  require('lazyeval')

  # Really just between subjects?
  if (is.null(withinvars)) {
    return( summary_se(data, measurevar, idvar, betweenvars) )
  }

  # Warn about impossibility of errorbars in mixed designs:
  if (!is.null(betweenvars)) {
    warning('Error bars cannot be accurately represented in mixed designs. ',
            'Treating each level of any between-subjects factors as separate experiment.')
  }

  # Collapse Multiple Observations in the Same Participant x Design Cell, Get Normed MeasureVar:
  normed = collapse_and_norm(data, measurevar, idvar, withinvars, betweenvars)

  # Get Correction factor:
  num_within_groups = prod( apply(data[,withinvars,drop=FALSE], MARGIN = 2, FUN = function(x) length(unique(x))) )
  correction_factor = sqrt( num_within_groups / (num_within_groups-1) )

  # Get Means, SDs, Etc:
  summarized = normed %>%
    group_by_(.dots= c(betweenvars, withinvars) ) %>%
    dplyr::summarise(N    = n(),
                     Mean = mean(MeasureVar, na.rm= TRUE),
                     SD   = sd(MeasureVarNormed, na.rm= TRUE),
                     SE   = SD / sqrt( N ),
                     CI   = SE * qt(.975, df = N-1)
    ) %>%
    dplyr::mutate(SD = SD*correction_factor,
                  SE = SE*correction_factor,
                  CI = CI*correction_factor)

  colnames(summarized)[colnames(summarized)=='Mean'] = measurevar

  summarized

}

collapse_and_norm = function(data, measurevar, idvar, withinvars, betweenvars= NULL) {
  require('dplyr')
  require('lazyeval')

  # Collapse Multiple Observations in the Same Participant x Design Cell :
  collapsed = data %>%
    group_by_(.dots= c(idvar, betweenvars, withinvars)) %>%
    dplyr::summarise_(.dots = list(MeasureVar = interp( ~mean( MEASUREVAR,na.rm=TRUE), MEASUREVAR = as.name(measurevar) )
    )) %>%
    ungroup() %>%
    group_by_(.dots= c(idvar, betweenvars) ) %>%
    dplyr::mutate(SubMean = mean(MeasureVar, na.rm=TRUE),
                  Diff    = MeasureVar - SubMean
    ) %>%
    ungroup()

  # Get Normed Data:
  normed = collapsed %>%
    dplyr::mutate(MeasureVarNormed = (MeasureVar-SubMean) + mean(MeasureVar, na.rm=TRUE) )
}


#' Get Within Standard Errors
#'
#' @param x data
#' @param variable specify which variable you want to calculate
#' @param WaveType which wave you want to compare (should be "Post" or "FollowUp")
withinSE <- function(x, variable, WaveType) {

  # variable <- "C1"

  SEdat <- x %>%
    filter(variable_code == variable)

  var <- SEdat %>% distinct(variable_code) %>% pull(variable_code)

  # if (WaveType == "Post" & var %in% c("C1", "C2", "C3")) {
  #
  #   # SEdat <- SEdat %>% filter(variable_code %nin% c("C1", "C2", "C3"))
  #
  #   return(tibble(variable_code = variable))
  #
  # }


  SEdat <- summary_se_within(data = SEdat,
                             measurevar = "Response",
                             withinvars = "Type",
                             idvar = "OMID") %>%
    spread(Type, Response) %>%
    mutate(variable_code = variable)

  if (nrow(SEdat) == 2) {

    if ("Post" %in% colnames(SEdat)) {
      SEdat <- SEdat %>%
        fill(Pre) %>%
        fill(Post, .direction = "up")%>%
        fill(Pre, .direction = "up") %>%
        fill(Post) %>%
        slice(1)
    }

    if ("FollowUp" %in%  colnames(SEdat)) {
      SEdat <- SEdat %>%
        fill(Pre) %>%
        fill(FollowUp, .direction = "up")%>%
        fill(Pre, .direction = "up") %>%
        fill(FollowUp) %>%
        slice(1)
    }

  }

  SEdat <- SEdat %>%
    select(variable_code, everything())

  return(SEdat)
}

#' Conducts t-tests and calculates Cohen's d
#'
#' This is lower-level function that belongs to om_summarize_comparisons. This function is not meant to be used outside of om_summarize_comparisons.
#'
#' @param x data
#' @param waves either PrePost or PreFollow
#' @param q14_q17 logical, Q14 and Q17 are coded seperately
#' @export
summarize_comparison <- function(x, waves, q14_q17 = F) {

  ## this function calculates ttests and cohens d

  if (waves == "PrePost") {
    WaveType <- "Post"
  }
  if (waves == "PreFollow") {
    WaveType <- "FollowUp"
  }


  vars <- x %>% select(variable_code) %>% distinct() %>% pull()

  # vars <- "C1"

  if (any(c("C1", "C2", "C3") %in% vars) & WaveType == "Post") {
    x <- x %>%
      filter(variable_code %nin% c("C1", "C2", "C3"))

    vars <- x %>% select(variable_code) %>% distinct() %>% pull()
  }

  within_stats <- vars %>% map_dfr(~withinSE(x, variable = .x, WaveType = WaveType))

  # withinSE(x, "Q18", WaveType)

  # print(head(x %>% select(variable_code, Response)))

  print(count(x, Type))
  print(vars)
  # print(x)

  ## This is one direction
  if (q14_q17) {
    final_dat <- x %>% dplyr::summarize(
      cohend = abs(effsize::cohen.d(Response~Type, paired = TRUE, conf.level = 0.95)$estimate),
      cohendCIlow = abs(effsize::cohen.d(Response~Type, paired = TRUE, conf.level = 0.95)$conf.int[1]),
      cohendCIhi = abs(effsize::cohen.d(Response~Type, paired = TRUE, conf.level = 0.95)$conf.int[2]),
      tstat = abs(t.test(Response~Type, paired=TRUE)$statistic),
      pvalue = t.test(Response~Type, paired=TRUE)$p.value,
      df = t.test(Response~Type, paired=TRUE)$parameter,
      # ttests = list(broom::tidy(t.test(Response~Type, paired=TRUE, data = .))),
      percentimproved = sum((Response[Type == "Pre"] > Response[Type == WaveType])==TRUE)/(df+1)
    ) %>%
      left_join(within_stats)

    return(final_dat)

  }





  ## This is the other direction
  if (magrittr::not(q14_q17)) {

    final_dat <- x %>% dplyr::summarize(
      cohend = abs(effsize::cohen.d(Response~Type, paired = TRUE, conf.level = 0.95)$estimate),
      cohendCIlow = abs(effsize::cohen.d(Response~Type, paired = TRUE, conf.level = 0.95)$conf.int[1]),
      cohendCIhi = abs(effsize::cohen.d(Response~Type, paired = TRUE, conf.level = 0.95)$conf.int[2]),
      tstat = abs(t.test(Response~Type, paired = TRUE)$statistic),
      pvalue = t.test(Response~Type, paired = TRUE)$p.value,
      df = t.test(Response~Type, paired = TRUE)$parameter,
      # ttests = list(broom::tidy(t.test(Response~Type, paired=TRUE, data = .))),
      percentimproved = sum((Response[Type == "Pre"] < Response[Type == WaveType]) == TRUE)/(df+1)
    ) %>%
      left_join(within_stats)

    return(final_dat)
  }
}

#' Conducts t-tests and calculates Cohen's d for Q14 and Q17 seperately
#'
#' This is lower-level function that belongs to om_summarize_comparisons. This function is not meant to be used outside of om_summarize_comparisons.
#'
#' @param .data dataset
#' @param waves which wave (passed to summarize_comparison)
#' @export
bind_questions <- function(.data, waves) {

  # .data <- moderate_dat_prePostfollow

  # ## Just Q14 and Q17
  # x <-    .data %>%
  # dplyr::filter(variable_code %nin% c("Q14", "Q17")) %>%
  # dplyr::group_by(variable_code) #%>%
  # summarize_comparison(#...,
  # q14_q17 = F),

  ##IMPORTANT: This function is needed because Q14 and Q17 are coded in different directions.

  dplyr::bind_rows(
    ## All variables that are not Q14 or Q17
    .data %>%
      dplyr::filter(variable_code %nin% c("Q14", "Q17")) %>%
      dplyr::group_by(variable_code) %>%
      summarize_comparison(waves = waves,
                           q14_q17 = F),
    ## Just Q14 and Q17
    .data %>%
      dplyr::filter(variable_code %in% c("Q14", "Q17")) %>%
      dplyr::group_by(variable_code) %>%
      summarize_comparison(waves = waves,
                           q14_q17 = T)
  )
}



#' A pipable if statement
#'
#' This function allows to create an if statement that can be used within a pipable workflow
#'
#' @importFrom magrittr %>%
#' @importFrom rlang parse_expr
#' @param .data tibble
#' @param condition logical test
#' @param call a formula descibing a pipe to be evaluated if condition is \code{code}
#' @examples
#' any_condition <- TRUE
#'
#' mtcars %>%
#' do_if(any_condition, ~{
#' .x %>%
#'  dplyr::filter(cyl == 6) %>%
#'  dplyr::mutate(x = disp > 170)
#' })
#' @export
do_if <- function(.data, condition, call){

  if(condition){
    .x <- .data

    call_str <- call %>%
      as.character %>%
      .[2]

    out <- eval(rlang::parse_expr(call_str))

    return(out)
  } else {
    return(.data)
  }
}



#' Conduct t-tests and calculate Cohen's d
#'
#' This is lower-level function that belongs to om_summarize_comparisons. This function is not meant to be used outside of om_summarize_comparisons.
#' @param gathered_dat Assessment data as long format
#' @param  compare With the `compare` argument you can specify either \code{"PrePost"}, \code{"PreFollow"} or both \code{c("PrePost", "PreFollow")} comparisons (the latter is the default).
#' @export
om_compare <- function(gathered_dat, compare = c("PrePost", "PreFollow", "PrePostFollow")) {

  # gathered_dat <- n3v4long

  # compare = c("PrePost", "PreFollow", "PrePostFollow")

  if ("PrePostFollow" %in% compare) {
    ## PrePost Data
    compare_dat_prepostfollow <- gathered_dat %>%
      dplyr::filter(Type %in% c("Pre", "Post", "FollowUp")) %>%
      tidyr::drop_na(Response) %>%
      # dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "Post", "FollowUp"))) %>%
      ## count OMIDs and PrePost Type
      dplyr::add_count(OMID, variable_code) %>%
      ## only keep cases where Pre, Post and FollowUp exist
      dplyr::filter(n == 3)

    # debugonce(withinSE)

    ## Calculate Scores for all data
    moderate_dat_prePostfollow <- compare_dat_prepostfollow %>%
      ## remove vars that depend on moderates being excluded (Q20 only for AV4)
      dplyr::filter(variable_code %nin% c("Q15", "Q16", "Q17", "Q20", "C1", "C2", "C3") | (variable_code == "Q20" & AssessmentVersion == 4)) %>%
      dplyr::filter(Type %in% c("Pre", "Post")) %>%
      dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "Post"))) %>%
      ## PrePost
      bind_questions(waves = "PrePost") %>%
      dplyr::mutate(Comparison = "PrePostFollowT1T2") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithModerates") %>%
      ## Deal with Culture Vars
      dplyr::filter(!is.nan(cohend)) %>%
      dplyr::mutate(moderates = ifelse(variable_code %in% c("C1", "C2", "C3"), "CultureVars", moderates))

    ## Calculate scores where Moderates need to be excluded
    no_moderate_dat_prePostfollow <- compare_dat_prepostfollow %>%
      ## only include vars that depend on moderates being excluded (Q20 only for AV5+)
      dplyr::filter(variable_code %in% c("Q15", "Q16", "Q17") | (variable_code == "Q20" & AssessmentVersion >= 5)) %>%
      dplyr::filter(Type %in% c("Pre", "Post")) %>%
      dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "Post"))) %>%
      tidyr::drop_na(ppol_cat) %>%
      ## PrePost
      bind_questions(waves = "PrePost") %>%
      dplyr::mutate(Comparison = "PrePostFollowT1T2") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithoutModerates")


    ## Calculate Scores for all data
    moderate_dat_prepostFollow <- compare_dat_prepostfollow %>%
      ## remove vars that depend on moderates being excluded (Q20 only for AV4)
      dplyr::filter(variable_code %nin% c("Q15", "Q16", "Q17", "Q20", "C1", "C2", "C3") | (variable_code == "Q20" & AssessmentVersion == 4)) %>%
      dplyr::filter(Type %in% c("Pre", "FollowUp")) %>%
      dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "FollowUp"))) %>%
      ## PreFollow
      bind_questions(waves = "PreFollow") %>%
      dplyr::mutate(Comparison = "PrePostFollowT1T3") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithModerates") %>%
      ## Deal with Culture Vars
      dplyr::filter(!is.nan(cohend)) %>%
      dplyr::mutate(moderates = ifelse(variable_code %in% c("C1", "C2", "C3"), "CultureVars", moderates))


    ## Calculate scores where Moderates need to be excluded
    no_moderate_dat_prepostFollow <- compare_dat_prepostfollow  %>%
      dplyr::filter(Type %in% c("Pre", "FollowUp")) %>%
      dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "FollowUp"))) %>%
      ## only include vars that depend on moderates being excluded (Q20 only for AV5+)
      dplyr::filter(variable_code %in% c("Q15", "Q16", "Q17") | (variable_code == "Q20" & AssessmentVersion >= 5)) %>%
      tidyr::drop_na(ppol_cat) %>%
      ## PreFollow
      bind_questions(waves = "PreFollow") %>%
      dplyr::mutate(Comparison = "PrePostFollowT1T3") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithoutModerates")

    ## Calculate scores for C1, C2 and C3
    culture_vars <- gathered_dat %>%
      dplyr::filter(Type %in% c("Pre", "FollowUp")) %>%
      dplyr::filter(variable_code %in% c("C1", "C2", "C3")) %>%
      tidyr::drop_na(Response) %>%
      # dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "Post", "FollowUp"))) %>%
      ## count OMIDs and PrePost Type
      dplyr::add_count(OMID, variable_code) %>%
      ## only keep cases where Pre and Post exist
      dplyr::filter(n == 2) %>%
      dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "FollowUp")))  %>%
      dplyr::group_by(variable_code) %>%
      ## PreFollow
      summarize_comparison(waves = "PreFollow",
                           q14_q17 = F) %>%
      dplyr::mutate(Comparison = "PrePostFollowT1T3") %>%
      ## add indicator
      dplyr::mutate(moderates = "CultureVars") %>%
      ## Deal with Culture Vars
      dplyr::filter(!is.nan(cohend))


    final_compared_prePostfollow <- moderate_dat_prePostfollow %>% dplyr::bind_rows(no_moderate_dat_prePostfollow)
    final_compared_prepostFollow <- moderate_dat_prepostFollow %>% dplyr::bind_rows(no_moderate_dat_prepostFollow) %>% dplyr::bind_rows(culture_vars)

    final_compared_prepostfollow <- dplyr::bind_rows(final_compared_prePostfollow, final_compared_prepostFollow)
  }

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
      ## remove vars that depend on moderates being excluded (Q20 only for AV4)
      dplyr::filter(variable_code %nin% c("Q15", "Q16", "Q17", "Q20", "C1", "C2", "C3") | (variable_code == "Q20" & AssessmentVersion == 4)) %>%
      ## PrePost
      bind_questions(waves = "PrePost") %>%
      dplyr::mutate(Comparison = "PrePost") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithModerates") %>%
      ## Deal with Culture Vars
      dplyr::filter(!is.nan(cohend)) %>%
      dplyr::mutate(moderates = ifelse(variable_code %in% c("C1", "C2", "C3"), "CultureVars", moderates))

    ## Calculate scores where Moderates need to be excluded
    no_moderate_dat_prepost <- compare_dat_prepost %>%
      ## only include vars that depend on moderates being excluded (Q20 only for AV5+)
      dplyr::filter(variable_code %in% c("Q15", "Q16", "Q17") | (variable_code == "Q20" & AssessmentVersion >= 5)) %>%
      tidyr::drop_na(ppol_cat) %>%
      ## PrePost
      bind_questions(waves = "PrePost") %>%
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
      ## remove vars that depend on moderates being excluded (Q20 only for AV4)
      dplyr::filter(variable_code %nin% c("Q15", "Q16", "Q17", "Q20", "C1", "C2", "C3") | (variable_code == "Q20" & AssessmentVersion == 4)) %>%
      ## PreFollow
      bind_questions(waves = "PreFollow") %>%
      dplyr::mutate(Comparison = "PreFollow") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithModerates") %>%
      ## Deal with Culture Vars
      dplyr::filter(!is.nan(cohend)) %>%
      dplyr::mutate(moderates = ifelse(variable_code %in% c("C1", "C2", "C3"), "CultureVars", moderates))


    ## Calculate scores where Moderates need to be excluded
    no_moderate_dat_prefollow <- compare_dat_prefollow  %>%
      ## only include vars that depend on moderates being excluded (Q20 only for AV5+)
      dplyr::filter(variable_code %in% c("Q15", "Q16", "Q17") | (variable_code == "Q20" & AssessmentVersion >= 5)) %>%
      tidyr::drop_na(ppol_cat) %>%
      ## PreFollow
      bind_questions(waves = "PreFollow") %>%
      dplyr::mutate(Comparison = "PreFollow") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithoutModerates")

    final_compared_prefollow <- moderate_dat_prefollow %>% dplyr::bind_rows(no_moderate_dat_prefollow)
  }

  ## if both PrePost and PreFollow are given
  if (all(c("PrePost" ,"PreFollow", "PrePostFollow") %in% compare)) {

    final_compared <- final_compared_prepost %>%
      dplyr::bind_rows(final_compared_prefollow) %>%
      dplyr::bind_rows(final_compared_prepostfollow)

  }

  if (all("PrePost" == compare)) final_compared <- final_compared_prepost
  if (all("PreFollow" == compare)) final_compared <- final_compared_prefollow
  if (all("PrePostFollow" == compare)) final_compared <- final_compared_prepostfollow

  final_compared <- final_compared %>%
    dplyr::mutate(cohendCIlow2 = ifelse(cohendCIhi < cohendCIlow, cohendCIhi, cohendCIlow)) %>%
    dplyr::mutate(cohendCIhi2 = ifelse(cohendCIhi < cohendCIlow, cohendCIlow, cohendCIhi)) %>%
    dplyr::select(-cohendCIlow, -cohendCIhi) %>%
    dplyr::rename(cohendCIlow = cohendCIlow2,
                  cohendCIhi = cohendCIhi2)


  return(final_compared)
}

#' Conduct t-tests and calculate Cohen's d
#'
#' This is a higher-level function that uses "om_compare", "bind_questions" and "summarize_comparison" to calculate t-tests and Cohen's d on Assessment data.
#' @param gathered_dat Assessment data as long format
#' @param compare With the `compare` argument you can specify either \code{"PrePost"}, \code{"PreFollow"} or both \code{c("PrePost", "PreFollow")} comparisons (the latter is the default).
#' @param aversion AssessmentVersion should be one of \code{"V4"}, \code{"V5/V5.1"} or \code{"All"}
#' @export
om_summarize_comparisons <- function(gathered_dat, aversion = "All", compare = c("PrePost", "PreFollow", "PrePostFollow")) {

  # Variant <- "V5"
  # aversion <- "V4"

  gathered_dat <- gathered_dat %>%
    do_if(.data = .,
          condition = aversion == "V4",
          call = ~{
            .x %>%
              dplyr::filter(AssessmentVersion == 4)
          }
    ) %>%
    do_if(.data = .,
          condition = aversion == "V5/V5.1",
          call = ~{
            .x %>%
              dplyr::filter(AssessmentVersion >= 5)
          }
    )


  basicsummarystats <- gathered_dat %>%
    om_compare(compare) %>%
    do_if(.data = .,
           condition = aversion == "V5/V5.1",
           call = ~{
             .x %>%
               dplyr::left_join(assessmentv5_codebook %>%
                                  dplyr::rename(variable_code = Mapping)) %>%
               dplyr::mutate(Variant = "V5/V5.1")
                     }
    ) %>%
    do_if(.data = .,
          condition = aversion == "V4",
          call = ~{
            .x %>%
              dplyr::left_join(assessmentv4_codebook %>%
                                 dplyr::rename(variable_code = Mapping)) %>%
              dplyr::mutate(Variant = "V4")
                     }
    ) %>%
    do_if(.data = .,
          condition = aversion == "All",
          call = ~{
            .x %>%
              dplyr::left_join(assessmentv5_codebook %>%
                                 dplyr::rename(variable_code = Mapping)) %>%
              dplyr::mutate(Variant = "All")
                     }
    ) %>%
    dplyr::rename(Question_txt = Content) %>%
    dplyr::mutate(Question_txt = dplyr::case_when(
      variable_code == "Q14" ~ "Affective Polarization",
      variable_code == "Q15" ~ "Liking for Ingroup",
      variable_code == "Q16" ~ "Liking for Outgroup",
      variable_code == "Q17" ~ "Ingroup-Outgroup Polarization",
      variable_code == "Q18" ~ "Intellectual Humility",
      variable_code == "Q19" ~ "Perspective-Taking",
      variable_code == "Q20" ~ "Social Closeness",
      T ~ Question_txt
    )) %>%
    dplyr::mutate(Outcome = dplyr::case_when(
      variable_code == "Q14" ~ 'Affective Polarization Measure',
      variable_code == "Q15" ~  'Liking of Ingroup',
      variable_code == "Q16" ~  'Liking of Outgroup',
      variable_code == "Q17" ~  'Ingroup-Outgroup Polarization Measure',
      variable_code == "Q18" ~  'Intellectual Humility Measure',
      variable_code == "Q19" ~ "Perspective-Taking",
      variable_code == "Q20" ~ "Social Closeness",
      T ~ Construct
    )) %>%
    dplyr::select(Outcome, Question_txt, cohend:percentimproved, variable_code,
                  N, sd = SD, se = SE, ci = CI, Pre, Post, FollowUp, Comparison, moderates, Variant) %>%
    tidyr::drop_na(Outcome)

  return(basicsummarystats)

}


#' Prepare paired data for plot with within subject error term
#'
#' This function calculates several measures for plotting
#' @param gathered_dat Assessment data as long format
#' @param aversion AssessmentVersion should be one of \code{"V4"}, \code{"V5/V5.1"} or \code{"All"}
#' @export
om_label_stats <- function(gathered_dat, aversion = "All") {

  gathered_dat <- gathered_dat  %>%
    openmindR::do_if(.data = .,
                     condition = aversion == "V4",
                     call = ~{
                       .x %>%
                         dplyr::filter(AssessmentVersion == 4)
                     }
    ) %>%
    openmindR::do_if(.data = .,
                     condition = aversion == "V5/V5.1",
                     call = ~{
                       .x %>%
                         dplyr::filter(AssessmentVersion >= 5)
                     }
    )

  plot_dat <- dplyr::bind_rows(
    openmindR::q_strings_seps %>%
      purrr::map_dfr(~summary_se_within(subset(gathered_dat, variable_code == .x),
                                             measurevar = "Response",
                                             withinvars = "Type",
                                             idvar = "OMID", na.rm = T) %>%
                       dplyr::mutate(variable_code = .x)) %>%
      dplyr::mutate(Variant = aversion) ,

    openmindR::c_strings_seps %>%
      purrr::map_dfr(~summary_se_within(subset(gathered_dat, variable_code == .x),
                                             measurevar = "Response",
                                             withinvars = "Type",
                                             idvar = "OMID", na.rm = T) %>%
                       dplyr::mutate(variable_code = .x)) %>%
      mutate(Variant = aversion) ,

    c("Q15", "Q16", "Q17") %>%
      purrr::map_dfr(~summary_se_within(subset(gathered_dat %>% tidyr::drop_na(ppol_cat), variable_code == .x),
                                             measurevar = "Response",
                                             withinvars = "Type",
                                             idvar = "OMID", na.rm = T) %>%
                       dplyr::mutate(variable_code = .x)) %>%
      dplyr::mutate(Variant = aversion)
  )

  final_dat <- plot_dat %>% dplyr::as_tibble()

  return(final_dat)

}



#' Run mixed effects model
#'
#' This function performs mixed models (Currently only works on Ann Miller experimental data)
#' @param gathered_dat Assessment data as long format
#' @param question Specify question that you want to perform analysis for (ex: \code{"Q18"})
#' @param plot_model logical. Show a coefficient plot of the model. Default is \code{FALSE}
#' @param get_effects logical. Get marginal effects. Default is \code{FALSE}
#' @param get_tidy logical. Get a tidy dataframe with estimates. Also calculates pseudo cohen's d efecct sizes. Default is \code{FALSE}
#' @export
om_mix_models <- function(gathered_dat, question, plot_model = F, get_effects = F, get_tidy = F) {
  ##todo:this function needs examples in the documentation
  ## where do we specify what the between subjects variable being tested in the model is?
  ## for ann miller, it's "condition"... does this function simply look for that variable?
  ## need to make this more general/robust to other experiments#

  # question <- "Q18"

  # is.data.frame(mods_dat2)

  ## some data wrangling
  mods_dat <- gathered_dat %>%
    dplyr::filter(variable_code == question) %>%
    dplyr::filter(Type %in% c("Pre", "Post")) %>%
    dplyr::mutate(Condition = as.factor(Condition)) %>%
    dplyr::mutate(Type = as.factor(Type)) %>%
    dplyr::mutate(OMID = as.factor(OMID)) %>%
    tidyr::drop_na(Response) %>%
    dplyr::add_count(OMID) %>%
    dplyr::filter(n == 2) %>%
    dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "Post"))) %>%
    base::as.data.frame()

  ## Assign to a symbol that's unlikely to be in use in .GlobalEnv
  ## (and make sure this function cleans up after itself!)
  assign(".TeMpVaR", mods_dat, envir = globalenv())
  on.exit(rm(.TeMpVaR, envir = globalenv()))

  ## getting unique IDs we will need this for calculating pseudo cohens d
  individs <- mods_dat %>% dplyr::distinct(OMID) %>% nrow

  ## Run model
  lme_dat <- lme4::lmer(Response ~ Condition*Type +(1+Condition|OMID) + (1+Type|OMID),
                        data = .TeMpVaR, REML = F,
                        control = lme4::lmerControl(check.nobs.vs.nRE = "ignore"))

  final <- list(lme_dat = lme_dat)

  if (plot_model) {
    ## get coefficient plot for model
    ggmod <- lme_dat %>%
      sjPlot::plot_model(type = "std", show.p = T, show.values = T)# +
    # ggplot2::theme_minimal()

    final <- rlist::list.append(final, ggmod = ggmod)

  }



  if (get_effects) {
    ### This tells R to give the AP estimate at levels of Time and Ideology that we
    ### specify. In this example we want an estimate of AP for each level of time
    ### (Pre, Post, Follow-up) plotted at 1 sd below (liberal) and above
    ### (conservative) the midpoint (4) for ideology. It uses the info from mod1
    ### above to adjust for the within subjects variance we're throwing out.
    effects_dat <- ggeffects::ggeffect(lme_dat, c("Condition", "Type" ),
                                       x.as.factor = T,
                                       ci.lvl = .95,
                                       typical = "mean") %>%
      ### Now we turn both of these into factors to make plots easier (i.e. add labels
      ### and make sure time is in the right order, not graphed alphabetically)
      openmindR::do_if(.data = .,
                       condition = "Article" %in% unique(mods_dat$Condition),
                       call = ~{.x %>% dplyr::mutate(Condition = factor(x,  levels = c("Article","OpenMind")))}
      ) %>%
      openmindR::do_if(.data = .,
                       condition = "Delayed Treatment" %in% unique(mods_dat$Condition),
                       call = ~{.x %>% dplyr::mutate(Condition = factor(x,  levels = c("Experimental Treatment", "Delayed Treatment")))}
      ) %>%
      dplyr::mutate(Type = factor(group, levels = c("Pre", "Post")))

    final <- rlist::list.append(final, effects_dat = effects_dat)
  }

  if (get_tidy) {
    ## get tidy dataframe
    coefs <- broom::tidy(lme_dat) %>%
      dplyr::filter(group == "fixed") %>%
      dplyr::mutate(n_coef = nrow(.)) %>%
      dplyr::mutate(n_dat = individs) %>%
      ## calculate pseudo cohens d
      # todo: add note in output that it is a pseudo-d
      dplyr::mutate(d = abs(estimate/(sqrt(n_dat - n_coef)*std.error)))

    final <- rlist::list.append(final, tidy_dat = coefs)
  }


  return(final)

}


#' Plot mixed effects model
#'
#' This function plots the results of mixed models (Currently only works on Ann Miller experimental data)
#' @param effects_dat \code{effects_dat} is a dataset produced by \code{\link{om_mix_models}} and supplies the marginal effects of the model
#' @param tidy_dat \code{tidy_dat} is a dataset produced by \code{\link{om_mix_models}} and supplies the pseudo cohen's d for plotting
#' @param var_label supply a character that is plotted as title and y-axis
#' @param show_stats Show statistics on the bottom right. Only possible if you supply \code{tidy_dat}
#' @export
om_mix_plot <- function(effects_dat, tidy_dat = NULL, var_label, show_stats = T) {

  ### Here's a pretty graph of effects by time period with the proper within subjects error bars/estimates

  # effects_dat <- gm.s$effects_dat
  # tidy_dat <- gm.s$tidy_dat
  # var_label <- "Intellctual Humility"

  ggmod <-
    effects_dat %>%
    ggplot2::ggplot(ggplot2::aes(x = Type, y = predicted, fill = Condition))+
    ggplot2::geom_bar(position = ggplot2::position_dodge(),
                      stat = "identity",
                      colour = "black", # Use black outlines,
                      size = .3,show.legend=TRUE) +      # Thinner lines
    ggplot2::geom_errorbar(ggplot2::aes(ymin = predicted - std.error, ymax = predicted + std.error),
                           size=.7,    # Thinner lines
                           width=.2,
                           position=ggplot2::position_dodge(.9)) +
    ggplot2::scale_fill_manual("Condition", values = c("#3d4fa1", "#65c6c3")) +
    ggplot2::ylab(var_label)+
    ggplot2::xlab("Time Point") +
    ggplot2::coord_cartesian(ylim=c(0, 1))+
    ggplot2::ggtitle(var_label) +
    ggplot2::scale_y_continuous(expand = c(0, 0),limits = c(0,1)) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill="white"),
                   text = ggplot2::element_text(family="Poppins",size=20),
                   legend.text = ggplot2::element_text(size=12),
                   legend.position = c(.08, .81),
                   #legend.title=element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5,size=28),
                   axis.line.x = ggplot2::element_line(),
                   axis.text.x = ggplot2::element_text(hjust=0.5,size=24),
                   axis.title.x = ggplot2::element_text(hjust=0.5, size=24),
                   axis.line.y = ggplot2::element_line(),
                   axis.text.y = ggplot2::element_text(hjust=0.5,size=24),
                   axis.title.y = ggplot2::element_text(hjust=0.5, size=24),
                   legend.margin = ggplot2::margin(0, 0, 0, 0))



  if (show_stats) {
    label_dat <- tidy_dat %>%
      dplyr::select(-group) %>%
      dplyr::mutate_at(dplyr::vars(estimate, std.error, d), ~openmindR::specify_decimal(.x, 3)) %>%
      #todo: add subscript p to d because it is a pseudo-d, not actual d
      dplyr::mutate(cite_stats = stringr::str_glue("B = {estimate}, SE = {std.error}, d = {d}")) %>%
      openmindR::do_if(.data = .,
                       condition = "ConditionExperimental Treatment" %in% unique(tidy_dat$term),
                       call = ~{.x %>%
                           dplyr::mutate(label = dplyr::case_when(
                             term == "(Intercept)" ~ stringr::str_glue("N = {n_dat}\n\n"),
                             term == "ConditionExperimental Treatment" ~ stringr::str_glue("Experimental v. Delayed: {cite_stats}\n\n"),
                             term == "TypePost" ~ stringr::str_glue("Pre v. Post: {cite_stats}\n\n"),
                             term == "ConditionExperimental Treatment:TypePost" ~ stringr::str_glue("Condition X Time: {cite_stats}")
                           )
                           )}
      ) %>%
      openmindR::do_if(.data = .,
                       condition = "ConditionOpenMind" %in% unique(tidy_dat$term),
                       call = ~{.x %>%
                           dplyr::mutate(label = dplyr::case_when(
                             term == "(Intercept)" ~ stringr::str_glue("N = {n_dat}\n\n"),
                             term == "ConditionOpenMind" ~ stringr::str_glue("Article v. OpenMind: {cite_stats}\n\n"),
                             term == "TypePost" ~ stringr::str_glue("Pre v. Post: {cite_stats}\n\n"),
                             term == "ConditionOpenMind:TypePost	" ~ stringr::str_glue("Condition X Time: {cite_stats}")
                           )
                           )}
      ) %>%
      dplyr::pull(label) %>% glue::glue_collapse() %>% as.character()


    ggmod <- ggmod +
      ggplot2::labs(caption = label_dat) +
      ggplot2::theme(plot.caption = ggplot2::element_text(family = "Poppins", size = 14, colour = "#756f71"))

  }

  return(ggmod)
}

#' Run and plot mixed effects model
#'
#' This function allows to run and plot a mixed model. It makes use of both \code{\link{om_mix_models}} and \code{\link{om_mix_plot}} (currently only works on Ann Miller experimental data)
#' @param experiment Specify a dataset with experimental data
#' @param title specify a title and y-label for the plot
#' @export
om_mix_complete <- function(experiment, title) {
  #todo: would be nice to be able to specify ALL, some, or just one of the outcome variables
  # question <- "Q11"

  question <- dplyr::case_when(
    title == "Growth Mindset" ~ "Q11",
    title == "Intellectual Humility" ~ "Q18",
    title == "Affective Polarization" ~ "Q14",
    title == "Social Closeness" ~ "Q10",
    title == "Perspective-Taking" ~ "Q4"
  )

  gg_dat <- openmindR::om_mix_models(experiment,
                                     question = question,
                                     plot_model = F,
                                     get_effects = T,
                                     get_tidy = T)

  openmindR::om_mix_plot(gg_dat$effects_dat, gg_dat$tidy_dat, title)

}


#' Run T-Tests on Long Format data
#'
#' This function performs t-tests on v6 and v7 data
#' @param gathered_dat Long format data
#' @param comparison Three possible comparisons "PrePost", "PreFollowUpT1T2" or "PreFollowUpT1T3"
#' @export
om_ttest <- function(gathered_dat, comparison) {

  gathered_dat <- gathered_dat %>%
    group_split(variable_code)

  final <- gathered_dat %>%
    map_dfr(~{

      if (comparison == "PrePost"){
        internal <- .x %>%
          dplyr::filter(Type %in% c("Pre", "Post")) %>%
          dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "Post"))) %>%
          dplyr::filter(variable_code %nin% c("C1", "C5", "C6"))

        if(nrow(internal)==0){
          return(NULL)
        }

        T2 <- "Post"
      }
      if (comparison == "PreFollowUpT1T2"){
        internal <- .x %>%
          dplyr::mutate(Type = as.factor(Type)) %>%
          dplyr::mutate(OMID = as.factor(OMID)) %>%
          tidyr::drop_na(Response) %>%
          dplyr::add_count(OMID) %>%
          dplyr::filter(n == 3) %>%
          dplyr::filter(Type %in% c("Pre", "Post")) %>%
          dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "Post"))) %>%
          dplyr::filter(variable_code %nin% c("C1", "C5", "C6"))

        if(nrow(gathered_dat)==0){
          return(NULL)
        }

        T2 <- "Post"
      }
      if (comparison == "PreFollowUpT1T3"){
        var <- .x %>%
          dplyr::slice(1) %>%
          dplyr::pull(variable_code)

        OMID_n <- 3
        if(var %in% c("C1", "C5", "C6")){
          OMID_n <- 2
        }

        internal <- .x %>%
          dplyr::mutate(Type = as.factor(Type)) %>%
          dplyr::mutate(OMID = as.factor(OMID)) %>%
          tidyr::drop_na(Response) %>%
          dplyr::add_count(OMID) %>%
          dplyr::filter(n == OMID_n) %>%
          dplyr::filter(Type %in% c("Pre", "FollowUp")) %>%
          dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "FollowUp")))

        T2 <- "FollowUp"
      }

      ttest_dat <-  internal %>%
        dplyr::mutate(Type = as.factor(Type)) %>%
        dplyr::mutate(OMID = as.factor(OMID)) %>%
        tidyr::drop_na(Response) %>%
        dplyr::add_count(OMID) %>%
        dplyr::filter(n == 2)  %>%
        dplyr::mutate(var_code = variable_code) %>%
        dplyr::group_by(variable_code) %>%
        dplyr::summarize(
          cohend = abs(
            effsize::cohen.d(Response~Type, paired = TRUE, conf.level = 0.95)$estimate
          )
          ,
          cohendCIlow = #abs(
            effsize::cohen.d(Response~Type, paired = TRUE, conf.level = 0.95)$conf.int[1]
          #)
          ,
          cohendCIhi = #abs(
            effsize::cohen.d(Response~Type, paired = TRUE, conf.level = 0.95)$conf.int[2]
          #)
          ,
          tstat = abs(t.test(Response~Type, paired=TRUE)$statistic),
          pvalue = t.test(Response~Type, paired=TRUE)$p.value,
          df = t.test(Response~Type, paired=TRUE)$parameter,
          percentimproved = perc_improved(Response[Type == "Pre"],
                                          Response[Type == T2],
                                          (df+1),
                                          .data$var_code[1])


        )

      vars <- internal %>% select(variable_code) %>% dplyr::distinct() %>% dplyr::pull()

      within_stats <- vars %>% map_dfr(~openmindR:::withinSE(internal, variable = .x, WaveType = NULL))


      final <- ttest_dat %>%
        dplyr::left_join(within_stats, by = "variable_code") %>%
        mutate(N = df+1)

      return(final)


    })

  return(final)

}

#' Helper function to calculate percent of people who improved
#'
#' This function calculates percent of people who improved from pre to post/followup
#' @param all_pre Pre Data
#' @param all_post Post data
#' @param total N
#' @param variable_code variable code
#' @export
perc_improved <- function(all_pre, all_post, total, variable_code){

  ups <- c("GrowthMindset", "CIHS_LIO",
           "OutgroupLiking", "OutgroupMotivation",
           "Preparedness", "C1", "C6",
           "IntellectualHumility", "GM", "IHSub1", "IHSub2", "IHSub3",
           "IHCultureSub1", "IHCultureSub2", "IHCultureSub3", "SE",
           "Belong", "Dissent", "Tolerance", "IngroupLiking", "IngroupMotivation",
           "OutgroupLiking", "OutgroupMotivation", "MotivationCon", "MotivationProg")

  downs <- c("AffPol1", "AffPol2",
             "GBSS", "MAA", "C5", "Anxiety", "Attribution", "IntAnx", "SocialDistance", "Avoidance")

  if (variable_code %in% ups) {
    p_improv <- sum((all_pre < all_post)==TRUE)/total
  }

  if (variable_code %in% downs) {
    p_improv <- sum((all_pre > all_post)==TRUE)/total
  }

  return(p_improv)
}
