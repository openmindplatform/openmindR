withinSE <- function(x, variable, WaveType) {

  # variable <- "C1"

  SEdat <- x %>%
    filter(variable_code == variable)

  var <- SEdat %>% distinct(variable_code) %>% pull(variable_code)

  if (WaveType == "Post" & var %in% c("C1", "C2", "C3")) {

    # SEdat <- SEdat %>% filter(variable_code %nin% c("C1", "C2", "C3"))

    return(tibble(variable_code = variable))

  }


  SEdat <- summarySEwithin(data = SEdat,
                           measurevar = "Response",
                           withinvars = "Type",
                           idvar = "OMID", na.rm = T) %>%
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
summarize_comparison <- function(x, waves = "PrePost", q14_q17 = F) {

  ## this function calculates ttests and cohens d

  if (waves == "PrePost") {
    WaveType <- "Post"
  }
  if (waves == "PreFollow") {
    WaveType <- "FollowUp"
  }


  vars <- x %>% select(variable_code) %>% distinct() %>% pull()

  # vars <- "C1"

  if (any(vars %in% c("C1", "C2", "C3") & WaveType == "Post")) {
    x <- x %>%
      filter(variable_code %nin% c("C1", "C2", "C3"))
  }

  within_stats <- vars %>% map_dfr(~withinSE(x, variable = .x, WaveType = WaveType))

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
      cohendCIlow = abs(effsize::cohen.d(Response~Type, paired=TRUE, conf.level = 0.95)$conf.int[1]),
      cohendCIhi = abs(effsize::cohen.d(Response~Type, paired=TRUE, conf.level = 0.95)$conf.int[2]),
      tstat = abs(t.test(Response~Type, paired=TRUE)$statistic),
      pvalue = t.test(Response~Type, paired=TRUE)$p.value,
      df = t.test(Response~Type, paired=TRUE)$parameter,
      # ttests = list(broom::tidy(t.test(Response~Type, paired=TRUE, data = .))),
      percentimproved = sum((Response[Type == "Pre"] < Response[Type == WaveType])==TRUE)/(df+1)
    ) %>%
    left_join(within_stats)

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

  # ## Just Q14 and Q17
  # x <- .data %>%
  # dplyr::filter(variable_code %nin% c("Q14", "Q17")) %>%
  # dplyr::group_by(variable_code)

  dplyr::bind_rows(
    ## All variables that are not Q14 or Q17
    .data %>%
      dplyr::filter(variable_code %nin% c("Q14", "Q17")) %>%
      dplyr::group_by(variable_code) %>%
      summarize_comparison(...,
                           q14_q17 = F),
    ## Just Q14 and Q17
    .data %>%
      dplyr::filter(variable_code %in% c("Q14", "Q17")) %>%
      dplyr::group_by(variable_code) %>%
      summarize_comparison(...,
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
#' any_condition <- T
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

  if ("PrePostFollow" %in% compare) {
    ## PrePost Data
    compare_dat_prepostfollow <- gathered_dat %>%
      dplyr::filter(Type %in% c("Pre", "Post", "FollowUp")) %>%
      tidyr::drop_na(Response) %>%
      dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "Post", "FollowUp"))) %>%
      ## count OMIDs and PrePost Type
      dplyr::add_count(OMID, variable_code) %>%
      ## only keep cases where Pre and Post exist
      dplyr::filter(n == 3)

    # debugonce(bind_questions)

    ## Calculate Scores for all data
    moderate_dat_prePostfollow <- compare_dat_prepostfollow %>%
      dplyr::filter(variable_code %nin% c("Q15", "Q16", "Q17")) %>%
      dplyr::filter(Type %in% c("Pre", "Post")) %>%
      dplyr::mutate(Type = forcats::fct_relevel(Type, c("Pre", "Post"))) %>%
      ## PrePost
      bind_questions(waves = "PrePost") %>%
      dplyr::mutate(Comparison = "PrePostFollow") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithModerates") %>%
      ## Deal with Culture Vars
      dplyr::filter(!is.nan(cohend)) %>%
      dplyr::mutate(moderates = ifelse(variable_code %in% c("C1", "C2", "C3"), "CultureVars", moderates))

    ## Calculate scores where Moderates need to be excluded
    no_moderate_dat_prePostfollow <- compare_dat_prepost %>%
      dplyr::filter(variable_code %in% c("Q15", "Q16", "Q17")) %>%
      dplyr::filter(Type %in% c("Pre", "Post")) %>%
      tidyr::drop_na(ppol_cat) %>%
      ## PrePost
      bind_questions(waves = "PrePost") %>%
      dplyr::mutate(Comparison = "PrePostFollow") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithoutModerates")


    ## Calculate Scores for all data
    moderate_dat_prepostFollow <- compare_dat_prepostfollow %>%
      dplyr::filter(variable_code %nin% c("Q15", "Q16", "Q17")) %>%
      dplyr::filter(Type %in% c("Pre", "FollowUp")) %>%
      ## PreFollow
      bind_questions(waves = "PreFollow") %>%
      dplyr::mutate(Comparison = "PrePostFollow") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithModerates") %>%
      ## Deal with Culture Vars
      dplyr::filter(!is.nan(cohend)) %>%
      dplyr::mutate(moderates = ifelse(variable_code %in% c("C1", "C2", "C3"), "CultureVars", moderates))


    ## Calculate scores where Moderates need to be excluded
    no_moderate_dat_prepostFollow <- compare_dat_prepostfollow  %>%
      dplyr::filter(Type %in% c("Pre", "FollowUp")) %>%
      dplyr::filter(variable_code %in% c("Q15", "Q16", "Q17")) %>%
      tidyr::drop_na(ppol_cat) %>%
      ## PreFollow
      bind_questions(waves = "PreFollow") %>%
      dplyr::mutate(Comparison = "PrePostFollow") %>%
      ## add indicator
      dplyr::mutate(moderates = "WithoutModerates")

    final_compared_prePostfollow <- moderate_dat_prePostfollow %>% dplyr::bind_rows(no_moderate_dat_prePostfollow)
    final_compared_prepostFollow <- moderate_dat_prepostFollow %>% dplyr::bind_rows(no_moderate_dat_prepostFollow)

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
      dplyr::filter(variable_code %nin% c("Q15", "Q16", "Q17")) %>%
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
      dplyr::filter(variable_code %in% c("Q15", "Q16", "Q17")) %>%
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
      dplyr::filter(variable_code %nin% c("Q15", "Q16", "Q17")) %>%
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
      dplyr::filter(variable_code %in% c("Q15", "Q16", "Q17")) %>%
      tidyr::drop_na(ppol_cat) %>%
      ## PreFollow
      bind_questions(waves = "PreFollow") %>%
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
om_summarize_comparisons <- function(gathered_dat, aversion = "All", compare = c("PrePost", "PreFollow")) {

  # Variant <- "V5"
  # aversion <- "V4"

  gathered_dat <- gathered_dat %>%
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


  basicsummarystats <- gathered_dat %>%
    om_compare(compare) %>%
    openmindR::do_if(.data = .,
          condition = aversion == "V5/V5.1",
          call = ~{
            .x %>%
              dplyr::left_join(assessmentv5_codebook %>%
              dplyr::rename(variable_code = Mapping)) %>%
              dplyr::mutate(Variant = "V5/V5.1")
          }
    ) %>%
    openmindR::do_if(.data = .,
          condition = aversion == "V4",
          call = ~{
            .x %>%
              dplyr::left_join(assessmentv4_codebook %>%
              dplyr::rename(variable_code = Mapping)) %>%
              dplyr::mutate(Variant = "V4")
          }
    ) %>%
    openmindR::do_if(.data = .,
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
      T ~ Question_txt
    )) %>%
    dplyr::mutate(Outcome = dplyr::case_when(
      variable_code == "Q14" ~ 'Affective Polarization Measure',
      variable_code == "Q15" ~  'Liking of Ingroup',
      variable_code == "Q16" ~  'Liking of Outgroup',
      variable_code == "Q17" ~  'Ingroup-Outgroup Polarization Measure',
      variable_code == "Q18" ~  'Intellectual Humility Measure',
      T ~ Construct
    )) %>%
    dplyr::select(Outcome, Question_txt, cohend:percentimproved, variable_code,
                  N, sd, se, ci, Pre, Post, FollowUp, Comparison, moderates, Variant) %>%
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
      purrr::map_dfr(~Rmisc::summarySEwithin(subset(gathered_dat, variable_code == .x),
                                             measurevar = "Response",
                                             withinvars = "Type",
                                             idvar = "OMID", na.rm = T) %>%
                       dplyr::mutate(variable_code = .x)) %>%
      dplyr::mutate(Variant = aversion) ,

    openmindR::c_strings_seps %>%
      purrr::map_dfr(~Rmisc::summarySEwithin(subset(gathered_dat, variable_code == .x),
                                             measurevar = "Response",
                                             withinvars = "Type",
                                             idvar = "OMID", na.rm = T) %>%
                       dplyr::mutate(variable_code = .x)) %>%
      mutate(Variant = aversion) ,

    c("Q15", "Q16", "Q17") %>%
      purrr::map_dfr(~Rmisc::summarySEwithin(subset(gathered_dat %>% tidyr::drop_na(ppol_cat), variable_code == .x),
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
