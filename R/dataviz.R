
#' @export
gg_plot_comparison <- function(results, variable_input, assessment_version, rangesel, comparison, y_nudger = -0.05) {

  if(comparison == "PrePost"){
    results <- results  %>%
      filter(Comparison == comparison)
    T2 <- "T2"
    Post <- "Post"

    if(variable_input %in% c("C1", "C5", "C6")){

      text = paste("\n   Analysis only available for Pre - FollowUp")

      gg <- ggplot() +
        annotate("text", x = 4, y = 25, size=8, label = text) +
        theme_void() +
        theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

      return(gg)
    }

  }
  # results <- ass6_results
  # variable_input <- "AffPol1"
  # assessment_version = "V6"


  seveners <- c("GBSS",
                "GrowthMindset",
                "CIHS_LIO",
                "IntellectualHumility",
                "Preparedness", "C1", "C5", "C6", "MAA", "Motivation"
  ) %>%
    paste0(collapse = "|")

  hundreders <- c("AffPol", "Liking") %>%
    paste0(collapse = "|")


  if (rangesel == "-1 to 1") {

    min_num <- -1
    max_num <- 1
    # y_nudger <- -0.05

    # results <- results %>%
    #   select(-SD:-Post, -FollowUp) %>%
    #   rename_all(~str_remove(.x, "_01"))

  }


  # results <- results %>%
  #   # filter(Outcome %nin% seveners) %>%
  #   # dplyr::count(Outcome, sort = T)
  #   mutate(Pre = case_when(
  #     str_detect(variable_input, seveners) ~ Pre*7,
  #     str_detect(variable_input, hundreders) ~ Pre*100
  #   )) %>%# select(Pre)
  #   mutate(Post = case_when(
  #     str_detect(variable_input, seveners) ~ Post*7,
  #     str_detect(variable_input, hundreders) ~ Post*100
  #   )) %>%# select(Pre)
  #   mutate(SE = case_when(
  #     str_detect(variable_input, seveners) ~ SE*7,
  #     str_detect(variable_input, hundreders) ~ SE*100
  #   ))
  # }

  results <- results  %>%
    tidyr::gather(Question, Response, dplyr::matches("Pre|Post")) %>%
    dplyr::mutate(Type = dplyr::case_when(
      stringr::str_detect(Question, "Pre") ~ "Pre",
      stringr::str_detect(Question, "Post") ~ "Post",
      stringr::str_detect(Question, "FollowUp") ~ "FollowUp"))  %>%
    mutate(Variant = paste0("V", Variant)) %>%
    mutate(Type = fct_relevel(Type, c("Pre", "Post")))  %>%
    dplyr::mutate(Type = as.factor(Type)) %>%
    filter(variable_code == variable_input) %>%
    filter(Variant == assessment_version)


  # if(comparison == "PreFollowUp"){
  #
  #   var <- results %>%
  #     slice(1) %>%
  #     pull(variable_code)
  #
  #   PrePostFollow <- c("Pre", "Post", "FollowUp")
  #
  #
  #
  #   results <- results  %>%
  #     mutate(com_type = paste0(Comparison,Type))
  #
  #   if(var %in% c("C1", "C5", "C6")){
  #
  #     PrePostFollow <- c("Pre", "FollowUp")
  #
  #
  #     results <- results %>%
  #       mutate(Type = PrePostFollow) %>%
  #       mutate(Type = fct_relevel(Type, PrePostFollow))
  #   }
  #
  #   if(var %nin% c("C1", "C5", "C6")){
  #
  #     results <- results %>%
  #       filter(com_type != "PreFollowUpT1T3Pre") %>%
  #       select(-com_type) %>%
  #       mutate(Type = PrePostFollow) %>%
  #       mutate(Type = fct_relevel(Type, PrePostFollow))
  #
  #   }
  #
  #
  # }


  cohend <- results %>% pull(cohend) %>% round(2)

  tlab <- results  %>%
    mutate(pval = paste0("p = ", openmindR::specify_decimal(pvalue, 3))) %>%
    mutate(tlab = glue::glue("t({round(df, 2)}) = {round(tstat, 2)}, {pval}")) %>%
    pull(tlab)

  # if(comparison == "PreFollowUp"){
  #   gg <- results %>%
  #     ggplot(aes(Type, Response, fill = Type)) +
  #     geom_col() +
  #     geom_errorbar(aes(ymin = Response - 1.96*SE, ymax = Response + 1.96*SE), width = 0.1, size = 0.5) +
  #     scale_fill_manual(values = pal_om(5)[3:5]) +
  #     theme_om(legend_position = "none") +
  #     labs(x = "", caption = paste0("T1 vs. ", "T2",": " ,"Cohen's d = ", cohend[1], "; ", tlab[1], "\n",
  #                                   "T1 vs. ", "T3",": " ,"Cohen's d = ", cohend[3], "; ", tlab[3], "\n")) +
  #     ggtitle(paste0(variable_input," (Pre vs. Post vs. FollowUp)")) +
  #     ylim(min_num, max_num) +
  #     ggplot2::geom_text(aes(label = openmindR::specify_decimal(Response, 2)), size = 8, nudge_y = y_nudger) +
  #     theme(panel.grid.major.x = element_blank())
  #
  #   if (var %in% c("C1", "C5", "C6")) {
  #     gg <- gg + labs(x = "", caption = paste0("T1 vs. ", "T3",": " ,"Cohen's d = ", cohend[1], "; ", tlab[1], "\n")) +
  #       ggtitle(paste0(variable_input," (Pre vs. FollowUp)"))
  #   }
  #
  #   return(gg)
  # }

  gg <- results %>%
    ggplot(aes(Type, Response, fill = Type)) +
    geom_col() +
    geom_errorbar(aes(ymin = Response - 1.96*SE, ymax = Response + 1.96*SE), width = 0.1, size = 0.5) +
    scale_fill_manual(values = pal_om(5)[3:5]) +
    theme_om(legend_position = "none") +
    labs(x = "", caption = paste0("T1 vs. ", T2,": " ,"Cohen's d = ", cohend[1], "; ", tlab[1])) +
    ggtitle(paste0(variable_input," (Pre vs. ",Post,")")) +
    ylim(min_num, max_num) +
    ggplot2::geom_text(aes(label = openmindR::specify_decimal(Response, 2)), size = 6, nudge_y = y_nudger) +
    theme(panel.grid.major.x = element_blank())

  return(gg)
}
