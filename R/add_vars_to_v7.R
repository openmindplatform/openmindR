# pacman::p_load(tidyverse, openmindR)
#
# # AssessmentV7 <- airtabler::airtable(om_base, "AssessmentV7Copy")$AssessmentV7Copy$select_all() %>%
# # janitor::remove_empty()
# key <- readr::read_lines("../../Research/Projects/Keys/airtabler.txt")
#
# assessmentv7 <- om_download_at(key, tables = "AssessmentV7",
#                                clean = T, v6.1 = T) %>%
#   janitor::remove_empty() %>%
#   filter(is.na(UserType)) %>%
#   select(-OpenMindVersion, -UserType,
#          -Country, -Research,
#          -ModsCompleteN, -QScore)
# # mutate(DateStarted = lubridate::as_date(DateStarted)) %>%
# # mutate(DateFinished = lubridate::as_date(DateFinished))
#
# ## ParticipantProgress1
# pp1 <- om_download_at(key, tables = "ParticipantProgress") %>%
#   select(OMID, AccessCode, OpenMindVersion, Country,
#          Research, DateStarted, DateFinished, StepsComplete, StepScores) %>%
#   mutate(ModsCompleteN = str_count(StepsComplete, "1")) %>%
#   mutate(QScore = StepScores %>% map_dbl(~{.x %>%
#       str_split(",") %>%
#       unlist %>%
#       parse_number %>%
#       sum(na.rm = T) %>%
#       magrittr::divide_by(33) %>%
#       magrittr::multiply_by(100)
#   }
#   )) %>%
#   select(-StepsComplete, -StepScores)
#
# ## ParticipantProgress2
# pp2 <- om_download_at(key, tables = "ParticipantProgress2") %>%
#   select(OMID, AccessCode, OpenMindVersion, Country,
#          Research, DateStarted, DateFinished, ModsComplete, ModScores) %>%
#   mutate(ModsCompleteN = str_count(ModsComplete, ",") + 1) %>%
#   mutate(QScore = ModScores %>% map_dbl(~{.x %>%
#       str_split(",") %>%
#       unlist %>%
#       parse_number %>%
#       sum(na.rm = T) %>%
#       magrittr::divide_by(33) %>%
#       magrittr::multiply_by(100)
#   }
#   )) %>%
#   select(-ModsComplete, -ModScores)
#
#
#
#
# str_split(pp2$ModScores, ",") %>%
#   map(unlist)
#
# ## AccessCodes
# acs <- om_download_at(key, tables = "AccessCodes") %>%
#   select(AccessCode, UserType, GroupName)
#
# ## bring it all together
# cleaned_dat <- pp1   %>%
#   bind_rows(pp2) %>%
#   ## turn dates into date format
#   mutate(DateFinished = lubridate::as_datetime(DateFinished)) %>%
#   mutate(DateStarted = lubridate::as_datetime(DateStarted)) %>%
#   ## compute WithinADay
#   mutate(WithinADay = as.numeric(DateStarted==DateFinished)) %>%
#   ## join in Access Code Data
#   coalesce_join(acs, join = dplyr::left_join) %>%
#   ## join in Assessment data
#   coalesce_join(assessmentv7, join = dplyr::right_join) %>%
#   ## if AccessCode is IndividualUser, then UserType is IndividualUser
#   # mutate(UserType = ifelse(AccessCode == "IndividualUser",
#   # "IndividualUser", UserType)) %>%
#   mutate(Research = ifelse(is.na(Research), "No", Research)) %>%
#   drop_na(UserType) %>%
#   drop_na(DateStarted) %>%
#   mutate(QScore = as.character(QScore)) %>%
#   mutate(ModsCompleteN = as.character(ModsCompleteN)) %>%
#   # mutate(DateStarted = lubridate::as_date(DateStarted)) %>%
#   # mutate(DateFinished  = lubridate::as_date(DateFinished )) %>%
#   mutate_all(~ifelse(is.na(.x), "", .x))
#
#   # cleaned_dat %>% View()
#
# #
# # assessmentv7$DateStarted
#
# om_base <- "appjU7KUyybZ4rGvT"
#
# #
# # cleaned_dat %>%
# #   slice(1:10) %>%# pull(OMID)
# #   split(1:nrow(.)) %>%
# #   map(
# #     ~{
# #       airtabler::air_update(om_base, "AssessmentV7", record_id = .x$id, record_data = .x %>% select(OpenMindVersion, UserType, Country, Research, ModsCompleteN, QScore, DateStarted, DateFinished) %>% as.list())
# #     }
# #   )
#
# variables_to_update <- c("OpenMindVersion", "UserType",
#                          "Country", "Research",
#                          "ModsCompleteN", "QScore")
#
#
# updater <- function(var) {
#   cleaned_dat %>%
#     # slice(1:10) %>% #pull(OMID)
#     select(id, var) %>%
#     split(1:nrow(.)) %>%
#     map(
#       ~{
#         airtabler::air_update(om_base, "AssessmentV7", record_id = .x$id, record_data = .x %>% select(var) %>% as.list())
#       }
#     )
#
# }
#
# variables_to_update %>%
#   walk(updater)
#
# # [1] "7754939640757" "6646961451955"
# # [3] "8566048222619" "9712024297384"
# # [5] "8032186098087" "181522735467"
# # [7] "2615940058292" "2966422999405"
# # [9] "1859687375674" "7145903704073"
#
# assessmentv7  %>%
#   select(-UserType) %>%
#   left_join(acs) %>%
#   ## join in Access Code Data
#   # coalesce_join(acs, join = dplyr::left_join) %>%
#   mutate(UserType = ifelse(AccessCode == "IndividualUser",
#   "IndividualUser", UserType)) %>%
#   select(OMID, UserType, everything()) %>%
#   # filter(is.na(UserType)) %>%
#   View
#
# updater2 <- function(var) {
#   assessmentv7  %>%
#     # select(-UserType) %>%
#     left_join(acs) %>%
#     mutate(UserType = ifelse(AccessCode == "IndividualUser",
#                              "IndividualUser", UserType)) %>%
#     # slice(1:10) %>% #pull(OMID)
#     select(id, var) %>%
#     split(1:nrow(.)) %>%
#     map(
#       ~{
#         airtabler::air_update(om_base, "AssessmentV7", record_id = .x$id, record_data = .x %>% select(var) %>% as.list())
#       }
#     )
#
# }
#
# c("UserType") %>%
#   walk(updater2)
#
# # assessmentv7  %>%
# #   select(-UserType) %>%
# #   left_join(acs) %>% select(GroupName)
