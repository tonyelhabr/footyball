
# # sb_clean <- StatsBombR::StatsBombFreeEvents() %>% StatsBombR::allclean()
# # sb_clean %>% write_rds('c:/users/aelhabr/desktop/r/sb_events_free_clean.rds')
# comps <- StatsBombR::FreeCompetitions() %>% as_tibble()
# comps %>% count(competition_name)
# comps %>% filter(competition_name == 'Champions League')
# matches <- comps %>% StatsBombR::FreeMatches()
# matches
# matches %>% count(competition.competition_name)
# matches_cl <- matches %>% filter(competition.competition_name == 'Champions League')
# matches_cl
# sb_clean <- matches_cl %>% select(match_id) %>% StatsBombR::StatsBombFreeEvents() %>% StatsBombR::allclean()
# sb_clean %>% write_rds('c:/users/aelhabr/desktop/r/sb_events_free_clean.rds')
# matches_cl1 <- tibble(match_id = '22912') %>% StatsBombR::get.matchFree() %>% StatsBombR::allclean()
# matches_cl1

