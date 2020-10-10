

.get_sb_comps <- memoise::memoise({function(...) {
  StatsBombR::FreeCompetitions(...)
}})

.get_sb_matches <- memoise::memoise({function(...) {
  StatsBombR::FreeMatches(...)
}})

.get_sb_events <- memoise::memoise({function(...) {
  StatsBombR::StatsBombFreeEvents(...)
}})

retrieve_sb_events <-
  function(competition_id,
           ...,
           clean = TRUE,
           export = TRUE,
           dir = .get_dir_data(),
           file = glue::glue('events_{competition_id}{ifelse(clean, "_clean", "")}.rds'),
           path = fs::path(.get_dir_data(), file),
           overwrite = FALSE,
           f_import = rio::import,
           f_export = rio::export) {
    path_exists <- fs::file_exists(path)
    if(path_exists & !overwrite) {
      return(f_import(path, ...))
    }
    comps <- .get_sb_comps()
    
    matches <-
      comps %>% 
      filter(competition_id == !!competition_id) %>%
      .get_sb_matches() %>% 
      arrange(match_date)
    
    events <- matches %>% .get_sb_events()
    if(clean) {
      events <- events %>% StatsBombR::allclean()
    }
    f_export(events, path)
    events
  }

retrieve_sb_events_timed <- .time_it(retrieve_sb_events)
