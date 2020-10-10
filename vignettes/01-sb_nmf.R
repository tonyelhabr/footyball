
# retrieve data ----
# path_data <- fs::path(.get_dir_data(), 'events_wc2018_clean.rds')
# if(!fs::file_exists(path_data)) {
#   comps <- StatsBombR::FreeCompetitions()
#   
#   # get all free games data from Men World Cup Rusia 2018 (id = 43)
#   matches <-
#     comps %>% 
#     filter(competition_id == 43) %>%
#     StatsBombR::FreeMatches() %>%
#     arrange(match_date)
#   
#   events <- matches %>% StatsBombR::StatsBombFreeEvents()
#   events_clean <- events %>% StatsBombR::allclean()
#   write_rds(events_clean, 'data/events_wc2018_clean.rds')
# }

events <- retrieve_sb_events_timed(competition_id = 43)
events

# py ----
players_wide_py <- 'data/unraveled_py.csv' %>% read_csv()
players_long_py <-
  players_wide_py %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(-id, names_to = 'name', values_to = 'n') %>% 
  mutate(across(name, ~as.integer(.x) + 1L))
players_long_py
players_long_py %>% arrange(desc(n))

.player_id_filt <- 3509L
events_py <- 'data/events_py.csv' %>% read_csv()
events_py
events_py %>% filter(index == 2694) %>% glimpse()
events %>% filter(player.id == .player_id_filt) %>% select(id, index, location.x, location.y)
events_py %>% filter(index == 12 & id == '513abcac-6a62-4855-8a2d-c0191c25cbed') %>% glimpse()
events_py %>% filter(id == '1c16e4cb-2364-4595-b489-dfaf0adb88a5')

events %>% filter(row_number() == 27221L) %>% select(index, player.id, player.name)
events %>% filter(row_number() == 2964) %>% glimpse()
# r ----
rng_x_yards <- .get_rng_yards('x')
rng_y_yards <- .get_rng_yards('y')
rng_x_m <- .get_rng_m('x')
rng_y_m <- .get_rng_m('y')
width_yards <- 4

bin <- function(x, width = width_yards) {
  x %>% 
    ggplot2::cut_interval(length = width) %>% 
    # str_remove_all('^[\\(|\\[]|,.*$') %>%
    str_remove_all('(^.*\\,)|[\\)\\]]$') %>%
    as.numeric()
}

seq_coord_yards <- function(coord = .get_valid_coords(), w = width_yards) {
  .validate_coord(coord)
  rng <- .get_rng_yards(coord)
  seq.int(rng[1], rng[2] - w, by = w)
}

grid_xy_yards <-
  crossing(
    x = seq_coord_yards('x'),
    y = seq_coord_yards('y')
  ) %>% 
  arrange(y, x) %>% 
  # mutate(idx = row_number(x + rng_x_yards[2] + y))
  mutate(idx = row_number())
grid_xy_yards

# Make sure this is in columnar order.
grid_xy_yards %>% pivot_wider(names_from = x, values_from = idx) %>% select(-y)

rescale_xy_cols_yards_to_m <- function(.data) {
  .rescale_xy_cols(
    .data,
    rng_x_from = rng_x_yards,
    rng_y_from = rng_y_yards,
    rng_x_to = rng_x_m, 
    rng_y_to = rng_y_m, 
    rgx_x = '^x$',
    rgx_y = '^y$',
    flip_x = FALSE, 
    flip_y = FALSE
  ) 
}

grid_xy_m <- grid_xy_yards %>% rescale_xy_cols_yards_to_m()
grid_xy_m

events_proc <-
  events %>% 
  # head(100) %>% 
  select(player_id = player.id, player_name = player.name, x = location.x, y = location.y) %>% 
  filter(!is.na(x))


grid_xy_yards_expand <-
  grid_xy_yards %>% 
  mutate(dummy = 1L) %>% 
  full_join(events_proc %>% distinct(player_id, player_name) %>% mutate(dummy = 1L)) %>% 
  select(-dummy)
grid_xy_yards_expand

# I have one extra player (compared to the python output)?
players <-
  events_proc %>% 
  # # Too slow!
  # fuzzyjoin::fuzzy_inner_join(
  #   grid_xy_yards %>% mutate(across(c(x, y), list(lead1 = ~.x + 4))),
  #   by = c('x' = 'x', 'y' = 'y', 'x' = 'x_lead1', 'y' = 'y_lead1'),
  #   match_fun = c(`>=`, `>=`, `<`, `<`)
  # ) %>% 
  mutate(across(c(x, y), bin)) %>%
  right_join(grid_xy_yards_expand) %>% 
  # Don't convert to meters yet! Compare to python dimensions and counts
  # rescale_xy_cols_yards_to_m() %>% 
  arrange(player_id, idx, x, y)
players

events_proc %>% mutate(idx = row_number()) %>% filter(player_id == .player_id_filt)

players_n <- players %>% count(player_id, idx)
players_n %>% arrange(desc(n))
players_long_py %>% arrange(desc(n))
players_n1 <- players_n %>% filter(player_id == .player_id_filt)
players_n1

players_long_py %>% distinct(id)
xy_n <- players %>% count(idx, x, y)
# xy_n
# grid_xy_yards %>% anti_join(xy_n) %>% count(x)
# players %>% count(x, y) %>% ggplot() + aes(x = x, y = y) + geom_point(aes(size = n))

players_decomp <-
  players_n %>% 
  widyr::widely_svd(
    item = idx,
    feature = player_id,
    value = n,
    nv = 30
  ) %>% 
  inner_join(grid_xy_m)
players_decomp
# players_decomp %>% skimr::skim()
players_decomp %>% filter(dimension == 1L) %>% arrange(desc(value)) %>% ggplot() + aes(x = idx, y = value) + geom_point()

# library(sklearn)


players_decomp
players_decomp %>% arrange(desc(x))
viz <-
  players_decomp %>% 
  filter(dimension <= 9) %>% 
  ggplot() +
  aes(x = x, y = y) +
  # theme_void() +
  .gg_pitch() +
  geom_raster(
    aes(fill = value),
    interpolate = TRUE,
    hjust = 1,
    vjust = 1,
    alpha = 0.5
  ) +
  scale_fill_viridis_c(direction = -1) +
  facet_wrap(~dimension)
viz

.pal2 <- c('home' = 'red', 'away' = 'blue')
.methods_valid <- c('fb', 'spearman', 'vor')
.pal_yardsethod <- c('fb' = 'magenta', 'spearman' = 'darkorange', 'vor' = 'limegreen')
.arw <- arrow(length = unit(3, 'pt'), type = 'closed')
# See https://stackoverflow.com/a/17313561/120898
.pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

.gg_constants <- function(..., tracking, events) {
  list(
    scale_color_yardsanual(values = .pal2),
    geom_segment(
      data = tracking %>% filter(!is.na(x)),
      size = 0.5,
      arrow = .arw,
      aes(x = x, y = y, xend = x + x_v, yend = y + y_v, color = side)
    ),
    ggrepel::geom_text_repel(
      data = tracking %>% filter(!is.na(x)),
      aes(x = x, y = y, color = side, label = player_id),
      force = 2,
      size = .pts(8)
    ),
    geom_point(
      data = tracking %>% filter(!is.na(x)),
      aes(x = x, y = y, color = side),
      size = 4
    ),
    geom_point(
      data = events,
      aes(x = start_x, y = start_y),
      size = 2,
      fill = 'black',
      color = 'black',
      shape = 21
    ),
    theme(
      plot.title = ggtext::element_yardsarkdown('Karla', face = 'bold', size = 18, color = 'gray20', hjust = 0.5),
      # plot.title.position = 'plot',
      plot.subtitle = ggtext::element_yardsarkdown('Karla', face = 'bold', size = 14, color = 'gray20', hjust = 0.5),
      # plot.subtitle = element_text(size = 16, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10),
      plot.caption = ggtext::element_yardsarkdown('Karla', size = 14, color = 'gray20', hjust = 0),
      plot.caption.position = 'plot'
    )
  )
}
