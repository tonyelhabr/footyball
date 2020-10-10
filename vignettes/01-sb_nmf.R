
# functions ----
rng_x_yards <- .get_rng_yards('x')
rng_y_yards <- .get_rng_yards('y')
rng_x_m <- .get_rng_m('x')
rng_y_m <- .get_rng_m('y')
# width_yards <- 4
n_bin <- 30

# seq_coord_yards <- function(coord = .get_valid_coords(), w = width_yards, truncate = TRUE) {
#   .validate_coord(coord)
#   rng <- .get_rng_yards(coord)
#   rng2 <- ifelse(truncate, rng[2] - w, rng[2])
#   seq.int(rng[1], rng2, by = w)
# }

seq_coord_yards <- function(coord = .get_valid_coords(), n = n_bin) {
  .validate_coord(coord)
  rng <- .get_rng_yards(coord)
  res <- seq(rng[1], rng[2], length.out = n)
  res
}

seq_x_yards <- seq_coord_yards('x', n = 30)
seq_y_yards <- seq_coord_yards('y', n = 30)
.bin <- function(x, breaks, side = c('left', 'right')) {
  side <- match.arg(side)
  rgx <- swtich(side, left = '^[\\(|\\[]|,.*$', right = '(^.*\\,)|[\\)\\]]$')
  x %>% 
    # ggplot2::cut_interval(length = width) %>% 
    cut(breaks = breaks) %>% 
    str_remove_all(rgx) %>%
    as.numeric()
}
bin_x_yards <- partial(.bin, breaks = seq_coord_yards('x', truncate = FALSE), ... = )
bin_y_yards <- partial(.bin, breaks = seq_coord_yards('y', truncate = FALSE), ... = )

grid_xy_yards <-
  crossing(
    x = seq_coord_yards('x', truncate = TRUE),
    y = seq_coord_yards('y', truncate = TRUE)
  ) %>% 
  # arrange(y, x) %>% 
  arrange(x, y) %>% 
  # mutate(idx = row_number(x + rng_x_yards[2] + y))
  mutate(idx = row_number())
grid_xy_yards

# Make sure this is in columnar order.
grid_xy_yards %>% 
  pivot_wider(names_from = x, values_from = idx) %>% 
  select(-y)

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

events <- retrieve_sb_events_timed(competition_id = 43, overwrite = FALSE)
events

# for debugging
.player_id_filt <- 3509L # key at index 328 in python
.player_id_filt_py <- 328L

# py ----
players_wide_py <- 'data/unraveled_py.csv' %>% read_csv()
players_long_py <-
  players_wide_py %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(-id, names_to = 'idx', values_to = 'n') %>% 
  mutate(across(idx, ~as.integer(.x) + 1L))
players_long_py
# players_long_py %>% arrange(desc(n))
players1_py <- players_long_py %>% filter(id == .player_id_filt_py)
players1_py

events_py <- 'data/events_py.csv' %>% read_csv()
events_filt <- events %>% filter(player.id == .player_id_filt)
events_py_filt <- 
  events_py %>% 
  select(id, player_py = player, location_py = location) %>%
  inner_join(
    events_filt %>% 
      select(
        id,
        player_id = player.id,
        x = location.x,
        y = location.y
      )
  )
events_py_filt

# cut(x = 2, breaks = c(0, 4, 8))
# cut(2, seq_x_yards)
# ggplot2::cut_width(2, n = 20, width = 4)
# ggplot2::cut_interval(2, length = 4, n = 3, breaks = c(0, 4, 8))
# ggplot2::cut_interval(4, length = 4, breaks = c(0, 4, 8))

# r ----
events_proc <-
  events %>% 
  # head(100) %>% 
  select(player_id = player.id, player_name = player.name, x = location.x, y = location.y) %>% 
  filter(!is.na(x))

grid_xy_yards_expand <-
  grid_xy_yards %>% 
  mutate(dummy = 0) %>% 
  full_join(
    events_proc %>% 
      distinct(player_id, player_name) %>% 
      mutate(dummy = 0)
  ) %>% 
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
  mutate(across(x, bin_x_yards), across(y, bin_y_yards)) %>%
  group_by(player_id, player_name, x, y) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  right_join(grid_xy_yards_expand) %>% 
  replace_na(list(n = 0L)) %>% 
  # Don't convert to meters yet! Compare to python dimensions and counts
  # rescale_xy_cols_yards_to_m() %>% 
  arrange(player_id, idx, x, y)
players

players1 <- players %>% filter(player_id == .player_id_filt)
players1
# players1 %>% filter(n > 1L) %>% summarize(across(n, sum)) # checks out
players1_py %>% inner_join(grid_xy_yards) %>% arrange(desc(n)) %>% ggplot() + aes(x, y) + geom_point(aes(size = n))
players1 %>% arrange(desc(n)) %>% ggplot() + aes(x, y) + geom_point(aes(size = n))

players %>% filter(player_id == .player_id_filt) %>% arrange(x + y)

players_long_py %>% distinct(id)
xy_n <- players %>% count(idx, x, y)
# xy_n
# grid_xy_yards %>% anti_join(xy_n) %>% count(x)
# players %>% count(x, y) %>% ggplot() + aes(x = x, y = y) + geom_point(aes(size = n))

players_decomp <-
  players %>% 
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
