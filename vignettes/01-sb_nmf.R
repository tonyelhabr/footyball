
# functions ----
rng_x_yards <- .get_rng_yards('x')
rng_y_yards <- .get_rng_yards('y')
rng_x_m <- .get_rng_m('x')
rng_y_m <- .get_rng_m('y')
# width_yards <- 4
# n_bin <- 30

# seq_coord_yards <- function(coord = .get_valid_coords(), w = width_yards, truncate = TRUE) {
#   .validate_coord(coord)
#   rng <- .get_rng_yards(coord)
#   rng2 <- ifelse(truncate, rng[2] - w, rng[2])
#   seq.int(rng[1], rng2, by = w)
# }

seq_coord_yards <- function(coord = .get_valid_coords(), n = 10, truncate = FALSE) {
  .validate_coord(coord)
  rng <- .get_rng_yards(coord)
  res <- seq(rng[1], rng[2], length.out = n)
  if(!truncate) {
    return(res)
  }
  res <- res[1:length(res)-1]
  res
}

# seq_x_yards <- seq_coord_yards('x', n = 30)
# seq_y_yards <- seq_coord_yards('y', n = 30)
.bin <- function(x, breaks, side = c('left', 'right')) {
  side <- match.arg(side)
  rgx <- switch(side, left = '^[\\(|\\[]|,.*$', right = '(^.*\\,)|[\\)\\]]$')
  x %>% 
    # ggplot2::cut_interval(length = width) %>% 
    # Note that `dig.lab` is defined specifically in this manner so that number above 100 have 1 decimal place and those below have 2.
    # This is **very** hacky. In conjunction with the `_dummy` and `_impute` columns added later, this is done to avoid joining on floats, which was found to be inconsistent. Alternatively, we could do a non-equi join, but that was found to be inconsistent. Or, as a last ditch effort, an actual binary tree function (which is essentially what numpy's `digitize()` is) could be written.
    cut(breaks = breaks, dig.lab = 4) %>% 
    str_remove_all(rgx) %>%
    as.numeric()
}

bin_x_yards <- partial(.bin, breaks = seq_coord_yards('x', n = 30, truncate = FALSE), ... = )
bin_y_yards <- partial(.bin, breaks = seq_coord_yards('y', n = 20, truncate = FALSE), ... = )

grid_xy_yards <-
  crossing(
    x = seq_coord_yards('x', n = 30), # , truncate = TRUE),
    y = seq_coord_yards('y', n = 20) # , truncate = TRUE)
  ) %>% 
  # arrange(y, x) %>% 
  arrange(x, y) %>% 
  # mutate(idx = row_number(x + rng_x_yards[2] + y))
  mutate(idx = row_number())
grid_xy_yards

# # Make sure this is in columnar order.
# grid_xy_yards %>% 
#   mutate(across(c(x, y), round)) %>% 
#   pivot_wider(names_from = x, values_from = idx)

# retrieve data ----
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
# Just a check to see that coordinates match.
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

# r ----
events_proc <-
  events %>% 
  # head(100) %>% 
  select(player_id = player.id, player_name = player.name, x = location.x, y = location.y) %>% 
  filter(!is.na(player_id)) %>% 
  filter(!is.na(x))

add_xy_dummy_cols <- function(.data) {
  .data %>% 
    mutate(across(c(x, y), list(dummy = ~case_when(.x < 100 ~ round(.x, 2), TRUE ~ round(.x, 1)))))
}

grid_xy_yards_dummy <-
  grid_xy_yards %>% 
  add_xy_dummy_cols() %>% 
  select(idx, x_impute = x, y_impute = y, x_dummy, y_dummy)
grid_xy_yards_dummy

grid_xy_yards_expand <-
  grid_xy_yards %>% 
  add_xy_dummy_cols() %>% 
  select(idx, x_impute = x, y_impute = y, x_dummy, y_dummy) %>% 
  mutate(dummy = 0) %>% 
  full_join(
    events_proc %>% 
      distinct(player_id, player_name) %>% 
      mutate(dummy = 0)
  ) %>% 
  select(-dummy)
# grid_xy_yards_expand %>% filter(y_dummy == 29.5)
# grid_xy_yards_expand %>% filter(is.na(player_id))
# grid_xy_yards_expand %>% drop_na()
# events_proc %>% filter(x >= 120)
# events_proc %>% filter(y >= 80)

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
  # filter(n > 0L) %>% 
  add_xy_dummy_cols() %>% 
  full_join(grid_xy_yards_expand) %>% 
  mutate(
    across(x, ~coalesce(.x, x_impute)),
    across(y, ~coalesce(.x, y_impute)),
    across(n, ~coalesce(.x, 0L))
  ) %>% 
  select(-matches('(dummy|impute)$')) %>% 
  # Don't convert to meters yet! Compare to python dimensions and counts
  # rescale_xy_cols_yards_to_m() %>% 
  arrange(player_id, idx, x, y)
players
players %>% filter(idx %>% is.na()) # Should have no rows.

players1 <- players %>% filter(player_id == .player_id_filt)
players1
# players1 %>% filter(n > 1L) %>% summarize(across(n, sum)) # checks out
# players1_py %>% inner_join(grid_xy_yards) %>% arrange(desc(n)) %>% ggplot() + aes(x, y) + geom_point(aes(size = n))
# players1 %>% arrange(desc(n)) %>% ggplot() + aes(x, y) + geom_point(aes(size = n))
# decomp <-
#   players %>% 
#   widyr::widely_svd(
#     item = idx,
#     feature = player_id,
#     value = n,
#     nv = 9
#   ) %>% 
#   inner_join(grid_xy_m)
# decomp

sklearn::import_sklearn()
sklearn <- reticulate::import('sklearn')
model <- sklearn$decomposition$NMF(n_components = 30L, init = 'random', random_state = 0L)

players_mat <-
  players %>% 
  select(player_id, idx, n) %>% 
  pivot_wider(names_from = idx, values_from = n) %>% 
  select(-player_id) %>% 
  as.matrix()

model$fit_transform(players_mat)
w <- NMF::nmf(NMF::rmatrix(players_mat), rank = 30, seed = 0, method = 'Frobenius', .options='v3')

grid_xy_m <- 
  grid_xy_yards %>%
  .rescale_xy_cols(
    rng_x_from = rng_x_m,
    rng_y_from = rng_y_m,
    rng_x_to = rng_x_m, 
    rng_y_to = rng_y_m
  )
grid_xy_m

grid_xy_rev_m <- 
  grid_xy_yards %>%
  .rescale_xy_cols(
    rng_x_from = rng_x_yards,
    rng_y_from = rng_y_yards,
    rng_x_to = rng_x_m, 
    rng_y_to = rev(rng_y_m)
  )
grid_xy_rev_m

.tidy_comp_mat <- function(.data, smooth = FALSE, ...) {
  if(smooth) {
    .data <-
      .data %>% 
      spatstat::as.im() %>% 
      spatstat::blur(...) %>% 
      # NOTE: Could use `spatstat::as.data.frame.im()`, but it converts directly to x,y,value.
      pluck('v')
  }
  res <-
    .data %>% 
    as_tibble() %>% 
    mutate(dimension = row_number()) %>% 
    pivot_longer(-dimension, names_to = 'idx', values_to = 'value') %>% 
    mutate(across(idx, ~str_remove(.x, '^V') %>% as.integer())) %>% 
    left_join(grid_xy_rev_m) %>% 
    group_by(dimension) %>% 
    mutate(frac = (value - min(value)) / (max(value) - min(value))) %>% 
    ungroup()
  res
}

decomp <- w@fit@H %>% .tidy_comp_mat()
decomp_py <- model$components_ %>% .tidy_comp_mat()
decomp_smooth_py <- model$components_ %>% .tidy_comp_mat(smooth = TRUE, sigma = 1.5)

smoothen_dimension <- function(.data, ...) {
  mat <-
    .data %>% 
    select(x, y, value) %>% 
    pivot_wider(names_from = x, values_from = value) %>% 
    select(-y) %>% 
    as.matrix()
  mat_smoothed <-
    mat %>% 
    spatstat::as.im() %>% 
    spatstat::blur(...) %>% 
    # NOTE: Could use `spatstat::as.data.frame.im()`, but it converts directly to x,y,value.
    pluck('v')
  
  res <-
    mat_smoothed %>% 
    as_tibble() %>% 
    mutate(y = row_number()) %>% 
    pivot_longer(-y, names_to = 'x', values_to = 'value') %>% 
    mutate(across(x, ~str_remove(.x, '^V') %>% as.integer())) %>% 
    arrange(x, y) %>% 
    mutate(idx = row_number()) %>% 
    select(-x, -y) %>% 
    inner_join(grid_xy_rev_m) %>% 
    mutate(frac = (value - min(value)) / (max(value) - min(value))) %>% 
    ungroup()
  res
}

decomp_smooth_py <-
  model$components_ %>% 
  as_tibble() %>% 
  mutate(dimension = row_number()) %>% 
  pivot_longer(-dimension, names_to = 'idx', values_to = 'value') %>% 
  mutate(across(idx, ~str_remove(.x, '^V') %>% as.integer())) %>% 
  inner_join(
    grid_xy_yards %>% 
      mutate(across(c(x, y), dense_rank))
  ) %>% 
  nest(data = -c(dimension)) %>% 
  # `sigma` passed into `...` of `smoothen_dimension()`. (`data` passed as first argument.)
  mutate(data = map(data, smoothen_dimension, sigma = 1.5)) %>% 
  unnest(data)
decomp_smooth_py


viz <-
  decomp_smooth_py %>% 
  # decomp_py %>% 
  # res_init %>% 
  filter(dimension <= 9) %>% 
  ggplot() +
  aes(x = x, y = y) +
  theme_void() +
  .gg_pitch() +
  # geom_tile(aes(fill = value))
  facet_wrap(~dimension, ncol = 3) +
  geom_contour_filled(
    # aes(fill = frac),
    aes(z = frac),
    # contour_var = 'ndensity',
    # breaks = seq(0, 1.0, length.out = 11)
    # binwidth = 0.1,
    alpha = 0.7
  ) +
  # geom_tile(aes(fill = frac), alpha = 0.5) +
  scale_fill_viridis_d(direction = 1)
viz
