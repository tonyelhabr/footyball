
extrafont::loadfonts(device = 'win', quiet = TRUE)
# functions ----
rng_x_yards <- .get_rng_yards('x')
rng_y_yards <- .get_rng_yards('y')
rng_x_m <- .get_rng_m('x')
rng_y_m <- .get_rng_m('y')

seq_coord_yards <- function(coord = .get_valid_coords(), n) {
  .validate_coord(coord)
  rng <- .get_rng_yards(coord)
  seq(rng[1], rng[2], length.out = n)
}

seq_x_yards <- seq_coord_yards('x', n = 30)
seq_y_yards <- seq_coord_yards('y', n = 20)

grid_xy_yards <-
  crossing(
    x = seq_x_yards,
    y = seq_y_yards
  ) %>% 
  arrange(x, y) %>% 
  mutate(idx = row_number()) %>% 
  select(idx, x, y) %>% 
  group_by(x) %>% 
  mutate(next_y = dplyr::lead(y) %>% coalesce(y + (y - dplyr::lag(y)))) %>% 
  ungroup() %>% 
  group_by(y) %>% 
  mutate(next_x = dplyr::lead(x) %>% coalesce(x + (x - dplyr::lag(x)))) %>% 
  ungroup()
grid_xy_yards

# retrieve data ----
events <- 
  retrieve_sb_events_timed(competition_id = 43, overwrite = FALSE) %>% 
  select(player_id = player.id, x = location.x, y = location.y)
events %>% mutate(across(player_id, factor)) %>% skimr::skim()

# for debugging
# .player_id_filt <- 3509L # key at index 328 in python
# .player_id_filt_py <- 328L

# py ----
# players_wide_py <- 'data/unraveled_py.csv' %>% read_csv()
# players_long_py <-
#   players_wide_py %>% 
#   mutate(id = row_number()) %>% 
#   pivot_longer(-id, names_to = 'idx', values_to = 'n') %>% 
#   mutate(across(idx, ~as.integer(.x) + 1L))
# players_long_py
# # players_long_py %>% arrange(desc(n))
# players1_py <- players_long_py %>% filter(id == .player_id_filt_py)
# players1_py

# r ----
viz_grid <-
  grid_xy_yards %>% 
  ggplot() +
  # .gg_pitch(pitch = ..get_pitch(dimension = ggsoccer::pitch_statsbomb)) +
  geom_rect(aes(xmin = x, ymin = y, xmax = next_x, ymax = next_y), fill = NA, color = 'black') +
  geom_text(
    aes(x = (x + next_x) / 2, y = (y + next_y) / 2, label = idx), size = 3
  ) +
  coord_fixed(ratio = 2 / 3) +
  theme_void()
viz_grid

ggsave(
  plot = viz_grid, 
  filename = fs::path(.get_dir_plots(), 'viz_grid_nnmf.png'), 
  width = 8, 
  height = 8 * 2 / 3
)
# library(data.table)
events_dt <- events %>% drop_na() %>% data.table::as.data.table()
grid_xy_yards_dt <- grid_xy_yards %>% data.table::as.data.table()
events_binned <-
  events_dt[grid_xy_yards_dt, on=.(x > x, x <= next_x, y >= y, y < next_y)] %>% 
  as_tibble() %>% 
  select(player_id, idx, x, y) %>% 
  drop_na()
events_binned

grid_players <-
  grid_xy_yards %>% 
  mutate(dummy = 0L) %>% 
  # Cartesian join of all possible cells in the grid and all players in `events`.
  full_join(
    events %>% 
      drop_na() %>% 
      distinct(player_id) %>% 
      mutate(dummy = 0L),
    by = 'dummy'
  )

players <- 
  events_binned %>% 
  group_by(player_id, x, y, idx) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  full_join(grid_players, by = c('player_id', 'x', 'y', 'idx')) %>% 
  select(-dummy, -next_x, -next_y) %>% 
  replace_na(list(n = 0L)) %>% 
  arrange(player_id, x, y)
players

.player_id_messi <- 5503L
# .player_id_ronaldo <- 5207L
# .player_id_modric <- 5463L
# .player_id_mbappe <- 3009L
viz_players1 <-
  ggplot() +
  .gg_pitch(pitch = ..get_pitch(dimension = ggsoccer::pitch_statsbomb)) +
  # geom_point(
  #   data =
  #     players %>% 
  #     filter(player_id == .player_id_filt),
  #   aes(x = x, y = y, size = n)
  # ) +
  coord_fixed(ratio = 2 / 3, clip = 'off') +
  theme_void() +
  geom_raster(
    data =
      players %>% 
      filter(player_id == .player_id_messi) %>% 
      filter(x != max(x) & y != max(y)) %>% 
      .rescale_xy_cols(
        rng_x_from = rng_x_yards,
        rng_y_from = rng_y_yards,
        rng_x_to = rng_x_yards, 
        # Need to flip y in order to put the origin on the bottom-left instead of the top-left.
        rng_y_to = c(rev(seq_y_yards)[1] - seq_y_yards[2], -seq_y_yards[2])
      ),
    aes(x = x, y = y, fill = n), alpha = 0.5, hjust = 1, vjust = 1
  ) +
  scale_fill_distiller(palette = 'Reds', direction = 1) +
  theme(
    legend.position = 'none',
    plot.title = element_text('Arial', face = 'bold', size = 18, color = 'black', hjust = 0.5),
    plot.subtitle = element_text('Arial', size = 14, color = 'black', hjust = 0.5),
    # plot.title.position = 'plot',
    plot.margin = margin(10, 10, 10, 10)
  ) +
  geom_segment(
    data = tibble(y = -4, x_start = 60 - 20, x_end = 60 + 20),
    aes(x = x_start, y = y, xend = x_end, yend = y),
    size = 1,
    arrow = arrow(length = unit(5, 'pt'), type = 'closed')
  ) +
  geom_text(
    data = tibble(y = -8, x = 60, lab = 'Direction of attack'),
    aes(x = x, y = y, label = lab),
    size = 4,
    # fontface = 'bold',
    family = 'Arial'
  ) +
  labs(title = 'Lionel Messi', subtitle = '2018 World Cup Heat Map')
viz_players1

ggsave(
  plot = viz_players1, 
  filename = fs::path(.get_dir_plots(), 'viz_43_messi_binned.png'), 
  width = 8, 
  height = 8 * 2 / 3 + 1
)

sklearn <- reticulate::import('sklearn')
comp <- 30L
model <- sklearn$decomposition$NMF(n_components = comp, init = 'random', random_state = 0L)

players_mat <-
  players %>% 
  select(player_id, idx, n) %>% 
  pivot_wider(names_from = idx, values_from = n) %>% 
  select(-player_id) %>% 
  as.matrix()
players_mat

W <- model$fit_transform(players_mat)
# W <- NMF::nmf(NMF::rmatrix(players_mat), rank = 30, seed = 0, method = 'Frobenius', .options='v3')
# W@fit@H

grid_xy_rev_m <- 
  grid_xy_yards %>%
  .rescale_xy_cols(
    rng_x_from = rng_x_yards,
    rng_y_from = rng_y_yards,
    rng_x_to = rng_x_m, 
    # Need to flip y in order to put the origin on the bottom-left instead of the top-left.
    rng_y_to = rev(rng_y_m)
  )
grid_xy_rev_m

decomp_unsmooth <-
  model$components_ %>%
  as_tibble() %>%
  mutate(dimension = row_number()) %>%
  pivot_longer(-dimension, names_to = 'idx', values_to = 'value') %>%
  mutate(across(idx, ~str_remove(.x, '^V') %>% as.integer())) %>% 
  left_join(grid_xy_rev_m) %>%
  group_by(dimension) %>%
  mutate(frac = (value - min(value)) / (max(value) - min(value))) %>%
  ungroup()


decomp_tidy <-
  model$components_ %>% 
  as_tibble() %>% 
  # "Un-tidy" tibble with 30 rows (one for each dimension) and 600 columns (one for every `idx`, of which there are 30 x 20 = 600)
  mutate(dimension = row_number()) %>% 
  # Convert to a tidy tibble with dimensions * x * y rows (30 * 30 * 20 = 1800)
  pivot_longer(-dimension, names_to = 'idx', values_to = 'value') %>% 
  # The columns from the matrix are named `V1`, `V2`, ... `V600` by default, so convert them to an integer that can be joined on.
  mutate(across(idx, ~str_remove(.x, '^V') %>% as.integer()))

decomp <-
  decomp_tidy %>% 
  # Join on our grid of x-y pairs.
  inner_join(
    # Using `dense_rank` because we need indexes here (i.e.. 1, 2, ..., 30 instead of 0, 4.1, 8.2, ..., 120 for `x`).
    grid_xy_yards %>% 
      select(idx, x, y) %>% 
      mutate(across(c(x, y), dense_rank))
  )

smoothen_component <- function(.data, ...) {
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
decomp_smooth <-
  decomp %>% 
  # Prep for applying smoothing to each dimension individually.
  nest(data = -c(dimension)) %>% 
  # `sigma` passed into `...` of `smoothen_component()`. (`data` passed as first argument.)
  mutate(data = map(data, smoothen_component, sigma = 1.5)) %>% 
  unnest(data)
decomp_smooth

plot_dimensions <-
  function(.data,
           ...,
           dir = .get_dir_plots(),
           suffix = c('smooth', 'unsmooth'),
           prefix = 'viz_nnmf_dimensions_1to9_r',
           path = fs::path(dir, sprintf('%s_%s.png', prefix, suffix))) {
    suffix <- match.arg(suffix)
    viz <-
      .data %>%
      filter(dimension <= 9L) %>%
      mutate(lab_facet = sprintf('Component %d', dimension)) %>%
      ggplot() +
      aes(x = x, y = y) +
      theme_void() +
      .gg_pitch() +
      facet_wrap( ~ lab_facet, ncol = 3) +
      geom_contour_filled(aes(z = frac), alpha = 0.7) +
      theme(strip.text = element_text('Arial', size = 8, hjust = 0.05, vjust = 0.01)) +
      # labs(title = sprintf(
      #   'First 9 Components, %s',
      #   ifelse(suffix == 'smooth', 'Smoothed', 'Not Smoothed')
      # )) +
      scale_fill_viridis_d(direction = 1)
    ggsave(
      plot = viz,
      filename = path,
      width = 10,
      height = 10 * 2 / 3
    )
    viz
  }

viz_smooth <- decomp_smooth %>% plot_dimensions(suffix = 'smooth')
viz_smooth

viz_unsmooth <- decomp_unsmooth %>% plot_dimensions(suffix = 'unsmooth')
viz_unsmooth

# header ----
pts <- function(x) {
  as.numeric(grid::convertUnit(grid::unit(x, 'pt'), 'mm'))
}

.generate_and_export_header <- function() {
  viz <- 
    tibble(
      x = c(-2, -1.1, 0, 1.1, 2),
      lab = c('', 'python', '', 'R', '')
    ) %>% 
    ggplot() +
    aes(x = x, y = 0) +
    geom_text(aes(label = lab), size = pts(18), fontface = 'bold', hjust = 0.5) +
    theme_void()
  ggsave(plot = viz, filename = fs::path(.get_dir_plots(), 'header.png'), height = 0.5, width = 16, type = 'cairo')
  viz
}

.import_png <- function(lang = c('python', 'r'), dir = .get_dir_plots()) {
  lang <- match.arg(lang)
  suffix <- ifelse(lang == 'python', 'py', 'r_smooth')
  path <- fs::path(dir, sprintf('viz_nnmf_dimensions_1to9_%s.png', suffix))
  magick::image_read(path)
}

.import_png_header <- function(dir = .get_dir_plots()) {
  path <- fs::path(dir, sprintf('header.png'))
  magick::image_read(path)
}

.png_scale <- function(img, dpi = 96, width = 8, height = 10, geometry = sprintf('%dx%d', width * dpi, height * dpi)) {
  magick::image_scale(img, geometry = geometry)
}

.generate_and_export_header()
viz_header <- .import_png_header()
viz_py <- .import_png(lang = 'python')
viz_r <- .import_png(lang = 'r')
res <- magick::image_append(c(.png_scale(viz_py), .png_scale(viz_r)))
# res <- magick::image_append(c(.png_scale(viz_header, height = 0.5, width = 16), res), stack = TRUE)
img_info <- magick::image_info(res)
w <- img_info$width
res <- magick::image_append(c(.png_scale(viz_header, geometry = sprintf('%dx%d', w, w)), res), stack = TRUE)
path <- fs::path(.get_dir_plots(), 'viz_nnmf_dimensions_1to9_combined.png')
magick::image_write(res, path = path)
res

