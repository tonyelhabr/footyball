
library(tidyverse)
.get_dims_sb <- function() {
  c(120, 80)
}

.get_dims_opta <- function() {
  c(100, 100)
}

.get_dims_actual <- function() {
  c(105, 68)
}

# TODO: Change `dims` to `rng`?
.get_valid_coords <- function() {
  c('x', 'y')
}

.validate_coord <- function(x = .get_valid_coords()) {
  match.arg(x)
}

#' @note StatsBomb
.get_rng_yards <- function(coord = .get_valid_coords()) {
  .validate_coord(coord)
  switch(coord, x = c(0, 120), y = c(0, 80))
}

#' @note Metrica
.get_rng_m <- function(coord = .get_valid_coords()) {
  .validate_coord(coord)
  switch(coord, x = c(0, 105), y = c(0, 68))
}

.rescale_vec <- function(x, rng1, rng2) {
  rng2[1] + ((x - rng1[1]) * (rng2[2] - rng2[1])) / (rng1[2] - rng1[1])
}

.get_valid_sides <- function() {
  c('away', 'home')
}

.validate_side <- function(x = .get_valid_sides()) {
  match.arg(x)
}

.get_default_pc_params_spearman <- function(time_to_control_veto = 3) {
  params <-
    list(
      max_player_accel = 7,
      max_player_speed = 5,
      reaction_time = 0.7,
      tti_sigma = 0.45,
      kappa_def = 1,
      lambda_att = 4.3,
      average_ball_speed = 15,
      int_dt = 0.04,
      max_int_time = 10,
      iter_min = 5,
      model_converge_tol = 0.01
    )
  params$lambda_def = 4.3 * params[['kappa_def']]
  params$lambda_gk = 3.0 * params[['lambda_def']]
  params$time_to_control_att = time_to_control_veto * log(10) * sqrt(3) * params[['tti_sigma']] * pi * (1 / params[['lambda_att']])
  params$time_to_control_def = time_to_control_veto * log(10) * sqrt(3) * params[['tti_sigma']] * pi * (1 / params[['lambda_def']])
  params
}

.get_dir_data <- function() {
  'data'
}

.get_dir_output <- function() {
  'output'
}

.get_dir_plots <- function() {
  'output/figs'
}

.to_coords <- function(data, dims) {
  res <-
    data %>%
    mutate(
      across(matches('x$'), ~{.x * dims[1]}),
      # across(matches('y$'), ~{.x * dims[2]})
      across(matches('y$'), ~{-1 * (.x * dims[2] - dims[2])})
    )
  res
}

# .to_coords_opta <- function(data, dims = .get_dims_opta()) {
#   .to_coords(data, dims)
# }
# 
# .to_coords_actual <- function(data, dims = .get_dims_actual()) {
#   .to_coords(data, dims)
# }


# TODO: Change usage of `.to_coords()` to this function.
# Split this function into a function that acts only on a single coord.
.rescale_xy_cols <-
  function(.data,
           rng_x_from = c(0, 1),
           rng_y_from = rng_x_from,
           rng_x_to = .get_rng_m('x'),
           rng_y_to = .get_rng_m('y'),
           rgx_x = '^x$',
           rgx_y = '^y$',
           scaler_x = 1,
           scaler_y = 1) {
    # multiplier_x <- ifelse(flip_x, -1, 1)
    # multiplier_y <- ifelse(flip_y, -1, 1)
    res <-
      .data %>%
      mutate(
        across(
        matches(rgx_x),
        ~ .rescale_vec(scaler_x * .x, rng_x_from, rng_x_to)
      ),
      across(
        matches(rgx_y),
        ~ .rescale_vec(scaler_y * .x, rng_y_from, rng_y_to)
      )
      )
    res
  }

# TODO: Use this function.
.rescale_coord_col <-
  function(.data,
           coord = .get_valid_coords(),
           col = coord,
           rng_from = NULL,
           rng_to = .get_rng_m(coord),
           scaler = 1) {
    col_sym <- col %>% sym()
    if(is.null(rng_from)) {
      rng_from <- range(.data[[col]])
    }
    res <-
      .data %>%
      mutate(
        across(
          !!col_sym,
          ~ .rescale_vec(scaler_x * .x, rng_from, rng_to)
        )
      )
    res
  }

save_plot <-
  function(viz,
           file = deparse(substitute(viz)),
           ext = 'png',
           # dir = here::here('output'),
           dir = .get_dir_plots(),
           path = fs::path(dir, sprintf('%s.%s', file, ext)),
           height = 8,
           scaler = 105 / 68,
           multiplier = 1,
           adder = -2,
           width = height * scaler * multiplier + adder,
           ...) {
    ggsave(plot = viz, filename = path, width = width, height = height, type = 'cairo', ...)
  }
