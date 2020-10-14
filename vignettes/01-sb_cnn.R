
# retrieve data ----
events <- 
  retrieve_sb_events_timed(competition_id = 43, overwrite = FALSE) %>% 
  filter(type.id == 30) %>% 
  mutate(idx = row_number()) %>% 
  select(
    idx,
    player_id = player.id,
    player_name = player.name,
    outcome_id = pass.outcome.id,
    outcome_name = pass.outcome.name,
    x_start = location.x,
    y_start = location.y,
    x_end = pass.end_location.x,
    y_end = pass.end_location.y
  )
events

`%<-%` <- zeallot::`%<-%`
np <- reticulate::import('numpy')
# grid_xy_wide <-
#   grid_xy %>% 
#   mutate(
#     dummy = 0L,
#     across(c(x, y), dense_rank)
#   ) %>%  
#   pivot_wider(names_from = x, values_from = dummy) %>% 
#   select(-y)
# grid_xy_wide
# grid_xy_wide_tensor <- grid_xy_wide %>% as.matrix() %>% torch::torch_tensor()
# grid_xy_wide_tensor

# point <- c(61, 41)
# point <- c(37, 37)

x_start <- 61
y_start <- 41
x_end <- 37
y_end <- 37
.n_x <- 52L
.n_y <- 34L
.seq_x <- seq(0, 120, length.out = .n_x)
.seq_y <- seq(0, 80, length.out = .n_y)

# c(xv, yv) %<-% np$meshgrid(seq_x, seq_y, sparse = FALSE, indexing = 'ij') 

transform <- function(x, y, seq_x = .seq_x, seq_y = .seq_y) {
  n_x <- length(seq_x)
  n_y <- length(seq_y)
  mat <- np$zeros(c(n_x, n_y))
  x_bin <- np$digitize(point[1], seq_x)
  y_bin <- np$digitize(point[2], seq_y)
  mat[x_bin + 1, y_bin + 1] <- 1
  mat
}
build_tensor <- function(x, y, seq_x = .seq_x, seq_y = .seq_y, x_goal = 120L, y_goal = 40L) {
  c(xv, yv) %<-% np$meshgrid(seq_x, seq_y, sparse = FALSE, indexing = 'ij')
  xv %>% dim()
  yv %>% dim()
  coords_mat <- np$dstack(list(xv, yv))
  coords_mat %>% dim()
  mat <- transform(x, y)
  mat %>% dim()
  coords_mat %>% dim()
  r_origin <- np$linalg$norm(np$dstack(list(x - coords_mat, y - coords_mat)), axis = 2L)
  r_origin %>% dim()
  r_goal <- np$linalg$norm(np$dstack(list(x_goal - coords_mat, y_goal - coords_mat)), axis = 2L)
  r_goal %>% dim()
  tensor <- np$dstack(list(mat, r_origin, r_goal))
  tensor %>% dim()
  tensor
}

xp <- build_tensor(x_start, y_start)
xd <- transform(x_end, y_end)
xp %>% dim()
xd %>% dim()

events_filt <- events # %>% head(10000)

f <- function() {
  res <-
    events_filt %>% 
    # split(.$idx) %>% 
    # pmap(
    #   list(.$x_start, .$x_end, .$y_start, .$y_end), ~print(..1, ..2)
    # )
    # nest(data = -c(idx)) %>% 
    mutate(
      res = 
        pmap(
          list(x_start, y_start, x_end, y_end, outcome_id), 
          ~{
            xp <- build_tensor(..1, ..2)
            xd <- transform(..3, ..4)
            y <- ifelse(is.na(..5), 1L, 0L)
            res <- list(xp = xp, xd = xd, y = y)
          }
        )
    ) %>% 
    select(idx, res) %>% 
    unnest_wider(res)
}
f_timed <- .time_it(f)
# Took about 36 minutes!
cnn_prep <- f_timed()
cnn_prep %>% write_rds('data/cnn_prep.rds')
