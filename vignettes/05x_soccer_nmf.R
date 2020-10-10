
library(tidyverse)
# install.packages('NMF')

path_shots <- here::here('data-raw', 'shots-player-understat.rds')

shots_exists <- path_shots %>% fs::file_exists()
# stats <- read_rds(path_stats)
shots <- read_rds(path_shots)
shots %>% 
  filter(player == 'Cristiano Ronaldo') %>% 
  relocate(h_team, a_team)

shots
shots_proc <-
  shots %>% 
  filter(situation != 'Penalty') %>% 
  mutate(
    is_home = if_else(h_a == 'h', TRUE, FALSE),
    team = if_else(is_home, h_team, a_team),
    opp = if_else(is_home, a_team, h_team)
  ) %>% 
  select(
    year,
    date,
    match_id,
    team,
    opp,
    situation,
    player_id = id,
    player,
    x = X,
    y = Y,
    xg = xG
  )
# shots_proc
# shots_proc %>% skimr::skim()
shots_n_by_year_team <- shots_proc %>% count(year, team, sort = TRUE)
shots_n_by_year_player <- shots_proc %>% count(year, team, player, sort = TRUE)
shots_n_by_year_player %>% skimr::skim()
shots_binned <-
  shots_proc %>% 
  # filter(year == 2019L) %>% 
  # filter(str_detect(player, 'Salah')) %>% 
  # semi_join(shots_n_by_year_team %>% head(9)) %>% 
  .rescale_xy_cols(flip_y = FALSE) %>% 
  mutate(
    across(
      c(x, y), ~3 * round(.x / 3, 0) # list(bin = ~ggplot2::cut_interval(.x, length = 1) %>% str_split(pattern = '\\,'))
    ) 
  ) %>% 
  count(year, team, player, x, y)
shots_binned

viz <-
  shots_binned %>% 
  semi_join(shots_n_by_year_team %>% head(9)) %>% 
  ggplot() +
  aes(x = x, y = y) +
  geom_tile(aes(fill = n)) +
  guides(fill = FALSE) +
  theme_void() +
  facet_wrap(~sprintf('%s, %s', team, year))
viz

shots_binned
sb <- 'c:/users/aelhabr/desktop/r/events_sb_free.rds' %>% read_rds()
arrow::write_parquet(sb, 'C:/users/aelhabr/desktop/r/events_sb_free.parquet')
feather::write_feather(sb, 'C:/users/aelhabr/desktop/r/events_sb_free.fst')
sb %>% 
  head(1000) %>% 
  as_tibble()
sb %>% filter(competition_id == 11)
sb_clean <- sb %>% StatsBombR::allclean()
sb_clean
# NOTE: Can join `shots` and `stats` with `player` and `player_name` and identify `player`'s team in `shots` with `team` in `stats`.

library(NMF)
n <- 20; counts <- c(5, 3, 2);
p <- sum(counts)
x <- syntheticNMF(n, counts)
dim(x)
widyr::widely_svd

library(torch)
x <- torch::torch_tensor(matrix(c(x = seq.int(1, 10), y = seq.int(1, 10)), ncol = 2))
x
arr <- array(c(x = seq.int(1, 10), y = seq.int(1, 10)), dim = c(10, 1))
arr
torch::torch_tensor(arr)
