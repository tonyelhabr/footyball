
library(tidyverse)
set.seed(42L)
cnn_prep <- read_rds('data/cnn_prep.rds') %>% sample_frac(0.1)
xp <- cnn_prep$xp
xd <- cnn_prep$xd
y <- cnn_prep$y

# learn ----
`%<-%` <- zeallot::`%<-%`
np <- reticulate::import('numpy')
sklearn <- reticulate::import('sklearn')
c(xp_trn, xp_tst, xd_trn, xd_tst, y_trn, y_tst) %<-% sklearn$model_selection$train_test_split(xp, xd, y, test_size = 0.1, random_state = 1L, shuffle = TRUE)
xd_tst %>% 
  tibble(value = .) %>% 
  mutate(idx = row_number())
# y_trn %>% tibble(y = .) %>% count(y)

reticulate::use_condaenv(condaenv = 'C:/Users/aelhabr/anaconda3/evns/footyball')
reticulate::conda_list(conda = 'C:/Users/aelhabr/anaconda3/envs/LaurieOnTracking/python.exe')
# install.packages('tensorflow')
# install.packages('keras')
# library(keras)
# keras <- reticulate::import('tensorflow')
reticulate::conda_python(envname = 'tf2r')
# library(tensorflow)
# tensorflow::use_condaenv('tf2r')
library(keras)
pass_input <- keras::layer_input(shape = c(52, 34, 3), name = 'pass_input')
dest_input <- keras::layer_input(shape = c(52, 34, 1), name = 'pass_input')
x <- keras::layer_conv_2d(filters = 16, kernel_size = c(3, 3), activation = 'relu', padding = 'valid')