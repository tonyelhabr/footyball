
path_r_profile <- '~/.Rprofile'
if(file.exists(path_r_profile)) {
  source(path_r_profile)
}
rm('path_r_profile')

options(readr.num_columns = 0)
if (interactive()) {
  suppressMessages(require(devtools))
}
library(tidyverse)
# devtools::load_all()
invisible(R.utils::sourceDirectory(file.path('R'), recursive = FALSE))
fs::dir_create(.get_dir_data())
fs::dir_create(.get_dir_output())
fs::dir_create(.get_dir_plots())
