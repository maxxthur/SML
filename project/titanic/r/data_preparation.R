# read csv files with training and test data
raw_data <- read.csv("project/titanic/data/train.csv")

# bit of feature engineering

library(keras)
load_data("IMDb", package = "keras")
dataset_imdb( 
  path = "imdb.npz", 
  num_words = NULL, 
  skip_top = 0L, 
  maxlen = NULL, 
  seed = 113L, 
  start_char = 1L, 
  oov_char = 2L, 
  index_from = 3L 
) 
