if (!require(reticulate)) {
  install.packages("reticulate")
  library(reticulate)
}
install.packages("remotes")
remotes::install_github("rstudio/tensorflow")

reticulate::install_python()

library(tensorflow)
install_tensorflow(envname = "r-tensorflow")

install.packages("keras")
library(keras)

dataset_imdb(
  
)
