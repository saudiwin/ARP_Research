model_data <- list(readRDS("data/oned_revised.rds"),readRDS("data/twod_revised.rds"))
make_round <- function(x) {
  save_names <- names(x)
  small_func <- function(y) {
    if(is.numeric(y)) 
      y <- round(y,digits=3)
    y
  }
  x <- as.data.frame(lapply(x,small_func))
  names(x) <- save_names
  x
}
model_data <- lapply(model_data,make_round)
party_types <- levels(model_data[[1]]$Party)
num_rows <- nrow(model_data[[1]])