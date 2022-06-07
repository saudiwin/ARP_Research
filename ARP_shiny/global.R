all_matrices <- readRDS("all_matrices.rds")

info_legis <- readRDS("info_legis.rds")
model_results <- list(readRDS("bin_yaynay.rds"),readRDS("bin_abstain.rds"))
model1_data <- data.frame(MP=info_legis$legis.names,Party=info_legis$Party,Highest_Ideal_Point=model_results[[1]]$means$x[,1] + 1.96*model_results[[1]]$bse$x,Average_Ideal_Point=model_results[[1]]$means$x[,1],
                          Lowest_Ideal_Point=model_results[[1]]$means$x[,1] - 1.96*model_results[[1]]$bse$x)
model2_data <- data.frame(MP=info_legis$legis.names,Highest_Ideal_Point=model_results[[2]]$means$x[,1] + 1.96*model_results[[2]]$bse$x,Party=info_legis$Party,Average_Ideal_Point=model_results[[2]]$means$x[,1],
                          Lowest_Ideal_Point=model_results[[2]]$means$x[,1] - 1.96*model_results[[2]]$bse$x)
model_data <- list(model1_data,model2_data)

make_round <- function(x) {
  save_names <- colnames(x)
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
party_types <- levels(info_legis$Party)
legis_names <- info_legis$legis.names
num_rows <- nrow(model_data[[1]])
