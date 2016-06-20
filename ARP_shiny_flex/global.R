#all_matrices <- readRDS(gzcon(url("https://virginia.box.com/shared/static/5g6l1l08umr7vhdy5vowgbm9xmrvmlo5.rds",open="rb")))

info_legis <- readRDS(gzcon(url("https://virginia.box.com/shared/static/zarwtxs191xt613eucuja5cuvgt4p1ub.rds")))
model_results <- list(readRDS(gzcon(url("https://virginia.box.com/shared/static/2ob87ujmmtig4oefcts8m3xr45m9ao4n.rds"))),
                      readRDS(gzcon(url("https://virginia.box.com/shared/static/0tcv5xadip74ru7w63hit7nlif160nks.rds"))))
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
