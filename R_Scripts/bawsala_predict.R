# load fitted models, plot & analyze data

#require(idealstan)
library(idealstan, lib.loc = "C:/Program Files/R/R-3.5.1/library")
require(bayesplot)
require(dplyr)
require(tidyr)
require(ggplot2)
require(lubridate)
require(stringr)
require(forcats)
require(xtable)

arp_ar1 <- readRDS('data/estimate_all_ar1_vb.rds')
arp_rw <- readRDS('data/estimate_all_rw_vb.rds')
group2_ar1 <- readRDS('data/estimate_all_2groups_ar_vb.rds')
group2_rw <- readRDS('data/estimate_all_2groups_rw_vb.rds')

comb_data <- readRDS('data/combine_sessions.rds')

# see how long it takes to do predictions

all_pred <- id_post_pred(group2_rw)

saveRDS(all_pred,'data/all_pred.rds')
