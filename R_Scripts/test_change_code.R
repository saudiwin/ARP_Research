l_open  <- adjust_con(2,num_legis,as.integer(constrain_pos),as.integer(constrain_neg),rep(1:(num_legis-nrow(true_constrain_num)),2),as.data.frame(constrain_num),as.data.frame(true_constrain_num))
col1 <- 1:nrow(l_open)
col2 <- 1:nrow(l_open)
col1 <- col1[-l_open[1:2,1]]
col2 <- col2[-l_open[1:2,2]]
l_open <- matrix(data=c(c(l_open[1:2,1],col1),c(l_open[1:2,2],col2)),nrow=nrow(l_open),ncol=ncol(l_open))
col_names1  <- row.names(sv.yay)[l_open[,1]]
col_names2  <- row.names(sv.yay)[l_open[,2]]
l_open2 <- data.frame(col_names1,col_names2)
D <- 2
N <- 102
pi <- vector(length=N)
for(n in 1:N) {
  d_hold <- character(length = D)
  for(i in 1:D) {
    d[i] <- as.character(l_open2[legislator_points[n,i],i])
    print(paste0("Dat Point ",n," and Legislator Point ",legislator_points[n,i]," selects L_adj point ",d[i],
          " in dimension ",i))
  }
  
  #pi[n] <- sum(dnorm(c(0,0),d,1,log=TRUE))
}
