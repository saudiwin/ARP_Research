# make affiliation matrix

votes_nozero  <- is.na(votes_matrix)
votes_matrix[votes_nozero] <- 0
votes_sparse <- Matrix(votes_matrix)
votes_affil <- tcrossprod(votes_sparse)
votes_normal_affil <- as.matrix(votes_affil)
svd_vectors <- svd(votes_normal_affil)
first_left <- t(svd_vectors$u[,1])
names(first_left) <- row.names(votes_matrix)
first_left_svd <- as.numeric(first_left)
first_left_scaled <- scales::rescale(first_left_svd,c(-1,1))
