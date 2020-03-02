indices <- arrayInd(which.max(train_token_matrix),dim(train_token_matrix))
rname = rownames(train_token_matrix)[indices[,1]]
cname = colnames(train_token_matrix)[indices[,2]]