x <- c(1:200)[c(T,F)]
z=x
z=as.character(z)
dat <- data.frame(intcol = x,charcol=z)
d=sapply(z,nchar)
dat$newcolumn<-d
