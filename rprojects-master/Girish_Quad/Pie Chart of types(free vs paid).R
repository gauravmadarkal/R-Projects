library(plyr)
library(plotrix)
data=read.csv("googleplaystore.csv") 
mytable <- count(data,vars = "Type")
lbls <- paste(mytable$Type , "\n", mytable$freq , sep="")
mytable=mytable[-c(as.numeric(which(mytable$Type=="NaN"))),]
mytable=mytable[-c(as.numeric(which(mytable$Type=="0"))),]
pie(mytable$freq , labels = mytable$Type , radius = 1)