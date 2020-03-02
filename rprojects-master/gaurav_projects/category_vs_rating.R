googleData = read.csv("googleplaystore.csv")
googleData = unique(googleData) 
googleData = googleData[-c(as.numeric(which(googleData$Rating>5))),]
Category_rating_map <- aggregate(googleData$Rating ~ googleData$Category, data = googleData,FUN=mean)
p = qplot(Category_rating_map$`googleData$Category`,Category_rating_map$`googleData$Rating`, data=Category_rating_map, geom=c("boxplot", "jitter"), fill=Category_rating_map$`googleData$Category`, main="category vs rating",xlab="category", ylab="rating")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
p = p + coord_flip()