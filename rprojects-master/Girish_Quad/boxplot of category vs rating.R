library("ggplot2")
data = read.csv("googleplaystore.csv")
data = unique(data)
data = data[-c(as.numeric(which(data$Rating>5))),]
Category_rating_map <- aggregate(data$Rating ~ data$Category, data = data,FUN=mean)
p = qplot(Category_rating_map$`data$Category`,Category_rating_map$`data$Rating`, data=Category_rating_map, geom=c("boxplot", "jitter"), fill=Category_rating_map$`data$Category`, main="category vs rating",xlab="category", ylab="rating")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
p = p + coord_flip()