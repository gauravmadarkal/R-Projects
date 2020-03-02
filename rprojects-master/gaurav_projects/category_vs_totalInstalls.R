Category_Installs_map <- aggregate(googleData$Installs ~ googleData$Category, data = googleData,FUN=sum)
barplot(Category_Installs_map$`googleData$Installs`,yaxt = "n",names.arg = Category_Installs_map$`googleData$Category`,main = "category vs total installs",xlab="Category",col = "blue" ,cex.names = 0.4,cex.axis = 0.8)
axis(2,las = 1,cex.axis=0.5)


