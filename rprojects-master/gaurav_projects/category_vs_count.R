library(plyr)
count_category <- count(googleData,vars = "Category")
barplot(count_category$freq,names.arg = count_category$Category,main = "category vs freq",xlab="Category",col = "blue" ,cex.names = 0.2,cex.axis = 0.8)