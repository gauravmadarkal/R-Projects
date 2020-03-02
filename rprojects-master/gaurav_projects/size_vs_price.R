sizePriceData = data.frame(googleData)
sizePriceData$Size = as.character(sizePriceData$Size)
sizePriceData$Size = substr(sizePriceData$Size,1,nchar(sizePriceData$Size)-1)
sizePriceData$Size = gsub("Varies with devic","10",sizePriceData$Size)
sizePriceData = sizePriceData[-c(9991),]
sizePriceData$Size = as.numeric(sizePriceData$Size)
#plot(sizePriceData$Price,sizePriceData$Size,col = "red",pch = 16,main="price vs size",xlab = "Price",ylab = "Size")
p = qplot(sizePriceData$Price,sizePriceData$Size, data=sizePriceData, geom=c("point", "jitter"), main="price vs size",xlab="price", ylab="size")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
