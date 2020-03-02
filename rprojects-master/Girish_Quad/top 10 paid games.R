data=read.csv("googleplaystore.csv") 
data$Installs=gsub("\\+", "", as.character(data$Installs))
data$Installs=gsub(",", "", as.character(data$Installs))
sortedFrame <- data[order(data$Installs,decreasing = TRUE),]
basicgames <- sortedFrame[sortedFrame$Category == "GAME",]
top10games=basicgames[1:10,]
top10PaidGames <- basicgames[basicgames$Type == "Paid",]
top10PaidGames=top10PaidGames[1:10,]
top10FreeGames <- basicgames[basicgames$Type == "Free",]
top10FreeGames=top10FreeGames[1:10,]