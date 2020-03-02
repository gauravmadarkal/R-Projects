sortedFrame <- googleData[order(googleData$Installs,decreasing = TRUE),]
top10games <- sortedFrame[sortedFrame$Category == "GAME",]
top10PaidGames <- top10games[top10games$Type == "Paid",]
top10FreeGames <- top10games[top10games$Type== "Free",]
top10FreeGames <- top10FreeGames[1:10,]
top10PaidGames <- top10PaidGames[1:10,]

