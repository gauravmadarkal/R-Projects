googleData = read.csv("googleplaystore.csv")
googleData$Last.Updated = gsub(",","",googleData$Last.Updated)
googleData$Last.Updated <- as.Date(googleData$Last.Updated,format = "%B %d %Y")
googleData$No_Days_Since_Update=Sys.Date()-googleData$Last.Updated
googleData = googleData[-c(as.numeric(which(googleData$Rating>5))),]
plot(googleData$No_Days_Since_Update,googleData$Rating, pch = 16,yaxt = "n",cex.axis = 0.5,col = "blue")
axis(2,las = 1,cex.axis=0.5)