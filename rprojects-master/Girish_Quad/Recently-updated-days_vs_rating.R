data=read.csv("googleplaystore.csv") 
data$Last.Updated=gsub(",","",data$Last.Updated)
data$Last.Updated=as.Date(paste(data$Last.Updated,sep="-"),"%B %d %Y")
data$date_diff<- Sys.Date()-data$Last.Updated
p = plot(data$date_diff,data$Rating,xlab = "Updated before this days",ylab = "Rating",main = "DATE_DIFF v/s RATING", col="purple"  , ylim=c(1,5),pch = 16,cex.axis = 0.5)
