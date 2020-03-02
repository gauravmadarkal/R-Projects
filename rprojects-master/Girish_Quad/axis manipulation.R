QuadData <- read.csv("Quad_Datasets/QUD5A84f3eb5304cc_set1.csv")
QuadData = subset(QuadData,select = c(1:14))
QuadData1$event_time <- strptime(QuadData1$event_time, "%Y-%m-%d %H:%M:%OS")
plot(QuadData1$event_time,QuadData1$switch, pch = 16,yaxt = "n",cex.axis = 0.5,col = "blue")

axis(2,las = 1,cex.axis=0.5)
hist(QuadData1$event_time ,"days")

plot(QuadData1$switch,QuadData1$event_time, pch = 16,cex.axis = 0.5,col = "blue")
hist(QuadData1$F)