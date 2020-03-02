data=read.csv("C:\\Users\\SakshamA\\Downloads\\dataFile.csv")#specifying .csv file path
data$Installs=gsub("\\+", "", as.character(data$Installs))
data$Installs=gsub(",", "", as.character(data$Installs))
data$Installs = as.numeric(data$Installs)
dataframe = data[log10(data$Installs) > 6 & data$Rating > 4.0, ]
dataframe = dataframe[order(dataframe$Installs,decreasing = TRUE),]
df = dataframe[1:10,]
p = plot(df$Rating,df$Installs,pch = 16,yaxt = "n",cex.axis = 0.5)
axis(2,las = 1,cex.axis=0.5)
text(df$Rating,df$Installs,labels = df$App,cex = 0.6,pos = 4,col = "blue")