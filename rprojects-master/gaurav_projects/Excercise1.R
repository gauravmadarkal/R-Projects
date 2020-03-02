df = read.csv("googleplaystore.csv")
df = unique(df) 
df$Installs = gsub(",","",df$Installs)
df$Installs = substr(df$Installs,1,nchar(as.character(df$Installs))-1)
df$Installs = as.numeric(df$Installs)
dataframe = df[log10(df$Installs) > 6 & df$Rating > 4.0, ]
dataframe = dataframe[order(dataframe$Installs,decreasing = TRUE),]
df = dataframe[1:10,]
p = plot(df$Rating,df$Installs,pch = 16,yaxt = "n",cex.axis = 0.5)
axis(2,las = 1,cex.axis=0.5)
text(df$Rating,df$Installs,labels = df$App,cex = 0.6,pos = 4,col = "blue")
