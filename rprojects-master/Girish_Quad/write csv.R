#library(caret)
#data <- read.csv("./src/googleplaystore_user_reviews.csv",stringsAsFactors = FALSE)
data=read.csv("googleplaystore_user_reviews.csv",stringsAsFactors = FALSE)

#print(length(which(!complete.cases(data))))
Mydata <- na.omit(data)
write.csv(Mydata,"playstoredata.csv")
dat = read.csv("playstoredata.csv")
#set.seed(4)
#indexes = createDataPartition(data$Sentiment,times = 1,p = 0.7,list = FALSE)
#train_data= data[indexes,]
#test_data = data[-indexes,]
#print(dim(data))
#print(table(is.na(train_data$Sentiment)))
#print(prop.table(table(train_data$Sentiment)))