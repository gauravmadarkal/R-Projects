library(caret)
data=read.csv("googleplaystore_user_reviews.csv",stringsAsFactors = FALSE)

length(which(!complete.cases(data)))
data_renew=na.omit(data)
set.seed(4)
indexes = createDataPartition(data_renew$Sentiment,times = 1,p = 0.7,list = FALSE)
train_data= data_renew[indexes,]
test_data = data_renew[-indexes,]
print(table(is.nan(train_data$Sentiment)))
print(prop.table(table(train_data$Sentiment)))