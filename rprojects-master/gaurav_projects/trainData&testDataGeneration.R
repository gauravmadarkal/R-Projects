library(caret)
reviews_data=read.csv("googleplaystore_user_reviews.csv",stringsAsFactors = FALSE)

length(which(!complete.cases(reviews_data)))
refined_reviews_data=na.omit(reviews_data)
set.seed(4)
indexes = createDataPartition(refined_reviews_data$Sentiment,times = 1,p = 0.7,list = FALSE)
train_data= refined_reviews_data[indexes,]
test_data = refined_reviews_data[-indexes,]
