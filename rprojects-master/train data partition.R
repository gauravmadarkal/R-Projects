library(caret)
reviews_data=read.csv("googleplaystore_user_reviews.csv",stringsAsFactors = FALSE)

length(which(!complete.cases(reviews_data)))
refined_reviews_data=na.omit(reviews_data)

#refined_reviews_data = refined_reviews_data[1:10000,] 
set.seed(4)
indexes = createDataPartition(refined_reviews_data$Sentiment,times = 1,p = 0.7,list = FALSE)
train1_data= refined_reviews_data[indexes,]
test1_data = refined_reviews_data[-indexes,]

indexes2 = createDataPartition(train1_data$Sentiment,times = 1,p = 0.7,list = FALSE)
train2_data= train1_data[indexes2,]
test2_data = train1_data[-indexes2,]

indexes3 = createDataPartition(train2_data$Sentiment,times = 1,p = 0.75,list = FALSE)
train3_data= train2_data[indexes3,]
test3_data = train2_data[-indexes3,]


indexes4 = createDataPartition(train3_data$Sentiment,times = 1,p = 0.75,list = FALSE)
train4_data= train3_data[indexes4,]
test4_data = train3_data[-indexes4,]


indexes5 = createDataPartition(train4_data$Sentiment,times = 1,p = 0.75,list = FALSE)
train5_data= train4_data[indexes5,]
test5_data = train4_data[-indexes5,]


indexes6 = createDataPartition(train5_data$Sentiment,times = 1,p = 0.75,list = FALSE)
train_data= train5_data[indexes6,]
test_data = train5_data[-indexes6,]



library(quanteda)
train_data.tokens=tokens(train_data$Translated_Review,what="word",remove_numbers=TRUE,remove_punct=TRUE,remove_symbols=TRUE,remove_hyphens=TRUE)
train_data.tokens=tokens_tolower(train_data.tokens)
#View(train_data.tokens)
train_data.tokens=tokens_select(train_data.tokens,stopwords(),selection = "remove")
train_data.tokens=tokens_wordstem(train_data.tokens,language = "english")
train_data.dfm=dfm(train_data.tokens,tolower = FALSE,remove=stopwords())

train_token_matrix=as.matrix(train_data.dfm)

library(caret)
train_tokens.df= cbind(Label=train_data$Sentiment,as.data.frame(train_token_matrix))
names(train_tokens.df) <- make.names(names(train_tokens.df))
#set.seed(48743)
cv.folds = createMultiFolds(train_data$Sentiment,k = 10,times = 3)
cv.cntrl = trainControl(method = "repeatedcv", number = 10,repeats = 3, index = cv.folds)
#install.packages("doSNOW")
library(doSNOW)
start.time = Sys.time()

cl = makeCluster(2 ,type = "SOCK")
registerDoSNOW(cl)

rpart.cv.1 = train(Label ~ .,data = train_tokens.df,method = "rpart",
                   trControl = cv.cntrl, tuneLength = 7)
cl
stopCluster()

totaltime = Sys.time() - start.time
totaltime

rpart.cv.1