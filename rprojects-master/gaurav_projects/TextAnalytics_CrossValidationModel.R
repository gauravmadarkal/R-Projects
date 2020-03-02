library(caret)
reviews_data=read.csv("googleplaystore_user_reviews.csv",stringsAsFactors = FALSE)

length(which(!complete.cases(reviews_data)))
refined_reviews_data=na.omit(reviews_data)

refined_reviews_data = refined_reviews_data[1:10000,] 
set.seed(4)
indexes = createDataPartition(refined_reviews_data$Sentiment,times = 1,p = 0.85,list = FALSE)
train_data= refined_reviews_data[indexes,]
test_data = refined_reviews_data[-indexes,]

library(quanteda)
test_data.tokens=tokens(test_data$Translated_Review,what="word",remove_numbers=TRUE,remove_punct=TRUE,remove_symbols=TRUE,remove_hyphens=TRUE)
test_data.tokens=tokens_tolower(test_data.tokens)
#View(test_data.tokens)
test_data.tokens=tokens_select(test_data.tokens,stopwords(),selection = "remove")
test_data.tokens=tokens_wordstem(test_data.tokens,language = "english")
test_data.dfm=dfm(test_data.tokens,tolower = FALSE,remove=stopwords())

test_token_matrix=as.matrix(test_data.dfm)

library(caret)
test_tokens.df= cbind(Label=test_data$Sentiment,as.data.frame(test_token_matrix))
names(test_tokens.df) <- make.names(names(test_tokens.df))
#set.seed(48743)
cv.folds = createMultiFolds(test_data$Sentiment,k = 10,times = 3)
cv.cntrl = trainControl(method = "repeatedcv", number = 10,repeats = 3, index = cv.folds)
#install.packages("doSNOW")
library(doSNOW)
start.time = Sys.time()

cl = makeCluster(2 ,type = "SOCK")
registerDoSNOW(cl)

rpart.cv.1 = train(Label ~ .,data = test_tokens.df,method = "rpart",
                   trControl = cv.cntrl, tuneLength = 7)

stopCluster(cl)

totaltime = Sys.time() - start.time
totaltime

rpart.cv.1

