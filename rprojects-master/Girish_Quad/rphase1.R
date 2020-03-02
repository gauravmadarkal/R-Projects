library(ggplot2)
#df <- read.csv("./src/googleplaystore.csv")
data=read.csv("googleplaystore_user_reviews.csv",stringsAsFactors = FALSE)

length(which(!complete.cases(data)))
data_renew=na.omit(data)
summary(data_renew$Review_length) #to find min,max,median,mean
prop.table(table(data_renew$Sentiment)) #to find the proportion of different values in dataframe column
data_renew$Review_length=nchar(data_renew$Translated_Review)
ggplot(data_renew,aes(x=data_renew$Review_length,fill=data_renew$Sentiment ))+theme_bw()+geom_histogram(binwidth = 5)+labs(y="Review_count",x="Length_of_review",title="Distribution of Sentiment")