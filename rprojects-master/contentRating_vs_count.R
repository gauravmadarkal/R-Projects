library(plyr)
count_content_rating <- count(googleData,vars = "Content.Rating")
barplot(count_content_rating$freq,names.arg = count_content_rating$Content.Rating,main = "content_rating vs freq",xlab="content_rating",col = "blue" )