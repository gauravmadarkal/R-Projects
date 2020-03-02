#p =qplot(category_ContentRating$count,category_ContentRating$`googleData$Category`, data=category_ContentRating, geom=c("bar", "jitter"), fill=category_ContentRating$`googleData$Content.Rating`, main="category vs count",xlab="category", ylab="count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
#p = p + coord_flip()
library(dplyr)
category_ContentRating = summarise(group_by(googleData,googleData$Category,googleData$Content.Rating),count=n())
p = ggplot(category_ContentRating,aes(x=category_ContentRating$`googleData$Category`,fill=category_ContentRating$`googleData$Content.Rating`))+geom_bar()+theme_classic()
p= p + coord_flip()

