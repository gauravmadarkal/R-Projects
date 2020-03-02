
library(quanteda)
train_data.tokens=tokens(train_data$Translated_Review,what="word",remove_numbers=TRUE,remove_punct=TRUE,remove_symbols=TRUE,remove_hyphens=TRUE)
train_data.tokens=tokens_tolower(train_data.tokens)
train_data.tokens=tokens_select(train_data.tokens,stopwords(),selection = "remove")
train_data.tokens=tokens_wordstem(train_data.tokens,language = "english")
train_data.dfm=dfm(train_data.tokens,tolower = FALSE,remove=stopwords())

train_token_matrix=as.matrix(train_data.dfm)