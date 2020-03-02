library(quanteda)
test_data.tokens=tokens(test_data$Translated_Review,what="word",remove_numbers=TRUE,remove_punct=TRUE,remove_symbols=TRUE,remove_hyphens=TRUE)
test_data.tokens=tokens_tolower(test_data.tokens)
#View(test_data.tokens)
test_data.tokens=tokens_select(test_data.tokens,stopwords(),selection = "remove")
test_data.tokens=tokens_wordstem(test_data.tokens,language = "english")
test_data.dfm=dfm(test_data.tokens,tolower = FALSE,remove=stopwords())

test_token_matrix=as.matrix(test_data.dfm)