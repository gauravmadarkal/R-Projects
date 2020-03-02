term.frequency <- function(row) {
  row / sum(row)
}
inverse.doc.freq <- function(col) {
  corpus.size <- length(col)
  doc.count <- length(which(col > 0))
  
  log10(corpus.size / doc.count)
}

# Our function for calculating TF-IDF.
tf.idf <- function(x, idf) {
  x * idf
}
test.tokens=tokens(test_data$Sentiment,what="word",remove_numbers=TRUE,remove_punct=TRUE,remove_symbols=TRUE,remove_hyphens=TRUE)
test.tokens=tokens_tolower(test.tokens)
test.tokens=tokens_select(test.tokens,stopwords(),selection = "remove")
test.tokens=tokens_wordstem(test.tokens,language="english")
test.tokens=tokens_ngrams(test.tokens,n=1:2)
test.tokens.dfm=dfm(test.tokens,tolower = FALSE)
train_data.dfm
test.tokens.dfm <- dfm_select(test.tokens.dfm, pattern = train_data.dfm,
                              selection = "keep")
test.tokens.matrix <- as.matrix(test.tokens.dfm)
test.tokens.dfm
test.tokens.df <- apply(test.tokens.matrix, 1, term.frequency)
str(test.tokens.df)

train.tokens.idf <- apply(train_token_matrix, 2, inverse.doc.freq)

test.tokens.tfidf <-  apply(test.tokens.df, 2, tf.idf, idf = train.tokens.idf)
dim(test.tokens.tfidf)
#View(test.tokens.tfidf[1:25, 1:25])

# Transpose the matrix
test.tokens.tfidf <- t(test.tokens.tfidf)

# Fix incomplete cases
summary(test.tokens.tfidf[1,])
test.tokens.tfidf[is.na(test.tokens.tfidf)] <- 0.0
summary(test.tokens.tfidf[1,])

test.svd.raw <- t(sigma.inverse * u.transpose %*% t(test.tokens.tfidf))

library(irlba)
train.irlba <- irlba(t(train.tokens.tfidf), nv = 300, maxit = 600)




sigma.inverse <- 1 / train.irlba$d




train.tokens.tfidf <-  apply(train_tokens.df, 2, tf.idf, 
                             idf = train.tokens.idf)


# Transpose the matrix
train.tokens.tfidf <- t(train.tokens.tfidf)


# Fix incomplete cases
incomplete.cases <- which(!complete.cases(train.tokens.tfidf))
train.tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(train.tokens.tfidf))


# Make a clean data frame.
train.tokens.tfidf.df <- cbind(Label = train$Label, data.frame(train.tokens.tfidf))
names(train.tokens.tfidf.df) <- make.names(names(train.tokens.tfidf.df))
