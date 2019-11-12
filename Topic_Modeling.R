library(topicmodels)
library(textreadr)
library(tm)


name <- list.files(path = "./DL2")
strings <- c()
for (i in 1:length(name)){
  new <- read_rtf(paste0("./DL2/",name[i]))
  new <- paste(new,collapse = ' ')
  strings <- c(strings,new)
}

m <- list(content = "strings")
strings <- as.data.frame(strings)
corpus <- Corpus(VectorSource(strings$strings))
dtm <- DocumentTermMatrix(corpus,control = list(stemming = TRUE, stopwords = TRUE,
                                               minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE))
tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i], dtm$j, mean) *
  log2(tm::nDocs(dtm)/slam::col_sums(dtm > 0))


harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

k <- 3
burnin <- 1000
iter <- 1000
keep <- 50
raw.sum=apply(dtm,1,FUN=sum)
dtm <- dtm[raw.sum!=0,]

fitted <- LDA(dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep))
news_topics <- topics(fitted,1)
news_terms <- as.data.frame(terms(fitted,30),stringsAsFactors=FALSE)
write.csv(news_terms,"dl2_topics.csv")


library(syuzhet)
library(tidytext)
sentiments <- lapply(news_terms,get_nrc_sentiment)
sentiments <- lapply(sentiments,colSums)
sentiments

# -- AA --

name <- list.files(path = "./AA2")
strings <- c()
for (i in 1:length(name)){
  new <- read_rtf(paste0("./AA2/",name[i]))
  new <- paste(new,collapse = ' ')
  strings <- c(strings,new)
}

m <- list(content = "strings")
strings <- as.data.frame(strings)
corpus <- Corpus(VectorSource(strings$strings))
dtm <- DocumentTermMatrix(corpus,control = list(stemming = TRUE, stopwords = TRUE,
                                                minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE))
tfidf <- tapply(dtm$v/slam::row_sums(dtm)[dtm$i], dtm$j, mean) *
  log2(tm::nDocs(dtm)/slam::col_sums(dtm > 0))


harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

k <- 3
burnin <- 1000
iter <- 1000
keep <- 50
raw.sum=apply(dtm,1,FUN=sum)
dtm <- dtm[raw.sum!=0,]

fitted <- LDA(dtm, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep))
news_topics <- topics(fitted,1)
news_terms <- as.data.frame(terms(fitted,30),stringsAsFactors=FALSE)
write.csv(news_terms,"aa2_topics.csv")


library(syuzhet)
library(tidytext)
sentiments <- lapply(news_terms,get_nrc_sentiment)
sentiments <- lapply(sentiments,colSums)
sentiments