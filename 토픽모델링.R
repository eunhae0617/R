install.packages('textdata')
library(textdata)
news<-dataset_ag_news(split = "test")
news
table(news$class)

library(tm)
docs<-VCorpus(VectorSource(news$description))
docs
lapply(docs, content)[1:3]

myRemove <- content_transformer(function(x, pattern)
  {return(gsub(pattern, "", x))})
toSpace <- content_transformer(function(x, pattern)
  {return(gsub(pattern, " ", x))})
mystopwords <- c(stopwords("english"),
                 c("first", "second", "one", "two", "three", "four", "another", 
                   "last", "least", "just", "will", "week", "weeks","quot", 
                   "ago", "day", "days", "night", "nights", "month","months", 
                   "years", "year", "next", "now", "today", "yesterday", 
                   "may", "new", "york", "according", "back", "say","says", 
                   "said", "can", "make","made", "reuters", "monday", "tuesday", 
                   "wednesday", "thursday", "friday", "saturday", "sunday"))

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, myRemove, "(f|ht)tp\\S+\\s*")
docs <- tm_map(docs, myRemove,"www\\.+\\S+")
docs <- tm_map(docs, removeWords, mystopwords)
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, ";")
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "\\.")
docs <- tm_map(docs, toSpace, "\\\\")
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(trimws))
lapply(docs, content)[1:3]

dtm <- DocumentTermMatrix(docs)
inspect(dtm)
rownames(dtm) <- paste0(rownames(dtm), "-", news$class)
inspect(dtm)

install.packages('topicmodels')
library(topicmodels)
news.lda <- LDA(dtm, k=4, method = "Gibbs",
               control = list(seed=123, 
                              burnin=1000,#처음 1000번의 결과는 버린다.
                              iter=1000,#sampling을 1000번 반복한다. 
                              thin=100))#100개 단위로 매 100번째 결과가 다음 sampling에서 사용되도록 한다.
                              #인접한 sample간에는 상관관계가 높기 때문에 상관관계가 적은 독립sample을 얻을 수 있다.

class(news.lda)

topics(news.lda)[1:5]

table(topics(news.lda))

terms(news.lda, 10)

str(news.lda, max.level = 2, nchar.max = 50)

news.lda@beta[, 1:5]
exp(news.lda@beta[, 1:5])

install.packages('reshape2')
install.packages('tidytext')
library(tidytext)
news.term <- tidy(news.lda, matrix="beta")
news.term

library(dplyr)
news.term.top <- news.term %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
news.term.top

library(ggplot2)
ggplot(news.term.top, 
       aes(reorder(term, beta), beta, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~ paste("Topic", topic), scales = "free") +
  labs(x=NULL, y="word-Topic Probability(Beta)") +
  coord_flip()
news.lda@gamma[1:5, ]

news.docs <- tidy(news.lda, matrix="gamma")
news.docs

library(tidyr)
news.docs <- news.docs %>%
  separate(document, c("id", "category"), sep = "-", convert = TRUE)
news.docs

ggplot(news.docs,
       aes(factor(topic), gamma, fill=category)) +
  geom_boxplot(color="gray50", show.legend = FALSE) +
  facet_wrap(~ category, scales = "free") +
  labs(x="Topic", "Document-Topic Probability (Gamma)")
