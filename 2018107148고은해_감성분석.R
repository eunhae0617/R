library(tidytext)
library(textdata)
sentiments

get_sentiments(lexicon = "bing")
unique(get_sentiments("bing")$sentiment)

get_sentiments(lexicon = "afinn")
unique(get_sentiments("afinn")$value)
summary(get_sentiments("afinn")$value)

get_sentiments(lexicon = "nrc")
unique(get_sentiments("nrc")$sentiment)

library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(lubridate)

url<-"https://archive.ics.uci.edu/ml/machine-learning-databases/00438/Health-News-Tweets.zip"
local.copy<-tempfile() #다운로드 파일 임시 파일 생성 
download.file(url, local.copy, mode='wb') #다운로드 파일 지속 사용시 두번째 인수에 실제로 사용할 파일명 입력
Sys.setlocale("LC_TIME", "English") #locale을 영어권으로 바꾼다
health.twiter <-
  map(unzip(zipfile=local.copy,
            files=c("Health-Tweets/bbchealth.txt",
                    "Health-Tweets/cnnhealth.txt",
                    "Health-Tweets/foxnewshealth.txt",
                    "Health-Tweets/NBChealth.txt")),
      read_delim, delim="|", quote="",
      col_types=list(col_character(), col_character(),col_character()),
      col_names=c("id","datetime","tweet")) %>% #1번째
  map2(c("bbc", "cnn", "foxnews", "nbc"),
       ~ cbind(.x, source=.y)) %>% 
  reduce(bind_rows) %>%
  as_tibble() %>% #2번째, 출처까지 포함하여 깔끔하게 정리
  mutate(datetime=ymd_hms(strptime(datetime,
                                   "%a %b %d %H:%M:%S +0000 %Y")))
health.twiter

unlink(local.copy)
Sys.setlocale()

health.twiter

health.twiter %>%
  count(source)

library(stringr)
health.words <- health.twiter %>%
  select(-id) %>%
  mutate(tweet=str_replace_all(tweet, pattern = "(f|ht)tp\\S+s*",replacement = "")) %>%
  mutate(tweet=str_replace_all(tweet, pattern = "\\d",replacement = "")) %>%
  mutate(tweet=str_replace_all(tweet, pattern = "\\bRT",replacement = "")) %>% #\b=문자열과 비문자열의 경계선을 식별 따라서 RT로 시작하는 문자열을 찾아줌 
  mutate(tweet=str_replace_all(tweet, pattern = "@\\S+",replacement = "")) %>%
  mutate(tweet=str_replace_all(tweet, pattern = "&amp",replacement = "")) %>% 
  unnest_tokens(word, tweet) #tweet열에서 텍스트를 읽어 word열로 저장 
health.words


health.words %>%
  inner_join(get_sentiments("bing"), by="word")
health.words %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort=TRUE)
health.words %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup()
health.sentiment <- health.words %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup()%>%
  mutate(nsign=ifelse(sentiment == "negative", -n, n))
health.sentiment

library(ggplot2)
library(scales)
ggplot(health.sentiment,
       aes(x=reorder(word, nsign), y=nsign,
           fill=factor(sentiment,
                       levels = c("positive", "negative")))) +
  geom_col(color ="lightslategray", width=0.8) +
  geom_text(aes(label=n), size=3, color = "black",
            hjust=ifelse(health.sentiment$nsign<0, 1.1, -0.1)) +
  scale_fill_manual(values = c("cornflowerblue", "tomato")) +
  scale_y_continuous(breaks = pretty(health.sentiment$nsign),
                     labels = abs(pretty(health.sentiment$nsign))) +
  labs(x=NULL, y="Count") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  coord_flip()

health.sentiment <- health.words %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(!(word=="patient"|word=="cancer"|word=="virus")) %>%
  count(word, sentiment, sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup()%>%
  mutate(nsign=ifelse(sentiment == "negative", -n, n))
health.sentiment

ggplot(health.sentiment,
       aes(x=reorder(word, n), y=n,
           fill=factor(sentiment,
                       levels = c("positive", "negative")))) +
  geom_col(color ="lightslategray", width=0.8, show.legend = FALSE) +
  geom_text(aes(label=n), size=3, color = "black",
            hjust=1.2) +
  scale_fill_manual(values = c("lightsteelblue", "lightsalmon")) +
  facet_wrap(~ factor(sentiment,
                    levels = c("positive", "negative")),
             ncol = 2, scales = "free") +
  labs(x=NULL, y="Count") +
  coord_flip()

install.packages("wordcloud")
library(wordcloud)
library(reshape2)
set.seed(123)
windows(width = 7, height = 7)
health.words %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(!(word=="patient"|word=="cancer"|word=="virus")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup() %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("tomato", "cornflowerblue"),
                   title.size = 2, 
                   title.colors = c("red", "blue"),
                   title.bg.colors = "wheat", 
                   scale = c(4, 0.3), max.words = 200)

health.sentiment <- health.words %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(!(word=="patient"|word=="cancer"|word=="virus")) %>%
  count(word, sentiment, source, sort=TRUE) %>%
  group_by(source, sentiment) %>%
  top_n(10, n) %>%
  ungroup()
health.sentiment

windows(width = 7, height = 9)
ggplot(health.sentiment,
       aes(reorder_within(x=word, by=n, within = source),
           y=n, fill=source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ factor(source,
                      labels=c("BBC", "CNN", "Fox News", "NBC")) + sentiment,
             ncol = 2, scales = "free") +
  scale_x_reordered() +
  labs(x=NULL, y="Count") +
  coord_flip()

health.sentiment <- health.words %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(!(word=="patient"|word=="cancer"|word=="virus")) %>%
  mutate(time=floor_date(x=datetime, unit="month")) %>%
  count(sentiment, time) %>%
  group_by(sentiment) %>%
  slice(2:(n()-1)) %>%
  ungroup()
health.sentiment

Sys.setlocale("LC_TIME", "English")
windows(width=7.0, height=5.5)
ggplot(health.sentiment,
       aes(x=time, y=n, fill=sentiment, color=sentiment)) +
  geom_area(position = "identity", alpha=0.3) +
  geom_line(size=1.5) +
  scale_fill_manual(labels = c("Negative", "Positive"),
                    values = c("Orangered", "deepskyblue2")) +
  scale_color_manual(labels = c("Negative", "Positive"),
                     values = c("Orangered", "deepskyblue2")) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
  labs(x=NULL, y="Count") +
  theme(legend.position = "bottom", legend.title = element_blank())

windows(width=10.0, height=10.0)
health.words %>%
  inner_join(get_sentiments("bing"), by="word") %>%
  filter(!(word=="patient"|word=="cancer"|word=="virus")) %>%
  mutate(time=floor_date(x=datetime, unit="month")) %>%
  count(source, sentiment, time) %>%
  group_by(source,sentiment) %>%
  slice(2:(n()-1)) %>%
  ungroup() %>%
  ggplot(aes(x=time, y=n, fill=sentiment, color=sentiment)) +
  geom_area(position = "identity", alpha=0.3) +
  geom_line(size=1.5) +
  facet_wrap(~ factor(source, 
                      labels = c("BBC", "CNN", "Fox News", "NBC")),
             nrow=4, scales = "free") +
  scale_fill_manual(labels = c("Negative", "Positive"),
                    values = c("coral", "cornflowerblue")) +
  scale_color_manual(labels = c("Negative", "Positive"),
                     values = c("coral", "cornflowerblue")) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "2 months") +
  labs(x=NULL, y="Count") +
  theme(legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(size=8))
