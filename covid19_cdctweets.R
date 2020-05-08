library(tidyverse)
library(tidytext)
library(topicmodels)

options(scipen=999)

tweets <- read_csv("Desktop/cdctweetsforR")

View(tweets)

##create year and month column
tweets <- tweets %>%
  mutate(month = substring(created_at, 5, 7)) %>%
  mutate(year = substring(created_at, nchar(created_at)-3, nchar(created_at))) %>%
  filter(year == "2020") %>%
  filter(month == "Feb" | month == "Mar" | month == "Apr")

##word frequencies and bigrams##
tweets.words <- tweets %>%
  select(text) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(word != "https") %>%
  filter(word != "t.co") %>%
  filter(word != "rt") %>%
  filter(word != "cdc") %>%
  filter(word != "covid19") %>%
  filter(word != "covid") %>%
  filter(word != "coronavirus") %>%
  filter(word != "amp") %>%
  count(word, sort = TRUE)

tweets.bi <- tweets %>%
  select(text) %>%
  unnest_tokens(bigram, text, token="ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

##CODE DATA##
##this is a function for coding
code = function(keywordtable, colnum, content) {
  
  code <- unlist(paste(na.omit(keywordtable[,colnum]), collapse = "|"))
  out = ifelse(grepl(code, content, ignore.case = TRUE, perl = TRUE),1,0)
  return (out)
  
}

##read in keywords 
keywords <- read.csv("Desktop/keywords.csv")
names <- colnames(keywords)

##code tweets
tweets$people <- code(keywords, 1, tweets$text)
tweets$spread <- code(keywords, 2, tweets$text)
tweets$alarm <- code(keywords, 3, tweets$text)
tweets$positive.action <- code(keywords, 4, tweets$text)
tweets$global <- code(keywords, 5, tweets$text)
#tweets$illness.symptoms <- code(keywords, 6, tweets$text)
#tweets$experts <- code(keywords, 7, tweets$text)
#tweets$stats <- code(keywords, 8, tweets$text)

write.csv(tweets, file="tweetsforena.csv", row.names = F)

##LDA for exploration##
##create document term matrix
tweets.dtm <- tweets %>%
  select(text,id) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, id, sort = TRUE) %>%
  ungroup() %>%
  filter(word != "https") %>%
  filter(word != "t.co") %>%
  filter(word != "rt") %>%
  filter(word != "cdc") %>%
  filter(word != "covid19") %>%
  filter(word != "covid") %>%
  filter(word != "coronavirus") %>%
  cast_dtm(id, word, n)

##take out tweets with no terms so LDA doesn't fail
rowTotals <- apply(tweets.dtm, 1, sum)
tweets.dtm <- tweets.dtm[rowTotals > 0 , ]

##run LDA
tweets.lda <- LDA(tweets.dtm, k = 10, control = list(seed = 1234))
tweets.lda.tidy <- tidy(tweets.lda)

##Show top words in each topic
tweets.lda.top <- tweets.lda.tidy %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(topic, -beta) %>%
  ungroup()

##Plot top words in each topic
tweets.lda.top %>%
  mutate(term = reorder(term,topic)) %>%
  ggplot(aes(fct_reorder(term, beta), beta, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#write.csv(tweets, file ="tweets.csv", row.names = F)

#optimize number of topics (takes 10 hours to run)
# topic.numbers <- FindTopicsNumber(chats.dtm,
#                  topics = seq(from = 2, to = 15, by = 1),
#                  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#                  method = "Gibbs",
#                  control = list(seed = 1234),
#                  mc.cores = 2L,
#                  verbose = TRUE)
# FindTopicsNumber_plot(topic.numbers)