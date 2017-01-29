library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(plotly)
library(stringr)
library(twitteR)
library(lubridate)
library(tidytext)
theme_set(theme_bw(base_size = 17))



# get data 
top_year <- read.csv("~/Projects/random_things/hack_cambridge_2017/top_year.csv", header = TRUE, sep=",")
controv_1000  <- read.csv("~/Projects/random_things/hack_cambridge_2017/politics_controversial_1000.csv", header = TRUE, sep=",")
top_1000  <- read.csv("~/Projects/random_things/hack_cambridge_2017/politics_top_1000.csv", header = TRUE, sep=",")


head(top_1000)
str(controv_1000)
head(controv_1000)
controv_1000$top_comment_body

# count number of "Trump" | "Donald", "Bernie" | "Sanders", "Hillary" | "Clinton"

# clean
top_year <- top_1000 %>% mutate(title = tolower(title))
top_year <- top_1000 %>% mutate(title = tolower(title))
head(top_year)

# filter out stop words
top_year <- top_year %>% mutate(mentions_donald_trump = as.factor(ifelse(grepl(x = title, pattern = "donald|trump")==TRUE, 1, 0)),
                    mentions_hillary_clinton = as.factor(ifelse(grepl(x = title, pattern = "hillary|clinton")==TRUE, 1, 0)),
                    mentions_bernie_sanders = as.factor(ifelse(grepl(x = title, pattern = "bernie|sanders")==TRUE, 1, 0)))
                    # title_clean = paste(str_split(title, " ")[[1]][!(str_split(title, " ")[[1]] %in% tidytext::stop_words$word)], collapse = " "))
                    # title_clean = paste(str_split(title, " "), collapse = " "))
top_year$date <- as.Date(top_year$date)
                    
head(top_year)
# paste(str_split(top_year$title, " ")[[1]][!(str_split(top_year$title, " ")[[1]] %in% tidytext::stop_words$word)], collapse = " ")

# summarise
# upvote ratio, mentioning donald trump
str(top_year)

### PLOT 1 DEMO
ggplot(data = top_year, aes(x=date, y = upvote_ratio, colour=mentions_donald_trump, size=score)) + 
  geom_point() + geom_smooth() + scale_color_manual(values = c("grey", "red"))
ggplotly()

# proportion of posts with the donald
# break down by month
head(top_year)
proportion_mentions <- top_year %>% 
  mutate(year = year(date), 
         month = month(date),
         month = ifelse(year == 2017, 13, month)) %>%
  group_by(year, month) %>% 
  summarise(prop_donald = sum(ifelse(mentions_donald_trump == "1", 1, 0))/n(),
            prop_hillary = sum(ifelse(mentions_hillary_clinton == "1", 1, 0))/n(),
            prop_bernie = sum(ifelse(mentions_bernie_sanders == "1", 1, 0))/n(),
            count = n()) %>%
  gather(key = politician, value = proportion, -c(year, month, count))

summary(proportion_mentions$month)
head(proportion_mentions)

#### PLOT 2
ggplot(data = proportion_mentions, aes(x=month, y = proportion, colour=politician)) + 
  geom_point() + geom_smooth() + scale_color_manual(values = c("blue", "pink", "red"))
ggplotly()


# most common words used to describe trump, clinton, sanders
head(top_year)

# need 1 row per word
by_word <- top_year %>% unnest_tokens(output = word, input = title); head(by_word)

# need to start doing sentiment analysis
# get sentiments for words
AFINN <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  select(word, afinn_score = score)

# join onto sentiment table
title_sentiment <- by_word %>%
  anti_join(tidytext::stop_words) %>%
  inner_join(AFINN, by = "word") %>%
  group_by(id) %>%
  summarize(sentiment = mean(afinn_score)) %>%
  ungroup() %>% as.data.frame() %>%
  inner_join((top_year %>% select(id, date, title, score, mentions_donald_trump:mentions_bernie_sanders)), by = "id") %>% 
  arrange(sentiment); head(title_sentiment)

# plot sentiment by candidates
title_sentiment$date <- as.Date(title_sentiment$date); str(title_sentiment); head(title_sentiment)
ggplot(data = title_sentiment, aes(x=date, y = sentiment, colour=mentions_donald_trump, size = score)) + 
  geom_point(alpha=0.9) + geom_smooth()
ggplotly()

head(title_sentiment)
head(top_year)
# 
# # most commonly used words and sentiments
# most_used_negative_words <- by_word %>% 
#   select(id, mentions_donald_trump:mentions_bernie_sanders, word) %>%
#     gather(key = politician, value = not_sure, -c(id, word)) %>%
#   inner_join(AFINN, by = "word") %>%
#   group_by(politician, word) %>%
#   summarize(sentiment = mean(afinn_score),
#             count = n()) %>%
#   ungroup() %>% as.data.frame() %>%
#   arrange(sentiment); head(most_used_negative_words)
# 
# # which words most likely to describe a trump-related post than a clinton or bernie post
# 

# NRC Word-Emotion Association lexicon, available from the tidytext package,
# associates words with 10 sentiments: positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

head(nrc)
# join onto sentiment table
title_sentiment <- by_word %>%
  anti_join(tidytext::stop_words) %>%
  inner_join(nrc, by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(count_sentiment = n()) %>%
  ungroup() %>% as.data.frame() %>%
  inner_join((top_year %>% select(id, date, title, score, mentions_donald_trump:mentions_bernie_sanders)), by = "id") %>% 
  arrange(sentiment) %>%
  mutate(year = year(date), 
         month = month(date),
         month = ifelse(year == 2017, 13, month)); head(title_sentiment)

head(title_sentiment)
nrow(top_year)
# clean up sentiment data, let's show averaged by month
title_sentiment %>% group_by(mentions_donald_trump, mentions_hillary_clinton, mentions_bernie_sanders, sentiment) %>%
  summarise(count_of_emotion = n())

### PLOT X plot sentiment in general
sentiment_in_general <- title_sentiment %>% group_by(month, sentiment) %>%
  summarise(count_of_emotion = n()); sentiment_in_general

ggplot(data = sentiment_in_general, aes(x=month, y = count_of_emotion, colour=sentiment)) + 
  geom_point(alpha=0.9) + geom_smooth(se = FALSE)
ggplotly()

### Scale for counts
head(title_sentiment)
posts_by_month <- title_sentiment %>% group_by(month) %>%
  summarise(n_posts = n_distinct(id)); posts_by_month

sentiment_in_general_scaled <- sentiment_in_general %>% 
  inner_join(posts_by_month, by = 'month') %>% 
  filter(n_posts > 5) %>%
  mutate(emotion_proportion = count_of_emotion/n_posts)

### This would be good to demo
ggplot(data = sentiment_in_general_scaled, aes(x=month, y = emotion_proportion, colour=sentiment)) + 
  geom_point(alpha=0.9) + geom_smooth(se = FALSE)
ggplotly()


# plot sentiment by candidates
head(title_sentiment)
mentions_donald <- title_sentiment %>% 
  group_by(mentions_donald_trump, month, sentiment) %>%
  summarise(count_of_emotion = n()) %>%
  inner_join(posts_by_month_donald, by = c('month', 'mentions_donald_trump')) %>% 
  filter(n_posts > 5) %>%
  mutate(emotion_proportion = count_of_emotion/n_posts)
head(mentions_donald)
str(mentions_donald)

# need diff scale
posts_by_month_donald <- title_sentiment %>% group_by(month, mentions_donald_trump) %>%
  summarise(n_posts = n_distinct(id)); posts_by_month_donald

ggplot(data = mentions_donald, aes(x=month, y = emotion_proportion, colour=sentiment)) + 
  geom_point(alpha=0.9) + geom_smooth(se = FALSE) + facet_wrap(~ mentions_donald_trump)
ggplotly()

#### NOW HILLARY
# need diff scale
posts_by_month_hillary <- title_sentiment %>% group_by(month, mentions_hillary_clinton) %>%
  summarise(n_posts = n_distinct(id)); posts_by_month_hillary

# plot sentiment by candidates
head(title_sentiment)
mentions_hillary <- title_sentiment %>% 
  group_by(mentions_hillary_clinton, month, sentiment) %>%
  summarise(count_of_emotion = n()) %>%
  inner_join(posts_by_month_hillary, by = c('month', 'mentions_hillary_clinton')) %>% 
  filter(n_posts > 5) %>%
  mutate(emotion_proportion = count_of_emotion/n_posts)

head(mentions_hillary)
str(mentions_hillary)

ggplot(data = mentions_hillary, aes(x=month, y = emotion_proportion, colour=sentiment)) + 
  geom_point(alpha=0.9) + geom_smooth(se = FALSE) + facet_wrap(~ mentions_hillary_clinton)
ggplotly()
head(title_sentiment)
head(top_year)


# # almost there, not quite
# more_emotions <- by_word %>%
#   anti_join(tidytext::stop_words) %>%
#   inner_join(nrc, by = "word") %>%
#   count(sentiment, id) %>%
#   ungroup() %>%
#   # complete(sentiment, id, fill = list(n = 0)) %>%
#   as.data.frame() %>%
#   inner_join((top_year %>% select(id, date, title, score, mentions_donald_trump:mentions_bernie_sanders)), by = 'id') %>%
#   group_by(mentions_donald_trump, mentions_hillary_clinton, mentions_bernie_sanders, sentiment) %>%
#   summarize(count = n()) 

# tests
library(broom)

sentiment_differences <- by_source_sentiment %>%
  group_by(sentiment) %>%
  do(tidy(poisson.test(.$words, .$total_words)))

sentiment_differences


# wordclouds
library(wordcloud)
by_word %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#Thus, Trump’s Android account uses about 40-80% more words related to disgust, 
# sadness, fear, anger, and other “negative” sentiments than the iPhone account does. 



# Which threads have the highest proportion of negative words?

# Which threads have the highest proportion of positive words?


# The Stanford CoreNLP tools and the sentimentr R package 
# (currently available on Github but not CRAN) are examples of such sentiment analysis algorithms. For these, we may want to tokenize text into sentences.





##### MICROSOFT STUFF #####
# parts of speech tagging
