library(tidytext)
library(SnowballC)
library(tidyverse)

tweets_df = read.csv('tweets on pandemic birth rate.csv', header = TRUE)
summary(tweets_df)

tweets_data=select(tweets_df,tweets)
tidy_dataset=unnest_tokens(tweets_data,word,tweets)
head(tidy_dataset)
counts = count(tidy_dataset, word)
result1 = arrange(counts, desc(n))
typeof(result1)
slice(result1,1:15)

#removing stop words
data("stop_words")
tidy_dataset2 = anti_join(tidy_dataset, stop_words)
counts2 = count(tidy_dataset2, word)
result2 = arrange(counts2, desc(n))
typeof(result2)
slice(result2,1:15)

#removing numerical values (and blank spaces)
patterndigits = '\\b[0-9]+\\b'
tidy_dataset2$word=str_remove_all(tidy_dataset2$word, patterndigits )
head(tidy_dataset2)
counts3 = count(tidy_dataset2, word)
result3 = arrange(counts3, desc(n))
slice(result3,1:15)

#removing certain words
list_remove=c("pandemic","birthrate", "https", "tmobilesprint", "â")
tidy_dataset3 = filter(tidy_dataset2, !(word %in% list_remove))
counts4= count(tidy_dataset3,word)
result4=arrange(counts4,desc(n))
slice(result4,1:15)

#removing new lines
tidy_dataset3$word=str_remove_all(tidy_dataset3$word, '\r?\n')
counts5= count(tidy_dataset3,word)
result5=arrange(counts5,desc(n))
slice(result5,1:15)

#removing spacing and tabs
tidy_dataset3$word = str_replace_all(tidy_dataset4$word, '[ \t]', '')
tidy_dataset4=filter(tidy_dataset3, !(word=='') )
counts6= count(tidy_dataset4,word)
result6=arrange(counts6,desc(n))
slice(result6,1:15)

tidy_dataset5 = mutate_at(tidy_dataset4, "word", funs(wordStem((.), language="en")))

counts5 = count(tidy_dataset5, word)

arrange(counts5, desc(n)) %>%
  ungroup %>%
  slice(1:15)

install.packages('textdata')
library(textdata)
get_sentiments('nrc') %>%
  distinct(sentiment)

#JOY and SADNESS
nrc_joysad = get_sentiments('nrc') %>%
  filter(sentiment == 'joy' | 
           sentiment == 'sadness')
nrow(nrc_joysad)
newjoin2 = inner_join(tidy_dataset5, nrc_joysad)
counts8 = count(newjoin2, word, sentiment)
spread2 = spread(counts8, sentiment, n, fill = 0)
content_data = mutate(spread2, contentment = joy - sadness, linenumber = row_number())
tweet_joysad = arrange(content_data, desc(contentment))

#generating plot of top 20
(tweet_joysad2 = tweet_joysad %>%
    slice(1:20,107:127))
ggplot(tweet_joysad2, aes(x=linenumber, y=contentment, fill=word)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Index Value',
    y='Contentment'
  ) +
  theme(
    legend.position = 'bottom',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  geom_col()

#positive and negative
nrc_posneg = get_sentiments('nrc') %>%
  filter(sentiment == 'positive' | 
           sentiment == 'negative')
nrow(nrc_posneg)
(tweet_posneg = tidy_dataset5 %>%
  inner_join(nrc_posneg) %>%
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(positiveness = positive - negative, linenumber = row_number()) %>%
  arrange(desc(positiveness)) %>%
  slice(1:20,361:381))

#generating plot of top 20
ggplot(tweet_posneg, aes(x=linenumber, y=positiveness, fill=word)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Index Value',
    y='Positiveness'
  ) +
  theme(
    legend.position = 'bottom',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  geom_col()

#Trust and Fear
nrc_trstfear = get_sentiments('nrc') %>%
   filter(sentiment == 'trust' |
        sentiment == 'fear')
nrow(nrc_trstfear)
(tweet_trstfear = tidy_dataset5 %>%
  inner_join(nrc_trstfear) %>%
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(trustworthy = trust - fear, linenumber = row_number()) %>%
  arrange(desc(trustworthy)) %>%
  slice(1:20,318:338))
ggplot(tweet_trstfear, aes(x=linenumber, y=trustworthy, fill=word)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Index Value',
    y='Trustworthiness'
  ) +
  theme(
    legend.position = 'bottom',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  geom_col()

#surprise and anticipation
nrc_surprise_anticipation = get_sentiments('nrc') %>%
   filter(sentiment == 'surprise' |
        sentiment == 'anticipation')
nrow(nrc_surprise_anticipation)
(tweet_surprise_anticipation = tidy_dataset5 %>%
  inner_join(nrc_surprise_anticipation) %>%
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(supriseness = surprise - anticipation, linenumber = row_number()) %>%
  arrange(desc(supriseness)) %>%
  slice(1:20,318:338))
ggplot(tweet_surprise_anticipation, aes(x=linenumber, y=supriseness, fill=word)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Index Value',
    y='supriseness'
  ) +
  theme(
    legend.position = 'bottom',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  geom_col()


#Part of Speech Tagging
library(udpipe)
ud_model = udpipe_download_model(language = "english")
tidy_post1 = tidy_dataset5 %>% 
                select(word)
ud_model = udpipe_load_model(ud_model$file_model)
tagging_data = as.data.frame(udpipe_annotate(ud_model, x = tidy_post1$word))
#Frequency of tags
post_stats = txt_freq(tagging_data$upos)
post_stats$key = factor(post_stats$key, levels = rev(post_stats$key))
ggplot(post_stats, aes(x=key, y=as.factor(freq), fill=key)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Frequency',
    y='',
    title='UPOS (Universal Parts of Speech)'
  ) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    title = element_text(size = 13)
  ) +
  geom_col() +
  scale_fill_grey()
  #POST NOUN
  noun_stats = subset(tagging_data, upos %in% c("NOUN"))
  noun_stats2 = txt_freq(noun_stats$token)
  noun_stats2$key = factor(noun_stats2$key, levels = rev(noun_stats2$key))
  noun_stats2 %>%
  slice(1:20) %>%
  ggplot(aes(x=key, y=as.factor(freq), fill=freq)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Frequency',
    y='',
    title='Noun Occurrences'
  ) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    title = element_text(size = 13)
  ) +
  scale_fill_gradient(low="orange", high="orange3") +
  geom_col()

  #POST ADJECTIVE
  adjstats = subset(tagging_data, upos %in% c("ADJ"))
  adjstats2 = txt_freq(adjstats$token)
  adjstats2$key = factor(adjstats2$key, levels = rev(adjstats2$key))
  adjstats2 %>%
  slice(1:20) %>%
  ggplot(aes(x=key, y=as.factor(freq), fill=freq)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Frequency',
    y='',
    title='Adjective Occurrences'
  ) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    title = element_text(size = 13)
  ) +
  scale_fill_gradient(low="chartreuse", high="chartreuse3") +
  geom_col()

#POST VERBS
verbstats = subset(tagging_data, upos %in% c("VERB"))

verbstats2 = txt_freq(verbstats$token)

verbstats2$key = factor(verbstats2$key, levels = rev(verbstats2$key))

verbstats2 %>%
  slice(1:20) %>%
  ggplot(aes(x=key, y=as.factor(freq), fill=freq)) +
  coord_flip() +
  theme_light(base_size = 15) +
  labs(
    x='Frequency',
    y='',
    title='Verb Occurrences'
  ) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    title = element_text(size = 13)
  ) +
  scale_fill_gradient(low="tan", high="tan3") +
  geom_col()

install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)
install.packages("tm")
library(tm)

df=data.frame(counts5)
set.seed(1234)
wordcloud(words = df$word, freq = df$n, min.freq = 1,
max.words=200, random.order=FALSE, rot.per=0.35,
colors=brewer.pal(8, "Dark2"),scale=c(8,0.5))
set.seed(1234)
wordcloud2(data=df, size=1, color='random-dark')
