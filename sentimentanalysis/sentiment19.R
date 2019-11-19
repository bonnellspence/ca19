library(tidytext)
library(textfeatures)
library(ggplot2)
library(dplyr)
library(magrittr)
library(tidyr)
library(SentimentAnalysis)
library(wordcloud)
library(reshape2)

#basically this tutorial is Robinson and Slige but with Kafka. Run our kafka tut to get the stories

#I am going to skip to the chase here. The TidyText book is pretty great. Occassionally there is an "n" where there needs to be an "nn"
#At the same time, it is a textbook with a really clean, stellar example - Jane Austen
#I tend to use Kafka because there is a lot of subelty there. 
#This is espeically important for teaching sentiment analysis. 

Kafka_words<-Kafka%>%
  unnest_tokens(word, text)%>%
  count(story, word, sort = TRUE)%>%
  ungroup()

total_words <- Kafka_words %>% 
  group_by(story) %>% 
  summarize(total = sum(n))

story_words <- left_join(Kafka_words, total_words)

tbl_df(story_words)

#a quick faceted histogram of counts/total
ggplot(story_words, aes(n/total, fill = story)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~story, ncol = 2, scales = "free_y")

#switching from raw to rank (similar to the pearsons/spearmans choice)
freq_by_rank <- story_words %>% 
  group_by(story) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

#this is a really clear and nice use of log scale
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = story)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#we did a lot with terms and letters last time (and likely some this time too)
#now really new stuff - setniments

#Let's get a sentiment dictionary 
#joy
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")


story_words %>%
  filter(story == "clothes") %>%
  inner_join(nrc_joy, by = "word") %>%
  count(word, sort = TRUE)

#two joy words - this is going to be a problem

#what sentiments are there?
??get_sentiments()

#word, score as integer
get_sentiments("afinn")

#word, and a descriptor; "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", or "trust", 
get_sentiments("nrc")

get_sentiments("nrc") %>% 
  count(sentiment)

#positive negative
get_sentiments("bing")

get_sentiments("bing") %>% 
  count(sentiment)

#once again positive negative (financial lexicon), also "litigious", "uncertainty", "constraining", and "superfluous".
get_sentiments("loughran")

get_sentiments("loughran") %>% 
  count(sentiment)

#applied to kafka
bing_word_counts <- story_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment)%>%
  top_n(10)%>%
  ungroup()%>%
  mutate(word = reorder(word, nn))%>%
  ggplot(aes(word, nn, fill = sentiment)) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#skipped method custom stop words

#wordclouds
story_words %>%
  count(word)%>%
  with(wordcloud(word, nn, random.order = TRUE, ordered.colors = TRUE))


#more control of the cloud
set.seed(1234)
wordcloud(words = story_words$word, freq = story_words$n, min.freq = 1,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#another cloud

story_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "nn", fill = 0) %>%
  comparison.cloud(colors = c("pink", "limegreen "),
                   max.words = 500)

#this is not from the book, this is pure Dan.
Kafka3<-Kafka%>%
  unnest_tokens(sentence, text, token = "sentences")%>%
  group_by(story)%>%
  mutate(sent_number = row_number())%>%
  unnest_tokens(word, sentence)%>%
  inner_join(get_sentiments("afinn"))%>%
  inner_join(get_sentiments("bing"))%>%
  ggplot(aes(sent_number, score, colour= sentiment))+
  geom_jitter()+facet_grid(story ~.)

Kafka3

#MOAR! said the interface
Kafka4<-Kafka%>%
  unnest_tokens(sentence, text, token = "sentences")%>%
  group_by(story)%>%
  mutate(sent_number = row_number())%>%
  unnest_tokens(word, sentence)%>%
  inner_join(get_sentiments("afinn"))%>%
  inner_join(get_sentiments("bing"))%>%
  group_by(sent_number)%>%
  mutate(word_number = row_number())%>%
  ungroup()%>%
  filter(story != "unmasking a confidence trickster")%>%
  filter(story != "on the tram")%>%
  ggplot(aes(word_number, score, colour= sentiment))+
  geom_jitter()+
  facet_grid(story ~ sent_number)

Kafka4

#the overlap in words is not huge

View(Kafka)
filter(Kafka, story == "AbsentmindedWindowgazing")
#that is an interesting story at least

#STOP STOP STOP - idea shift, now we do tf-idf

#this will look really familiar - this is the basic unnest and join function
#honestly, we should just write a function for this, but we do need to adjust things

kafka_words <- Kafka %>%
  unnest_tokens(word, text) %>%
  count(story, word, sort = TRUE) %>%
  ungroup()

total_words <- kafka_words %>% 
  group_by(story) %>% 
  summarize(total = sum(n))

kafka_words <- left_join(kafka_words, total_words)

kafka_words

#here is your standard corpus plot (also very familar at this point)
ggplot(kafka_words, aes(n/total, fill = story)) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~story, ncol = 2, scales = "free_y")

#Zipf's law - see Robinson and Slidge chapter 3.2

freq_by_rank <- kafka_words %>% 
  group_by(story) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

#well, there is an insight, Kafka says "the" a lot

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = story)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#basically, really common words don't do much, at least if you follow this line of reasoning
#there are folks who think that stop words matter alot
#also Kafka is really hard to think about this way

#here is one of the hotter games in town - tf_idf

kafka_words <- kafka_words %>%
  bind_tf_idf(word, story, n)
kafka_words

kafka_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#visualized 
kafka_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(story) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = story)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~story, ncol = 3, scales = "free") +
  coord_flip()

#let's drill down into this a little
filter(kafka_words, story == "rejection")%>%
  arrange(desc(tf_idf))

#n-grams can also be extracted
kafka_ngram <- Kafka %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
kafka_ngram

kafka_ngram %>%
  count(bigram, sort = TRUE)

#taking this wonderful cleaner code from the book

bigrams_separated <- kafka_ngram %>%
  #breaks apart the columns
  separate(bigram, c("word1", "word2"), sep = " ")

#cleans the stopwords 
bigrams_filtered <- bigrams_separated %>%
  #pull the stopwords from column 1
  filter(!word1 %in% stop_words$word) %>%
  #pull the stopwords from column 2
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

#here is the problem - once we pull the stop words, there is basically nothing left of our corpus
bigram_counts

#WAIT STOP THINK! the language in Jane Austen uses a lot of negation 
#Kafka doesn't. You should read their book.
#here is the proof

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)
#not even might be important, but basically other than that...

#let's check Kafka for trigrams
Kafka %>%
  #oh boy, this looks like a dang near refactored function 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

#it's a dry well.

bigram_tf_idf <- kafka_ngram %>%
  count(story, bigram) %>%
  bind_tf_idf(bigram, story, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

#let's try their fun bigram network
library(igraph)
bigram_counts
bigram_graph <- bigram_counts %>%
  #don't filter, it just isn't needed
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#the other layout
set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


#chapter 4.2.1 is about chunking, we pre-chunked into pargraphs and even sentences

#TidyText is nice, running topic models from tidytext is also very nice