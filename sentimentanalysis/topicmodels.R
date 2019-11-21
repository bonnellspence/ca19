#calling mallet from tidy

#why Mallet? It is an old, but solid python library for topic modeling. 
#you will need to install it, if you are using cosine R it will run really smoothly
#you will notice that it really does just about one thing and not much more or less

#THIS PROGRAM IS DIFFERENT THAN THE BOOK - you get more control here and it will use our Kafka dataset

library(mallet)
#you know what this library does
library(dplyr)
#we used this some in class
library(tidytext)
#you also know what this is
library(ggplot2)

#first - we need to get some stopwords
tmp <- tempfile()
writeLines(stop_words$word, tmp)

#if you want to know what the stopwords are
stop_words$word
#think carefully about those words and what they are about...

#here is where we really start to do some interseting stuff
#this code calls for a mallet import of the Kafka data
#the first argument is the name of each document, which we can assume is the text
#the second argument is the text of the paragraph
#the third argument is calling for the stopwords file
docs <- mallet.import(Kafka$text, Kafka$text, tmp)

# create and run a topic model
#this will tell the model to make five topics, this is an arbitrary number, you can make as many topics as you have ram
topic_model <- MalletLDA(num.topics = 5)
#the model then loads the docs we specified above
topic_model$loadDocuments(docs)
#lets train the model 2000 times. 
topic_model$train(2000)

#cosine put some extra-cores in today so this ran wicked fast.

# tidy the word-topic combinations
td_beta <- tidy(topic_model)
td_beta

# Examine the five topics
td_beta %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta)) +
  geom_col() +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#at this point you can start looking for how the categories differ, what is in 3 that isn't in 4, for instance

#this produces a nice table with all the probabilites, super useful if you want to JOIN these back to other details
doc.topics<-mallet.doc.topics(topic_model, normalized = TRUE, smoothed = TRUE)

#these won't make sense to you, but that is ok
topic.words<-mallet.topic.words(topic_model, normalized = TRUE, smoothed = TRUE)

#and this is my personal absolute favorite - this is a dendrogram (like a family tree) of how the topics relate to each other
plot(mallet.topic.hclust(doc.topics, topic.words, balance = .4))

#these are the topic labels, which means the keys the process created to see the key contents
mallet.topic.labels(topic_model, topic.words, num.top.words = 3)

#so then you want to add your data to this, YOU ONLY NEED TO CHANGE ONE THING.
#switch Kafka$text and Kafka$text for the text column of your corpus. That's it and the rest of this code is BOILERPLATE.
docs <- mallet.import(Kafka$text, Kafka$text, tmp)