#Let's start from the beginning
library(dplyr)
library(ggplot2)
library(nycflights13)

#We started by loading some libraries, these give us lots of fun instructions to work with

#we can start by reviewing some key functions in our data manipulaiton toolbox
filter(flights, carrier=="UA")

#summaries
na.omit(flights)%>%
  summarize_all(median)

#counts
count(flights, carrier)

#and if we want to search WITHIN a result
#filter to just get flights on delta
filter(flights, carrier=="DL")%>%
  #and flights on delta longer than 2500 miles
  filter(distance>2500)%>%
  #how many distinct destinations were there
  distinct(dest)

flights%>%
  #use the pipe to say that we want to group within that
  group_by(carrier)%>%
  #and now we want to see how long the average departure delay is in each group
  summarise(mean(dep_delay, na.rm=TRUE))

#here is where it starts to get chippy
#we begin with the most basic plot possible
qplot(x = flights$dep_time, y = flights$dep_delay, colour = flights$carrier)

#qplot is a fast way to add some nice features
qplot(x = flights$dep_time, y = flights$dep_delay, shape = flights$carrier)

#we prolly want our plots to look really nice...
#a very fun little histogram
qplot(flights$carrier)
#the BINS are really easy on that histogram

#lets do when were the bins are a real problem
qplot(flights$sched_dep_time)
#notice, it only giave us 30 binswhat we can see here are clear luls

#we need more control here, this isn't enough

#one variable
ggplot(flights, aes(dep_time))+geom_dotplot()
#that is both gross and took too long, lets pipe ourselves a better opening

#ok feed 1000 random flights into a plot 
sample_n(flights, 1000, replace=TRUE)%>%
  ggplot(aes(dep_time))+geom_dotplot()

#just for ease, let's store a new table of just 10000 flights as a variable
little_flights<-sample_n(flights, 10000, replace=TRUE)

#now lets run all of our 
ggplot(little_flights, aes(dep_time))+geom_density(kernel="gaussian")  

#HERE IS HOW THE CHEATSHEET WORKS
#assign a FUNCTION to a variable
l_flight<-ggplot(little_flights, aes(dep_time))

#try invoking it
l_flight
#the result is an empty graph with that friendly axis to say: hey this actually worked

l_flight+geom_freqpoly()
#lets make a really narrow binned histogram
l_flight+geom_histogram(binwidth = 1)

#two ConCon
c_flight<-ggplot(flights, aes(distance, air_time))

c_flight+geom_jitter()

#and the central tendency
c_flight+geom_quantile()

#that was boring, basically a linear relationship, which like makes sense
c_flight+geom_smooth()

#the following code may be too intense for your computer
#notice that I am calling the original dataset in the GEOM
c_flight+geom_text(label=flights$carrier)
  
#or a nice little box and whisker plot
ggplot(flights, aes(month, arr_delay, colour=carrier))+geom_jitter()

#colors
ggplot(flights, aes(carrier))+geom_bar(aes(fill=carrier))

ggplot(data = flights)+
  geom_bar(mapping = aes(x = month, fill=carrier))

#facets
ggplot(flights, aes(distance, arr_delay, color=distance))+geom_jitter()+facet_wrap(~carrier)

ggplot(flights, aes(distance, arr_delay, color=carrier))+geom_jitter()+facet_wrap(~origin)

#labels
ggplot(flights, aes(distance, arr_delay, color=carrier))+geom_jitter()+facet_wrap(~origin)+ggtitle("WUT?")+theme(legend.position = "bottom")

#Let's Track Down a Mystery - where the worst delays caused by weather...







