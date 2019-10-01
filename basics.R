#this is our environment, and specifically a comment

#take a look at this happy little expression
A<-1

#this should get us 1, right?
print(a)
#why didn't that work

#we can write some basic programs, here is a really simple one
if (A<5) {
  print("you are in luck")
  A<-A+1
  print(A)
} else {
  print("Bust")
}

#there are many basic functions in r
sin(2)
B<-c(0,1,2,3,4,5,6)
plot(sin(B))

C<-c(2,4,5)
mean(C)

#sometimes we want to use a library to call functions to make things easier for us
#one family of libraries is called the tidyverse
#we will call one library now that is really useful
library(dplyr)

#we will also want a fun dataset
library(nycflights13)

#what exactly is in that dataset
head(flights)

#what else is in that package, check the documentation...

#so let's start with something really straight forward, like all the flights in january
filter(flights, month==1)

#and the average delay in January? let's do this the hard way
january<-filter(flights, month==1)

#take a look at this expression
mean(january$dep_delay, na.rm=TRUE)
#$ is how we select within a dataset

#what if we want to know the average delay each month
#call the dataset
flights%>%
  #use the pipe to say that we want to group within that
  group_by(month)%>%
  #and now we want to see how long the average departure delay is in each group
  summarise(mean(dep_delay, na.rm=TRUE))

#so there are some missing values, so let's start by pulling all those values   
na.omit(flights)%>%
  summarize_all(mean)

#how do we figure out how many airlines flew to New York?
distinct(flights, carrier)

#we can also write a quick if_else
if_else(flights$arr_delay>20, "SO LATA", "ALRIGHT")

#what if we want more summaries
summarize(flights, median(dep_delay, na.rm=TRUE))
summarize(flights, IQR(dep_delay, na.rm=TRUE))

#lets say we want a tabular distribution 
arrange(flights, desc(dep_delay))

#what if we want to think about opposites
filter(flights, carrier != "UA")
#how many flights were NOT united?

#thinking about visual inference
plot(flights$month, flights$dep_delay)
#why all that gross overplotting?

#closer, but times are tricky
plot(flights$sched_dep_time, flights$dep_delay)

#density plotting
plot(density(flights$distance))

#bonus - MAPS!
plot(airports$lon, airports$lat)

