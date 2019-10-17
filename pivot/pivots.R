#so we can make some graphics
library(ggplot2)
#so we can clean stuff up
library(tidyr)
#for the filters, also Lulz
library(dplyr)

#in this file we will be using a few datasets that are in R from the old gather and spread days. We will also be using a few new datasets: feetball, Zords, and politics

#LONG TO WIDE
#let's start with the politcs example
resultuptodate

#for those of you playing long at home: index is mean sentiment by paragraph, var is the SD of the mentions, volume is paragrpahs that week, date and name are pretty straight forward
ggplot(resultuptodate, aes(date, index, size=volume, colour=name))+geom_jitter()

#that is pretty fun right? You can see some trends in the race and some outliers that need explanation


#but the data is very hard to read
resultuptodate%>%
  pivot_wider(
  names_from = date,
  #these become a UNIQUE KEY COMBINATION NAME AND DATE
  values_from = -c(date, name),
  )

#what if we just want one?
#so we can move things around more easily in the joining we will assign this to a new variable
P<-resultuptodate%>%
  pivot_wider(
    names_from = date,
    #these become a UNIQUE KEY COMBINATION NAME AND DATE
    values_from = -c(date, name),
  )

#so now the long form of our data is called P
#lets put all the columns starting with index in a separate table
B<-select(P, starts_with("index"))
#but we didn't get the names, so we need those two
C<-select(P, 'name')
#lets put it back together - notice C comes before B in this example
D<-bind_cols(C, B)
View(D)
#now you have a nice little table that can tell you about the election

  
#NOW WE GO FROM WIDE TO LONG
#we have a happy little dataset called feetball. It's about the NFL
head(feetball)
#take a good look: there is a first pargraph that tells you what we are looking at then many cases

#pipe in feetball
feetball%>%
  #call the function
  pivot_longer(
    #tell the function we want to pivot everything OTHER than measures
    cols = -Measures
  ) 

#for future use, let's go ahead and save this to a new variable
I<-feetball%>%
  pivot_longer(
    cols = -Measures
  ) 

#let's get longer with an old gather spread example
table4a

table4a%>%
  #we want to select everthing that IS NOT the country
  pivot_longer(cols=-country)

#and we can fix it
I%>%
  pivot_wider(
    #what do we want the columns to be?
    names_from = name,
    values_from = value
  )

#another really fun one
#import the zords datatable

#what do you notice?
#a. missing values, b. its all discrete
#how many rangers have all zord types?

#we likely need to make this long to make it useful

#call the dataset
zords%>%
  #the reference column is ranger and we want to make EVERY other column longer, so we do -
  pivot_longer(cols=-Ranger)

#and we can plot this
Z<-zords%>%
  #move down every column except Ranger
  pivot_longer(cols=-Ranger,
              #make the new values column Zord
               values_to = "Zord",
              #and those names to
              names_to = "Era",
              #kick all those NA values to the curb
               values_drop_na=TRUE)

head(Z)
#Z is now really useful

#lets make a fun plot
ggplot(data = Z) +
  #geom_count is the only discreet, discreet method we have on hand. 
  geom_count(mapping = aes(x = Era, y = Ranger, colour=Ranger))

#that was just mean - none of the colors synch up. 

#here, this might be more plesant
ggplot(Z, aes(Ranger, colour=Ranger))+geom_bar()

#or to write it the other way...
ggplot(Z)+geom_bar(aes(Ranger, colour=Ranger, fill=Era))

#What if we wanted a huge table of NA values where we just see which ranger was in an era?

#so let's revesre the transform!
Z%>%
  pivot_wider(
    #the columns will be Era
    names_from=Era,
    #the contents will be the Zord names
    values_from=Zord
  )

#Or if you want to see some options, take each of the names and try it and an inverse - of it
#it is worth your time


#and now we can widen feetball

I%>%
  pivot_wider(
    names_from = name,
    values_from = value
  )


#the Billboard example - from the documentation
View(billboard)

blong<-billboard %>%
  pivot_longer(
    #if you have repetitive columns you can grab everything that STARTS WITH
    cols = starts_with("wk"),
    #we can direct this to a really useful column like Era or Zord
    names_to = "week",
    names_prefix = "wk",
    #notice that we didn't use an inverse here as the columns we wanted had a common key
    values_to = "rank",
    #once you are out of the hot 100 we don't care anymore
    values_drop_na = TRUE
  )

#lets have some fun with the plotting here - fo ease, let's focus on the top of the charts
blong%>%
  filter(rank==1)%>%
  ggplot(aes(date.entered, as.integer(week), colour=artist))+geom_jitter()

head(blong)

#who had the most hot 100 hits?
blong%>%
  filter(rank==1)%>%
  group_by(artist)%>%
  count(artist)

#a few things to know...
#pivot_longer has:
#A. names_prefix to delete extraneous stuff, B. sep and pattern to look at combined headings like those created by wider, _repair to deal with damaged names
#pivot_wider:
#relies on unique identifiers (which can be multiple columns) as id_cols
#names and values are a PAIR that you use to create the top and the values
#you can add values using _fill or _fn to do a function to that which you are moving

#more on these advanced functions later in the quarter. 

