#so we can make some graphics
library(ggplot2)
#so we can clean stuff up
library(tidyr)
#for the filters, also Lulz
library(dplyr)

#in this file we will be using a few datasets that are in R from the old gather and spread days. We will also be using a few new datasets: feetball, Zords, and politics

#LONG TO WIDE

#let's start with the politcs example
ggplot(resultuptodate, aes(date, index, size=volume, colour=name))+geom_jitter()
resultuptodate

#but the data is very hard to read
resultuptodate%>%
  pivot_wider(
  names_from = name,
  #these become a UNIQUE KEY COMBINATION NAME AND DATE
  values_from = -c(date, name),
  )

#the classic 
#what is table 1?
table1

table1%>%
  pivot_wider(
    names_from = year,
    #KEY COMBINATION
    values_from = -c(country, year),
  )

  
#NOW WE GO FROM WIDE TO LONG

I<-feetball%>%
  pivot_longer(
    cols = -Measures
  ) 
View(I)

#let's get longer
table4a

table4a%>%
  #we want to select everthing that IS NOT the country
  pivot_longer(cols=-country)

#and we can fix it
I%>%
  pivot_wider(
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

ggplot(data = Z) +
  #geom_count is the only discreet, discreet method we have on hand. 
  geom_count(mapping = aes(x = Era, y = Ranger, colour=Ranger))

#that was just mean

#here, this might be more plesant
ggplot(Z, aes(Ranger, colour=Ranger))+geom_bar()

#or to write it the other way...
ggplot(Z)+geom_bar(aes(Ranger, colour=Ranger, fill=Era))

#What if we wanted a huge table of NA values where we just see which ranger was in an era?
Z%>%
  pivot_wider(
    names_from=Era,
    values_from=Ranger
  )

#this is going to get really trippy - lets use negations and all of our vars
#we can get names from OR NOT FROM 
Z%>%
  pivot_wider(
    #Ranger, Era, Zord
    names_from=Ranger,
    values_from=Era
  )

#Lets put the pieces together here inductively.
#run all the options and the NULLS

#and to get back to ZORDS
Z%>%
  pivot_wider(
    names_from=name,
    values_from=Ranger
  )
#but what if Brenden wants feetball back as a WIDE

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
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )

#lets have some fun with the plotting here
blong%>%
  filter(rank==1)%>%
  ggplot(aes(date.entered, as.integer(week), colour=artist))+geom_jitter()

View(blong)

blong%>%
  filter(rank==1)%>%
  group_by(artist)%>%
  count(artist)

zeek<-blong%>%
  filter(rank==1)%>%
  ggplot(aes(date.entered, as.integer(week), colour=artist))+geom_jitter()

library(ggthemes)
zeek+theme_excel()+scale_fill_excel()
zeek+scale_colour_fivethirtyeight()+theme_fivethirtyeight()
zeek+theme_economist()+scale_fill_economist()

#lets explore the themes

show_col(excel_pal()(7))



