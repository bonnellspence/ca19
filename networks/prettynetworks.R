library(stringr)
library(rvest)
library(dplyr)
library(tidyr)
library(ggnetwork)
library(ggthemes)
library(network)
library(sna)


#RUN LINES 7:65 to get all the data you need
#get the data
feetball<-"https://www.sports-reference.com/cfb/years/2019-schedule.html"

#parse the football data
foot<-feetball%>%
  read_html()%>%
  #this is the key - this is actually stored as a true table so we can easily take the xpath
  html_nodes(xpath='//*[@id="schedule"]')%>%
  html_table(header = TRUE)

#make the data tidy
foot<-as_tibble(data.frame(foot))
View(foot)

foot<-filter(foot, Pts>0)
#structure data 
football_data<-rename(foot, "SOURCE" = "Loser")%>%
  rename("TARGET" = "Winner")%>%
  select(SOURCE, TARGET)

#now cleaning
A<-str_replace_all(football_data$SOURCE, "\\(", "")
B<-str_replace_all(A, "\\)", "")
C<-str_replace_all(B, "[:digit:]", "")
D<-str_trim(C, side="left")

G<-str_replace_all(football_data$TARGET, "\\(", "")
H<-str_replace_all(G, "\\)", "")
I<-str_replace_all(H, "[:digit:]", "")
J<-str_trim(I, side="left")

#combine and rename
clean_football<-as_tibble(data.frame(D, J))
clean_football<-rename(clean_football, "SOURCE" = "D")%>%
  rename("TARGET" = "J")

View(clean_football)
#grab all but the rows that include "loser"

clean_football<-filter(clean_football, SOURCE != "Loser")

#now export for gephi
#it is really importat to not write the row names, it really confuses everything in this world
write.csv(clean_football, "clean_football.csv", row.names = FALSE)

#extract a clean version of football_data by removing all rows where the "winner" is winner
foot2<-filter(foot, Winner != "Winner")
#you could easily export this now

#get the conference data
conferences<-read_html("https://en.wikipedia.org/wiki/List_of_NCAA_Division_I_FBS_football_programs")

conf<-conferences%>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div/table[1]')%>%
  html_table(header = TRUE)

conf_data<-data.frame(conf)
conf_data<-rename(conf_data, vertex.names = Team)

#there are really two datasets you will want here
#data about the football teams
View(conf_data)

#the full datatable - like an edge list
View(foot2)

#the network itself
View(clean_football)

#test to see if these lists can be applied to each other
dim(clean_football)[1]==dim(foot2)[1]

#its true, meaning that you can use the metadata from foot2 to augment your network

#create the football network
footy<-network(clean_football, directed = TRUE, matrix.type="edgelist")

#let's add some key features from the list of cool or intersting data
set.vertex.attribute(footy, "conference", conf_data$CurrentConference)
set.vertex.attribute(footy, "state", conf_data$State.1.)

#and a few centralities - here are a few
footy %v% "kcores" <- kcores(footy)
footy %v% "evcent" <- evcent(footy)
footy %v% "degree" <- evcent(footy)
footy %v% "prestige" <- prestige(footy, cmode="eigenvector", rescale=TRUE)

#get a count of which edge is which
e <- network.edgecount(footy)
#set an edge based on the conference nickname
set.edge.attribute(footy, attrname = "type", conf_data$Nickname, )

#Let's get a vector of scores
balance<-data.frame("team"=as.character(clean_football$TARGET), "score"=as.numeric(foot2$Pts)-as.numeric(foot2$Pts.1), stringsAsFactors = FALSE)
#create weights for the edges based on the values of the network
set.edge.attribute(footy, attrname = "score", balance$score, )
#and all the game
set.edge.attribute(footy, attrname = "date", foot2$Date, )

#the absolute basic plot
ggplot(footy, aes(x, y, xend = xend, yend = yend)) +
  #just make the lines orange
  geom_edges(color = "orange") +
  #set the other aesthetics as static values
  geom_nodes(size = 3, shape = 21, color = "steelblue", fill = "white") +
  theme_blank()

#this is quite intentionally a mess
ggplot(footy, aes(x = x, y = y, xend = xend, yend = yend)) +
  #date is discrete, so is color, curvature can't be controlled as an aes
  #aes generally means color, shape, size
  #linetype can only handle a small discrete
  geom_edges(aes(linetype = date, color = type), curvature = .2) +
  #controlling color and state
  geom_nodes(aes(color = between, size = prestige))+
  #and lets add the same aesthetics to the text
  geom_nodetext(aes(color = between, size=prestige, label = vertex.names), fontface = "bold") +
  #and throw a ggtheme on it two, because why not?
  theme_excel()



#now this will be much brettier
#we need to control the layouts, these are all controlled through the SNA package
#this is really about calling the specifications within that code
#niter is how many times it runs, default is 1k, 10 saves memory
target<-ggnetwork(footy, layout = "target", niter = 10)

#a more simplistic plot
ggplot(target, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = score, color = conference), curvature = .2) +
  geom_nodes(aes(size = degree))+
  geom_nodetext(aes(size=evcent, label = vertex.names), fontface = "bold") +
  theme_blank()

#you might also want a force directed diagram
blaster<-ggnetwork(footy, layout = "fruchtermanreingold", niter = 1000, cell.jitter=5)
#this is force directed so you really start to get the sense of the structure of the data and the system
ggplot(blaster, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(linetype = conference, color = score), curvature = .2) +
  geom_nodes(aes(size = degree))+
  geom_nodetext(aes(size=evcent, label = vertex.names), fontface = "bold") +
  theme_blank()

#here is the name of the game
??sna::gplot.layout

#look in there, find the documentation and you can see how to pass all kinds 
#of information through layout = 




#WARNING WARNING WARNING - THIS WILL MAKE YOUR LIFE WORSE.
#THIS WILL CAUSE CHAOS! DON'T RUN THESE LINES THEY BRING ONLY WOE
#but also amazing power. 
library(modMax)
smasher<-as.matrix.network.adjacency(footy)
#calculate modularities - detect groups and clumps and stuff
#a fav that comes up null
louvain(smasher, numRandom=0, initial=c("general","own"))
#here is another, lets do it
fuel<-greedy(smasher, refine = "fast")
#check the structure of the result
hoobastank<-data.frame(fuel, stringsAsFactors = FALSE)
View(hoobastank)
write.csv(hoobastank, "coldplay.csv", row.names = FALSE)
detach("package:modMax", unload = TRUE)
#NOW RESTART R. 
#if we rewrote this code to use the :: call, it would work with less pain

#where ever you wrote coldplay, import it now
confirm<-data.frame("vertex.names"=get.vertex.attribute(footy, "vertex.names"), 
                    "modularity"=coldplay$community.structure,
                    "wins"=degree(footy, cmode="indegree"), 
                    "losses"=degree(footy, cmode="outdegree"),
                    "eigen"=evcent(footy),
                    "prestige"=prestige(footy, cmode="eigenvector", rescale=TRUE),
                    "between"=betweenness(footy)
)

View(confirm)

#let's see what is sees
confirm%>%
  filter(modularity==3)
#and you can see the SEC and some patsy teams

#add vertext attributes that are the detected modularities
set.vertex.attribute(footy, attrname = "detect_mod", confirm$modularity)
list.vertex.attributes(footy)
get.vertex.attribute(footy, "detect_mod")

#just about as good as the Europeans can make
blackeyedpeas<-ggnetwork(footy, layout = "kamadakawai", niter = 1000)

#close to the european
ggplot(blackeyedpeas, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(aes(alpha=.1),curvature = .2) +
  geom_nodes(aes(size = prestige, color=as.character(detect_mod)))+
  geom_nodetext_repel(aes(size=(prestige*2), label = vertex.names), fontface = "bold") +
  theme_blank()+
  theme(legend.position = "none")

#Now that you have access to such a powerful dataset, you could compare the detected conferences
#to the actual ones and really jazzercise your brain.

#and now for some cor fun
cor.test(confirm$wins, confirm$prestige)
library(ggrepel)
ggplot(confirm, aes(wins, prestige, colour=between))+
  geom_text_repel(label=confirm$team)

#if you want to try the European method
write.csv(clean_football, "footballnet.csv", row.names = FALSE)
  
#And the game...
nickelback<-inner_join(conf_data, confirm)
View(nickelback)
nickelback%>%
  filter(CurrentConference=="SEC")

nickelback%>%
  filter(CurrentConference=="Pac-12")

#USC was not named the same between the datasets
#here is the list of datapoints where the names were inconsistent
anti_join(conf_data, confirm)
