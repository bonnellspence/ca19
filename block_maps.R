#lets do some simple chloropleth maps

#we need to start with some mapdata - you will need to install these
library(maps)
library(mapdata)
library(ggmap)

#how do these maps work...
states <- map_data("state")
head(states)
#basically, its a list of points that would draw a state border

#before we modify the data, let's try a few quick maps to get a hang of how the code works
ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") + 
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend

#look at the aesthetic binidings: as long as you have values that can inner_join, you are in flavor country

#and you can get subregions pretty easily
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))

#graph the west_coast
ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)

#here is my favorite example, I wrote it during some wishful thinking once...
elector <- inner_join(states, elect4, by = "region")
tbl_df(elector)

#now, lets try to sub-in our new data
ggplot(data = elector) + 
  geom_polygon(aes(x = long, y = lat, fill = Status, group = group), color = "white") + 
  coord_fixed(1.3)

#install this next package if you didn't earlier this quarter...
library(ggthemes)
ggplot(elector, aes(x=long, y=lat, group=group, fill=as.factor(Value)))+ 
  scale_fill_brewer()+
  geom_polygon()+coord_map()+
  labs(fill="type of connection between voters and electors", title="electoral college, 2016",x="",y="")

#pretty clear how the system works. Just join your data to your regions



