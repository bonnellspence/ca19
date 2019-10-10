#Let's start by getting back to discrete and continuous
#what did we learn about diamonds
head(diamonds)

#which are continuous and which are discrete?
#continuous: price, depth, carat,x/y/z, table 
#discrete: clarity, color, cut

#let's hit our cheat sheet

#onevar continuous
ggplot(mpg)+geom_qq(aes(sample = hwy))
ggplot(diamonds)+geom_qq(aes(sample=price))
ggplot(diamonds, aes(price))+geom_density(kernel = "gaussian")
ggplot(diamonds, aes(price))+geom_area(stat="bin")

#we dive into cocaine
ggplot(cocaine, aes(state, price))+ geom_violin(scale = "area")
#where to buy?
ggplot(cocaine, aes(price, potency, colour=state))+geom_jitter()
ggplot(cocaine, aes(state, potency, colour=price))+geom_jitter()
ggplot(cocaine, aes(potency, price, colour=state, size=weight))+geom_jitter()

#let's look at a few states
cocaine%>%
  filter(state=="CA" | state=="AZ" | state=="FL")%>%
  ggplot(aes(month, price, colour=potency))+geom_jitter()+facet_wrap(~state)


#continuous continuous
ggplot(diamonds, aes(carat, depth))+geom_smooth(method=lm)


#bonus - add really funky fun lables
ggplot(diamonds, aes(carat, depth, colour=price))+geom_text(aes(label=color))

#discrete continuous
ggplot(diamonds, aes(cut, price))+geom_violin()

#continuous bivariate - this is a great multi-dimensional chart
ggplot(diamonds, aes(carat, price))+geom_hex()

#bonus - with cut faceting
ggplot(diamonds, aes(carat, price))+geom_hex()+facet_wrap(~cut)

#some work with colors - discrete, continuous, 
ggplot(diamonds, aes(color, table, colour=price, size=carat))+geom_jitter()+ scale_fill_distiller(palette = "Blues")

#go big
ggplot(diamonds, aes(color, table, colour=price, size=carat))+geom_jitter()+scale_colour_gradientn(colours=rainbow(4))

#now onto joins
library(nycflights13)

#first step: boolean logic
#filters FOR X and NOT Y
filter(flights, dest=="LAX" & origin != "EWR")
filter(flights, dest=="LAX" & origin == "JFK")

filter(flights, dest=="LAX" & origin != "EWR")%>%
  filter(origin !="LGA")

#joins
#start by random sample
small_flights<-sample_n(flights, 500)
airlines

flights_2<-inner_join(small_flights, airlines)


inner_join(flights, airlines)
airports

#is this going to work
inner_join(flights, airports)
#caleb says where is the overlap?
filter(airports, faa=="OAK")

#make one with a KEY
places<-airports
#renaming allows us to have an artifical key
colnames(places)[1]<-"dest"
View(places)
D<-inner_join(flights, places)
ggplot(D, aes(lat, lon))+geom_point()

#groups and summaries
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)



