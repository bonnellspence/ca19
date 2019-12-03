#see what kind of account you have
getOption("ggmap")

#google will return all sorts of fun things for you
a<-geocode("Univeristy of Hawaii")

#we can also get data for a number of strings...
B<-c("University of Minnesota", "University of Wisconsin,", "University of Texas")
geocode(B)

#basic stamen example
us <- c(left = -125, bottom = 25.75, right = -67, top = 49)
get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap() 

#we can get a map this way as well...
get_googlemap("university of minnesota", zoom = 12) %>% ggmap()

get_googlemap("corvallis, oregon", zoom = 12, maptype = "satellite") %>% ggmap()
get_googlemap("corvallis, oregon", zoom = 12, maptype = "terrain") %>% ggmap()
get_googlemap("corvallis, oregon", zoom = 12, maptype = "roadmap") %>% ggmap()
get_googlemap("corvallis, oregon", zoom = 12, maptype = "hybrid") %>% ggmap()

#and crime in Corvallis via Zac
#import corvallis noise and let's start cleaning
library(lubridate)
#parsed times
bonkers<-mdy_hms(corvallis_noise$DATE)
hour<-hour(bonkers)
month_day<-day(bonkers)
month<-month(bonkers)
year<-year(bonkers)
bh<-data.frame(hour, month_day, month, year)
noise<-bind_cols(corvallis_noise, bh)
View(noise)

#NOW we must be frugal
noiseA<-filter(noise, year > 2017)%>%
  filter(month > 8)

#make a dataframe with the street_addresses
df <- data.frame(
  street_address = noiseA$ADDRESS,
  stringsAsFactors = FALSE
)

#mutate_geocode to put that list into a new dataframe
locations<-df %>% mutate_geocode(street_address)

colnames(noiseA)[4]<-"ADDRESS"
colnames(locations)[1]<-"ADDRESS"
#inner_join that to the original
noise2 <- inner_join(noiseA, locations, by = "ADDRESS")

#take a quick look
View(noise2)

#get a road map of town
corvallis<-get_googlemap("corvallis, oregon", zoom = 14, maptype = "road")
corvallis2<-ggmap(corvallis)

#and the fun begins...
corvallis2 +
  geom_jitter(aes(x = lon, y = lat, colour = type), data = noise2)

#and the police really helped us here with formatting
corvallis2 +
  geom_jitter(aes(x = lon, y = lat, colour = NATURE.CODE), data = noise2)+facet_wrap(. ~DOW)

#from the Houston crimes example
corvallis2 +
  stat_bin2d(
    aes(x = lon, y = lat, colour = NATURE.CODE, fill = NATURE.CODE),
    size = .5, bins = 30, alpha = 1/2,
    data = noise2
  )

