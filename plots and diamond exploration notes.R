#in these notes we talk though G/W on ggplot and add a few things 

#notice how they wrote the code a little differently here
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

#how do we get situational awareness of the dataset?
head(diamonds)
dim(diamonds)

#start thinking about binning - always think about bins
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

#Here is when hadley made the data smaller, by restricting all of that massive diamond data
smaller <- diamonds %>% 
  #less than three carats
  #remember, a carat is a weird measurement of part of a rasin
  filter(carat < 3)

#multiple frequencies
ggplot(data = smaller, mapping = aes(x = carat, colour = cut)) +
  #really restricting the binwidth
  geom_freqpoly(binwidth = .1)

#now we are going to work with categories and continuous
ggplot(data = diamonds, mapping = aes(x = price)) + 
  #the bins for price are in 500s
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)

#lets look at the cor
cor.test(diamonds$price, diamonds$carat)

#first the black and white plot
ggplot(diamonds, aes(price, carat))+geom_jitter()+facet_wrap(~cut)

#ADD the clarity
ggplot(diamonds, aes(price, carat, colour=clarity))+geom_jitter()+facet_wrap(~cut)

#Angel's fresh graph
ggplot(diamonds, aes(depth, price, colour=clarity))+geom_jitter()+facet_wrap(~cut)

#ok depth to price
cor.test(diamonds$depth, diamonds$price)

#box and whisker
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()+facet_wrap(~clarity)

#what if you want to know about two categoricals?
F<-diamonds %>% 
  count(color, cut)
View(F)

#And the plot ends up being real griddy, but that geom_count can offer us measure
ggplot(data = diamonds) +
  #geom_count is the only discreet, discreet method we have on hand. 
  geom_count(mapping = aes(x = cut, y = color))
