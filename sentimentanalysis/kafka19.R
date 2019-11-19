library(rvest)
library(reshape2)
library(stringr)
library(dplyr)

kafka_home<-read_html("http://franzkafkastories.com/shortStories.php")

#get all the titles - in this case text a
kafka_titles<-kafka_home%>%
  html_nodes("a")%>%
  html_text

#get all attributes - in this case the values of a
kafka_links<-kafka_home%>%
  html_nodes("a")%>%
  html_attrs

kafka_links_vector<-melt(kafka_links)

#example URL: http://franzkafkastories.com/shortStories.php?story_id=kafka_abent_minded_window_gazing
#slug: http://franzkafkastories.com/shortStories.php?

A <- "read_html("
B <- "\'http://franzkafkastories.com/shortStories.php"
C <- kafka_links_vector$value
D <- "\')"

#create new reference vars to store the documents and pull all the special characters
Z<-str_replace_all(kafka_titles, " ", "")
Z<-str_replace_all(Z, "'", "")
Z<-str_replace_all(Z, "-", "")
Y<-"<-"

#store as an intermediate dataframe just in case and for checking 
kafka_read<-tibble(Z, Y, A, B, C, D)


#use paste to produce actionalbe code
kafka_scraper <-paste(Z,Y,A,B,C,D, sep = "")

#pull off the first three useless lines
writeLines(kafka_scraper)
eval(parse(text = kafka_scraper))

#we have the entire site in memory under var names Z

#how to write a quick interprerer
kafka_fulltext<-paste(Z,"_text<-", Z, "%>%html_nodes(\'p\')%>%html_text", sep="")
writeLines(kafka_fulltext)
eval(parse(text = kafka_fulltext))

#now you have a lot of short stories, let's check a few out
#oh, there is messyness

TheJudgment_text<-str_replace_all(TheJudgment_text, "\n", "")
TheJudgment_text<-str_replace_all(TheJudgment_text, "\t\t\t", "")





#we need a vector of dataframes
X <- paste(Z, "_text", sep="")


cleaner_code<-paste(X,"<-str_replace_all(", X, "," , "'\\n'", "," , "''", ")", sep = "")
writeLines(cleaner_code)
eval(parse(text = cleaner_code))

cleaner_code<-paste(X,"<-str_replace_all(", X, "," , "'\\t\\t\\t'", "," , "''", ")", sep = "")
writeLines(cleaner_code)
eval(parse(text = cleaner_code))



#now lets go ahead and pick some datasets

#here are ten
AbsentmindedWindowgazing_text
Rejection_text
Resolutions_text
OntheTram_text
Passersby_text
Clothes_text
BachelorsIllLuck_text
ReflectionsforGentlemenjockeys_text
TheWayHome_text
UnmaskingaConfidenceTrickster_text

#refactor key variables to make it easier
parser<-paste(Z,"<-",Z,"_text", sep = "")
writeLines(parser)
eval(parse(text=parser))

parser<-paste(Z,"<-","data.frame(",Z,")", sep = "") 
writeLines(parser)
eval(parse(text = parser))

writeLines(Z)

UnmaskingaConfidenceTrickster<-mutate(UnmaskingaConfidenceTrickster, story = "unmasking a confidence trickster")
Resolutions<-mutate(Resolutions, story = "resolutions")
BachelorsIllLuck<-mutate(BachelorsIllLuck, story = "bachelors ill luck")
AbsentmindedWindowgazing<-mutate(AbsentmindedWindowgazing, story = "AbsentmindedWindowgazing")
TheWayHome<-mutate(TheWayHome, story = "the way home")
Passersby<-mutate(Passersby, story = "passerby")
OntheTram<-mutate(OntheTram, story="on the tram")
Clothes<-mutate(Clothes, story="clothes")
Rejection<-mutate(Rejection, story="rejection")

parser<-paste(Z,"<-rename(",Z, ",", "text=", Z,")", sep = "")
writeLines(parser)
eval(parse(text = parser))

parser<-paste(Z,"<-","data.frame(",Z,")", sep = "") 
writeLines(parser)
eval(parse(text = parser))

parser<-paste(Z, "<- ",Z, "%>% mutate(paragraph = row_number())", sep="")
writeLines(parser)
eval(parse(text = parser))

parser<-paste(Z,"<-","data.frame(",Z,")", sep = "") 
writeLines(parser)
eval(parse(text = parser))

#some of the lines didn't work, and that is cool
X<-bind_rows(
  UnmaskingaConfidenceTrickster,
  Resolutions,
  BachelorsIllLuck,
  AbsentmindedWindowgazing,
  TheWayHome,
  Passersby,
  OntheTram,
  Clothes,
  Rejection
)

View(X)

#now a complete corpus
Kafka<-filter(X, text != "Back to Franz Kafka stories index.")
write.csv(Kafka, "Kafka.csv")
