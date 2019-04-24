#Compiled for R text mining with tidyverse workshop in Rīgas Tehniskā universitāte, April 2019, by Peeter Tinits

# Workshop goals.
# 1) Practice tidyverse
# 2) Learn basic text mining tools







#This command installs the libraries needed to run the code, if you don't have them.
lapply(c("tidyverse", "tidytext", "gutenbergr", "scales","zoo"),
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

#Libraries need to be opened each time you open R. These commands open the libraries/packages in the current environment.
library(tidyverse)
library(tidytext)
library(scales)
library(zoo)


#### 1) Let's practice!









#Basic tidytext processing commands
#
#basic model is the following
#data %>%
#process()
#
# %>% - carry the data into function
# filter - take subset of the data
# str_detect - find part of string
#
#anti_join - remove the matching rows
#stop_words - dataset of stopwords  
#rename - rename column for practical reasons
#inner_join - keep only the matching items
#mutate - create new variable
#ungroup - ungroup
#get_sentiments - vocabulary sentimetns
#spread - make one column into two
#new functions
#anti_join(with_what, „var“) – remove matching values
#inner_join(with_what, „var“) – keep only matching values




library(gutenbergr)
#The library "gutenbergr" gave us some data to work with, for example "gutenberg_metadata"
#To look at a variable we just type it in
View(gutenberg_metadata)


## First task just to remind you is - get acquinted with the data
# find out what authors are there, what texts, what other information is included








gutenberg_metadata %>%
  filter(str_detect(author,"Wells, H. G."))

gutenberg_metadata %>%
  filter(str_detect(author,"Austen"))

gutenberg_metadata %>%
  filter(str_detect(title,"Time Machine"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(title,"Time Machine"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(language,"en"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(language,"de"))

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(language,"de")) %>%
  filter(str_detect(author,"Shakespeare"))



#To download texts
#1) build index
gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(author,"Wells, H. G.")) %>%
  filter(str_detect(language,"en")) -> hgwells_index

gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(author,"Verne, Jules")) %>%
  filter(str_detect(language,"en")) -> jverne_index

#2) downlaod the texts
#hgwells_texts <- gutenberg_download(hgwells_index$gutenberg_id[1:15], meta_fields = "title")
#jverne_texts <- gutenberg_download(jverne_index$gutenberg_id[1:15], meta_fields = "title")

#lazy option with no internet
load("data/wells_verne.RData")

## Let's start looking at the texts













#count (what does this count?)
hgwells_texts %>%
  count(title)

jverne_texts %>%
  count(title)

#unnest_tokens - make text into tokens
hgwells_texts %>%
  unnest_tokens(word, text) %>%
  count(title)

#group_by - group by the item (fur future operations)
#count - by group, count the words
hgwells_texts %>%
  unnest_tokens(word, text) %>%
  group_by(title) %>%
  count(word, sort = TRUE) #can also be done with count(title, word, sort=TRUE)



# Stopwords again.
data("stop_words")
hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE)

#let's make two groups
comparison1 <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  filter(title=="The Time Machine") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

comparison2 <- jverne_texts %>%
  unnest_tokens(word, text) %>%
  filter(title=="A Journey into the Interior of the Earth") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)


comparison <- comparison1 %>%
  rename(comparison1 = n) %>%
  inner_join(comparison2,"word") %>%
  rename(comparison2 = n) %>%
  mutate(comparison1 = comparison1 / sum(comparison1),
         comparison2 = comparison2 / sum(comparison2))

#this is just to plot the comparison
ggplot(comparison, aes(comparison1, comparison2)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")


#we can do the same thing with more texts
hgwells_texts %>%
  count(title)
comparison1 <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

jverne_texts %>%
  count(title)
comparison2 <- jverne_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

comparison <- comparison1 %>%
  rename(comparison1 = n) %>%
  inner_join(comparison2,"word") %>%
  rename(comparison2 = n) %>%
  mutate(comparison1 = comparison1 / sum(comparison1),
         comparison2 = comparison2 / sum(comparison2))

#this is just to plot the comparison
ggplot(comparison, aes(comparison1, comparison2)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")











## search for particular words
jverne_texts %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word,"god")) %>%
  count(title)


hgwells_texts %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word,"god")) %>%
  count(title)






#Add grouping variables:
#Location within books

jverne_texts <- jverne_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()
jverne_texts

hgwells_texts <- hgwells_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup()

jverne_texts%>%
  group_by(title)%>%
  ggplot(aes(x=title, y=max(linenumber)))+
  geom_bar(stat="identity")
  

hgwells_texts %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word,"god")) %>%
  ggplot(aes(x=linenumber,y=title))+
  geom_point(shape=124)


jverne_texts%>%
  group_by(title)%>%
  ggplot(aes(x=title, y=max(linenumber)))+
  geom_bar(stat="identity")















### Sentiment analysis with locations
##
## A list of words, as last time

hgwellssentiment <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(title, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(hgwellssentiment, aes(index, sentiment, fill = title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x")

jvernesentiment <- jverne_texts %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(title, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(jvernesentiment, aes(index, sentiment, fill = title)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, scales = "free_x")



#Location by decile instead of rows











all_texts_comp1 <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  mutate(word_position = row_number() / n()) %>%
  ungroup() %>%
  mutate(decile = ceiling(word_position * 10) / 10) %>%
  count(decile, word) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(decile) %>%
  summarize(score = sum(score * n) / sum(n))

all_texts_comp2 <- jverne_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  mutate(word_position = row_number() / n()) %>%
  ungroup() %>%
  mutate(decile = ceiling(word_position * 10) / 10) %>%
  count(decile, word) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(decile) %>%
  summarize(score = sum(score * n) / sum(n))

ggplot(data=all_texts_comp1,aes(decile, score)) +
  geom_line(colour="red") +
  geom_line(data=all_texts_comp2,colour="blue") +
  scale_x_continuous(labels = percent_format()) +
  expand_limits(y = 0) +
  labs(x = "Position within a story",
       y = "Average AFINN sentiment score")







#Location by chapter










jverne_texts %>% 
  mutate(chapter=str_extract(text,"Chapter .*|CHAPTER .*")) %>% 
  count(chapter)
  

jverne_texts %>% 
  mutate(chapter=str_extract(text,"Chapter .*|CHAPTER .*")) %>% 
  count(chapter)

see<- jverne_texts %>% 
  filter(str_detect(text,"Chapter|CHAPTER"))

see<- jverne_texts %>% 
  filter(str_detect(text,"Chapter|CHAPTER")) %>% 
  count(title)

hgwells_texts %>% 
  filter(str_detect(text,"Chapter|CHAPTER")) %>% 
  count(title)


all_texts_comp1 <- jverne_texts %>% 
  mutate(chapter=str_extract(text,"Chapter .*|CHAPTER .*")) %>% 
  mutate(chapter=na.locf(chapter,na.rm=F))%>%
  group_by(title,chapter)%>%
  summarise(n=n()) #number of lines in chapter
  


#Mood in chapter














#Words in chapter, which chapter













### Inside one text


hamlet <-gutenberg_metadata %>%
  filter(has_text==TRUE) %>%
  filter(str_detect(title,"Hamlet"))

hamlet_text <- gutenberg_download(hamlet$gutenberg_id[1], meta_fields = "title")

hamlet_text %>%
  mutate(row_nr = row_number()) %>%
  filter(str_detect(text,"THE TRAGEDY OF HAMLET, PRINCE OF DENMARK"))






## Characters, scenes and acts...
# all you need for a cooccurrence is a grouping factor








hamlet_conv <- hamlet_text %>%
  mutate(row_nr = row_number()) %>%
  filter(row_nr>211) %>% 
  mutate(character=str_extract(text,"[A-Z][a-z][a-z]\\. ")) %>% 
  mutate(character=na.locf(character,na.rm=F))

hamlet_conv <- hamlet_conv %>%
  mutate(act=str_extract(text,"Act [A-Z]\\.|ACT [A-Z]\\.")) %>% 
  mutate(act=na.locf(act,na.rm=F)) %>% 
  mutate(scene=str_extract(text,"Scene .*")) %>% 
  mutate(scene=na.locf(scene,na.rm=F))



#lines per character
hamlet_conv %>% 
  count(character)


#characters in scene
hamlet_summary <- hamlet_conv %>% 
  group_by(act,scene) %>% 
  summarise(characters=list(unique(character)))
  
hamlet_summary




members_of_scenes <- hamlet_conv %>%
  group_by(act,scene) %>%
  distinct(character)

pairs_in_scenes <- members_of_scenes %>%
  left_join(members_of_scenes, by=c("act","scene")) %>%
  filter(character.x!=character.y) %>%
  group_by(character.x,character.y) %>%
  count() %>%
  arrange(desc(n))



lapply(c("igraph","ggraph"),
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

library(igraph)
library(ggraph)
pairs_in_scenes %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()









## Keyword analysis
## tf_idf https://en.wikipedia.org/wiki/Tf%E2%80%93idf
## "term frequency–inverse document frequency"
## reflects how important a word is to a document in a collection or corpus
## Meaning: it finds the words that are special to that text, compared to all other texts in the comparison set
## Read more in https://www.tidytextmining.com/tfidf.html
## http://www.tfidf.com/


hgwells_tf_idf <- hgwells_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, title, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()


hgwells_tf_idf %>% 
  group_by(title) %>%
  filter(n>10) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = title)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~title,scale="free_y")




jverne_tf_idf <- jverne_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, title, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()

jverne_tf_idf %>% 
  group_by(title) %>%
  filter(n>10) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = title)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~title,scale="free_y")


#plot by location within text
#hgwells_texts %>%
jverne_texts %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(title) %>%
  mutate(word_position = row_number() / n()) %>%
  ungroup() %>%
  mutate(decile = ceiling(word_position * 10) / 10) %>%
  count(decile, word) %>%
  bind_tf_idf(word, decile, n) %>%
  arrange(desc(tf_idf)) %>%
  filter(n>30) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = factor(decile))) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~decile,scale="free_y")


#Now you can try it yourself:

#1) Find the texts you want to work with and download them
#2) Figure out what you want to test, and process the texts
#3) Visualize the results and save the files
















#Some more resources and ideas to try

gutenberg_subjects #lists topics considered in each book

#get an overview of the subjects
subjects <- gutenberg_subjects %>%
  count(subject) %>%
  arrange(desc(n))
View(subjects)

#Consider only the ones that have text in them
subjects <- gutenberg_subjects %>%
  inner_join(gutenberg_metadata,"gutenberg_id") %>%
  filter(has_text==TRUE) %>%
  count(subject) %>%
  arrange(desc(n))
View(subjects)


#It contains for example markings on the characters present
subjects %>%
  filter(str_detect(subject,"character"))

#For example we could get all the Tarzan stories
tarzan_index <- gutenberg_subjects %>%
  filter(str_detect(subject,"Tarzan")) %>%
  inner_join(gutenberg_metadata,"gutenberg_id") %>%
  filter(has_text==TRUE) %>%
  filter(language=="en") #8 books

#And compare them with the Robinson Crusoe stories
crusoe_index <- gutenberg_subjects %>%
  filter(str_detect(subject,"Crusoe, Robinson")) %>%
  inner_join(gutenberg_metadata,"gutenberg_id") %>%
  filter(has_text==TRUE) %>%
  filter(language=="en") #8 books


#We can also look for particular authors
jausten_index <- gutenberg_metadata %>%
  filter(str_detect(author,"Austen, Jane")) %>%
  filter(has_text==TRUE) %>%
  filter(language=="en")

cbronte_index <- gutenberg_metadata %>%
  filter(str_detect(author,"Bront")) %>%
  filter(str_detect(author,"Charlotte")) %>%
  filter(has_text==TRUE) %>%
  filter(language=="en")

shelley_index <- gutenberg_metadata %>%
  filter(str_detect(author,"Shelley")) %>%
  filter(str_detect(author,"Mary")) %>%
  filter(has_text==TRUE) %>%
  filter(language=="en")


#In some cases a category fits in another. In this case its useful to substract one from the other (anti_join)
sherlock_index <- gutenberg_subjects %>%
  filter(str_detect(subject,"Holmes, Sherlock")) %>%
  inner_join(gutenberg_metadata,"gutenberg_id") %>%
  filter(has_text==TRUE) %>%
  filter(language=="en") #19 books

detective_index <- gutenberg_subjects %>%
  filter(subject == "Detective and mystery stories") %>%
  inner_join(gutenberg_metadata,"gutenberg_id") %>%
  filter(has_text==TRUE) %>%
  filter(language=="en") %>%
  anti_join(sherlock_index, "gutenberg_id") #455 book


#Let's download Tarzan and Robinson Crusoe collections
#the numbers behing gutenberg_id, say which books to take, for now, don't take more than 20, as downloading will otherwise take too much time
#tarzan_texts <- gutenberg_download(tarzan_index$gutenberg_id[1:8], meta_fields = "title")
#crusoe_texts <- gutenberg_download(crusoe_index$gutenberg_id[1:8], meta_fields = "title")

#check the texts we got
tarzan_texts %>%
  count(title)
crusoe_texts %>%
  count(title)


#For author texts
#jausten_texts <- gutenberg_download(jausten_index$gutenberg_id, meta_fields = "title")
#cbronte_texts <- gutenberg_download(cbronte_index$gutenberg_id, meta_fields = "title")
#shelley_texts <- gutenberg_download(shelley_index$gutenberg_id, meta_fields = "title")

#For sherlock texts
#sherlock_texts <- gutenberg_download(sherlock_index$gutenberg_id[1:19], meta_fields = "title")
#detective_texts <- gutenberg_download(detective_index$gutenberg_id[1:19], meta_fields = "title")

#If you can't connect to internet with the gutenberg_download function, you can simply open the example datasets
load("data/offline_gutenberg/austen_bronte_shelley.RData")
load("data/offline_gutenberg/tarzan_crusoe.RData")
#load("data/offline_gutenberg/wells_verne.RData")




#See what you can find there, or make your own sample.

#1) How does the vocabulary differ between Crusoe and Tarzan stories?

#2) What are the distinct words for each Tarzan story (compared to all of them)

#3) What are the distinct words for the Tarzan stories compared to the Crusoe stories

#4) How are the words distributed within text, what are the key words in the first 10th of the text?

#5) How is the sentiment distributed within Tarzan and Crusoe stories? Are there noticeable differences between them?

















#There is also a list of authors
gutenberg_authors

#We can get some 18th century authors
enl_authors <- gutenberg_authors %>%
  filter(birthdate < 1800) %>%
  filter(birthdate > 1700)

c18_texts <- gutenberg_metadata %>%
  filter(author %in% enl_authors$author)

c18_subjects <- c18_texts  %>%
  left_join(gutenberg_subjects, by = c("gutenberg_id"="gutenberg_id"))

subjects_counts <- c18_subjects %>%
  count(subject) %>%
  arrange(desc(n))
View(subjects_counts)

c18_histfiction <- c18_subjects %>%
  filter(subject == "Historical fiction") %>%
  filter(language == "en")


#Find a set of comparison between authors in the 18th century, see what differences you can find!





















####
# We can see also the billboard data and song lyrics
##############


billboard_data <- read_csv("data/billboard_lyrics_1964-2015.csv")


# Try to find location within song for eminem,
# And look at the sentiments within it














#Looking at sentiments within songs
#Eminem has fairly negative songs throughout
billboard_data %>%
  filter(Artist=="eminem") %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song) %>%
  mutate(wordnumber = row_number()) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  mutate(line = wordnumber %/% 20) %>% #Change the number here to group the words into smaller or larger chunks
  count(line,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=line,y=sentiment))+
  geom_bar(aes(fill = sentiment>0),stat = 'identity')+
  theme_minimal()+
  facet_wrap(~Song)

#Find the keywords of each lady gaga song compared to other lady gaga songs
tf_idf <- billboard_data %>%
  filter(Artist=="lady gaga") %>% #Here only lady gaga songs are considered
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(Artist,Song) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, Song, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()

#Now plot the dataframe
tf_idf %>% 
  group_by(Song) %>%
  top_n(10) %>% #And take the top 10 from them - might be less available
  filter(tf_idf>0) %>% #Ifnore keywords that are not at all special to the text
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = Song)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~Song,scale="free_y")




## Try to find distinguishing keywords for top 10 artists












#Top 10 all time stored as variable
top10 <- billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10)

##Distinguishing keywords for each artist compared to other artists (all songs are lumped together per artist)
tf_idf_artist <- billboard_data %>%
  #filter(Artist=="lady gaga") %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(Artist) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, Artist, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()

tf_idf_artist %>%
  rename(count=n) %>%
  inner_join(top10,by="Artist") %>%
  group_by(Artist) %>%
  filter(count>5) %>% #If we filter for words that occurred at least 5 times per that artist. Must be done AFTER tf_idf is calculated.
  top_n(10,tf_idf) %>%
  filter(tf_idf>0) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = Artist)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~Artist,scale="free_y")




# Try to find distinguishing keyword for each decade











##Distinguishing keywords for each decade compared to other decades (all songs and artists are lumped together per decade)
tf_idf_decade <- billboard_data %>%
  mutate(Decade=floor(Year/10)*10) %>%
  unnest_tokens(word, Lyrics) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(Decade) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, Decade, n) %>%
  arrange(desc(tf_idf)) %>%
  ungroup()

tf_idf_decade %>%
  rename(count=n) %>%
  group_by(Decade) %>%
  filter(count>10) %>% #If we filter for words that occurred at least 10 times for that decade. This must be done after tf_idf
  top_n(10,tf_idf) %>%
  filter(tf_idf>0) %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = factor(Decade))) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()+
  guides(fill=FALSE) +
  facet_wrap(~Decade,scale="free_y")



