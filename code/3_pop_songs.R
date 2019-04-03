#Compiled for R text mining with tidyverse workshop in Rīgas Tehniskā universitāte, April 2019, by Peeter Tinits
#dataset: https://github.com/walkerkq/musiclyrics
#based on: https://en.wikipedia.org/wiki/Billboard_Hot_100
#the concept inspired by work behind Brand, Charlotte; Acerbi, Alberto & Mesoudi, Alex. 2018. Cultural evolution of emotional expression in 50 years of song lyrics. https://osf.io/3j6wx/




#This command installs the libraries needed to run the code, if you don't have them.
lapply(c("tidytext","tidyverse","gridExtra","scales"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

#Libraries need to be opened each time you open R. These commands open the libraries/packages in the current environment.
library(tidyverse)
library(tidytext)
library(gridExtra)
library(scales)



billboard_data <- read_csv("data/billboard_lyrics_1964-2015.csv")

#basic model is the following
#data %>%
#  process()
#
# %>% - carry the data into function
#select() selecting variables
#filter() provides basic filtering capabilities
#arrange() ordering data
#group_by() groups data by categorical levels
#summarise() summarise data by functions of choice
#join() joining separate dataframes
#mutate() create new variables


# Commands for text processing
# count(variable) - counts the number of items
# top_n(number, variable) - make toplists by variable
# left_join(dataframe) - add one dataframe to another
# unnest_tokens(unit, variable) - make texts into tokens
# str_detect(variable, "string") - partial match of a string



#We can make a variable and view it, by clicking on it on the right,
#or writing view(var)
var <- billboard_data
view(var)


###############################################
#### Frequencies
###############################################



#Let's take a random sample of 10 songs in the last year
#For fun, how many can songs or artists you recognize?
now <- billboard_data %>%
  filter(Year==max(Year)) %>%
  sample_n(10)


#Let's take a random sample of 10 songs from 1997
year1997 <- billboard_data %>%
  filter(Year==1997) %>%
  sample_n(10)


#Number of instances in top 100 per artist
billboard_data %>%
  count(Artist)

#Arrange by number in top 100
billboard_data %>%
  count(Artist) %>%
  arrange(desc(n))

#All by some artist
#Data for beyonce

#Beyonce's songs by rank and year
beyonce %>%
  ggplot(aes(x=Year,y=-Rank,color=Song))+
  geom_point()+
  geom_line()

#Taylor Swift songs by Rank and year (same as with beyonce, except that we don't store a variable)
billboard_data %>%
  filter(Artist=="taylor swift") %>%
  ggplot(aes(x=Year,y=-Rank,color=Song))+
  geom_point()+
  geom_line()


#The number of times the best group was in top 100 in each year
billboard_data %>%
  group_by(Year) %>%
  count(Artist) %>%
  top_n(1) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=Year,y=n))+
  geom_point()+
  geom_line()


#Top 10 of all time, per instances in top 100
billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10)


#Top 20 of all time, per instances in top 100
billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  head(20)

#How many artists have how many songs
billboard_data %>%
  count(Artist) %>%
  count(n) %>%
  arrange(desc(n))


#The worst year in music (most repetitions)
billboard_data %>%
  group_by(Year) %>%
  count(Artist) %>%
  arrange(desc(n))



#Top 10 all time stored as variable
top10 <- billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10)


#Add the count of how many times total to the main dataframe
billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  left_join(billboard_data)



#The best rank for each song of lady gaga
billboard_data %>%
  filter(Artist=="lady gaga") %>%
  group_by(Song) %>%
  summarise(minrank=min(Rank))

#All song ranks of lady gaga plotted
billboard_data %>%
  filter(Artist=="lady gaga") %>%
  ggplot(aes(x=Year,y=-Rank,group=Song,color=Song))+
  geom_line()+
  geom_point()+
  theme_minimal()


##The duration of productive lifespan, 
# Taylor swift is very quick, michael jackson was on top for very long
billboard_data %>%
  count(Artist) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  left_join(billboard_data) %>%
  group_by(Artist) %>%
  summarise(min_year=min(Year),max_year=max(Year)) %>%
  gather(type,year,c(min_year,max_year)) %>% # a new function that changes the dataframe a bit
  ggplot(aes(y=Artist))+
  geom_line(aes(x=year),size=3)+
  theme_minimal()



## Trends within lyrics
## Uses tidytext tools https://www.tidytextmining.com/tidytext.html

#Words in song lyrics
billboard_tokens <- billboard_data %>%
  unnest_tokens(word, Lyrics) #new function of unnest_tokens, takes the words from the text, and sets them as separate observations

#Are songs getting shorter or longer?  (just plotting the averages here per year)
type_token_count <- billboard_tokens %>%
  group_by(Song,Artist,Year) %>%
  summarise(types=n_distinct(word),tokens=length(word))

type_token_count %>%
  ggplot(aes(x=Year,y=tokens))+
  geom_point()+
  geom_smooth()

#Is the vocabulary increasing in time?
type_token_count  %>%
  ggplot(aes(x=Year,y=types))+
  geom_point()+
  geom_smooth()

#Is the vocabulary becoming more repetitive/diverse
#see "type-token ratio" online for more information
type_token_count  %>%
  ggplot(aes(x=Year,y=types/tokens))+
  geom_point()+
  geom_smooth()



# VISUALISE TRENDS OF SINGLE WORDS:
# as proportion of all the words

word_to_search <- "love"
word_to_search <- "bad"


my_title <- paste("Trend of '",word_to_search, "'", sep="")

total_words <- count(unnest_tokens(billboard_data,word, Lyrics), Year)

billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  count(word, Year) %>%
  filter(word==word_to_search) %>%
  #  complete(Year=1965:2015, fill=list(n=0)) %>%
  rename(count=n) %>%
  left_join(total_words) %>%
  ggplot(aes(x=Year, y=count/n)) +
  geom_point(shape=19, alpha=.5, colour="red" ) +
  geom_smooth(method=loess, colour="red", fill="grey") +
  labs(y="Proportion", title=my_title)+
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5)) # +

# ggsave(filename="plots/single_word_example.pdf", width = 5, height = 5)

#Same thing with separate variable for tokens (quicker)
billboard_tokens %>%
  count(word, Year) %>%
  filter(word==word_to_search) %>%
  #  complete(Year=1965:2015, fill=list(n=0)) %>%
  rename(count=n) %>%
  left_join(count(billboard_tokens, Year)) %>%
  ggplot(aes(x=Year, y=count/n)) +
  geom_point(shape=19, alpha=.5, colour="red" ) +
  geom_smooth(method=loess, colour="red", fill="grey") +
  labs(y="Proportion", title=my_title)+
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5)) # +

############################################################
###### Compare texts
############################################################



comparison1 <- billboard_data %>%
  filter(Artist=="beyonce") %>%
  unnest_tokens(word, Lyrics) %>%
  count(word)
  
  
comparison2 <- billboard_data %>%
  filter(Artist=="lady gaga") %>%
  unnest_tokens(word, Lyrics) %>%
  count(word)

comparison <- comparison1 %>%
  rename(comparison1 = n) %>%
  inner_join(comparison2,"word") %>%
  rename(comparison2 = n) %>%
  mutate(comparison1 = comparison1 / sum(comparison1),
         comparison2 = comparison2 / sum(comparison2))

#this is just to plot the comparison
comparison %>%
  ggplot(aes(comparison1, comparison2)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")



data("stop_words")

comparison %>%
  anti_join(stop_words, by = "word") %>%
  ggplot(aes(comparison1, comparison2)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = word), check_overlap = TRUE,
            vjust = 1, hjust = 1) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")+
  labs(x="Beyonce",y="Lady Gaga")


#######################################################################
##### Lexicon based approaches (sentiment analysis)
######################################################################




## Sentiment analysis
## uses a simple vocabulary "get_sentiments("bing")" to track sentiments
## https://www.tidytextmining.com/sentiment.html

get_sentiments("bing")

#How do the sentiments go over time
billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song,Year) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(Artist,Song,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=Year,y=sentiment))+
  geom_point(aes(color = sentiment>0),stat = 'identity')+
  geom_smooth()+
  theme_minimal()
#More or less fair coverage of all varieties across the time, somewhat more below 0 (negative) emotions in 2000s


#How about prevalence of negative or positive emotions in individual songs
plot1 <- 
  billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song,Year) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(Artist,Song,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=Year,y=positive))+
  geom_point(color="red",stat = 'identity')+
  geom_smooth()+
  theme_minimal()

plot2 <- 
  billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song,Year) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(Artist,Song,sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(x=Year,y=negative))+
  geom_point(color="green",stat = 'identity')+
  geom_smooth()+
  theme_minimal()

#Put multiple graphs in one plot
gridExtra::grid.arrange(plot1,plot2)







#Looking at sentiments between songs. Is there an association between rank and sentiment?
billboard_data %>%
  #filter(Artist=="lady gaga") %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song) %>%
  mutate(min_rank = min(Rank)) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  group_by(Artist,Song,min_rank) %>%
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  ggplot(aes(y=sentiment, x=min_rank))+
  geom_point()+
  geom_smooth()


#Are happier artists more popular? By artist, number songs in top and their sentiment scores. Top scoring artists seem fairly balanced, lower artists are all over.
billboard_data %>%
  #filter(Artist=="lady gaga") %>%
  unnest_tokens(word, Lyrics) %>%
  group_by(Artist,Song) %>%
  mutate(wordnumber = row_number()) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  group_by(Artist,Song,Rank) %>%
  count(sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  group_by(Artist) %>%
  summarise(positive=mean(positive),negative=mean(negative),sentiment=mean(sentiment),songs_in_top=n_distinct(Song)) %>%
  ggplot(aes(y=sentiment, x=songs_in_top))+
  geom_point()+
  geom_smooth()


# General trends in positive emotions
plot1 <-
  billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  inner_join(get_sentiments("bing")) %>%
  count(Year, sentiment) %>%
  spread(sentiment, n, fill = 0, convert=T) %>%
  left_join(count(unnest_tokens(billboard_data,word, Lyrics), Year)) %>%
  ggplot(aes(Year, (positive)/n)) + # change here
  geom_point(shape=19, alpha=.5, colour="black" ) +
  geom_smooth(method=lm, colour="black", fill="grey") +
  labs(y="Proportion", title="Positive emotions")  +
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5))# +
#  ggsave(filename="plots/trend_example1.pdf", width = 5, height = 5)

#General trend for negative words
plot2 <-
  billboard_data %>%
  unnest_tokens(word, Lyrics) %>%
  inner_join(get_sentiments("bing")) %>%
  count(Year, sentiment) %>%
  spread(sentiment, n, fill = 0, convert=T) %>%
  left_join(count(unnest_tokens(billboard_data,word, Lyrics), Year)) %>%
  ggplot(aes(Year, (negative)/n)) + # change here
  geom_point(shape=19, alpha=.5, colour="black" ) +
  geom_smooth(method=lm, colour="black", fill="grey") +
  labs(y="Proportion", title="Negative emotions")  +
  theme_minimal() +
  theme(plot.title = element_text(face="bold", hjust=.5))# +
#  ggsave(filename="plots/trend_example2.pdf", width = 5, height = 5)


#Positive emotions go down, negative emotions go up
#But the scale is proportion of all of vocabulary, and it is around 4-5% for positive vocabulary and 2-3% for negative vocabulary for the entire duration
gridExtra::grid.arrange(plot1,plot2,ncol=2)


