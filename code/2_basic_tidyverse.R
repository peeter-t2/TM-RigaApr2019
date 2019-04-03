#Compiled for R text mining with tidyverse workshop in Rīgas Tehniskā universitāte, April 2019 by Peeter Tinits
#
#from https://github.com/rstudio/webinars/blob/master/46-tidyverse-visualisation-and-manipulation-basics/ (RStudio webinar by garretgman, CC)
#and http://rpubs.com/aelhabr/tidyverse-basics
#
# The purpose of this file is to introduce to the basic tidyverse commands with the help of simple manipulations of a simple dataframe.
#
#The manipulations are done with the Gapminder basic dataset
#
#For an analysis with the basic variables see here: 
#https://www.gapminder.org/answers/how-does-income-relate-to-life-expectancy/
#There are more variables in the dataset that are not handled in the file


#This command installs the libraries needed to run the code, if you don't have them.
lapply(c("gapminder","tidyverse"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

#Libraries need to be opened each time you open R. These commands open the libraries/packages in the current environment.
library(gapminder)
library(tidyverse)




#A somewhat similar plot from our data. This may look complicated at first but should become more clear as you go through the code and return to it later.
gapminder %>%
  group_by(country) %>%
  filter(year==max(year)) %>%
  ggplot(aes(y=lifeExp,x=gdpPercap,size=pop,color=continent))+
  geom_point()+
  scale_x_log10()


#Basic tidyverse transformations
#
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
#mutate() create new variables

#We can make a variable and view it, by clicking on it on the right,
#or writing view(var)
var <- gapminder

#Only finland
finland <- gapminder %>%
  filter(country=="Finland")

#Finland's year and population only.
gapminder %>%
  filter(country=="Finland") %>%
  select(year,pop)
  

#Population sizes ordered from highest to lowest in 1952
gapminder %>% 
  filter(year == 1952) %>% 
  arrange(desc(pop))

#Lowest life expectancies in 2007
gapminder %>% 
  filter(year == 2007) %>% 
  arrange(lifeExp) %>% 
  select(country, lifeExp)

#Make a new variable - gdp
gapminder %>% 
  mutate(gdp = pop * gdpPercap)


#Plotting function
#data %>%
#ggplot(mapping = aes(<MAPPINGS>)) +
#  <GEOM_FUNCTION>()
#
#look for more info on ggplot2 to understand better

#A simple plot similar to  our graph, but for one country in time
gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=lifeExp,y=gdpPercap))+
  geom_point()+
  theme_minimal()

gapminder %>%
  filter(country=="Finland") %>%
  ggplot(aes(x=year,y=pop))+
  geom_line()+
  theme_minimal()




gapminder

#The biggest gdp over the entire dataset
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  summarise(max_gdp = max(gdp))

#The biggest gdp per continent
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(continent) %>% 
  summarise(max_gdp = max(gdp))

#The biggest gdp per capita per country
gapminder %>% 
  group_by(country) %>% 
  summarise(max_gdpPercap = max(gdpPercap)) %>% 
  arrange(desc(max_gdpPercap))

#mean gdp per capita across continents
gapminder %>% 
  group_by(continent) %>% 
  summarise(mean_gdpPercap = mean(gdpPercap)) %>% 
  arrange(desc(mean_gdpPercap))

#Trends over time for continent
gapminder %>% 
  group_by(continent,year) %>% 
  summarise(mean_gdpPercap = mean(gdpPercap)) %>% 
  arrange(desc(continent,year)) %>% 
  ggplot(aes(x=year,y=mean_gdpPercap,color=continent))+
  geom_line()+
  theme_minimal()




#Trends in time
#First and last
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  summarise(first_gdp = first(gdp), last_gdp = last(gdp))
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(first_gdp = first(gdp), last_gdp = last(gdp))


#comparing first and last
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(gdp1952 = first(gdp), gdp2007 = last(gdp))

#Percentage increase per year
gapminder %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(gdp1952 = first(gdp), gdp2007 = last(gdp)) %>% 
  mutate(cagr = ((gdp2007 / gdp1952) ^ (1/55) - 1) * 100) %>% 
  arrange(desc(cagr)) %>% 
  select(country, cagr)


#gdp per country in 1952
gapminder
gapminder %>% 
  filter(year == 1952) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  arrange(desc(gdp)) %>% 
  select(country, gdp)



#Top 10 countries by gdp in 1952 (this variable is needed later)
top_10 <-
  gapminder %>% 
  filter(year == 1952) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  arrange(desc(gdp)) %>% 
  top_n(10, gdp) %>% 
  pull(country) #makes it into a simple sequence instead of datframe


#Temporal trends for top 10 countries (notice the %in%)
gapminder
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  ggplot() +
  geom_line(mapping = aes(x = year, y = gdp, color = country))



#Gdps for top 10
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap)

#Gdps scaled to 1952, e.g. how many times bigger gdp does China have in 2007
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  mutate(scaled_gdp = gdp / first(gdp)) %>% 
  ggplot() +
  geom_line(mapping = aes(x = year, y = scaled_gdp, color = country)) +
  labs(title = "GDP Per Capita (Scaled)")



#The growth rates of top 10 countries
gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(start = first(gdp), end = last(gdp)) %>% 
  mutate(cagr = ((end/start) ^ (1 / 55) - 1) * 100) %>% 
  arrange(desc(cagr)) %>% 
  select(country, cagr)

gapminder %>% 
  filter(country %in% top_10) %>% 
  mutate(gdp = pop * gdpPercap) %>% 
  group_by(country) %>% 
  summarise(start = first(gdp), end = last(gdp)) %>% 
  mutate(cagr = ((end/start) ^ (1 / 55) - 1) * 100) %>% 
  arrange(desc(cagr)) %>% 
  select(country, cagr) %>% 
  ggplot() +
  geom_col(mapping = aes(x = country, y = cagr))

