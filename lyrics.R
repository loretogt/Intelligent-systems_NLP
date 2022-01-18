library(tm)
library(SnowballC)
library(tidytext)
library(dplyr)
library(ggplot2)
library(qdap)
library(tidyr)
library(lubridate)

# Load de data
data <- read.csv("songLyrics.csv",  sep=';')

# Take only no remix
data <- subset(data, data$remix== "FALSE")

#Divide date
data <- separate(data, "album_rd", c("DayRelease", "MonthRelease", "YearRelease"), sep = "/")

# words to delete 
stopwords = c("yeah" ,"youre" ) 

#Clean the data 
data$lyrics <- removeWords(data$lyrics, stopwords("english")) #Remove the english stopwords
data$lyrics <- tolower(data$lyrics) #Makes all text lowercase
data$lyrics <- removePunctuation(data$lyrics) #Removes punctuation
data$lyrics <- removeNumbers(data$lyrics) #remove numerals
data$lyrics <-replace_contraction(data$lyrics) #Remove contraction the english stopwords
data$lyrics <- gsub('\\b\\w{1,2}\\b','',data$lyrics) #remove words with 1 or 2 letters
data$lyrics <- removeWords(data$lyrics,stopwords)#remove the words that i consider stopwords in the domine
data$lyrics <- stripWhitespace(data$lyrics) #Removes tabs, extaspaces 


#stemming
data$lyrics <- wordStem(data$lyrics,  language = "english")

str(data[1, ]$lyrics, nchar.max = 300)


#tokenization
df <- tibble(lyrics = data$lyrics)
df <- df %>% unnest_tokens(lyrics, lyrics)


df %>%
  count(lyrics, sort = TRUE) %>%
  filter(n > 200) %>%
  mutate(lyrics = reorder(lyrics, n)) %>%
  ggplot(aes(lyrics, n)) +
  geom_col(fill = "#800080") +
  xlab(NULL) +
  coord_flip()





#Data visualization
ggplot(data, aes(YearRelease)) +
  geom_bar(fill = "#800080") +
  ggtitle("Song by year") +  
  theme_classic()
