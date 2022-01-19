library(tm)
library(SnowballC)
library(tidytext)
library(dplyr)
library(ggplot2)
library(qdap)
library(tidyr)
library(lubridate)
library(wordcloud2)
library(textdata)

# Load de data
data <- read.csv("songLyrics.csv",  sep=';')

# Take only no remix
data <- subset(data, data$remix== "FALSE")
# Delete the 'skits' that are not songs
data <- subset(data, !grepl('^Skit', data$track_title))
# Delete the notes
data <- subset(data, data$album_seq!=0)
# Delete instrumental songs 
data <- subset(data, !is.na(data$lang))

#Divide date
data <- separate(data, "album_rd", c("DayRelease", "MonthRelease", "YearRelease"), sep = "/")

# words to delete 
stopwords = c("yeah" ,"youre" ) 

#Clean the data 
data$lyrics <- tolower(data$lyrics) #Makes all text lowercase
data$lyrics <-replace_contraction(data$lyrics) #Remove contraction the english stopwords
data$lyrics <- removePunctuation(data$lyrics) #Removes punctuation
data$lyrics <- removeNumbers(data$lyrics) #remove numerals
data$lyrics <- gsub('\\b\\w{1,2}\\b','',data$lyrics) #remove words with 1 or 2 letters
data$lyrics <- removeWords(data$lyrics,stopwords)#remove the words that I consider stopwords in the domine
data$lyrics <- removeWords(data$lyrics, stopwords("english")) #Remove the english stopwords
data$lyrics <- stripWhitespace(data$lyrics) #Removes tabs, extaspaces 

#stemming
data$lyrics <- wordStem(data$lyrics,  language = "english")

str(data[1, ]$lyrics, nchar.max = 300)


#tokenization
data_words_filtered <- data %>% unnest_tokens(word, lyrics)

#####Data visualization#######
#Song released by year
ggplot(data, aes(YearRelease)) +
  geom_bar(fill = "#800080") +
  ggtitle("Song by year") +  
  theme_classic()

#Most Frequently Used Words in BTS Lyrics
data_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "#800080") +
  xlab(NULL) +
  ggtitle("Most Frequently Used Words in BTS Lyrics") +
  ylab("Song Count") +
  coord_flip()+
  theme_classic()

#Most Frequently Used Words in BTS Lyrics in a shape
BTS_words_counts <- data_words_filtered %>%count(word, sort = TRUE) 
wordcloud2(BTS_words_counts[1:300, ], size = .5,color = "#800080")

#Most Frequently Used Words in BTS Lyrics by album
popular_words_album <- data_words_filtered %>% 
  group_by(eng_album_title) %>%
  count(word, eng_album_title, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(eng_album_title,n) %>%
  mutate(row = row_number()) 

popular_words_album %>%
  ggplot(aes(row, n, fill = eng_album_title)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Most Frequently Used Words in BTS Lyrics by album") + 
  facet_wrap(~eng_album_title, scales = "free", ncol = 5) +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words_album$row, # notice need to reuse data frame
    labels = popular_words_album$word) +
  coord_flip()

#Most Frequently Used Words in BTS Lyrics by year
popular_words_year <- data_words_filtered %>% 
  group_by(YearRelease) %>%
  count(word, YearRelease, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(YearRelease,n) %>%
  mutate(row = row_number()) 

popular_words_year %>%
  ggplot(aes(row, n, fill = YearRelease)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Most Frequently Used Words in BTS Lyrics by year") + 
  facet_wrap(~YearRelease, scales = "free", ncol = 5) +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words_year$row, # notice need to reuse data frame
    labels = popular_words_year$word) +
  coord_flip()
 
#sentiment analysis
data_nrc <- data_words_filtered %>% inner_join(get_sentiments("nrc"))
data_bing <- data_words_filtered %>%inner_join(get_sentiments("bing"))
data_afinn <- data_words_filtered %>%inner_join(get_sentiments("afinn"))

#General look 
data_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col(fill = "#800080") +
  ggtitle("NRC sentiment in songs") +
  ylab("Word Count") +
  coord_flip()+
  theme_classic()

data_bing %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col(fill = "#800080") +
  ggtitle("NRC sentiment in songs") +
  ylab("Word Count") +
  coord_flip()+
  theme_classic()

data_afinn %>%
  group_by(value) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(value = reorder(value, word_count)) %>%
  ggplot(aes(factor(value, level = c('-5', '-4', '-3','-2','-1','0','1','2','3','4','5')), word_count, fill = -word_count)) +
  geom_col(fill = "#800080") +
  ggtitle("Afinn sentiment in songs") +
  ylab("Word Count") +
  xlab("Level of positive/negative") +
  coord_flip()+
  theme_classic()
                 
# Gruped by album
sentiment_album_nrc <- data_nrc %>% 
  group_by(eng_album_title) %>%
  count(sentiment, eng_album_title, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(eng_album_title,n) %>%
  mutate(row = row_number()) 

sentiment_album_nrc %>%
  ggplot(aes(row, n, fill = eng_album_title)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Sentiment in BTS Lyrics by album") + 
  facet_wrap(~eng_album_title, scales = "free", ncol = 5) +
  scale_x_continuous(  # This handles replacement of row 
    breaks = sentiment_album_nrc$row, # notice need to reuse data frame
    labels = sentiment_album_nrc$sentiment) +
  coord_flip()


sentiment_album_data_bing <-  data_bing %>% group_by(eng_album_title)

ggplot(sentiment_album_data_bing, aes(eng_album_title, fill=sentiment)) +
  geom_bar(position = "fill",) +
  ggtitle("Sentiment by album")  +
  scale_fill_manual(values = c("positive" = "#bf7fbf", "negative" = "#800080")) +
  theme_classic() + 
  coord_flip()

