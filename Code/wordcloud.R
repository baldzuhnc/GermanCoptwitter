library(tidyverse)
library(tm)
library(wordcloud)
library(textclean)

#Import
entity <- read.csv('Temporary datasets/entitydez.csv', encoding = "UTF-8-mac", numerals = c("no.loss")) 
tweet <- read.csv('Tweetsfebruar.csv', encoding = 'UTF-8-mac', numerals = c("no.loss")) 
user <- read.csv('Temporary datasets/userdez.csv', encoding = 'UTF-8-mac', numerals = c("no.loss"))


test <- grep("RT @", tweet$full_text, fixed = T)

#df <- replace_emoji(tweet$tweet_text)
#exp <- data.frame(df)
#write.csv(exp, 'dataframesucio.csv')
#suc <- fread('dataframesucio.csv', encoding = 'UTF-8', fill = T)


#Create TextCorpus####
corpus <- tweet$full_text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])


#Clean text####
corpus <- tm_map(corpus, tolower) #Lowercase
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
cleanset <- tm_map(corpus, removeWords, stopwords('german'))
inspect(cleanset[1:5])

#Create function to remove URL from corpus
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL)) #Contenttransformer(x)

#Replace Emojis
#cleanset <- replace_emoji(cleanset$content) 
#cleanset <- iconv(cleanset, to = 'utf-8')

cleanset <- tm_map(cleanset, stripWhitespace) #Remove extra whitespace

#Code to remove non-informative words
cleanset <- tm_map(cleanset, removeWords, c('wurde', 'wurden','amp', 'kam', 'dass', 'gibt')) 

#Replace words
cleanset <- tm_map(cleanset, gsub, pattern = 'jährigen', replacement = 'jährig')
cleanset <- tm_map(cleanset, gsub, pattern = 'jähriger', replacement = 'jährig')
cleanset <- tm_map(cleanset, gsub, pattern = 'jährige', replacement = 'jährig')
cleanset <- tm_map(cleanset, gsub, pattern = 'unserer', replacement = 'unsere')
cleanset <- tm_map(cleanset, gsub, pattern = '➡️', replacement = '*Pfeil_nach_rechts*')
cleanset <- tm_map(cleanset, gsub, pattern = '➡', replacement = '*Pfeil_nach_rechts*')
cleanset <- tm_map(cleanset, gsub, pattern = '▶️', replacement = '*Pfeil_nach_rechts*')
cleanset <- tm_map(cleanset, gsub, pattern = '\U0001f4cc', replacement = '*Pinnadel*')
cleanset <- tm_map(cleanset, gsub, pattern = '☎', replacement = '*Telefon*')
cleanset <- tm_map(cleanset, gsub, pattern = '☎️', replacement = '*Telefon*')
cleanset <- tm_map(cleanset, gsub, pattern = '–', replacement = '')
cleanset <- tm_map(cleanset, gsub, pattern = '…', replacement = '')
cleanset <- tm_map(cleanset, gsub, pattern = '\U0001f6a8', replacement = '*Blaulicht*')
cleanset <- tm_map(cleanset, gsub, pattern = '\U0001f694', replacement = '*Herannahender_Polizeiwagen*')
cleanset <- tm_map(cleanset, gsub, pattern = '▶', replacement = '*Pfeil_nach_rechts*')
cleanset <- tm_map(cleanset, gsub, pattern = '\U0001f44d', replacement = '*Daumen_hoch*')
cleanset <- tm_map(cleanset, gsub, pattern = '\U0001f4de', replacement = '*Telefonhörer')
cleanset <- tm_map(cleanset, gsub, pattern = '\U0001f609', replacement = '*Zwinkerndes_Gesicht*')
cleanset <- tm_map(cleanset, gsub, pattern = '\U0001f4f8', replacement = '*Fotoapparat*')
cleanset <- tm_map(cleanset, gsub, pattern = '\U0001f447', replacement = '*Zeigefinger_nach_unten*')
cleanset <- tm_map(cleanset, gsub, pattern = 'ℹ️', replacement = '*Information*')
cleanset <- tm_map(cleanset, gsub, pattern = '\U0001f449', replacement = '*Zeigefinger_nach_rechts*')

#Term document matrix####
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:20, 1:20] #Show Matrix with first 20 rows and columns -> Inspect whether any words are redundant for the analysis

#####Visualisierung#####
#Bar plot
w <- rowSums(tdm)
ws <- subset(w, w>=300)

barplot(ws, las = 2)

#Wordcloud
ww <- sort(rowSums(tdm), decreasing = T)

set.seed(222)
wordcloud(words = names(ww), 
          freq = ww, 
          max.words = 200,
          scale = c(3, 0.3),
          random.order = F,
          min.freq = 150,
          colors = brewer.pal(8, 'Dark2'),
          rot.per = 0.2)

