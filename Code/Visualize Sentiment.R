library(tidyverse)
library(esquisse)
library(lubridate)

####Import#####
sentijoin <- read.csv("Tweetsfebruar.csv", numerals = c("no.loss")) %>% 
  as_tibble() %>%
  select(-X)

sentijoin$date <- ymd(sentijoin$date) 
sentijoin[sentijoin == 0] <- NA
sum(is.na(sentijoin$SentimentScore))
#Alle tweets drin allerdings fehlt bei 9385 der sentiscore -> zu kurz

####Visualize Sentiment####
datejoin <- sentijoin %>%
  group_by(date) %>%
  summarise(mean_sentiment = mean(SentimentScore, na.rm = T))

s <- ggplot(data = datejoin) +
  aes(x = date, y = mean_sentiment) +
  geom_area(size = 1L) +
  ylab("Mean Sentiment") +
  xlab("Date")

noposneg <- ggplot(data = sentijoin) +
  aes(x = SentimentLabel, fill= SentimentLabel) +
  geom_bar() +
  scale_fill_manual(values=c("red", "grey", "darkgreen")) +
  ylab("Number of tweets") +
  xlab("Sentiment") +
  ggtitle("Sentiment of all tweets in the dataset (n=25057, na = 9385)") +
  theme(legend.position="none") 
  
####Most negative/positive tweets####
sortsentin <- arrange(sentijoin, SentimentScore)
sortsentip <- arrange(sentijoin, desc(SentimentScore))

sortsentineg <- head(sortsentin, n=50) %>% 
  select(name, full_text, SentimentScore)

sortsentipos <- head(sortsentip, n=50) %>%
  select(name, full_text, SentimentScore)

#write.csv(sortsentineg, file = "50negativstetweets.csv")
#write.csv(sortsentipos, file = "50positivstetweets.csv")

####Many RTs/favs####

sortf <- arrange(sentijoin, desc(favorite_count)) %>%
  slice(1:52)
pnf <- ggplot(data = sortf) +
  aes(x = SentimentLabel, fill= SentimentLabel) +
  geom_bar() +
  scale_fill_manual(values=c("red", "grey", "darkgreen")) +
  ylab("Number of tweets") +
  xlab("Sentiment") +
  ggtitle("Sentiment of tweets that got favourited more than 1000 times (n=52)") +
  theme(legend.position="none") 

sortrt <- arrange(sentijoin, desc(retweet_count)) %>%
  slice(1:20)
pnrt <- ggplot(data = sortrt) +
  aes(x = SentimentLabel, fill= SentimentLabel) +
  geom_bar() +
  scale_fill_manual(values=c("red", "grey", "darkgreen")) +
  ylab("Number of tweets") +
  xlab("Sentiment") +
  ggtitle("Sentiment of tweets that got retweeted more than 100 times (n=20)") +
  theme(legend.position="none") 

####Aggregate####
aktivität <- data.frame(name = c("Polizei Frankfurt",
                                     "Polizei Brandenburg",
                                     "Polizei Sachsen",
                                     "Polizei Mittelhessen",
                                     "Polizei München",
                                     "Polizei Hamburg",
                                     "Polizei NRW DO",
                                     "Polizei Mittelfranken",
                                     "Polizei Karlsruhe",
                                     "Polizei Mannheim"), numbertweets =
                          c(1035, 895, 861, 749, 706, 642, 639, 567, 532, 516)) %>% as_tibble() 

stationsenti <- sentijoin %>% 
  group_by(name) %>% 
  summarise(sentimean = mean(SentimentScore, na.rm = T)) %>% 
  merge(aktivität, by = "name") %>% 
  arrange(desc(sentimean))

stasen <- ggplot(data=stationsenti) +
  aes(x = name, weight = sentimean, fill = "red") +
  geom_bar() +
  ggtitle("Mean sentiment for the 10 most active police accounts") +
  xlab ("Police accounts") +
  ylab ("Mean sentiment") +
  theme(legend.position="none") 

####Sentiment nach Bundesländern####
normalisiertbula <- sentijoin %>%
  group_by(bula) %>% summarise(Negative = ((sum(SentimentLabel == "negative")/n())*100), 
                               Positive = ((sum(SentimentLabel == "positive")/n())*100),
                               Neutral = ((sum(SentimentLabel == "neutral")/n())*100))
#From wide to long für stacked barchart
normalisiertbula <- gather(normalisiertbula, key = "Sentiment", value = "Score", Negative, Positive, Neutral, factor_key = T)

normbusenti <- ggplot(data = normalisiertbula, aes(x=bula, y=Score, fill = Sentiment)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("red", "darkgreen", "grey")) +
  ylab("Percent of tweets") +
  xlab("Bundesländer and Bundespolizei")
  
  


busenti <- ggplot(sentijoin) +
  aes(x = bula, fill = SentimentLabel) +
  geom_bar() +
  scale_fill_manual(values=c("red", "grey", "darkgreen")) +
  ylab("Number of Tweets") +
  xlab("Bundesländer and Bundespolizei")

#####Sentiment Ost/West####
ostwestse <- ggplot(sentijoin) +
  aes(x = ostwest, fill = SentimentLabel) +
  geom_bar() +
  labs(x = "Bundesländer", y = "Number of tweets") +
  scale_fill_manual(values=c("red", "grey", "darkgreen"))

