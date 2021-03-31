library(tidyverse)
library(lubridate)
library(esquisse)

#Import
cdata <- read.csv("Tweetsfebruar.csv") %>% select(-X)
cdata$date <- ymd(cdata$date)

####Aktivität####
sumfre <- cdata %>% group_by(date) %>% summarise(tweetcount = n())

fre <- ggplot(data = sumfre) +
  aes(x = date, weight = tweetcount) +
  geom_bar() +
  ylab("Number of tweets") +
  xlab ("Date")


fre2 <- sumfre %>% slice(35:62) %>%
  ggplot() +
  aes(x = date, weight = tweetcount) +
  geom_bar() +
  ylab("Number of tweets") +
  xlab ("Date")

sort(table(cdata$name))
aktivität <- data.frame(Accounts = c("Polizei Frankfurt",
                                     "Polizei Brandenburg",
                                     "Polizei Sachsen",
                                     "Polizei Mittelhessen",
                                     "Polizei München",
                                     "Polizei Hamburg",
                                     "Polizei NRW DO",
                                     "Polizei Mittelfranken",
                                     "Polizei Karlsruhe",
                                     "Polizei Mannheim"), Anzahl_Tweets =
                          c(1035, 895, 861, 749, 706, 642, 639, 567, 532, 516)) %>% as_tibble() 

ak <- ggplot(data = aktivität, 
             aes(x=Anzahl_Tweets, y=reorder(Accounts, -Anzahl_Tweets))) + 
  geom_bar(inherit.aes = T, stat = "identity") +
  xlab("Number of tweets") +
  ylab("Police accounts")


