library(tidyverse)
library(devtools)
library(tidytweetjson)
library(magrittr)
library(furrr)
library(stringr)
library(future)
library(maps)
library(tidyjson)


#Import from JSON with tidytweetjson
filepath1 <- "/Users/Clemens/TEMP/CB Coptwitter/coptwitter_hydrated_21-01-21.jsonl"
filepath2 <- "/Users/Clemens/TEMP/CB Coptwitter/coptwitter_hydrated_22-02-21.jsonl"

jsontodf <- function(file_path) 
{ 
file_name <- strsplit(x = file_path, split = "[/]")
file_name <- file_name[[1]][length(file_name[[1]])]
listed <- read_json(file_path, format = c("jsonl"))
locations <- listed %>% enter_object("user") %>% enter_object("location") %>% 
  append_values_string() %>% as_tibble %>% rename(location = "string")
userid <- listed %>% enter_object("user") %>% enter_object("id") %>% append_values_number() %>% rename(userid = "number") %>% as_tibble()
followers <- listed %>% enter_object("user") %>% enter_object("followers_count") %>% append_values_number() %>% rename(user.followers_count = "number") %>% as_tibble()
name <- listed %>% enter_object("user") %>% enter_object("name") %>% append_values_string() %>% rename(name = "string") %>% as_tibble()
screen_name <- listed %>% enter_object("user") %>% enter_object("screen_name") %>% append_values_string() %>% rename(screen_name = "string") %>% as_tibble()
df <- listed %>% spread_values(tweetid = jnumber("id"), 
                               created_at = jstring("created_at"), 
                               full_text = jstring("full_text"), 
                               retweet_count = jnumber("retweet_count"), 
                                favorite_count = jnumber("favorite_count"), 
                                user.friends_count = jnumber("user.friends_count")) %>% as_tibble
    message(paste("Parsing", file_name, "done."))
    outcome <- full_join(locations, userid) %>% full_join(name) %>% full_join(screen_name) %>% full_join(followers) %>% full_join(df)
    outcome %>% select(-c("document.id"))
}

tweetsjan <- jsontodf(filepath1)
tweetsfeb <- jsontodf(filepath2)

tweetsjan <- add_date(tweetsjan)
tweetsfeb <- add_date(tweetsfeb)

#Remove Artefacts
tweetsjan$user.friends_count <- NULL

tweetsfeb$user.friends_count <- NULL

#write_csv(tweetsjan, "TweetsJanuar.csv")
write_csv(tweetsfeb, "OldFebruar.csv")


####Sentimentjson####
filepath3 <- "/Users/Clemens/TEMP/CB coptwitter/Februarsentidump.json"
filepath4 <- "/Users/Clemens/TEMP/CB coptwitter/Februar2sentidump.json"

sentijs <- read_json(filepath3, format = "json")

sentimentsc <- gather_array(sentijs) %>% 
  enter_object("sentiment") %>% 
  enter_object("document") %>% 
  enter_object("score") %>%
  append_values_number() %>%
  as.tibble() %>%
  select(number, array.index) %>%
  rename(SentimentScore = "number", ID = "array.index")

sentimentcat <-  gather_array(sentijs) %>% 
  enter_object("sentiment") %>% 
  enter_object("document") %>% 
  enter_object("label") %>%
  append_values_string() %>%
  as.tibble() %>%
  select(string, array.index) %>%
  rename(SentimentLabel = "string", ID = "array.index")

textlength <- gather_array(sentijs) %>%
  enter_object("usage") %>%
  enter_object("text_characters") %>%
  append_values_number() %>%
  as.tibble() %>%
  select(number, array.index) %>%
  rename (TweetLength = "number", ID = "array.index")

Sentimentfeb <- merge(sentimentcat, sentimentsc, by = "ID")
Sentimentfeb <- merge(Sentimentfeb, textlength, by = "ID") %>% rename(X = "ID")

####Gleiches für 2. Teil####
sentijs2 <- read_json(filepath4, format = "json")

sentimentsc2 <- gather_array(sentijs2) %>% 
  enter_object("sentiment") %>% 
  enter_object("document") %>% 
  enter_object("score") %>%
  append_values_number() %>%
  as.tibble() %>%
  select(number, array.index) %>%
  rename(SentimentScore = "number", ID = "array.index")

sentimentcat2 <-  gather_array(sentijs2) %>% 
  enter_object("sentiment") %>% 
  enter_object("document") %>% 
  enter_object("label") %>%
  append_values_string() %>%
  as.tibble() %>%
  select(string, array.index) %>%
  rename(SentimentLabel = "string", ID = "array.index")

textlength2 <- gather_array(sentijs2) %>%
  enter_object("usage") %>%
  enter_object("text_characters") %>%
  append_values_number() %>%
  as.tibble() %>%
  select(number, array.index) %>%
  rename (TweetLength = "number", ID = "array.index")

Sentimentfeb2 <- merge(sentimentcat2, sentimentsc2, by = "ID")
Sentimentfeb2 <- merge(Sentimentfeb2, textlength2, by = "ID") %>% rename(X = "ID")

##Beide zusammenfügen
Sentiment <- bind_rows(Sentimentfeb, Sentimentfeb2)
write_rds(Sentiment, file = "Sentiment")
