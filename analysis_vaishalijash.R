#Setup

library(stringr)
library(twitteR)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)
library(rvest)
library(XML)
library(magrittr)
library(plyr)
library(ROAuth)
library(httr)
library(tm)
library(SnowballC)
library(wordcloud)
library(stargazer)

#API keys to tap Twitter information
api_key <- "-"
api_secret <- "-"
access_token <- "-"
access_token_secret <- "-"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


####1. Extracting tweets containing #MeToo and by verified users

##MeToo
verified <- searchTwitter("#metoo + #MeToo filter:verified", n=10000, since="2017-10-15")
verifieddf <- twListToDF(verified)

write.csv(verifieddf, "verifieddf.csv")

verifieddf <- read.csv("verifieddf.csv")

###2. Building a word cloud of #metoo tweets by Twitter influencers

metooCorpus <- Corpus(VectorSource(verifieddf$text))

metooCorpus <- tm_map(metooCorpus, removePunctuation)
metooCorpus <- tm_map(metooCorpus, removeWords, stopwords('english'))

metooCorpus <- tm_map(metooCorpus, removeWords, c('the', 'this', stopwords('english')))


metooCorpus <- tm_map(metooCorpus, stemDocument)

wordcloud(metooCorpus, max.words = 100, random.order = FALSE)


###3. Summary statistics of extracted tweets

##Mean, min, max and sd of tweet lengths
mean(str_length(verifieddf$text))
min(str_length(verifieddf$text))
max(str_length(verifieddf$text))
sd(str_length(verifieddf$text))

##Top tweets by number of times they are favorited
populartweets <- verifieddf %>%
select(text, favoriteCount) %>% 
group_by(favoriteCount) %>%
arrange(desc(favoriteCount)) 

knitr(head(populartweets))


##RTs vS. regular tweets

verifieddf$rt <- str_count(verifieddf$text, "RT")

table(verifieddf$rt)

###Plotting the tweet breakdown
ggplot(data=verifieddf, aes(x = factor(rt))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "maroon", width=0.35) + 
geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
scale_x_discrete(breaks=c("0","1","2"),labels=c("Original tweets", "Retweets", "Retweets of retweets")) +
  labs(x="Type of tweets", y="Percentage of total tweets") + ggtitle("Breakdown of total tweets extracted")



###4. Sentiment Analysis of tweets

####Importing the AFINN lexicon 
positive = scan('C:/Users/169435/Dropbox/Hertie School of Governance/Semester 4/Web data collection and social media mining/Final Assignment - Group/positive-words.txt', what='character', comment.char=';')
negative = scan('C:/Users/169435/Dropbox/Hertie School of Governance/Semester 4/Web data collection and social media mining/Final Assignment - Group/negative-words.txt', what='character', comment.char=';')

verifieddf <- verifieddf[1:1000,]
tweets <- dplyr::select(verifieddf, text)
tweets$text <- toString(tweets$text)

tweets_clean <- tweets %>% 
  unnest_tokens(word,text)

tweets_clean %>%
  dplyr::count(word, sort = TRUE)

tweets_clean <- tweets_clean %>% 
  dplyr:: anti_join(stop_words)

sentiment_afinn <- tweets_clean %>% 
  dplyr::inner_join(get_sentiments("afinn")) %>% 
  dplyr::group_by(word) %>% 
  dplyr::ungroup()

write.csv(sentiment_afinn, "sentiment_afinn.csv")

sentiment_afinn <- read.csv("sentiment_afinn.csv")

table(sentiment_afinn$score)

####Plotting the sentiment analysis 
ggplot(data=sentiment_afinn, aes(x = factor(score))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "maroon") + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + 
  scale_y_continuous(labels = percent) + 
  xlab ("AFINN score (-5: highly negative, 5: highly positive)") + 
  ylab ("Percentage of total words") + 
  ggtitle("Sentiment analysis of tweets containing #MeToo (n=1000)")





