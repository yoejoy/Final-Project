library(devtools)
library(twitteR)
library(dplyr)
library(purrr)

library(ROAuth)
library(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(plyr)
library(wordcloud)

library(tidytext)
library(ggplot2)
library(maps)
library(googleVis)

# read sentiment words classidication files for future sentiment score analysis
positives = readLines("positive-words.txt")
negatives = readLines("negative-words.txt")

# set up twitter API for future tweets search
api_key <- 	"JLC0JSn2DGxA0KWNFyIfnZX1P"
api_secret <- "BgECfg7Kj0LWHVQJMhVSXIC08nt0prOoLWrxw86Ut6xFPT5Byr"
access_token <- "793886972641157121-pk7WwpzF1DkuKaD3s3SCs1hhMDWuQwr"
access_token_secret <- "mIGbwWBJDCY88T6srHlyOCSjR2hKLovKE6XWHlyN3OzsD"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# [1] "Using direct authentication"
# Use a local file ('.httr-oauth'), to cache OAuth access credentials between R sessions?
# 
# 1: Yes
# 2: No
# 
# Selection: 1
# Adding .httr-oauth to .gitignore

# get tweets of related topics using twitter API, save data for future analysis
searchResults <- searchTwitter("@realdonaldtrump OR #donaldtrump", n=2500, lang="en")# Gather Tweets
saveRDS(searchResults, "results.rds")
searchResults <- readRDS("results.rds")
tweetFrame <- twListToDF(searchResults)  # Convert to a nice dF
write.csv(tweetFrame, file = "tweetframe.csv")
tweetFrame <- read.csv("tweetframe.csv", row.names = 1)
searchResults <- list(tweetFrame$text)

# sentiment scores
# based on the tweets texts, analyze the sentiments behind and score them for futher analysis
sentiment_scores = function(tweets, positive_words, negative_words, .progress='none'){
  scores = laply(tweets,
                 function(tweets, positive_words, negative_words){
                   tweets = gsub("[[:punct:]]", "", tweets)    # remove punctuation
                   tweets = gsub("[[:cntrl:]]", "", tweets)   # remove control characters
                   tweets = gsub('\\+', '', tweets)          # remove digits
                   
                   # Let's have error handling function when trying tolower
                   tryTolower = function(x){
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply
                   tweets = sapply(tweets, tryTolower)
                   # split sentence into words with str_split function from stringr package
                   word_list = str_split(tweets, "\\s+")
                   words = unlist(word_list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   positive.matches = match(words, positive_words)
                   negative.matches = match(words, negative_words)
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   positive_matches <- !is.na(positive.matches)
                   negative_matches <- !is.na(negative.matches)
                   # final score
                   score = sum(positive_matches) - sum(negative_matches)
                   return(score)
                 }, positive_words, negative_words, .progress=.progress)
  return(scores)
}

# get sentiment socres for each search
score = sentiment_scores(tweetFrame$text, positives, negatives, .progress='text')
score
par(mar = rep(2, 4))
hist(score,xlab=" ",main="Sentiment of sample tweets for Donald Trump",
     border="black",col="darkgreen")

s1 <- cbind(tweetFrame, score)
s1 <- cbind(s1, abs(score))

# get users' location using twitter API
userInfo <- lookupUsers(sentiment$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userInfo)  # Convert to a nice dF
write.csv(userFrame, file = "user.csv")
userFrame <- read.csv("user.csv", row.names = 1)

locatedUsers <- !is.na(userFrame$location)  # Keep only users with location info

# use google map API to get user's specific longitude and latitude
locations <- geocode(userFrame$location[locatedUsers])  # Use amazing API to guess
write.csv(locations, file = "userlocation.csv")
locations <- read.csv("userlocation.csv", row.names = 1)

# combine and select useful information together as a new dataframe, omit data whose locations are not in USA
userlocation <- cbind(userFrame, locations)
s2 <- merge(s1,userlocation, by="screenName")
sentiment <- data.frame(s2$screenName,s2$text,s2$retweetCount,s2$score, s2$`abs(score)`, s2$lon, s2$lat)
names(sentiment) <- c("screenName", "text", "retweetCount","score","absolute_score", "lon", "lat")
s3 <- na.omit(sentiment)
#omit locations that are not in USA
#s4 is the final data for now
s3$lon[s3$lon >= -66] <- NA
s3$lon[s3$lon <= -125] <- NA
s3$lat[s3$lat <= 24] <- NA
s3$lat[s3$lat >= 55] <- NA
s4 <- na.omit(s3)

# repeat procedures above to get more data from twitter with different topics and hashtags
# For convenience, codes to get other data are omitted here, we will load those downloaded data for further analysis.

# Dataset got from twitter under topic "#makeamericagreatagain"
s42 <- read.csv("Final-J.csv",row.names = 1)
names(s42) <- c("x","screenName", "text", "retweetCount","score","lon", "lat", "absolute_score")
s42 <- data.frame(s42$screenName,s42$text,s42$retweetCount,s42$score,s42$absolute_score,s42$lon,s42$lat)
names(s42) <- c("screenName", "text", "retweetCount","score", "absolute_score","lon", "lat")
write.csv(s42, "MAGA.csv")
data2 <- read.csv("MAGA.csv", row.names = 1)

# other data under "@realdonaldtrump OR #donaltrump"
s43 <- read.csv("Final-A.csv", row.names = 1)
s43 <- data.frame(s43$screenName,s43$text,s43$retweetCount,s43$sentiment_score,s43$abs.score,s43$lon,s43$lat)
names(s43) <- c("screenName", "text", "retweetCount","score", "absolute_score","lon", "lat")
s43

# Dataset got from twitter under topic "#nevertrump"
s44 <- read.csv("Final-A2.csv", row.names = 1)
s44 <- data.frame(s44$screenName,s44$text,s44$retweetCount,s44$sentiment_score,s44$abs.score,s44$lon,s44$lat)
names(s44) <- c("screenName", "text", "retweetCount","score", "absolute_score","lon", "lat")
write.csv(s44, "NEVER.csv")
data3 <- read.csv("NEVER.csv", row.names = 1)

# Combined Dataset got from twitter under topic "@realdonaldtrump OR #donaltrump"
s41 <- rbind(s4,s43)
write.csv(s41,"TRUMP.csv")
data1 <- read.csv("TRUMP.csv", row.names = 1)

# Dataset Summary
data3 <- read.csv("never.csv",row.names = 1)
data2 <- read.csv("maga.csv", row.names = 1)
data1 <- read.csv("trump.csv",row.names = 1)

total <- rbind(data1, data2, data3)
write.csv(total,file = "total.csv")


# wordcloud of tweets
# function to clean text for future preparation
clean.text <- function(some_txt)
{
  some_txt = gsub("&amp", "", some_txt)
  some_txt = gsub("(RT|via)((?:\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

# get cleaned tweets
tweets1 <- gettext(s41$text)  # tweets of "@realdonaldtrump or #donaldtrump"
tweets2 <- gettext(s42$text)  # tweets of "#makeamericagreatagain"
tweets3 <- gettext(s44$text)  # tweets of "nevertrump"
tweets4 <- gettext(total$text)  # all tweets together

# function to produce wordcloud
tweetwordcloud <- function(tweets){
  clean_text = clean.text(tweets)
  tweet_corpus = Corpus(VectorSource(clean_text))
  tdm = TermDocumentMatrix(tweet_corpus, control = list(removePunctuation = TRUE,stopwords = c("donald", "trump", "real", stopwords("english")), removeNumbers = TRUE, tolower = TRUE))
  m = as.matrix(tdm) #we define tdm as matrix
  word_freqs = sort(rowSums(m), decreasing=TRUE) #now we get the word orders in decreasing order
  dm = data.frame(word=names(word_freqs), freq=word_freqs) #we create our data set
  y <- wordcloud(dm$word, dm$freq,scale=c(5,0.1), max.words=200, random.order=FALSE, 
                          rot.per=0.35, use.r.layout=FALSE, colors = brewer.pal(8, "Dark2")) #and we visualize our data
  return(y)
}

# wordcloud for each topic and for the whole dataset
wordclound1 <- tweetwordcloud(tweets1)
wordclound2 <- tweetwordcloud(tweets2)
wordclound3 <- tweetwordcloud(tweets3)
wordclound4 <- tweetwordcloud(tweets4)

# mapping with sentiment scores
usa_center <- as.numeric(geocode("United States"))
USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="device")

# mapping under topic "trump"
MAP <- USAMap +
  geom_point(aes(x=lon, y=lat), col=ifelse(((s41$score>=0)),"orange", "blue"), data=s41, alpha=0.4, size=s41$absolute_score) +
  scale_size_continuous(range=s41$score)
MAP

# mapping under topic "make america great again"
MAP2 <- USAMap +
  geom_point(aes(x=lon, y=lat), col=ifelse(((s42$score>=0)),"orange", "blue"), data=s42, alpha=0.4, size=s42$absolute_score) +
  scale_size_continuous(range=s42$score)
MAP2

# mapping under topic "never trump"
MAP3 <- USAMap +
  geom_point(aes(x=lon, y=lat), col=ifelse(((s44$score>=0)),"orange", "blue"), data=s44, alpha=0.4, size=s44$absolute_score) +
  scale_size_continuous(range=s42$score)
MAP3


