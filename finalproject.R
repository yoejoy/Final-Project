library(devtools)
library(twitteR)
library(dplyr)
library(purrr)

library(ROAuth)
library(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(wordcloud)

library(tidytext)

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

searchResults <- searchTwitter("@RealDonaldTrump OR #donald trump", n=2500, lang="en")# Gather Tweets 
tweetFrame <- twListToDF(searchResults)  # Convert to a nice dF

# wordcloud of tweets
tweets.text = laply(searchResults,function(t)t$getText())
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

clean_text = clean.text(tweets.text)

tweet_corpus = Corpus(VectorSource(clean_text))
tdm = TermDocumentMatrix(tweet_corpus, control = list(removePunctuation = TRUE,stopwords = c("donald", "trump", stopwords("english")), removeNumbers = TRUE, tolower = TRUE))
m = as.matrix(tdm) #we define tdm as matrix
word_freqs = sort(rowSums(m), decreasing=TRUE) #now we get the word orders in decreasing order
dm = data.frame(word=names(word_freqs), freq=word_freqs) #we create our data set
wordcloud(dm$word, dm$freq,scale=c(5,0.1), max.words=200, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors = brewer.pal(8, "Dark2")) #and we visualize our data


library(ggplot2)
library(maps)
library(googleVis)

positives = readLines("positive-words.txt")
negatives = readLines("negative-words.txt")

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



score = sentiment_scores(tweetFrame$text, positives, negatives, .progress='text')
score
par(mar = rep(2, 4))
hist(score,xlab=" ",main="Sentiment of sample tweets for Donald Trump",
     border="black",col="darkgreen")

sentiment <- cbind(tweetFrame, score)

# get users' location
userInfo <- lookupUsers(sentiment$screenName)  # Batch lookup of user info
userFrame <- twListToDF(userInfo)  # Convert to a nice dF

locatedUsers <- !is.na(userFrame$location)  # Keep only users with location info

locations <- geocode(userFrame$location[locatedUsers])  # Use amazing API to guess
userlocation <- cbind(userFrame, locations)

sentiment <- merge(sentiment,userlocation, by="screenName")


library(XML)
american_results<-subset(locations, grepl(", USA", locations$interpretedPlace)==TRUE)
# approximate lat/lon from textual location data.
with(locations, plot(lon, lat))


sentiment_tweets <- sentiment[complete.cases(sentiment),]