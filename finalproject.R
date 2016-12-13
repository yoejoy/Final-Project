list.of.packages <- c("devtools","twitteR","dplyr","purrr","ROAuth","RCurl","stringr","tm","ggmap",
                      "plyr","wordcloud","tidytext","maps","googleVis","leaflet","shiny","ggplot2",
                      "MASS","lubridate","scales","wesanderson","tidyr","broom","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

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
library(MASS)
library(tidytext)
library(ggplot2)
library(maps)
library(googleVis)

library(lubridate)
library(scales)
library(tidyr)
library(broom)
library(reshape2)

library(dplyr)
library(leaflet)
library(shiny)
library(memoise)
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
write.csv(tweetFrame, file = "tweetframe2.csv")
tweetFrame <- read.csv("tweetframe2.csv", row.names = 1)
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
userInfo <- lookupUsers(s1$screenName)  # Batch lookup of user info
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
names(s42) <- c("screenName", "text", "retweetCount","score","lon", "lat", "absolute_score")
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

# Dataset Collection Summary
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
tweets1 <- gettext(data1$text)  # tweets of "@realdonaldtrump or #donaldtrump"
tweets2 <- gettext(data2$text)  # tweets of "#makeamericagreatagain"
tweets3 <- gettext(data3$text)  # tweets of "nevertrump"
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

# word clouds for each topic and for the whole dataset
wordclound1 <- tweetwordcloud(tweets1)
wordclound2 <- tweetwordcloud(tweets2)
wordclound3 <- tweetwordcloud(tweets3)
wordclound4 <- tweetwordcloud(tweets4)


library(MASS)
d3 = read.csv("NEVER.csv", row.names = 1)
d2 = read.csv("MAGA.csv", row.names = 1)
d4 = read.csv("total.csv", row.names = 1)

#add a column name "hashtag"
d3$hashtag=rep("NEVERTRUMP", 1279)
d2$hashtag=rep("MAGA", 1238)
# combine the two datasets into one for comparison
d5 <- rbind(d3, d2)

score.response = na.omit(d4$score)

summary(score.response)
retweet <- total$retweetCount                # sample standard deviation 
summary(retweet)
## Shapiro.test, confidence level= 95%. If p < 0.05, our data deviates from deviation.

data_score= d4$score
shapiro.test(data_score)

## Plot using a qqplot
qqnorm(data_score);qqline(data_score, col = 2)

## Hypothesis Testing (Ho: People are repulsed by Trump even after election. score = -1  )

alpha=0.05
mu0 = -1                        # hypothesized value 
t = (mean(score.response) - mu0)/(s/sqrt(n)) 
t                      # test statistic 
#compute the critical values at .05 significance leveL                      
t.half.alpha = qt(1 - alpha/2, df= n - 1) 
cv<- c(-t.half.alpha, t.half.alpha) 
cv

# Anova Table -- analyze the relationship between sentiment and retweet number
total = read.csv("total.csv")
aov.out = aov(total$retweetCount~total$score, data=total)
summary(aov.out)
model = lm(total$retweetCount~total$score)
summary(model)
ggplot(data = total, aes(x = score, y = retweetCount)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  scale_y_continuous(limits = c(0, 15000))
theme_bw()

# Anova Table -- check if there is any difference in the retweet number between #NeverTrump and #MAGA groups
# compare the retweet mean difference across the 10 score groups from -5 to 5
results <- aov(retweetCount ~ factor(score), data=d5) 
summary(results)
summary.lm(results)

# compare the score mean difference between the two hashtag groups
results <- aov(score ~ factor(hashtag), data=d5) 
summary(results)
summary.lm(results)

# use unnest_tokens function to remove some "stopwords"
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- d4 %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(tweets = text) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tweet_words


#find the most common words in tweets
commonword <- tweet_words %>%
  dplyr::count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()

commonword
#
bing <- get_sentiments("bing")
bing
bing_word_counts <- tweet_words %>%
  inner_join(bing) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts <- bing_word_counts[-1, ]
bing_word_counts

#remove the top key word "trump"
bing_word_counts %>%
  filter(n > 20) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment")

# mapping with sentiment scores
usa_center <- as.numeric(geocode("United States"))
USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="device")

# mapping under topic "trump"
MAP <- USAMap +
  geom_point(aes(x=lon, y=lat), col=ifelse(((data1$score>=0)),"brown1", "blue"), data=data1, alpha=0.4, size=data1$absolute_score) +
  scale_size_continuous(range=data1$score)+
  ggtitle("U.S Mapping under #DonaldTrump")
MAP

# mapping under topic "make america great again"
MAP2 <- USAMap +
  geom_point(aes(x=lon, y=lat), col=ifelse(((data2$score>=0)),"brown1", "blue"), data=data2, alpha=0.4, size=data2$absolute_score) +
  scale_size_continuous(range=data2$score)+
  ggtitle("U.S Mapping under #MakeAmericaGreatAgain")
MAP2

# mapping under topic "never trump"
MAP3 <- USAMap +
  geom_point(aes(x=lon, y=lat), col=ifelse(((data3$score>=0)),"brown1", "blue"), data=data3, alpha=0.4, size=data3$absolute_score) +
  scale_size_continuous(range=data3$score)+
  ggtitle("U.S Mapping under #NeverTrump")
MAP3

# mapping with all data
MAP4 <- USAMap +
  geom_point(aes(x=lon, y=lat), col=ifelse(((total$score>=0)),"brown1", "blue"), data=total, alpha=0.4, size=total$absolute_score) +
  scale_size_continuous(range=total$score)+
  ggtitle("U.S Mapping under the Total Dataset")
MAP4

# sentiment across different states
florida_center <- as.numeric(geocode("Florida"))
FLMap = get_googlemap("Florida", zoom=6, maptype = "roadmap", crop = FALSE)
FL <- ggmap(FLMap) +
  geom_point(aes(x=lon, y=lat), col=ifelse(((total$score>=0)),"brown1", "blue"), data=total, alpha=0.4, size=total$absolute_score) +
  scale_size_continuous(range=total$score)
FL

CA_center <- as.numeric(geocode("California"))
CAMap <- get_googlemap("California", zoom = 6, maptype = "roadmap", crop = FALSE)
CA <- ggmap(CAMap) +
  geom_point(aes(x=lon, y=lat), col=ifelse(((total$score>=0)),"brown1", "blue"), data=total, alpha=0.4, size=total$absolute_score) +
  scale_size_continuous(range=total$score)
CA

MI_center <- as.numeric(geocode("Michigan"))
MIMap <- get_googlemap("Michigan", zoom = 6, maptype = "roadmap", crop = FALSE)
MI <- ggmap(MIMap) +
  geom_point(aes(x=lon, y=lat), col=ifelse(((total$score>=0)),"brown1", "blue"), data=total, alpha=0.4, size=total$absolute_score) +
  scale_size_continuous(range=total$score)
MI


# shiny
# wordcloud
maga <- read.csv("MAGA.csv", row.names = 1)
never <- read.csv("NEVER.csv", row.names = 1)
trump <- read.csv("TRUMP.csv", row.names = 1)
# The list of valid books
write.table(maga$text, "maga.txt", fileEncoding='utf8')
write.table(never$text, "never.txt",fileEncoding='utf8' )
write.table(trump$text, "trump.txt",fileEncoding='utf8' )

books <- list("#MakeAmericanGreatAgain" = "maga" ,
              "#NeverTrump" =  "never",
              "#DonaldTrump" = "trump")

# Using "memoise" to automatically cache the results

getTermMatrix <- memoise(function(book) {
  
  text <- readLines(sprintf("./%s.txt", book),encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, PlainTextDocument)
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, content_transformer(function(x) iconv(enc2utf8(x), sub = "byte")))
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "donald", "trump","real"))
  
  try.tolower = function(x){
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  myCorpus = sapply(myCorpus, try.tolower)
  myCorpus = myCorpus[myCorpus != ""]
  names(myCorpus) = NULL
  return(myCorpus)
  
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  word_freqs = sort(rowSums(m), decreasing = TRUE)
  dm = data.frame(word=names(word_freqs), freq=word_freqs)
})

###server
server<- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    wordcloud_rep(dm$word,dm$freq, scale=c(5,1),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })
}

###ui
ui<-fluidPage(
  # Application title
  titlePanel("Word Cloud"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a hashtag:",
                  choices = books),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 100, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot")
    )
  )
)
shinyApp(ui, server)


# retweetCount
s5 <- read.csv("total.csv", row.names = 1)
s5 <- mutate(s5, popup_info=paste(sep = "<br/>", paste0("<b>", s5$screenName, "</b>"), paste0 ("retweet count: ", s5$retweetCount), paste0 ("sentiment score: ",s5$score)))

factorpal<- colorFactor(
  palette = "RdPu",
  domain = c(s5$retweetCount),
  level = NULL,
  ordered= FALSE, 
  na.color = "#808080"
)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("PopularityMap"),
  p()
)

server <- function(input, output, session) {
 
  output$PopularityMap <- renderLeaflet({
    leaflet(s5) %>%
      addTiles(
      ) %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(lng=~lon,
                       lat = ~lat, 
                       popup= ~popup_info,
                       radius = 3,
                       color = ~factorpal(s5$retweetCount),
                       fillOpacity = 1) %>%
      addProviderTiles("Stamen.Watercolor") %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
    
  })
  
}

shinyApp(ui, server)
