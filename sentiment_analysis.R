library(Rserve) #External Connection for Tableau
Rserve(args = "--no-save") #External Connection for Tableau
library(xlsx)
library(tm)
library(ROAuth)
library(openssl)
library(httpuv)
library(twitteR)
library(SnowballC)
library(syuzhet)
library(dplyr)
library(scales)
library(SentimentAnalysis)
library(sentimentr)

#Twitter API's and tokens
consumer_key <- '*****'
consumer_secret <- '****'
access_token <- '****'
access_secret <- '****'
setup_twitter_oauth(consumer_key, consumer_secret, access_token , access_secret)

#Extract Data From Twitter
realDonaldTrump <- searchTwitter("@realDonaldTrump", n=10000, lang="en")
trump_tweets <- twListToDF(realDonaldTrump)
View(trump_tweets)

#Create and Write into Excel Sheet
write.xlsx(trump_tweets, file="E:/BI/datasets/data.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)

#Create Courpus w.r.t text column
corpus <- iconv(trump_tweets$text, to="utf-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Convert all tweets to lowercase
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

#Replace rt(starting blank) with ""
removert <- function(x) gsub('rt', "", x)
corpus <- tm_map(corpus, content_transformer(removert))
inspect(corpus[1:5])

#Replace username with ""
removeUsername <- function(x) gsub('@[[:alnum:]]*', "", x)
corpus <- tm_map(corpus, content_transformer(removeUsername))
inspect(corpus[1:5])

#Remove Puntuation Marks
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

#Replace URL's with ""
removeURL <- function(x) gsub('http[[:alnum:]]*', "", x)
corpus <- tm_map(corpus, content_transformer(removeURL))
inspect(corpus[1:5])

#Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus[1:5])

#Remove Numbers
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

#Remove Weird symbols(emojis)
removenonascii <- function(x) gsub("[^a-zA-Z0-9 ]","\\1", x)
corpus <- tm_map(corpus, content_transformer(removenonascii))
inspect(corpus[1:5])

#Replace starting blank with ""
remove_startblank <- function(x) gsub('^ ', "", x)
corpus <- tm_map(corpus, content_transformer(remove_startblank))
inspect(corpus[1:5])

#Replace end blank space with ""
remove_endblank <- function(x) gsub(' $', "", x)
corpus <- tm_map(corpus, content_transformer(remove_endblank))
inspect(corpus[1:5])

#Remove White Space
corpus <- tm_map(corpus, stripWhitespace)
inspect(corpus[1:5])

#Get Common Words in df
dtm <- TermDocumentMatrix(corpus) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
head(df)
#Write df into Excel sheet
write.xlsx(df, file="E:/BI/datasets/commonwords.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)

#Convert Corpus class to character
trump_tweets_data <- as.character(corpus)
class(trump_tweets_data)


#Create corpus for status_source column
corpus2 <- iconv(trump_tweets$statusSource, to = "utf-8")
corpus2 <- Corpus(VectorSource(corpus2))
inspect(corpus2[1:5])
#Replace html tags with ""
removehref <- function(x) gsub('<.*?>', "", x)
corpus2 <- tm_map(corpus2, content_transformer(removehref))
inspect(corpus2[1:5])
#Convert corpus to dataframe
status <- data.frame(text = get("content", corpus2))
#Write into Excel Sheet
write.xlsx(status, file="E:/BI/datasets/statussource.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)


#Sentiment Analysis

#Get sentiment
trump_sentiment <- get_nrc_sentiment(trump_tweets_data)
#Write into Excel Sheet
write.xlsx(trump_sentiment, file="E:/BI/sentiment.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)

#Get Sentiment Scores
trump_sentimentscores<-data.frame(colSums(trump_sentiment[,]))
#Write into Excel Sheet
write.xlsx(trump_sentimentscores, file="E:/BI/datasets/scores.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)

names(trump_sentimentscores)<-"Score"
trump_sentimentscores<-cbind("sentiment"=rownames(trump_sentimentscores),trump_sentimentscores)
rownames(trump_sentimentscores)<-NULL

