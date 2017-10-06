library(wordcloud)
library(twitteR)
library(ROAuth)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(topicmodels)
library(ggplot2)
library(sentiment)

#Twitter Credentials
apiKey <- 'OY9AlUG1fM49rytP7T8hqInoL'
apiSecret <- 'cjKBprKRaZoUUvwPUv7MX3NeRCehn8dSs9vBOwGjsbvAXBjI9s'
access_token <- '2195494530-V7csBiOqiffi69fZuhkZEqMRZO5xqHHWcSiBl9V'
access_token_secret <- 'CQ96GaCgeIFqq2XVPtigJnFVD9momtZqFBEsHpH8n7q3Z'

#setting up twitter third party authentication
setup_twitter_oauth(apiKey,apiSecret,access_token,access_token_secret)

#twitter search parameters
searchString <- c('#indianfood')
no <- 100
lang <- "en"

tweets <- searchTwitter(searchString,no,lang,since = "2017-10-04",until = "2017-10-06")
tweets.df <- twListToDF(tweets)
myCorpus <- Corpus(VectorSource(tweets.df$text)) #corpus for documents

myCorpus <- tm_map(myCorpus,removePunctuation)
myCorpus <- tm_map(myCorpus,content_transformer(tolower))
myCorpus <- tm_map(myCorpus,removeWords,stopwords("english"))
myCorpus <- tm_map(myCorpus, removeWords, c("like", "video","agirlfromodisha","foodi","travel"))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, stemDocument)

# Word cloud part
tdm <- TermDocumentMatrix(myCorpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
pal <- brewer.pal(9,"BuGn")[-(1:4)]
wordcloud(words = d$word, freq = d$freq, min.freq = 5,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))



#segration of tweets based on a twitter code

f = list()

#function to add to list
AddItemNaive <- function(item)
{
  .GlobalEnv$f[[length(.GlobalEnv$Result)+1]] <- item
}

#loop through each document
for(i in 1:length(myCorpus)) {
  #condition to check if document matches content
  temp1 <- unlist(strsplit(toString(myCorpus[[i]]$content),split=" "))
  if("crab" %in% temp1) {
    #if satisfied add then add the text sentence to new corpus
    AddItemNaive(myCorpus[[i]]$content)
  }
  temp1 <- NULL
}

foodCorpus <- as.VCorpus(f)
print(foodCorpus$content)

#topic modelling terms part
dtm <- as.DocumentTermMatrix(tdm)
lda <- LDA(dtm,k=8)
term <- terms(lda, 7) # first 7 terms of every topic
topics <- topics(lda) # 1st topic identified for every document (tweet)
topics <- data.frame(date=as.Date(tweets.df$created), topic=topics)

ggplot(topics, aes(date, fill = term[topic])) +geom_density(position = "stack")

#code to plot sentiment
sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)

#create sentiments of scores
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.Date(tweets.df$created)
#result <- aggregate(score ~ date, data = sentiments, sum)

source("twitterMap.R")
twitterMap("@narendramodi",fileName ="abc.pdf",nMax = 100)