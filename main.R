#------------------------- IMPORTING LIBRARIES ---------------------------
library(wordcloud)
library(twitteR)
library(ROAuth)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(topicmodels)
library(ggplot2)
library(sentiment)
library(igraph)

#---------------------------TWITTER CREDENTIALS----------------------
apiKey <- 'OY9AlUG1fM49rytP7T8hqInoL'
apiSecret <- 'cjKBprKRaZoUUvwPUv7MX3NeRCehn8dSs9vBOwGjsbvAXBjI9s'
access_token <- '2195494530-V7csBiOqiffi69fZuhkZEqMRZO5xqHHWcSiBl9V'
access_token_secret <- 'CQ96GaCgeIFqq2XVPtigJnFVD9momtZqFBEsHpH8n7q3Z'

#setting up twitter third party authentication
setup_twitter_oauth(apiKey,apiSecret,access_token,access_token_secret)

#------------------------TWITTER SEARCH PARAMETERS---------------------
searchString <- c("#indianfood","@worldfoodindia")
no <- 3000
lang <- "en"
tweets <- searchTwitter(searchString,no,lang,since = "2017-10-01",until = "2017-10-07")

tweets.df <- twListToDF(tweets)
myCorpus <- Corpus(VectorSource(tweets.df$text)) #corpus for documents

#----------------------- CLEANING THE TWEETS -------------------------

myCorpus <- tm_map(myCorpus,removePunctuation)
myCorpus <- tm_map(myCorpus,content_transformer(tolower))
myCorpus <- tm_map(myCorpus,removeWords,stopwords("english"))
myCorpus <- tm_map(myCorpus, removeWords, c("like", "video"))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, stemDocument)

# ---------------------- WORD CLOUD GENERATION OF FREQUENT TWEETS----------------

tdm <- TermDocumentMatrix(myCorpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

set.seed(1234)
pal <- brewer.pal(9,"BuGn")[-(1:4)]
wordcloud(words = d$word, freq = d$freq, min.freq = 5,max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#-----------------------SEGREGATION OF TWEETS --------------------

f = list()

#function to add to list
AddItemNaive <- function(item)
{
  .GlobalEnv$f[[length(.GlobalEnv$Result)+1]] <- item
}

#food items to be isolated

foodItem <- c("chicken")
foodItem1 <- c("naan")
foodItem2 <- c("curri")

#loop through each document
for(i in 1:length(myCorpus)) {
  #condition to check if document matches content
  temp1 <- unlist(strsplit(toString(myCorpus[[i]]$content),split=" "))
  if((foodItem %in% temp1) | (foodItem1 %in% temp1) | (foodItem2 %in% temp1)) {
    #if satisfied add then add the text sentence to new corpus
    AddItemNaive(myCorpus[[i]]$content)
    print(myCorpus[[i]]$content)
  }
  temp1 <- NULL
}

foodCorpus <- as.VCorpus(f) #corpus to used for sentiment analysis

print(foodCorpus)
#------------------------TOPIC MODELING TO FIND MOST FREQUENT TWEETS---------------------------
dtm <- as.DocumentTermMatrix(tdm)
lda <- LDA(dtm,k=8)
term <- terms(lda, 7) # first 7 terms of every topic
topics <- topics(lda) # 1st topic identified for every document (tweet)
topics <- data.frame(date=as.Date(tweets.df$created), topic=topics)

ggplot(topics, aes(date, fill = term[topic])) +geom_density(position = "stack")

#------------------SENTIMENT ANALYSIS----------------------

sentiments <- sentiment(foodCorpus$content)
print(foodCorpus$content)

table(sentiments$polarity)

sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
#result <- aggregate(score ~ date, data = sentiments, sum)


# ----------------MOST RETWEETED TWEETS--------------------
selected <- which(tweets.df$retweetCount >= 15)
# plot them
dates <- strptime(tweets.df$created, format="%Y-%m-%d")

#plot 1 for twitter data
plot(x=dates, y=tweets.df$retweetCount, type="l", col="grey",
     xlab="Date", ylab="Times retweeted")
colors <- rainbow(10)[1:length(selected)]
points(dates[selected], tweets.df$retweetCount[selected],
       pch=19, col=colors)
text(dates[selected], tweets.df$retweetCount[selected],
     tweets.df$text[selected], col=colors, cex=.9)

#plot 2 
#data = head(selected,dates)
#ggplot(data,aes(x=Times-Retweeted,y=Date))+geom_point()+geom_text(label=tweets.df$retweetCount[selected],
 #                                                                 nudge_x = 0.25,nudge_y = 0.25,check_overlap = T)

# ---------------------- ASSOCIATION OF TWEETS ------------------------

#plot 1 - association
m <- as.matrix(removeSparseTerms(tdm,.96))
print(m)
m[m>=1] <- 1
m <- m %*% t(m)

g <- graph.adjacency(m,weighted = T,mode="undirected")
g <- simplify(g)

V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

set.seed(3952)
network <- layout.fruchterman.reingold(g)
plot(g,layout=network)

