#importing libraries
library(twitteR)
library(ROAuth)
library(tm)
library(xlsx)
library(plyr)
library(stringr)

#Twitter Credentials
apiKey <- 'OY9AlUG1fM49rytP7T8hqInoL'
apiSecret <- 'cjKBprKRaZoUUvwPUv7MX3NeRCehn8dSs9vBOwGjsbvAXBjI9s'
access_token <- '2195494530-V7csBiOqiffi69fZuhkZEqMRZO5xqHHWcSiBl9V'
access_token_secret <- 'CQ96GaCgeIFqq2XVPtigJnFVD9momtZqFBEsHpH8n7q3Z'

#setting up twitter third party authentication
setup_twitter_oauth(apiKey,apiSecret,access_token,access_token_secret)

#retriving twitter based on hashtags 
foodData <- 