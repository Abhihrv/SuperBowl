
#Read the data
tweet_data<-read.csv(file.choose(), stringsAsFactors = F)

#convert language to factor
tweet_data$lang<-factor(tweet_data$lang)

#Get english tweets
levels(tweet_data$lang)
tweet_data<-subset(tweet_data, tweet_data$lang=="en")

#Taking the required columns
tweet_data<-tweet_data[,c(1,2,5,16,17,24,27,57,65,66,67,75,99,303,304,305,306,307)]


#Importing Brand and Keywords. The CSV contains unique brands and their Keywords
Brand_Keywords<-read.csv(file.choose(),stringsAsFactors = F)

#Code to check Brands in each tweet and then Create a dummy Variable for each brand
for (i in c(1:48)){
  tweet_data[i+18]<-ifelse(grepl((Brand_Keywords$Keywords[i]),tolower(tweet_data$text)),1,0)#used [i+18] as counter to start adding new columns after the 18th column
  colnames(tweet_data)[i+18]<-Brand_Keywords$Brand[i]
}

#Adding adnames to the data
ad_names<-read.csv(file.choose(),stringsAsFactors = F)
for (i in c(1:59)){
  tweet_data[i+66]<-ifelse(grepl((ad_names$Keywords[i]),tolower(tweet_data$text)),1,0)
  colnames(tweet_data)[i+66]<-paste('Ad name - ',ad_names$Ad[i])
}

#Counting Number of Tweets per brand
Number_of_Tweets<-as.data.frame("",stringsAsFactors=F)
for (i in 19:66){
  Number_of_Tweets[i-18,1]<-colnames(tweet_data)[i]
  Number_of_Tweets[i-18,2]<-sum(subset(tweet_data,tweet_data[i]==1)[i])
}
colnames(Number_of_Tweets)<-c("Brand Name","Number of Tweets")

#Counting retweets for each Brand
Number_of_Retweets<-as.data.frame("",stringsAsFactors=F)
for (i in 19:66){
  Number_of_Retweets[i-18,1]<-colnames(tweet_data)[i]
  Number_of_Retweets[i-18,2]<-sum(subset(tweet_data,tweet_data[i]==1)[10])
}
colnames(Number_of_Retweets)<-c("Brand Name","Number of Retweets")

# Quarterly Keywords
library(qdap)
install.packages('qdap')
Quarterly_Keywords<-aggregate(ad_names[,5], list(ad_names[,2]), function(x) paste(toString(x)))
Quarterly_Keywords[1,2] <- mgsub(', ', '|', Quarterly_Keywords[1,2])
Quarterly_Keywords[1,2] <- mgsub(' ', '', Quarterly_Keywords[1,2])
Quarterly_Keywords[2,2] <- mgsub(', ', '|', Quarterly_Keywords[2,2])
Quarterly_Keywords[2,2] <- mgsub(' ', '', Quarterly_Keywords[2,2])
Quarterly_Keywords[3,2] <- mgsub(', ', '|', Quarterly_Keywords[3,2])
Quarterly_Keywords[3,2] <- mgsub(' ', '', Quarterly_Keywords[3,2])
Quarterly_Keywords[4,2] <- mgsub(', ', '|', Quarterly_Keywords[4,2])
Quarterly_Keywords[4,2] <- mgsub(' ', '', Quarterly_Keywords[4,2])
colnames(Quarterly_Keywords)<-c('Quarter','Keywords')

for (i in c(1:4)){
  c<-0
  for (j in c(1:nrow(tweet_data))){
  ifelse(grepl((Quarterly_Keywords$Keywords[i]),tolower(tweet_data$text[j])),c<-c+1,c)
  }
  Quarterly_Keywords[i,3]<-c
}
write.csv(Quarterly_Keywords,file='Quarterly Tweets.csv')
#Sorting Number_of_Tweets
Number_of_Tweets<-Number_of_Tweets[order(-Number_of_Tweets$`Number of Tweets`),]
rownames(Number_of_Tweets)<-1:48
write.csv(Number_of_Tweets,file = 'Number_of_Tweets.csv')

#Sorting Number_of_Tweets
Number_of_Retweets<-Number_of_Retweets[order(-Number_of_Retweets$`Number of Retweets`),]
rownames(Number_of_Retweets)<-1:48
 order(-Number_of_Retweets$`Number of Retweets`)

 ## sentiment analysis
 library(tm)
 
 ## building CORPUS
 library(tm)
 corpus<- iconv(tweet_data2$text)
 corpus<- Corpus(VectorSource(corpus))
 inspect(corpus[1:5])
 
 ## data cleaning of text
 corpus<- tm_map(corpus, tolower)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removePunctuation)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removeNumbers)
 inspect(corpus[1:5])
 
 clean_set<-tm_map(corpus, removeWords, stopwords('english'))
 inspect(clean_set[1:5])
 
 removeURL<- function(x) gsub('http[[:alnum:]]*', '', x)
 clean_set<-tm_map(clean_set, content_transformer(removeURL))
 inspect(clean_set[1:5])
 
 clean_set <- tm_map(clean_set, stripWhitespace)
 inspect(clean_set[1:5])
 
 # converting the unstructured data to structured data ## TERM Document Matrix
 tdm <- TermDocumentMatrix(clean_set)
 tdm
 tdm <- as.matrix(tdm)
 ###############################################################################
 
 marvel_tweets<-as.data.frame("")
 marvel_tweets<-subset((tweet_data2$text), (tweet_data2$Marvel==1))
 
 budlight_tweets<-as.data.frame("")
 budlight_tweets<-subset((tweet_data2$text), (tweet_data2$Budlight==1))
 
 hbobud_tweets<-as.data.frame("")
 hbobud_tweets<-subset((tweet_data2$text), (tweet_data2$HBO_Budweiser==1))
 
 doritos_tweets<-as.data.frame("")
 doritos_tweets<-subset((tweet_data2$text), (tweet_data2$Doritos==1))
 
 bk_tweets<-as.data.frame("")
 bk_tweets<-subset((tweet_data2$text), (tweet_data2$Burger_King==1))
 
 verizon_tweets<-as.data.frame("")
 verizon_tweets<-subset((tweet_data2$text), (tweet_data2$Verizon==1))
 
 ########## CLEANING  MARVEL TWEETS ##################
 ## building CORPUS
 library(tm)
 corpus_marvel<- iconv(marvel_tweets)
 corpus<- Corpus(VectorSource(corpus_marvel))
 inspect(corpus[1:5])
 
 ## data cleaning of text
 corpus<- tm_map(corpus, tolower)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removePunctuation)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removeNumbers)
 inspect(corpus[1:5])
 
 clean_set<-tm_map(corpus, removeWords, stopwords('english'))
 inspect(clean_set[1:5])
 
 removeURL<- function(x) gsub('http[[:alnum:]]*', '', x)
 clean_set<-tm_map(clean_set, content_transformer(removeURL))
 inspect(clean_set[1:5])
 
 clean_set <- tm_map(clean_set, stripWhitespace)
 inspect(clean_set[1:5])
 
 ##################MARVEL TWEETS ####################SENTIMENT SCORES
 
 marvel_tweets.df<-as.vector(marvel_tweets)
 emotion_marvel.df<-get_nrc_sentiment(marvel_tweets.df)
 
 sent.value_marvel <- (get_sentiment(marvel_tweets.df))
 
 positive_tweets_marvel<-marvel_tweets.df[sent.value_marvel>0]
 View(positive_tweets_marvel)
 
 negative_tweets_marvel<-marvel_tweets.df[sent.value_marvel<0]
 View(negative_tweets_marvel)
 neutral_tweets_marvel<-marvel_tweets.df[sent.value_marvel==0]
 
 
 
 
 ################### CLEANING BUDLIGHT ###############
 ## building CORPUS
 library(tm)
 corpus_budlight<- iconv(budlight_tweets)
 corpus<- Corpus(VectorSource(corpus_budlight))
 inspect(corpus[1:5])
 
 ## data cleaning of text
 corpus<- tm_map(corpus, tolower)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removePunctuation)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removeNumbers)
 inspect(corpus[1:5])
 
 clean_set<-tm_map(corpus, removeWords, stopwords('english'))
 inspect(clean_set[1:5])
 
 removeURL<- function(x) gsub('http[[:alnum:]]*', '', x)
 clean_set<-tm_map(clean_set, content_transformer(removeURL))
 inspect(clean_set[1:5])
 
 clean_set <- tm_map(clean_set, stripWhitespace)
 inspect(clean_set[1:5])
 
 ###################### BUDLIGHT #################### SENTIMENT SCORES
 
 budlight_tweets.df<-as.vector(budlight_tweets)
 emotion_budlight.df<-get_nrc_sentiment(budlight_tweets.df)
 
 sent.value_budlight <- (get_sentiment(budlight_tweets.df))
 
 positive_tweets_budlight<-budlight_tweets.df[sent.value_budlight>0]
 View(positive_tweets_budlight)
 
 negative_tweets_budlight<-budlight_tweets.df[sent.value_budlight<0]
 View(negative_tweets_budlight)
 neutral_tweets_budlight<-budlight_tweets.df[sent.value_budlight==0]
 
 
 
 
 
 ################# HBO_BUDWEISER ####################CLEANING
 ## building CORPUS
 library(tm)
 corpus_hbo<- iconv(hbobud_tweets)
 corpus<- Corpus(VectorSource(corpus_hbobud))
 inspect(corpus[1:5])
 
 ## data cleaning of text
 corpus<- tm_map(corpus, tolower)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removePunctuation)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removeNumbers)
 inspect(corpus[1:5])
 
 clean_set<-tm_map(corpus, removeWords, stopwords('english'))
 inspect(clean_set[1:5])
 
 removeURL<- function(x) gsub('http[[:alnum:]]*', '', x)
 clean_set<-tm_map(clean_set, content_transformer(removeURL))
 inspect(clean_set[1:5])
 
 clean_set <- tm_map(clean_set, stripWhitespace)
 inspect(clean_set[1:5])
 
 
 ############### HBO_BUDWEISER ###################### SENTIMENT SCORES
 
 hbobud_tweets.df<-as.vector(hbobud_tweets)
 emotion_hbobud.df<-get_nrc_sentiment(hbobud_tweets.df)
 
 sent.value_hbobud <- (get_sentiment(hbobud_tweets.df))
 
 positive_tweets_hbobud<-hbobud_tweets.df[sent.value_hbobud>0]
 View(positive_tweets_hbobud)
 
 negative_tweets_hbobud<-hbobud_tweets.df[sent.value_hbobud<0]
 View(negative_tweets_hbobud)
 neutral_tweets_hbobud<-hbobud_tweets.df[sent.value_hbobud==0]
 View(neutral_tweets_hbobud)
 
 
 
 
 
 ################ DORITOS ########################## CLEANING
 ## building CORPUS
 library(tm)
 corpus_doritos<- iconv(doritos_tweets)
 corpus<- Corpus(VectorSource(corpus_doritos))
 inspect(corpus[1:5])
 
 ## data cleaning of text
 corpus<- tm_map(corpus, tolower)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removePunctuation)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removeNumbers)
 inspect(corpus[1:5])
 
 clean_set<-tm_map(corpus, removeWords, stopwords('english'))
 inspect(clean_set[1:5])
 
 removeURL<- function(x) gsub('http[[:alnum:]]*', '', x)
 clean_set<-tm_map(clean_set, content_transformer(removeURL))
 inspect(clean_set[1:5])
 
 clean_set <- tm_map(clean_set, stripWhitespace)
 inspect(clean_set[1:5])
 
 
 ##################### DORITOS ####################### SENTIMENT SCORES
 
 doritos_tweets.df<-as.vector(doritos_tweets)
 emotion_doritos.df<-get_nrc_sentiment(doritos_tweets.df)
 
 sent.value_doritos <- (get_sentiment(doritos_tweets.df))
 
 positive_tweets_doritos<-doritos_tweets.df[sent.value_doritos>0]
 View(positive_tweets_doritos)
 
 negative_tweets_doritos<-doritos_tweets.df[sent.value_doritos<0]
 View(negative_tweets_doritos)
 neutral_tweets_doritos<-doritos_tweets.df[sent.value_doritos==0]
 View(neutral_tweets_doritos)
 
 
 
 
 
 
 ################ BURGER KING ####################### CLEANING
 ## building CORPUS
 library(tm)
 corpus_bk<- iconv(bk_tweets)
 corpus<- Corpus(VectorSource(corpus_bk))
 inspect(corpus[1:5])
 
 ## data cleaning of text
 corpus<- tm_map(corpus, tolower)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removePunctuation)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removeNumbers)
 inspect(corpus[1:5])
 
 clean_set<-tm_map(corpus, removeWords, stopwords('english'))
 inspect(clean_set[1:5])
 
 removeURL<- function(x) gsub('http[[:alnum:]]*', '', x)
 clean_set<-tm_map(clean_set, content_transformer(removeURL))
 inspect(clean_set[1:5])
 
 clean_set <- tm_map(clean_set, stripWhitespace)
 inspect(clean_set[1:5])
 
 
 ################## BURGER KING ###################### SENTIMENT SCORES
 
 bk_tweets.df<-as.vector(bk_tweets)
 emotion_bk.df<-get_nrc_sentiment(bk_tweets.df)
 
 sent.value_bk <- (get_sentiment(bk_tweets.df))
 
 positive_tweets_bk<-bk_tweets.df[sent.value_bk>0]
 View(positive_tweets_bk)
 
 negative_tweets_bk<-bk_tweets.df[sent.value_bk<0]
 View(negative_tweets_bk)
 
 neutral_tweets_bk<-bk_tweets.df[sent.value_bk==0]
 View(neutral_tweets_bk)
 
 
 
 
 #################### VERIZON ########################## CLEANING
 
 ## building CORPUS
 library(tm)
 corpus_verizon<- iconv(verizon_tweets)
 corpus<- Corpus(VectorSource(corpus_verizon))
 inspect(corpus[1:5])
 
 ## data cleaning of text
 corpus<- tm_map(corpus, tolower)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removePunctuation)
 inspect(corpus[1:5])
 
 corpus<-tm_map(corpus, removeNumbers)
 inspect(corpus[1:5])
 
 clean_set<-tm_map(corpus, removeWords, stopwords('english'))
 inspect(clean_set[1:5])
 
 removeURL<- function(x) gsub('http[[:alnum:]]*', '', x)
 clean_set<-tm_map(clean_set, content_transformer(removeURL))
 inspect(clean_set[1:5])
 
 clean_set <- tm_map(clean_set, stripWhitespace)
 inspect(clean_set[1:5])
 
 
 
 ################## VERIZON ##################### SENTIMENT SCORES
 
 verizon_tweets.df<-as.vector(verizon_tweets)
 emotion_verizon.df<-get_nrc_sentiment(verizon_tweets.df)
 
 sent.value_verizon <- (get_sentiment(verizon_tweets.df))
 
 positive_tweets_verizon<-verizon_tweets.df[sent.value_verizon>0]
 View(positive_tweets_verizon)
 
 negative_tweets_verizon<-verizon_tweets.df[sent.value_verizon<0]
 View(negative_tweets_verizon)
 
 neutral_tweets_verizon<-verizon_tweets.df[sent.value_verizon==0]
 View(neutral_tweets_verizon)