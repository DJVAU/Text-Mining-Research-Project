library(tm)
library(tidyverse)
library(tidytext)
library(tidyr)

## IMPORT THE DATA
allthesongs <- read.csv("BillboardHot100_Lyrics_1965-2015.csv", stringsAsFactors=FALSE)
#view(allthesongs)

## FILTERDATA

# Filter Data into separate 10 year periods (1965-2015)

Decade_2 <- allthesongs %>% 
    filter(Year>=1975,Year<=1984)

## PREPROCESS THE DATA

# Create a dataframe called "Lyrics_Decade_()" of just the Lyrics in the dataset of each decade.

Lyrics_Decade_2 <- data.frame(ID=seq(1:nrow(Decade_2)),text=Decade_2$Lyrics)

# Create Corpus for each Separate Decade

corpus_2 <- VCorpus(VectorSource(Lyrics_Decade_2),readerControl = readDataframe(Lyrics_Decade_2,"en",id = ID))

# Strip White Space

corpus_2 <- tm_map(corpus_2, stripWhitespace)

# Remove Stop Words

corpus_2 <- tm_map(corpus_2, removeWords, stopwords("english"))
corpus_2 <- tm_map(corpus_2, removeWords, stopwords("SMART"))

# Remove Punctuation

corpus_2 <- tm_map(corpus_2, removePunctuation)

# Remove Numbers

corpus_2 <- tm_map(corpus_2, removeNumbers)

# Create TDM for each seperate Corpus of Decade Lyrics

tdm_2 <- TermDocumentMatrix(corpus_2,control=list(weighting=weightTf))
tdm.Decade2.m <- as.matrix(tdm_2)
term.freq.2 <- rowSums(tdm.Decade2.m)
freq.df.2 <- data.frame(word=names(term.freq.2),frequency=term.freq.2)
freq.df.2 <- freq.df.2[order(freq.df.2[,2],decreasing = T),]

# Create an object freq.df, representing the frequency of words within each Decade Lyrics dataset. 

freq.df.2$word <- factor(freq.df.2$word,levels = unique(as.character(freq.df.2$word)))

# Now use ggplot2 to visualize this data.

ggplot(freq.df.2[1:25,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="skyblue") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)

# Download two of the three seperate lexicons in the sentiment dictionary

get_sentiments("bing") 
get_sentiments("nrc")

# Create Document Term Matrix for Sentiment Analysis

dtm_2 <- DocumentTermMatrix(corpus_2)

# Tidy dtm and assign it to new tidy object

dtm_2_td <- tidy(dtm_2)

# Create object called bing_dtm_sentiment to conduct sentiment analysis of entire lyrics corpus with bing lexicon

bing_dtm_2_sentiment <- dtm_2_td %>% 
    inner_join(get_sentiments("bing"), by=c(term="word"))

# Determine the count of positive v negative terms in lyrics corpus

bing_dtm_2_sentiment %>% 
    count(document, sentiment, wt = count) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment= positive - negative)

# Visualize which words contributed to positive and negative sentiment

bing_dtm_2_sentiment %>%
    count(sentiment, term, wt = count) %>%
    filter(n >= 100) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(term, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Contribution to sentiment")


# Create object called nrc_dtm_sentiment to conduct sentiment analysis of entire lyrics corpus with nrc lexicon

nrc_dtm_2_sentiment <- dtm_2_td %>%
    inner_join(get_sentiments("nrc"), by=c(term="word"))

# Get count of each emotion in NRC lexicon
               
nrc_count <- nrc_dtm_2_sentiment %>%
    group_by(sentiment) %>%
    filter(!sentiment %in% c("positive","negative")) %>% 
    count(sentiment,sort=TRUE)
               
nrc_count
               
# Plot emotions of NRC lexicon to determine highest emotion in corpus lyrics in descending order
nrc_count %>%
    ggplot(aes(x=reorder(sentiment,-n),y=n,fill=sentiment)) +
    geom_bar(stat="identity") +
    labs(y="Sentiment Scores", x=NULL)
               
               
# Store Overall patterns for Each Decade in separate csv files for creation of Overall Patterns Graphs over decades
decade <- 1975

word_frequency_decade2 <- freq.df.2[1:5,]
word_frequency_decade2 <- cbind(decade,word_frequency_decade2)
write.csv(word_frequency_decade2,"Decade2_Top25WordFrequency.csv", row.names = FALSE)

overall_sentiment_decade2 <- bing_dtm_2_sentiment %>% 
    count(document, sentiment, wt = count) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment= positive - negative)

overall_sentiment_decade2 <- overall_sentiment_decade2[, -c(1,4)]
overall_sentiment_decade2 <- cbind(decade,overall_sentiment_decade2)
overall_sentiment_decade2 <- as.data.frame(overall_sentiment_decade2)
write.csv(overall_sentiment_decade2,"Decade2_OverallSentiment.csv", row.names = FALSE)

nrc_count_decade2 <- nrc_count
nrc_count_decade2 <- cbind(decade,nrc_count_decade2)
write.csv(nrc_count_decade2,"Decade2_OverallbyEmotion.csv", row.names = FALSE)

