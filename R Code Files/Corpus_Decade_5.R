library(tm)
library(tidyverse)
library(tidytext)

## IMPORT THE DATA
allthesongs <- read.csv("BillboardHot100_Lyrics_1965-2015.csv", stringsAsFactors=FALSE)
#view(allthesongs)

## FILTERDATA

# Filter Data into separate 10 year periods (1965-2015)

Decade_5 <- allthesongs %>%
    filter(Year>=2005,Year<=2014)

## PREPROCESS THE DATA

# Create a dataframe called "Lyrics_Decade_()" of just the Lyrics in the dataset of each decade.

Lyrics_Decade_5 <- data.frame(ID=seq(1:nrow(Decade_5)),text=Decade_5$Lyrics)

# Create Corpus for each Separate Decade

corpus_5 <- VCorpus(VectorSource(Lyrics_Decade_5),readerControl = readDataframe(Lyrics_Decade_5,"en",id = ID))

# Strip White Space

corpus_5 <- tm_map(corpus_5, stripWhitespace)

# Remove Stop Words

corpus_5 <- tm_map(corpus_5, removeWords, stopwords("english"))
corpus_5 <- tm_map(corpus_5, removeWords, stopwords("SMART"))

# Remove Punctuation

corpus_5 <- tm_map(corpus_5, removePunctuation)

# Remove Numbers

corpus_5 <- tm_map(corpus_5, removeNumbers)

# Create TDM for each seperate Corpus of Decade Lyrics

tdm_5 <- TermDocumentMatrix(corpus_5,control=list(weighting=weightTf))
tdm.Decade5.m <- as.matrix(tdm_5)
term.freq.5 <- rowSums(tdm.Decade5.m)
freq.df.5 <- data.frame(word=names(term.freq.5),frequency=term.freq.5)
freq.df.5 <- freq.df.5[order(freq.df.5[,2],decreasing = T),]

# Create an object freq.df, representing the frequency of words within each Decade Lyrics dataset. 

freq.df.5$word <- factor(freq.df.5$word,levels = unique(as.character(freq.df.5$word)))

# Now, we will use ggplot2 to visualize this data.

ggplot(freq.df.5[1:25,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="pink") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)

# Download two of the three seperate lexicons in the sentiment dictionary

get_sentiments("bing") 
get_sentiments("nrc")

# Create Document Term Matrix for Sentiment Analysis

dtm_5 <- DocumentTermMatrix(corpus_5)

# Tidy dtm and assign it to new tidy object

dtm_5_td <- tidy(dtm_5)

# Create object called bing_dtm_sentiment to conduct sentiment analysis of entire lyrics corpus with bing lexicon

bing_dtm_5_sentiment <- dtm_5_td %>% 
    inner_join(get_sentiments("bing"), by=c(term="word"))

# Determine the count of positive v negative terms in lyrics corpus

bing_dtm_5_sentiment %>% 
    count(document, sentiment, wt = count) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment= positive - negative)

# Visualize which words contributed to positive and negative sentiment

bing_dtm_5_sentiment %>%
    count(sentiment, term, wt = count) %>%
    filter(n >= 100) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(term, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Contribution to sentiment")


# Create object called nrc_dtm_sentiment to conduct sentiment analysis of entire lyrics corpus with nrc lexicon

nrc_dtm_5_sentiment <- dtm_5_td %>%
    inner_join(get_sentiments("nrc"), by=c(term="word"))
               
# Get count of each emotion in NRC lexicon
               
nrc_count <- nrc_dtm_5_sentiment %>%
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
decade <- 2005

word_frequency_decade5 <- freq.df.5[1:5,]
word_frequency_decade5 <- cbind(decade,word_frequency_decade5)
write.csv(word_frequency_decade5,"Decade5_Top25WordFrequency.csv", row.names = FALSE)

overall_sentiment_decade5 <- bing_dtm_5_sentiment %>% 
    count(document, sentiment, wt = count) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment= positive - negative)

overall_sentiment_decade5 <- overall_sentiment_decade5[, -c(1,4)]

overall_sentiment_decade5 <- cbind(decade,overall_sentiment_decade5)
overall_sentiment_decade5 <- as.data.frame(overall_sentiment_decade5)
write.csv(overall_sentiment_decade5,"Decade5_OverallSentiment.csv", row.names = FALSE)

nrc_count_decade5 <- nrc_count
nrc_count_decade5 <- cbind(decade,nrc_count_decade5)
write.csv(nrc_count_decade5,"Decade5_OverallbyEmotion.csv", row.names = FALSE)

