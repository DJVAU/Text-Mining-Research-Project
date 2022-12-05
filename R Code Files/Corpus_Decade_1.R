library(tm)
library(tidyverse)
library(tidytext)
library(tidyr)

## IMPORT THE DATA
allthesongs <- read.csv("BillboardHot100_Lyrics_1965-2015.csv", stringsAsFactors=FALSE)
#view(allthesongs)

## FILTERDATA

# Filter Data into separate 10 year periods (1965-2015)

Decade_1 <- allthesongs %>% 
    filter(Year>=1965,Year<=1974)

## PREPROCESS THE DATA

# Create a dataframe called "Lyrics_Decade_()" of just the Lyrics in the dataset of each decade.

Lyrics_Decade_1 <- data.frame(ID=seq(1:nrow(Decade_1)),text=Decade_1$Lyrics)

# Create VCorpus Object for each Separate Decade

corpus_1 <- VCorpus(VectorSource(Lyrics_Decade_1),readerControl = readDataframe(Lyrics_Decade_1,"en",id = ID))

# Strip White Space

corpus_1 <- tm_map(corpus_1, stripWhitespace)

# Remove Stop Words

corpus_1 <- tm_map(corpus_1, removeWords, stopwords("english"))
corpus_1 <- tm_map(corpus_1, removeWords, stopwords("SMART"))

# Remove Punctuation

corpus_1 <- tm_map(corpus_1, removePunctuation)

# Remove Numbers

corpus_1 <- tm_map(corpus_1, removeNumbers)

# Create TDM for each separate Corpus of Decade Lyrics

tdm_1 <- TermDocumentMatrix(corpus_1,control=list(weighting=weightTf))
tdm.Decade1.m <- as.matrix(tdm_1)
term.freq.1 <- rowSums(tdm.Decade1.m)
freq.df.1 <- data.frame(word=names(term.freq.1),frequency=term.freq.1)
freq.df.1 <- freq.df.1[order(freq.df.1[,2],decreasing = T),]

# Create an object freq.df, representing the frequency of words within each Decade Lyrics dataset. 

freq.df.1$word <- factor(freq.df.1$word,levels = unique(as.character(freq.df.1$word)))

# Now use ggplot2 to visualize the data

ggplot(freq.df.1[1:25,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="red") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)

# Download two of the three seperate lexicons in the sentiment dictionary

get_sentiments("bing") 
get_sentiments("nrc")

# Create Document Term Matrix for Sentiment Analysis

dtm_1 <- DocumentTermMatrix(corpus_1)

# Tidy dtm and assign it to new tidy object

dtm_1_td <- tidy(dtm_1)

# Create object called bing_dtm_sentiment to conduct sentiment analysis of entire lyrics corpus with bing lexicon

bing_dtm_1_sentiment <- dtm_1_td %>% 
    inner_join(get_sentiments("bing"), by=c(term="word"))

# Determine the count of positive v negative terms in lyrics corpus
 
bing_dtm_1_sentiment %>% 
    count(document, sentiment, wt = count) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment= positive - negative)
    
# Visualize which words contributed to positive and negative sentiment

bing_dtm_1_sentiment %>%
    count(sentiment, term, wt = count) %>%
    filter(n >= 100) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(term, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Contribution to sentiment")

    
# Create object called nrc_dtm_sentiment to conduct sentiment analysis of entire lyrics corpus with nrc lexicon

nrc_dtm_1_sentiment <- dtm_1_td %>%
    inner_join(get_sentiments("nrc"), by=c(term="word"))

# Get count of each emotion in NRC lexicon

nrc_count <- nrc_dtm_1_sentiment %>%
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
decade <- 1965

word_frequency_decade1 <- freq.df.1[1:5,]
word_frequency_decade1 <- cbind(decade,word_frequency_decade1)
write.csv(word_frequency_decade1,"Decade1_Top25WordFrequency.csv", row.names = FALSE)

overall_sentiment_decade1 <- bing_dtm_1_sentiment %>% 
    count(document, sentiment, wt = count) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment= positive - negative)

overall_sentiment_decade1 <- overall_sentiment_decade1[, -c(1,4)]
overall_sentiment_decade1 <- cbind(decade,overall_sentiment_decade1)
overall_sentiment_decade1 <- as.data.frame(overall_sentiment_decade1)
write.csv(overall_sentiment_decade1,"Decade1_OverallSentiment.csv", row.names = FALSE)

nrc_count_decade1 <- nrc_count
nrc_count_decade1 <- cbind(decade,nrc_count_decade1)
write.csv(nrc_count_decade1,"Decade1_OverallbyEmotion.csv", row.names = FALSE)



