library(tm)
library(tidyverse)
library(tidytext)

## IMPORT THE DATA
allthesongs <- read.csv("BillboardHot100_Lyrics_1965-2015.csv", stringsAsFactors=FALSE)
#view(allthesongs)

## FILTERDATA

# Filter Data into separate 10 year periods (1965-2015)

Decade_3 <- allthesongs %>% 
    filter(Year>=1985,Year<=1994)

## PREPROCESS THE DATA

# Create a dataframe called "Lyrics_Decade_()" of just the Lyrics in the dataset of each decade.

Lyrics_Decade_3 <- data.frame(ID=seq(1:nrow(Decade_3)),text=Decade_3$Lyrics)

# Create Corpus for each Separate Decade

corpus_3 <- VCorpus(VectorSource(Lyrics_Decade_3),readerControl = readDataframe(Lyrics_Decade_3,"en",id = ID))

# Strip White Space

corpus_3 <- tm_map(corpus_3, stripWhitespace)

# Remove Stop Words

corpus_3 <- tm_map(corpus_3, removeWords, stopwords("english"))
corpus_3 <- tm_map(corpus_3, removeWords, stopwords("SMART"))

# Remove Punctuation

corpus_3 <- tm_map(corpus_3, removePunctuation)

# Remove Numbers

corpus_3 <- tm_map(corpus_3, removeNumbers)

# Create TDM for each seperate Corpus of Decade Lyrics

tdm_3 <- TermDocumentMatrix(corpus_3,control=list(weighting=weightTf))
tdm.Decade3.m <- as.matrix(tdm_3)
term.freq.3 <- rowSums(tdm.Decade3.m)
freq.df.3 <- data.frame(word=names(term.freq.3),frequency=term.freq.3)
freq.df.3 <- freq.df.3[order(freq.df.3[,2],decreasing = T),]

# Create an object freq.df, representing the frequency of words within each Decade Lyrics dataset. 

freq.df.3$word <- factor(freq.df.3$word,levels = unique(as.character(freq.df.3$word)))

# Now use ggplot2 to visualize this data.

ggplot(freq.df.3[1:25,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="lightgreen") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)

# Download two of the three seperate lexicons in the sentiment dictionary

get_sentiments("bing") 
get_sentiments("nrc")

# Create Document Term Matrix for Sentiment Analysis

dtm_3 <- DocumentTermMatrix(corpus_3)

# Tidy dtm and assign it to new tidy object

dtm_3_td <- tidy(dtm_3)

# Create object called bing_dtm_sentiment to conduct sentiment analysis of entire lyrics corpus with bing lexicon

bing_dtm_3_sentiment <- dtm_3_td %>% 
    inner_join(get_sentiments("bing"), by=c(term="word"))

# Determine the count of positive v negative terms in lyrics corpus

bing_dtm_3_sentiment %>% 
    count(document, sentiment, wt = count) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment= positive - negative)

# Visualize which words contributed to positive and negative sentiment

bing_dtm_3_sentiment %>%
    count(sentiment, term, wt = count) %>%
    filter(n >= 100) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(term, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Contribution to sentiment")


# Create object called nrc_dtm_sentiment to conduct sentiment analysis of entire lyrics corpus with nrc lexicon

nrc_dtm_3_sentiment <- dtm_3_td %>%
    inner_join(get_sentiments("nrc"), by=c(term="word"))
               
# Get count of each emotion in NRC lexicon
               
nrc_count <- nrc_dtm_3_sentiment %>%
    group_by(sentiment) %>%
    filter(!sentiment %in% c("positive","negative")) %>% 
    count(sentiment,sort=TRUE)
               
nrc_count
               
# Plot emotions of NRC lexicon to determine highest emotion in corpus lyrics in descending order
nrc_count %>%
    ggplot(aes(x=reorder(sentiment,-n),y=n,fill=sentiment)) +
    geom_bar(stat="identity") +
    labs(y="Sentiment Scores", x=NULL)

