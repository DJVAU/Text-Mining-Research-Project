library(tm)
library(tidyverse)
library(tidytext)
library(tidyr)

## IMPORT THE DATA
allthesongs <- read.csv("BillboardHot100_Lyrics_1965-2015.csv", stringsAsFactors=FALSE)
#view(allthesongs)

## FILTERDATA

# Filter Data into separate 10 year periods (1965-2015)

Decade_4 <- allthesongs %>% 
    filter(Year>=1995,Year<=2004)

## PREPROCESS THE DATA

# Create a dataframe called "Lyrics_Decade_()" of just the Lyrics in the dataset of each decade.

Lyrics_Decade_4 <- data.frame(ID=seq(1:nrow(Decade_4)),text=Decade_4$Lyrics)

# Create Corpus for each Separate Decade

corpus_4 <- VCorpus(VectorSource(Lyrics_Decade_4),readerControl = readDataframe(Lyrics_Decade_4,"en",id = ID))

# Strip White Space

corpus_4 <- tm_map(corpus_4, stripWhitespace)

# Remove Stop Words

corpus_4 <- tm_map(corpus_4, removeWords, stopwords("english"))
corpus_4 <- tm_map(corpus_4, removeWords, stopwords("SMART"))

# Remove Punctuation

corpus_4 <- tm_map(corpus_4, removePunctuation)

# Remove Numbers

corpus_4 <- tm_map(corpus_4, removeNumbers)

# Create TDM for each seperate Corpus of Decade Lyrics

tdm_4 <- TermDocumentMatrix(corpus_4,control=list(weighting=weightTf))
tdm.Decade4.m <- as.matrix(tdm_4)
term.freq.4 <- rowSums(tdm.Decade4.m)
freq.df.4 <- data.frame(word=names(term.freq.4),frequency=term.freq.4)
freq.df.4 <- freq.df.4[order(freq.df.4[,2],decreasing = T),]

# Create an object freq.df, representing the frequency of words within each Decade Lyrics dataset. 

freq.df.4$word <- factor(freq.df.4$word,levels = unique(as.character(freq.df.4$word)))

# Now, we will use ggplot2 to visualize this data. We will spend more time later going over the details of how to use and customize ggplot2. For now, please just copy the code as is. Before we do create our visualization, the unique words have to be changed from a string to a factor and we have to calculate unique levels.

ggplot(freq.df.4[1:25,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="purple") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)

# Download two of the three seperate lexicons in the sentiment dictionary

get_sentiments("bing") 
get_sentiments("nrc")

# Create Document Term Matrix for Sentiment Analysis

dtm_4 <- DocumentTermMatrix(corpus_4)

# Tidy dtm and assign it to new tidy object

dtm_4_td <- tidy(dtm_4)

# Create object called bing_dtm_sentiment to conduct sentiment analysis of entire lyrics corpus with bing lexicon

bing_dtm_4_sentiment <- dtm_4_td %>% 
    inner_join(get_sentiments("bing"), by=c(term="word"))

# Determine the count of positive v negative terms in lyrics corpus

bing_dtm_4_sentiment %>% 
    count(document, sentiment, wt = count) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment= positive - negative)

# Visualize which words contributed to positive and negative sentiment

bing_dtm_4_sentiment %>%
    count(sentiment, term, wt = count) %>%
    filter(n >= 100) %>%
    mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
    mutate(term = reorder(term, n)) %>%
    ggplot(aes(term, n, fill = sentiment)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Contribution to sentiment")


# Create object called nrc_dtm_sentiment to conduct sentiment analysis of entire lyrics corpus with nrc lexicon

nrc_dtm_4_sentiment <- dtm_4_td %>%
    inner_join(get_sentiments("nrc"), by=c(term="word"))
               
# Get count of each emotion in NRC lexicon
               
 nrc_count <- nrc_dtm_4_sentiment %>%
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
decade <- 1995

word_frequency_decade4 <- freq.df.4[1:5,]
word_frequency_decade4 <- cbind(decade,word_frequency_decade4)
write.csv(word_frequency_decade4,"Decade4_Top25WordFrequency.csv", row.names = FALSE)

overall_sentiment_decade4 <- bing_dtm_4_sentiment %>% 
    count(document, sentiment, wt = count) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment= positive - negative)

overall_sentiment_decade4 <- overall_sentiment_decade4[, -c(1,4)]

overall_sentiment_decade4 <- cbind(decade,overall_sentiment_decade4)
overall_sentiment_decade4 <- as.data.frame(overall_sentiment_decade4)
write.csv(overall_sentiment_decade4,"Decade4_OverallSentiment.csv", row.names = FALSE)

nrc_count_decade4 <- nrc_count
nrc_count_decade4 <- cbind(decade,nrc_count_decade4)
write.csv(nrc_count_decade4,"Decade4_OverallbyEmotion.csv", row.names = FALSE)

