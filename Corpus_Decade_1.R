library(tm)
library(tidyverse)
library(tidytext)

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

# Create Corpus for each Separate Decade

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

# Nowuse ggplot2 to visualize the data

ggplot(freq.df.1[1:25,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="red") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)



















