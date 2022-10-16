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



