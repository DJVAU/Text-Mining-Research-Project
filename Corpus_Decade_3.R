library(tm)
library(tidyverse)
library(tidytext)

## IMPORT THE DATA
allthesongs <- read.csv("BillboardHot100_Lyrics_1964-2015.csv", stringsAsFactors=FALSE)
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

# Now, we will use ggplot2 to visualize this data.

ggplot(freq.df.3[1:20,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="lightgreen") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)



