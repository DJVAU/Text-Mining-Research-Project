library(tm)
library(tidyverse)
library(tidytext)

## IMPORT THE DATA
allthesongs <- read.csv("BillboardHot100_Lyrics_1964-2015.csv", stringsAsFactors=FALSE)
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

# Now, we will use ggplot2 to visualize this data.

ggplot(freq.df.2[1:20,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="skyblue") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)

