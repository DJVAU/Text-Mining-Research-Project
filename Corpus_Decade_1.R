library(tm)
library(tidyverse)
library(tidytext)

## IMPORT THE DATA
allthesongs <- read.csv("BillboardHot100_Lyrics_1964-2015.csv", stringsAsFactors=FALSE)
view(allthesongs)

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

corpus_1 <- tm_map(corpus_1, removeWords, stopwords("en"))

# Remove Punctuation
corpus_1 <- tm_map(corpus_1, removePunctuation)

# Remove Numbers

corpus_1 <- tm_map(corpus_1, removeNumbers)

# Create TDM for each seperate Corpus of Decade Lyrics

tdm_1 <- TermDocumentMatrix(corpus_1,control=list(weighting=weightTf))
tdm.Decade1.m <- as.matrix(tdm_1)
term.freq.1 <- rowSums(tdm.Decade1.m)
freq.df.1 <- data.frame(word=names(term.freq.1),frequency=term.freq.1)
freq.df.1 <- freq.df.1[order(freq.df.1[,2],decreasing = T),]

# Create an object freq.df, representing the frequency of words within each Decade Lyrics dataset. 

freq.df.1$word <- factor(freq.df.1$word,levels = unique(as.character(freq.df.1$word)))

# Now, we will use ggplot2 to visualize this data. We will spend more time later going over the details of how to use and customize ggplot2. For now, please just copy the code as is. Before we do create our visualization, the unique words have to be changed from a string to a factor and we have to calculate unique levels.

ggplot(freq.df.1[1:50,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="darkred") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)



















