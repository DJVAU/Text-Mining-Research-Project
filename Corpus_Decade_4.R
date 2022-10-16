library(tm)
library(tidyverse)
library(tidytext)

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



