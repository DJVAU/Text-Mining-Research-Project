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

Decade_2 <- allthesongs %>% 
    filter(Year>=1975,Year<=1984)

Decade_3 <- allthesongs %>% 
    filter(Year>=1985,Year<=1994)

Decade_4 <- allthesongs %>% 
    filter(Year>=1995,Year<=2004)

Decade_5 <- allthesongs %>%
    filter(Year>=2005,Year<=2014)


## PREPROCESS THE DATA

# Create a dataframe called "Lyrics_Decade_()" of just the Lyrics in the dataset of each decade.

Lyrics_Decade_1 <- data.frame(ID=seq(1:nrow(Decade_1)),text=Decade_1$Lyrics)
Lyrics_Decade_2 <- data.frame(ID=seq(1:nrow(Decade_2)),text=Decade_2$Lyrics)
Lyrics_Decade_3 <- data.frame(ID=seq(1:nrow(Decade_3)),text=Decade_3$Lyrics)
Lyrics_Decade_4 <- data.frame(ID=seq(1:nrow(Decade_4)),text=Decade_4$Lyrics)
Lyrics_Decade_5 <- data.frame(ID=seq(1:nrow(Decade_5)),text=Decade_5$Lyrics)

# Create Corpus for each Separate Decade
corpus_1 <- VCorpus(VectorSource(Lyrics_Decade_1),readerControl = readDataframe(Lyrics_Decade_1,"en",id = ID))
corpus_2 <- VCorpus(VectorSource(Lyrics_Decade_2),readerControl = readDataframe(Lyrics_Decade_2,"en",id = ID))
corpus_3 <- VCorpus(VectorSource(Lyrics_Decade_3),readerControl = readDataframe(Lyrics_Decade_3,"en",id = ID))
corpus_4 <- VCorpus(VectorSource(Lyrics_Decade_4),readerControl = readDataframe(Lyrics_Decade_4,"en",id = ID))
corpus_5 <- VCorpus(VectorSource(Lyrics_Decade_5),readerControl = readDataframe(Lyrics_Decade_5,"en",id = ID))

# Create Pre-processing Function which:
# Creates a custom stopword dictionary based on the existing English stopword dictionary, and adds four additional elements; and 
# Creates a new custom function called "clean.corpus" which takes corpus, and applies five cleaning techniques: 
# (removes stopwords, removes punctuation, removes whitespace, and removes numbers).

# Strip White Space
corpus_1 <- tm_map(corpus_1, stripWhitespace)
corpus_2 <- tm_map(corpus_2, stripWhitespace)
corpus_3 <- tm_map(corpus_3, stripWhitespace)
corpus_4 <- tm_map(corpus_4, stripWhitespace)
corpus_5 <- tm_map(corpus_5, stripWhitespace)

# Remove Stop Words

corpus_1 <- tm_map(corpus_1, removeWords, stopwords("en"))
corpus_1 <- tm_map(corpus_1, removeWords, stopwords("SMART"))
corpus_2 <- tm_map(corpus_1, removeWords, stopwords("english"))
corpus_3 <- tm_map(corpus_1, removeWords, stopwords("english"))
corpus_4 <- tm_map(corpus_1, removeWords, stopwords("english"))
corpus_5 <- tm_map(corpus_1, removeWords, stopwords("english"))


# Remove Punctuation
corpus_1 <- tm_map(corpus_1, removePunctuation)
corpus_2 <- tm_map(corpus_2, removePunctuation)
corpus_3 <- tm_map(corpus_3, removePunctuation)
corpus_4 <- tm_map(corpus_4, removePunctuation)
corpus_5 <- tm_map(corpus_5, removePunctuation)

# Remove Numbers
corpus_1 <- tm_map(corpus_1, removeNumbers)
corpus_2 <- tm_map(corpus_2, removeNumbers)
corpus_3 <- tm_map(corpus_3, removeNumbers)
corpus_4 <- tm_map(corpus_4, removeNumbers)
corpus_5 <- tm_map(corpus_5, removeNumbers)

# Create TDM for each seperate Corpus of Decade Lyrics

tdm_1 <- TermDocumentMatrix(corpus_1,control=list(weighting=weightTf))
tdm.Decade1.m <- as.matrix(tdm_1)
term.freq.1 <- rowSums(tdm.Decade1.m)
freq.df.1 <- data.frame(word=names(term.freq.1),frequency=term.freq.1)
freq.df.1 <- freq.df.1[order(freq.df.1[,2],decreasing = T),]

tdm_2 <- TermDocumentMatrix(corpus_2,control=list(weighting=weightTf))
tdm.Decade2.m <- as.matrix(tdm_2)
term.freq.2 <- rowSums(tdm.Decade2.m)
freq.df.2 <- data.frame(word=names(term.freq.2),frequency=term.freq.2)
freq.df.2 <- freq.df.2[order(freq.df.2[,2],decreasing = T),]

tdm_3 <- TermDocumentMatrix(corpus_3,control=list(weighting=weightTf))
tdm.Decade3.m <- as.matrix(tdm_3)
term.freq.3 <- rowSums(tdm.Decade3.m)
freq.df.3 <- data.frame(word=names(term.freq.3),frequency=term.freq.3)
freq.df.3 <- freq.df.3[order(freq.df.3[,2],decreasing = T),]

tdm_4 <- TermDocumentMatrix(corpus_4,control=list(weighting=weightTf))
tdm.Decade4.m <- as.matrix(tdm_4)
term.freq.4 <- rowSums(tdm.Decade4.m)
freq.df.4 <- data.frame(word=names(term.freq.4),frequency=term.freq.4)
freq.df.4 <- freq.df.4[order(freq.df.4[,2],decreasing = T),]

tdm_5 <- TermDocumentMatrix(corpus_5,control=list(weighting=weightTf))
tdm.Decade5.m <- as.matrix(tdm_5)
term.freq.5 <- rowSums(tdm.Decade5.m)
freq.df.5 <- data.frame(word=names(term.freq.5),frequency=term.freq.5)
freq.df.5 <- freq.df.5[order(freq.df.5[,2],decreasing = T),]

# Create an object freq.df, representing the frequency of words within each Decade Lyrics dataset. 

freq.df.1$word <- factor(freq.df.1$word,levels = unique(as.character(freq.df.1$word)))
freq.df.2$word <- factor(freq.df.2$word,levels = unique(as.character(freq.df.2$word)))
freq.df.3$word <- factor(freq.df.3$word,levels = unique(as.character(freq.df.3$word)))
freq.df.4$word <- factor(freq.df.4$word,levels = unique(as.character(freq.df.4$word)))
freq.df.5$word <- factor(freq.df.5$word,levels = unique(as.character(freq.df.5$word)))

# Now, we will use ggplot2 to visualize this data. We will spend more time later going over the details of how to use and customize ggplot2. For now, please just copy the code as is. Before we do create our visualization, the unique words have to be changed from a string to a factor and we have to calculate unique levels.

ggplot(freq.df.1[1:50,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="darkred") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)

ggplot(freq.df.2[1:20,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="skyblue") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)

ggplot(freq.df.1[1:20,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="darkred") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)

ggplot(freq.df.1[1:20,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="darkred") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)

ggplot(freq.df.1[1:20,], aes(x=word,y=frequency)) + geom_bar(stat="identity",fill="darkred") + 
    coord_flip() +
    geom_text(aes(label=frequency),colour="white",hjust=1.25, size=5.0)










    