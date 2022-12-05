library(tidyverse)
library(psych)
library(ggrepel)

# Graphs for Word Frequency Counts
WFQ1 <- read_csv("Decade1_Top25WordFrequency.csv")
WFQ2 <- read_csv("Decade2_Top25WordFrequency.csv")
WFQ3 <- read_csv("Decade3_Top25WordFrequency.csv")
WFQ4 <- read_csv("Decade4_Top25WordFrequency.csv")
WFQ5 <- read_csv("Decade5_Top25WordFrequency.csv")

Word_Frequency_Over_Decades <- data.frame()
Word_Frequency_Over_Decades <- rbind(WFQ1,Word_Frequency_Over_Decades)
Word_Frequency_Over_Decades <- rbind(WFQ2,Word_Frequency_Over_Decades)
Word_Frequency_Over_Decades <- rbind(WFQ3,Word_Frequency_Over_Decades)
Word_Frequency_Over_Decades <- rbind(WFQ4,Word_Frequency_Over_Decades)
Word_Frequency_Over_Decades <- rbind(WFQ5,Word_Frequency_Over_Decades)

## Stacked Bar Graph for Top 5 Word by Decade
Word_Frequency_Over_Decades %>%
    group_by(decade) %>%
    ggplot(aes(x=decade,y=frequency,fill=word,label=frequency)) +
    geom_bar(stat="identity") +
    geom_text(size=3,position=position_stack(vjust=0.5))   

## Word Count for Love over Decades
# Bar Graph
Word_Frequency_Over_Decades %>%
    filter(word=="love") %>%
    group_by(decade) %>%
    ggplot(aes(x=decade,y=frequency,label=frequency)) +
    geom_bar(stat="identity",fill="red") +
    geom_text(size=3,position=position_stack(vjust=0.5)) 

# Line Graph
Word_Frequency_Over_Decades %>%
    filter(word=="love") %>%
    group_by(decade) %>%
    ggplot(aes(x=decade,y=frequency, label=frequency)) +
    geom_point() +
    geom_line(color="red") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 4000)) +
    geom_text(vjust = 2, nudge_y = 0.5)

# Compute Summary Statistics for Term Love
Word_Frequency_Over_Decades %>%
    group_by(word) %>%
    filter(word=="love") %>%
    select(frequency) %>%
    describe()

## Word Count for Baby over Decades
# Bar Graph
Word_Frequency_Over_Decades %>%
    filter(word=="baby") %>%
    group_by(decade) %>%
    ggplot(aes(x=decade,y=frequency,label=frequency)) +
    geom_bar(stat="identity",fill="lightblue") +
    geom_text(size=3,position=position_stack(vjust=0.5))  

# Line Graph
Word_Frequency_Over_Decades %>%
    filter(word=="baby") %>%
    group_by(decade) %>%
    ggplot(aes(x=decade,y=frequency, label=frequency)) +
    geom_point() +
    geom_line(color="lightblue") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
    geom_text(vjust = 2, nudge_y = 0.5)

# Compute Summary Statistics for Term Baby
Word_Frequency_Over_Decades %>%
    group_by(word) %>%
    filter(word=="baby") %>%
    select(frequency) %>%
    describe()

# Graph for Overall Sentiment over Decades
PvN1 <- read_csv("Decade1_OverallSentiment.csv")
PvN2 <- read_csv("Decade2_OverallSentiment.csv")
PvN3 <- read_csv("Decade3_OverallSentiment.csv")
PvN4 <- read_csv("Decade4_OverallSentiment.csv")
PvN5 <- read_csv("Decade5_OverallSentiment.csv")

Overall_Positive_v_Negative <- data.frame()
Overall_Positive_v_Negative <- rbind(PvN1,Overall_Positive_v_Negative)
Overall_Positive_v_Negative <- rbind(PvN2,Overall_Positive_v_Negative)
Overall_Positive_v_Negative <- rbind(PvN3,Overall_Positive_v_Negative)
Overall_Positive_v_Negative <- rbind(PvN4,Overall_Positive_v_Negative)
Overall_Positive_v_Negative <- rbind(PvN5,Overall_Positive_v_Negative)

Overall_Positive_v_Negative %>% 
    gather(type, count, negative:positive) %>% 
    ggplot(., aes(x=decade,y=count,fill=type,label=count)) +
    geom_bar(stat="identity") +
    geom_text(size = 3,position=position_stack(vjust = 0.5))   

# Graph for Overall by Emotion over Decades

E1 <- read_csv("Decade1_OverallbyEmotion.csv")
E2 <- read_csv("Decade2_OverallbyEmotion.csv")
E3 <- read_csv("Decade3_OverallbyEmotion.csv")
E4 <- read_csv("Decade4_OverallbyEmotion.csv")
E5 <- read_csv("Decade5_OverallbyEmotion.csv")

Overall_by_Emotion <- data.frame()
Overall_by_Emotion <- rbind(E1,Overall_by_Emotion)
Overall_by_Emotion <- rbind(E2,Overall_by_Emotion)
Overall_by_Emotion <- rbind(E3,Overall_by_Emotion)
Overall_by_Emotion <- rbind(E4,Overall_by_Emotion)
Overall_by_Emotion <- rbind(E5,Overall_by_Emotion)

# Overall Stacked Bar Graphs by 8 different sentiments
Overall_by_Emotion %>%
    rename(decade=...1,count=n) %>%
    group_by(decade) %>%
    ggplot(aes(x=decade,y=count,fill=sentiment,label=count)) +
    geom_bar(stat="identity") +
    geom_text(size=3,position=position_stack(vjust=0.5))  

# Joy vs Sadness Line Graph
Overall_by_Emotion %>%
    rename(decade=...1,count=n) %>%
    group_by(decade) %>%
    filter(sentiment %in% c("joy", "sadness")) %>%
    ggplot(aes(x=decade,y=count,group=sentiment,label=count)) +
    geom_point() +
    geom_line(aes(color=sentiment)) +
    scale_color_manual(values=c("skyblue","darkblue")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 450)) +
    geom_text_repel() 

# Fear Line Graph

Overall_by_Emotion %>%
    rename(decade=...1,count=n) %>%
    group_by(decade) %>%
    filter(sentiment=="fear") %>%
    ggplot(aes(x=decade,y=count,group=sentiment,label=count)) +
    geom_point() +
    geom_line(aes(color=sentiment)) +
    scale_color_manual(values="green") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 500)) +
    geom_text(vjust=2) 

# Anger Line Graph

Overall_by_Emotion %>%
    rename(decade=...1,count=n) %>%
    group_by(decade) %>%
    filter(sentiment=="anger") %>%
    ggplot(aes(x=decade,y=count,group=sentiment,label=count)) +
    geom_point() +
    geom_line(aes(color=sentiment)) +
    scale_color_manual(values="red") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 450)) +
    geom_text(vjust=2) 

