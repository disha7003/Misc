setwd("c:\\workarea\\CBA2015")
# Required packages
library(tm)
library(wordcloud)
library(stringr)
library(ggplot2)
library(SnowballC)


# Locate and load the data.
examplesCSV = read.csv("example.csv", stringsAsFactors=FALSE)


# Create Corpus
examplesCorpus = Corpus(VectorSource(c(examplesCSV$Text)))
examplesCorpus <- tm_map(examplesCorpus, tolower)
inspect(examplesCorpus)

examplesCorpus <- tm_map(examplesCorpus, removeNumbers)
inspect(examplesCorpus)

examplesCorpus <- tm_map(examplesCorpus, removePunctuation)
inspect(examplesCorpus)

examplesCorpus <- tm_map(examplesCorpus, removeWords, stopwords("english"))
inspect(examplesCorpus)

examplesCorpus <- tm_map(examplesCorpus, removeWords, c("presentations"))
inspect(examplesCorpus)

removeURL <- function(x) gsub("http[[:alnum:]]*", " ", x)
examplesCorpus <- tm_map(examplesCorpus, removeURL)
inspect(examplesCorpus)

examplesCorpus <- tm_map(examplesCorpus, PlainTextDocument)
examplesCorpus <- tm_map(examplesCorpus, stemDocument, language="english")
examplesCorpus <- tm_map(examplesCorpus, tolower)
inspect(examplesCorpus)

