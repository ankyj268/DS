## -- Install the packages ----
options(java.parameters = "-Xmx2048m")
list.of.packages <- c("tm", "NLP", "RWeka", "wordcloud", "RColorBrewer","dplyr","rJava","RWekajars","qpcR","DBI","assertthat","R6","syuzhet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")

library(RWekajars)
library(rJava)
library(tm)
library(NLP)
library(RWeka)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(qpcR)
library(syuzhet)
Sys.sleep(5)

## -- Getting path and setting directory ----
args<-commandArgs(trailingOnly=T)
Rpath<-as.String(args[1])
print(Rpath)
setwd(Rpath)

## -- Reading the data -----

#dt <- read.csv("RawData/Rinput.csv", header = T, stringsAsFactors = FALSE)
setwd("Path")
dt <- read.csv("File Name", header = T, stringsAsFactors = FALSE)

#### -- Sentiment ----
temp <- dt$comment_message
temp <- removeNumbers(temp)
temp <- gsub("."," ",temp,fixed = TRUE)
temp <- gsub(","," ",temp,fixed = TRUE)
temp <- gsub("[]$*+?[^{|(\\#%&~_/<=>'!,:;`\")}@-]","",temp)
temp <- removePunctuation(temp)
temp <- tolower(temp)
temp <- removeWords(temp, c("http","https","httpt","httpst","rt","etc","tco","auspost", stopwords("english")))
temp <- removeNumbers(temp)

dt <- dt %>% mutate(Sentiment_R1 = get_sentiment(temp, method = "syuzhet"))
dt$Sentiment_R1 <- ifelse(dt$Sentiment_R1<0.0, "Negative", ifelse(dt$Sentiment_R1 > 0.44, "Positive", "Neutral"))
dt$CleanedText <- temp

p_index <- dt$Sentiment_R1 == "Positive"
n_index <- dt$Sentiment_R1 == "Negative"

#### -- ngrams -----
## -- Setting up functions
f_tokens <- function(x,mn,mx) NGramTokenizer(x, Weka_control(min =mn,max = mx))
f_phr.len <- function(x) length(unlist(strsplit(x," ")))
f_nGram.tokens.select.gr <- function(len) nGram.tokens[which(nGram.tokens.phr.len==len)]

## -- Processing the data
corpus <- Corpus(VectorSource(temp))
# corpus <- tm_map(corpus, tolower)
# corpus <- tm_map(corpus, removeNumbers)
# corpus <- tm_map(corpus, removeWords, c("http","https","httpt","httpst","rt","etc","tco", stopwords("english")))
#corpus <- tm_map(corpus, removeWords, c("what","get","i","will","can","a","may","the","we","us","its","at", "is", "etc", "u", "n"))

## -- Tokenization
nGram.tokens <- f_tokens(x=unlist(temp),mn=1,mx=4)
nGram.tokens.phr.len <- sapply(1:length(nGram.tokens), function(i) f_phr.len(nGram.tokens[i]))

rm(corpus)
gc()

## -- Sorting according to requirements 
final_ngrams <- data.frame(matrix(nrow = 200))
for(len in c(1:4)){
  ngrams_df = data.frame(x = f_nGram.tokens.select.gr(len), stringsAsFactors = FALSE)
  ngrams_df = ngrams_df %>% group_by(x) %>% summarize(freq = n()) %>% mutate(prop = freq/sum(freq),
                                                                             ngram_len = len) %>% arrange(desc(freq))
  ngrams_df = ngrams_df[1:200,c(1,2)]
  # Take top 200 for length 1,2,3 and 4
  final_ngrams <- cbind(final_ngrams,ngrams_df)
}
final_ngrams <- final_ngrams[,-1]
colnames(final_ngrams) <-  c("ngrams - 1", "Frequency","ngrams - 2", "Frequency","ngrams - 3", "Frequency","ngrams - 4", "Frequency")

#### -- Weekly ngrams


#### -- Wordclouds -----
## -- Generate the wordcloud

## -- Tokenization and Structure the Data
# Overall
nGram.tokens <- f_tokens(x=unlist(temp),mn=1,mx=1)
nGram.tokens.phr.len <- sapply(1:length(nGram.tokens), function(i) f_phr.len(nGram.tokens[i]))
dt_wc = data.frame(x = f_nGram.tokens.select.gr(1), stringsAsFactors = FALSE)
dt_wc = dt_wc %>% group_by(x) %>% summarize(freq = n()) %>% arrange(desc(freq))
dt_wc = dt_wc[1:200,]
# Positive
nGram.tokens <- f_tokens(x=unlist(temp[p_index]),mn=1,mx=1)
nGram.tokens.phr.len <- sapply(1:length(nGram.tokens), function(i) f_phr.len(nGram.tokens[i]))
dt_wcp = data.frame(x = f_nGram.tokens.select.gr(1), stringsAsFactors = FALSE)
dt_wcp = dt_wcp %>% group_by(x) %>% summarize(freq = n()) %>% arrange(desc(freq))
dt_wcp = dt_wcp[1:200,]
# Negative
nGram.tokens <- f_tokens(x=unlist(temp[n_index]),mn=1,mx=1)
nGram.tokens.phr.len <- sapply(1:length(nGram.tokens), function(i) f_phr.len(nGram.tokens[i]))
dt_wcn = data.frame(x = f_nGram.tokens.select.gr(1), stringsAsFactors = FALSE)
dt_wcn = dt_wcn %>% group_by(x) %>% summarize(freq = n()) %>% arrange(desc(freq))
dt_wcn = dt_wcn[1:200,]
# Pick Colors
pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
pal_p <- brewer.pal(9,"BuGn")
pal_p <- pal_p[-(1:4)]
pal_n <- brewer.pal(9,"BuGn")
pal_n <- pal_n[-(1:4)]

# Genereate and save the WC
# Overall
png("WC_o.png", width = 3.5, height = 3.5, units = "in", res = 800)
wordcloud(words = dt_wc$x, freq = dt_wc$freq, max.words = 75, rot.per = 0, random.order = F, colors = pal)
dev.off()
png("WC_p.png", width = 3.5, height = 3.5, units = "in", res = 800)
wordcloud(words = dt_wcp$x, freq = dt_wcp$freq, max.words = 75, rot.per = 0, random.order = F, colors = pal_p)
dev.off()
png("WC_n.png", width = 3.5, height = 3.5, units = "in", res = 800)
wordcloud(words = dt_wcn$x, freq = dt_wcn$freq, max.words = 75, rot.per = 0, random.order = F, colors = pal_n)
dev.off()

#### -- Location Maps ----

#### -- Clean up and write the files ----
write.csv(dt, "Routput.csv", row.names = F,na = "")
write.csv(final_ngrams, "nGrams.csv", row.names = F,na = "")
rm(list=ls())
gc()
