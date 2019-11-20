#install.packages("tidyverse")
#install.packages("twitteR")
library(twitteR)
library(tm)
library(ggplot2)
library('tidyverse')
library('rvest')
library('dplyr')
library(DBI)
library(RMySQL)

#defining URLs abd reading the html from website
url2 <- "https://www.booking.com/reviews/nl/hotel/best-western-amsterdam-airport-hotel.en-gb.html?aid=356980;label=gog235jc-1FEgdyZXZpZXdzKIICOOgHSDNYA2ipAYgBAZgBCbgBB8gBDNgBAegBAfgBDYgCAagCA7gCvpnT7AXAAgE;sid=b075a00677b76feaaf6f19336ed7d1a3"
webpage2 <- read_html(url2)

url <- "https://www.booking.com/reviews/vn/hotel/au-lac.en-gb.html?aid=356980;label=gog235jc-1BEgdyZXZpZXdzKIICOOgHSDNYA2ipAYgBAZgBCbgBB8gBDNgBAegBAYgCAagCA7gCvpnT7AXAAgE;sid=b075a00677b76feaaf6f19336ed7d1a3"
webpage <- read_html(url)

#selecting html elements from website and converting them to text only
rank_data_html_neg <- html_nodes(webpage,'.review_neg')
rank_data_neg <- html_text(rank_data_html_neg)

rank_data_html_pos <- html_nodes(webpage,'.review_pos')
rank_data_pos <- html_text(rank_data_html_pos)

#creating data frames
df_neg <- data.frame("review" = rank_data_neg, "label" = "negative")
df_pos <- data.frame("review" = rank_data_pos, "label" = "positive")

#same for the other website
rank_data_html_neg2 <- html_nodes(webpage2,'.review_neg')
rank_data_neg2 <- html_text(rank_data_html_neg2)

rank_data_html_pos2 <- html_nodes(webpage2,'.review_pos')
rank_data_pos2 <- html_text(rank_data_html_pos2)

df_neg2 <- data.frame("review" = rank_data_neg2, "label" = "negative")
df_pos2 <- data.frame("review" = rank_data_pos2, "label" = "positive")

#binding df from both websites into one
finalPos <- bind_rows(df_pos,df_pos2)
finalNeg <- bind_rows(df_neg,df_neg2)

#small cleaning of webscraped reviews
final$review <- gsub("Nothing", "", final$review, perl = TRUE)

finalNeg <- finalNeg  %>%
  filter(str_remove(review, "[\n]"))

finalPos <- finalPos  %>%
  filter(str_remove(review, "[\n]"))

finalNeg <- finalNeg %>%
  filter(!str_detect(review, "nothing") )

finalPos <- finalPos %>%
  filter(!str_detect(review, "nothing") )

#insert the webscraped reviews to hotel_reviews db on MySQL
conMySQL <- dbConnect(MySQL(), dbname="hotel_reviews", user="root",password= "kurczak1", host="localhost")

dbWriteTable(conMySQL,name="positive_webscraped", value=finalPos, row.names=FALSE, overwrite=TRUE)
dbWriteTable(conMySQL,name="negative_webscraped", value=finalNeg, row.names=FALSE, overwrite=TRUE)










######
#barplots made from webscraped reviews
# build a corpus
myCorpus <- Corpus(VectorSource(finalPos$review))
# make all text lowercase
myCorpus <- tm_map(myCorpus, tolower)
# show first items
strwrap(myCorpus[1:15])
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# show first items
strwrap(myCorpus[1:5])

# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# show first items
strwrap(myCorpus[1:15])

removeURL <-function(x) gsub("http[[:alnum:]]*","",x)
myCorpus <-tm_map(myCorpus,removeURL)
# show first items
strwrap(myCorpus[11:15])

# remove stopwords
myStopwords <- stopwords('english')
myStopwords
#remove custom stopwords
myStopwords<- setdiff(myStopwords, c("hotel","room"))

myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# Building a document term matrix
myTdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths=c(1,Inf)))
myTdm
rownames(myTdm)
# findFreqTerms(myTdm, lowfreq=10)
termFrequency <- rowSums(as.matrix(myTdm))

termFrequency <-subset(termFrequency, termFrequency>=15)

barplot(sort(termFrequency, decreasing=TRUE),las=2 )  

#negative plot

myCorpusNeg <- Corpus(VectorSource(finalNeg$review))
# make all text lowercase
myCorpusNeg <- tm_map(myCorpusNeg, tolower)
# show first items
strwrap(myCorpusNeg[1:15])
# remove punctuation
myCorpusNeg <- tm_map(myCorpusNeg, removePunctuation)

# remove numbers
myCorpusNeg <- tm_map(myCorpusNeg, removeNumbers)

myCorpusNeg <-tm_map(myCorpusNeg,removeURL)

myCorpusNeg <- tm_map(myCorpusNeg, removeWords, myStopwords)
# show first items
strwrap(myCorpus[11:15])

# Building a document term matrix
myTdmNeg <- TermDocumentMatrix(myCorpusNeg, control = list(wordLengths=c(1,Inf)))
myTdmNeg
rownames(myTdmNeg)
# findFreqTerms(myTdm, lowfreq=10)
termFrequencyNeg <- rowSums(as.matrix(myTdmNeg))

termFrequencyNeg <-subset(termFrequencyNeg, termFrequencyNeg>=15)

barplot(sort(termFrequencyNeg, decreasing=TRUE),las=2 )  



