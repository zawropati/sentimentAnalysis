####################
#install.packages("gmodels")
#install.packages("wordcloud")
#install.packages("textstem")
library(tidyverse)
library(textstem)
library(gmodels)  # Crosstable
library(tm)
library(e1071)
library(DBI)
library(RMySQL)

conMySQL <- dbConnect(MySQL(), dbname="hotel_reviews", user="root", password= "kurczak1", host="localhost")

data <- dbGetQuery(conMySQL, "CALL getAllReviews();")
data$label <- as.factor(data$label)

sample <- sample.int(n = nrow(data), size = floor(.05*nrow(data)), replace = F)
data = data[sample,]
data$label <- as.factor(data$label)
#rownames(data) <- NULL
data <- data[sapply(gregexpr("\\W+", data$review), length) >3,]

#setting the number of handwritten reviews
n_reviews_new = 2
n <- nrow(data)

review1 <- as.character("I wish wed never left this beautiful spot. 
Wendy's house was spectacular, we loved every minute there. 
We were a group of 12 people including an infant, and there was plenty of space for all of us. 
The views from the terrace were beautiful, short walk to the beach and the house was equipped with everything we could possibly need. Wendy was so responsive and gracious, we felt so spoiled. 
Thank you for everything Wendy, we hope to come back one day!")

review2 <- as.character("Our stay in this beautiful home was fantastic! 
The house is located in a prime location near some of the island's best beaches and other attractions
including Queens Bath, Hanalei, the Na Pali Coast, etc. 
Tammy is very kind and easy to work with and the home is clean and welcoming. Highly recommended")

#creating vectors from new reviews and labels
new_reviews = c(review1, review2)
new_label = c("positive","positive")

#iterating through data and replacing last reviews with handwritten ones
j = 1
for (i in (n-n_reviews_new):(n-1)){
  data[i,1] = new_reviews[j]
  data[i,2] = new_label[j]
  j = j + 1
}

#creating corpus
corpus <- Corpus(VectorSource(data$review))
#cleaning the corpus
cleanCorpus <- corpus %>%
  #tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)

#removing stopwrods and custom words
myStopwords<- c(stopwords("english"),"hotel","room","Room","staff","the","The","breakfast")
cleanCorpus <- tm_map(cleanCorpus, removeWords, myStopwords)

#lematization
lemmatize_words(cleanCorpus)

#creating DTM
cleanCorpusDtm <- DocumentTermMatrix(cleanCorpus)

#splitting the data into train and validation - in this case validation is the last five rows of my data
n <- nrow(data)
raw.text.train <- data[1:as.integer(0.7*n),]
raw.text.validation  <- data[(n-n_reviews_new):(n-1) ,]

nn <- length(cleanCorpus)
clean.corpus.train <- cleanCorpus[1:(as.integer(0.7*nn))]
clean.corpus.validation  <- cleanCorpus[(nn-n_reviews_new):(nn-1)]

nnn <- nrow(cleanCorpusDtm)
clean.corpus.dtm.train <- cleanCorpusDtm[1:as.integer(0.7*nnn),]
clean.corpus.dtm.validation  <- cleanCorpusDtm[(nnn-n_reviews_new):(nnn-1) ,]

#removing terms that do not appear often
freq.terms <- findFreqTerms(clean.corpus.dtm.train, 20)
clean.corpus.dtm.freq.train <- DocumentTermMatrix(clean.corpus.train, list(dictionary = freq.terms))
clean.corpus.dtm.freq.validation  <- DocumentTermMatrix(clean.corpus.validation, list(dictionary = freq.terms))

#removing sparse terms
clean.corpus.dtm.freq.train <- removeSparseTerms(clean.corpus.dtm.freq.train, 0.99)
clean.corpus.dtm.freq.validation <- removeSparseTerms(clean.corpus.dtm.freq.validation, 0.99)

#creatin a function that converts the cells to be categorical instead of numerical 0 -> no and 1 -> yes 
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}
#applying the function to matrix
clean.corpus.dtm.freq.train <- apply(clean.corpus.dtm.freq.train, MARGIN = 2, convert_counts)
clean.corpus.dtm.freq.validation  <- apply(clean.corpus.dtm.freq.validation, MARGIN = 2, convert_counts)

#training the classifier
modelNaiveBayes <- naiveBayes(clean.corpus.dtm.freq.train, raw.text.train$label)
#predicting on handwritten reviews
text.pred <- predict(modelNaiveBayes, clean.corpus.dtm.freq.validation)

table(text.pred, raw.text.validation$label)

clean.corpus.validation[[2]]$content
