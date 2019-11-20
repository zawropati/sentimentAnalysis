#libraries
install.packages("gmodels")
install.packages("wordcloud")
install.packages("textstem")
install.packages("party")
install.packages("rpart.plot")
library(party)
library(DBI)
library(RMySQL)
library(tidyverse)
library(textstem)
library(gmodels) 
library(tm)
library(wordcloud)
library(e1071)
library(dplyr)
library(strucchange)


#connection to MySQL
conMySQL <- dbConnect(MySQL(), dbname="hotel_reviews", user="root", password= "kurczak1", host="localhost")

#retreiveing data from DB, there are 2 ways = R function or stored procedure
data <- dbGetQuery(conMySQL, "CALL getAllReviews();")

#data <- dbReadTable(conMySQL, "cleaned_data", fileEncoding="UTF-8",  stringsAsFactors=TRUE)
#dataPositive <- dbGetQuery(conMySQL, "CALL getReviewsByLabel('positive');")

#removing sentences with less than 3 words
data <- data[sapply(gregexpr("\\W+", data$review), length) >3,]

#sampling my data
sample <- sample.int(n = nrow(data), size = floor(.11*nrow(data)), replace = F)
data = data[sample,]
data$label <- as.factor(data$label)

summary(data)

### CLEANING

corpus <- Corpus(VectorSource(data$review))

cleanCorpus <- corpus %>%
  #tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, myStopwords) %>%
  tm_map(stripWhitespace)

#expanding stopwords list with words that occur both in positive as in negative reviews
myStopwords<- c(stopwords("english"),"hotel","room","staff","t","the","breakfast")
cleanCorpus <- tm_map(cleanCorpus, removeWords, myStopwords)

#lemmatization
lemmatize_words(cleanCorpus)

cleanCorpusDtm <- DocumentTermMatrix(cleanCorpus)

#dividing data into train and test 70% / 30%
n <- nrow(data)
raw.text.train <- data[1:round(.7 * n),]
raw.text.test  <- data[(round(.3 * n)+1):n,]

nn <- length(cleanCorpus)
clean.corpus.train <- cleanCorpus[1:round(.7 * nn)]
clean.corpus.test  <- cleanCorpus[(round(.3 * nn)+1):nn]

nnn <- nrow(cleanCorpusDtm)
clean.corpus.dtm.train <- cleanCorpusDtm[1:round(.7 * nnn),]
clean.corpus.dtm.test  <- cleanCorpusDtm[(round(.3 * nnn)+1):nnn,]

#creating wordclouds from train data
wordcloud(clean.corpus.train, min.freq = 50, max.words = 50, random.order = FALSE)

positive <- subset(raw.text.train, label == "positive")
negative <- subset(raw.text.train, label == "negative")

wordcloud(positive$review, max.words = 30, scale = c(3, 0.5))
wordcloud(negative$review, max.words = 30, scale = c(3, 0.5))

#rmeoving terms that have frequency lower than 20
freq.terms <- findFreqTerms(clean.corpus.dtm.train, 20)
clean.corpus.dtm.freq.train <- DocumentTermMatrix(clean.corpus.train, list(dictionary = freq.terms))
clean.corpus.dtm.freq.test  <- DocumentTermMatrix(clean.corpus.test, list(dictionary = freq.terms))

str(clean.corpus.dtm.freq.train)
#removing sparse items - words that do not occur often
clean.corpus.dtm.freq.train<- removeSparseTerms(clean.corpus.dtm.freq.train, 0.99)
clean.corpus.dtm.freq.test <- removeSparseTerms(clean.corpus.dtm.freq.test, 0.99)

#creatin a function that converts the cells to be categorical instead of numerical 0 -> no and 1 -> yes 
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

#applying the function to matrix
clean.corpus.dtm.freq.train <- apply(clean.corpus.dtm.freq.train, MARGIN = 2, convert_counts)
clean.corpus.dtm.freq.test <- apply(clean.corpus.dtm.freq.test, MARGIN = 2, convert_counts)

#training the nb classifier   
naive.classifer <- naiveBayes(clean.corpus.dtm.freq.train, raw.text.train$label, laplace = 1)
#predicition
naive.pred <- predict(naive.classifer, clean.corpus.dtm.freq.test)

#confusion matrix
xtable = CrossTable(naive.pred, raw.text.test$label,
           prop.chisq = FALSE, 
           prop.t = FALSE,
           dnn = c('predicted', 'actual'))


#################### solution to fix the error in glm() & tree
n <- nrow(clean.corpus.dtm.freq.train)
clean.corpus.dtm.freq.train2 <- clean.corpus.dtm.freq.train[1:round(.7 * n), ]
raw.text.train2  <- raw.text.train[1:round(.7 * n),]
clean.corpus.dtm.freq.test2  <- clean.corpus.dtm.freq.train[(round(.3 * n)+1):n, ]
raw.text.test2  <- raw.text.train[(round(.3 * n)+1):n, ]
##################

#logistic
#creating df for logistic model
trainLogistic <- cbind(clean.corpus.dtm.freq.train2, Label=raw.text.train2$label)  
trainLogistic <- as.data.frame(trainLogistic)
testLogistic <- cbind(clean.corpus.dtm.freq.test2, Label=raw.text.test2$label)  
testLogistic <- as.data.frame(testLogistic)

#logistic regression model
log_model <- glm(Label~., data=trainLogistic, family=binomial(link="logit"))

#prediction
log.pred <- predict(log_model, newdata=testLogistic, type="response")

#confusion matrix
table(raw.text.test2$label, log.pred>.5)

#coefficients
summary(log_model)


#Decision tree
tree = ctree(Label~., data = trainLogistic)
predictions = predict(tree, testLogistic)
table(predictions, raw.text.test2$label)
tree


#plot(tree, type="simple")  

  

############################## NEW ##################
glm_response_scores <- predict(log_model, testLogistic, type="response")

table(raw.text.test2$label, glm_response_scores>.5)

library(pROC)
roc(raw.text.test2$label, glm_link_scores, plot=TRUE, 
    xlab="False positive porcentage", ylab="True positive porcentage" , col="#377eb8")

############################## NEW ##################
#text.classifer <- naiveBayes(clean.corpus.dtm.freq.train, raw.text.train$label)
#text.pred <- predict(text.classifer, clean.corpus.dtm.freq.test)

#xtable = CrossTable(text.pred, raw.text.test$label,
#                    prop.chisq = FALSE, 
#                    prop.t = FALSE,
#                    dnn = c('predicted', 'actual'))

#roc(raw.text.test$label,  text.pred, plot=TRUE, 
#    xlab="False positive porcentage", ylab="True positive porcentage" , col="#377eb8")

