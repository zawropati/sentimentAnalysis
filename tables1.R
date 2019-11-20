install.packages("stringr")
install.packages("tidyverse")
install.packages("dplyr")
library(dplyr)
library(stringr)
library(DBI)
library(RMySQL)


#reading the dataset from Kaggle
hotelReviews <- read.csv("Hotel_Reviews.csv")
dim(hotelReviews)

#connecting to mySql server
conMySQL <- dbConnect(MySQL(), dbname="hotel_reviews", user="root",password= "kurczak1", host="localhost")
dbListTables(conMySQL)

#creating data frames from hotel_reviews
positiveReviewsDf <- data.frame("review" = hotelReviews$Positive_Review, "label" = "positive")
negativeReviewsDf <- data.frame("review" = hotelReviews$Negative_Review, "label" = "negative")


#initially cleaning dataframes (removing N/As and "no positive"/"no negative" reviews )
positiveReviewsDf <- positiveReviewsDf  %>%
  # recode strings without meaningful text (no positive) by NAs
  na_if("No positive") %>%
  # remove NAs
  na.omit

positiveReviewsDf  = positiveReviewsDf  %>%
  # recode empty strings "" by NAs
  na_if(" ") %>%
  # remove NAs
  na.omit

negativeReviewsDf <- data.frame("review" = hotelReviews$Negative_Review, "label" = "negative")

negativeReviewsDf$review <- as.character(negativeReviewsDf$review)
head(negativeReviewsDf)

negativeReviewsDf = negativeReviewsDf  %>%
  # recode empty strings "" by NAs
  na_if(" ") %>%
  # remove NAs
  na.omit

negativeReviewsDf = negativeReviewsDf  %>%
  # recode empty strings "" by NAs
  na_if("No negative") %>%
  # remove NAs
  na.omit

#binding dataset created from kaggle with webscraped ones - they are coming from other file
negativeReviewsDf <- bind_rows(negativeReviewsDf,finalNeg)
positiveReviewsDf <- bind_rows(positiveReviewsDf, finalPos)

#uploading it separatly to database
dbWriteTable(conMySQL,name="positive_reviews", value=positiveReviewsDf, row.names=FALSE, overwrite=FALSE)
dbWriteTable(conMySQL,name="negative_reviews", value=negativeReviewsDf, row.names=FALSE, overwrite=TRUE)

#binding both positive and negative, uploading in to db
data <- bind_rows(negativeReviewsDf, positiveReviewsDf)
dbWriteTable(conMySQL,name="cleaned_data", value=data, row.names=FALSE, overwrite=TRUE)


