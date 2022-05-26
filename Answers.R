# Capstone (DocDigitizer)

## Loading the libraries
library(tidyverse)
library(dplyr)
library(plyr)
library(caret)
library(lme4)
library(ggcorrplot)
library(corrplot)
library(tidyr)
library(datetime)
library(lubridate)
library(randomForest)
library(factoextra)
library(Matrix)
library(glmnet)
library(data.table)
library(TTR)
library(outliers)
library(tm)

## Get working directory
setwd('./ALY6980/DocDigitizer/')

## Reading the file
clean_answers <- read.csv('./cleananswers.csv')
head(clean_answers)

## Removing outliers
answer_outliers <- function(x) {
  
  q1 <- quantile(x, probs=.25)
  q3 <- quantile(x, probs=.75)
  iqr = q3-q1
  
  upper_limit = q3 + (iqr*1.5)
  lower_limit = q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outer <- function(clean_answers, cols = names(clean_answers)) {
  for (col in cols) {
    clean_answers <- clean_answers[!answer_outliers(clean_answers[[col]]),]
  }
  clean_answers
}

## Implemeting the outlier removal function
clean_answers <- remove_outer(clean_answers, c('PositiveScore', 'NegativeScore', 'RejectScore', 
                                           'Confidence', 'AnswerBool'))


# Visualizations after cleaning the data
## Histogram of the Positive Score distribution
ggplot(clean_answers, aes(PositiveScore))+
  geom_histogram(fill="orange", bins=20)+ggtitle("Positive Score Distribution")+xlab("Positive Score")

## Scatterplot of Confidence vs. Positive Score
ggplot(clean_answers, aes(Confidence, PositiveScore, size=PositiveScore)) + geom_point()

## Scatterplot of Negative Score vs. Positive Score
ggplot(clean_answers, aes(NegativeScore, PositiveScore, size=PositiveScore)) + geom_point()

## Scatterplot of Is AnswerBool vs. Positive Score
ggplot(clean_answers, aes(AnswerBool, PositiveScore,size=PositiveScore)) + geom_point()

## Boxplot for Positive Score
ggplot(clean_answers, aes(x=PositiveScore)) + geom_boxplot(color="khaki", fill = "silver")

## Boxplot for Is Correct Answer
ggplot(clean_answers, aes(x=Confidence)) + geom_boxplot(color="purple", fill = "orange")

## Boxplot for RejectScore
ggplot(clean_answers, aes(x=RejectScore)) + geom_boxplot(color="darkgreen", fill="salmon")


## Selecting the columns for analysis
answers_select <- clean_answers %>%
  select(PositiveScore, NegativeScore, RejectScore, Confidence, end_year, end_month, end_day)

head(agentanswers_select) #checking the first few rows to make sure that the selected dataset works

## Z-score
zscore_select <- clean_answers %>%
  select(PositiveScore, Confidence, month, day)

z_scores <- as.data.frame(sapply(zscore_select, function(zscore_select) 
  (abs(agentanswers_select-mean(zscore_select))/sd(zscore_select))))
head(z_scores)

## Correlation for selected columns
cleana_cor <- cor(answers_select)
ggcorrplot(cleana_cor)
