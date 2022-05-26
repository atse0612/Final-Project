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
clean_agent <- read.csv('./cleanagent.csv')
head(clean_agent)

## Removing outliers
agentanswer_outliers <- function(x) {
  
  q1 <- quantile(x, probs=.25)
  q3 <- quantile(x, probs=.75)
  iqr = q3-q1
  
  upper_limit = q3 + (iqr*1.5)
  lower_limit = q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outer <- function(clean_agent, cols = names(clean_agent)) {
  for (col in cols) {
    clean_agent <- clean_agent[!agentanswer_outliers(clean_agent[[col]]),]
  }
  clean_agent
}

## Implemeting the outlier removal function
clean_agent <- remove_outer(clean_agent, c('TotalHumans', 'DistinctHumans', 'NumberOfContributors', 
                                       'IsContributor', 'IsCorrectAnswer', 'PositiveScore', 'NegativeScore', 'RejectScore'))


# Visualizations after cleaning the data
## Histogram of the Positive Score distribution
ggplot(clean_agent, aes(PositiveScore))+
  geom_histogram(fill="orange", bins=20)+ggtitle("Positive Score Distribution")+xlab("Positive Score")

## Scatterplot of Number of Contributors vs. Positive Score
ggplot(clean_agent, aes(NumberOfContributors, PositiveScore, size=PositiveScore)) + geom_point()

## Scatterplot of Distinct Humans vs. Positive Score
ggplot(clean_agent, aes(DistinctHumans, PositiveScore, size=PositiveScore)) + geom_point()

## Scatterplot of Is Correct Answer vs. Positive Score
ggplot(clean_agent, aes(IsCorrectAnswer, PositiveScore,size=PositiveScore)) + geom_point()

## Boxplot for Positive Score
ggplot(clean_agent, aes(x=PositiveScore)) + geom_boxplot(color="khaki", fill = "silver")

## Boxplot for Is Correct Answer
ggplot(clean_agent, aes(x=IsCorrectAnswer)) + geom_boxplot(color="purple", fill = "orange")

## Boxplot for Total Humans
ggplot(clean_agent, aes(x=TotalHumans)) + geom_boxplot(color="darkgreen", fill="salmon")


## Selecting the columns for analysis
agentanswers_select <- clean_agent %>%
  select(TotalHumans, DistinctHumans, NumberOfContributors, IsContributor, IsCorrectAnswer, 
         PositiveScore, NegativeScore, RejectScore, year, month, day)

head(agentanswers_select) #checking the first few rows to make sure that the selected dataset works

## Z-score
zscore_select <- clean_agent %>%
  select(TotalHumans, DistinctHumans, NumberOfContributors, IsContributor, IsCorrectAnswer, 
         PositiveScore, month, day)

z_scores <- as.data.frame(sapply(zscore_select, function(zscore_select) 
  (abs(agentanswers_select-mean(zscore_select))/sd(zscore_select))))
head(z_scores)

## Correlation for selected columns
cleanaa_cor <- cor(agentanswers_select)
ggcorrplot(cleanaa_cor)




