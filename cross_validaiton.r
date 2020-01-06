library(tidyverse)
library(caret)

preprocess_df <- function(df){

  df$Age[is.na(df$Age)] = 29.70
  
  #Filtering not meaningfull variables at first sight (just for now, for example Embarked)
  df <- df %>%
    select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked)
  
  
  
  df <- df %>%
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex),
           Survived = factor(Survived))
}



df_train <- preprocess_df(read_csv("train.csv"))

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)


cvfit <- train(Survived ~ ., data = df_train, 
                 method = "glm", 
                 family=binomial,
                 trControl = fitControl)
cvfit
