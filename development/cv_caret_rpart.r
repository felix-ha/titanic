library(tidyverse)
library(caret)


preprocess_df <- function(df){
  #Filtering not meaningfull variables at first sight (just for now, for example Embarked)
  
  df <- df %>%
    select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked)
  
  
  df <- df %>%
    mutate(Survived = factor(Survived),
           Pclass = factor(Pclass),
           Sex = factor(Sex))
  

  df <- df %>%
    mutate(Age = ifelse(is.na(Age), 29.7, Age))

  
  
  return(df)
  
  
  
  
}


df <- preprocess_df(read_csv("train.csv"))




train_control <- trainControl(method = "repeatedcv", repeats = 2, number = 10)


fit  <-  train(
  form = Survived ~.,
  data = df,
  trControl = train_control,
  method = "rpart",
  tuneLength = 100
  #,tuneGrid = data.frame(cp = seq(0.0001, 0.001,0.0001))
  #,control = rpart.control(minsplit = 20, minbucket = 2)
)


#



print(head(fit$results))
