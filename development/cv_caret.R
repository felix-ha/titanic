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


fit  <-  train(
  form = Survived ~.,
  data = df,
  trControl = trainControl(method = "repeatedcv", repeats = 10, number = 10),
  method = "glm",
  family = "binomial"
)
#

print(summary(fit))

head(fit$results)
