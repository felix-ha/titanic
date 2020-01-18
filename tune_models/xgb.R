
rm(list = ls())

library(tidyverse)
library(caret)
library(magrittr)



preprocess_final <- function(df) {
  
  ex_title <- "[a-zA-Z]*\\."
  
  df <- df %>%
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex),
           Embarked = factor(Embarked, levels = c("C", "Q", "S")),
           Survived = factor(Survived),
           
           Fare = sqrt(Fare),
           
           Title = str_extract(Name, ex_title),
           Title = case_when(
             str_detect(Title, "(Mrs|Miss|Ms)\\.") ~ "Miss",
             str_detect(Title, "(Mr)\\.") ~ "Mr",
             str_detect(Title, "(Master)\\.") ~ "Master",
             TRUE ~ "Special"),
           Title = factor(Title),
           
           Cabin = str_sub(df$Cabin,1,1),
           Cabin = case_when(
             str_detect(Cabin, "(D|E|B)") ~ "Top",
             str_detect(Cabin, "(F|C|G|A|T)") ~ "AVG",
             TRUE ~ "NotAv"),
           Cabin = factor(Cabin),
           
           RelativesFriends = SibSp + Parch,
           RelativesFriends = case_when(
             RelativesFriends == 0 ~ "None",
             RelativesFriends  == 1 ~ "One",
             RelativesFriends  <= 4 ~ "Small",
             TRUE ~ "Large"),
           RelativesFriends = factor(RelativesFriends)
           
           
    ) 
  
  df$Embarked[is.na(df$Embarked)] <- "S"
  
  df  <- df %>% select(-PassengerId, -Ticket, -Name)
  
  
  # Impute Age with knn---------------------
  df_impute <- df %>% select( -Survived)
  
  mean_age = mean(df$Age, na.rm = TRUE)
  sd_age = sd(df$Age, na.rm = TRUE)
  
  
  fit  <- preProcess(df_impute, method = c("center", "scale", "knnImpute"), k = 5)
  df_imputed <- predict(fit, df_impute)
  
  
  df <- df %>%
    mutate(Age = df_imputed$Age * sd_age + mean_age)
  
  
  #-----------------------
  
  return(df)
  
}


df_raw <- read_csv("train.csv")
df <- preprocess_final(df_raw)


set.seed(2017)



train_control <- trainControl(method = "repeatedcv", repeats = 2, number = 10)

fit  <-  train(
  form = Survived ~.,
  data = df,
  trControl = train_control,
  method = "xgbTree",
  tuneLength = 1
)
