rm(list = ls())

library(stringr)
library(tidyverse)
library(magrittr)

source("model_selection.r")


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
           RelativesFriends = factor(RelativesFriends),
           
           Ticket = str_sub(Ticket,1,1),
           Ticket = case_when(
             str_detect(Ticket, "^\\d") ~ "Digit",
             TRUE ~ Ticket
           ),
           Ticket = factor(Ticket)
           
           
    ) 
  
  df$Embarked[is.na(df$Embarked)] <- "S"
  
  df  <- df %>% select(-PassengerId, -Name)
  
  
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


preprocess_temporary <- function(df){
  
  df %<>%
    select(-PassengerId, -Ticket) %>% 
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex),
           
           Embarked = factor(ifelse(is.na(Embarked), "S", Embarked)),

           
           
           Age = ifelse(is.na(Age), 29.7, Age),
           
           Fare = ifelse(is.na(Fare), 0, Fare)) %>%
    
    mutate(Title = str_extract(Name, "[a-zA-Z]*\\."),
           Title = factor(Title),
           TitleGroup = case_when(
             str_detect(Title, "(Mrs|Miss|Ms)\\.") ~ "Miss",
             str_detect(Title, "(Mr)\\.") ~ "Mr",
             str_detect(Title, "(Master)\\.") ~ "Master",
             TRUE ~ "Special"
           ),
           TitleGroup = factor(TitleGroup)) %>%
    
    mutate( Cabin = str_sub(df$Cabin,1,1),
            Cabin = case_when(
              str_detect(Cabin, "(D|E|B)") ~ "Top",
              str_detect(Cabin, "(F|C|G|A|T)") ~ "AVG",
              TRUE ~ "NotAv"
            ),
            Cabin = factor(Cabin)) %>%
    
    mutate(RelativesFriends = SibSp + Parch,
           RelativesFriends = case_when(
             RelativesFriends == 0 ~ "None",
             RelativesFriends  == 1 ~ "One",
             RelativesFriends  <= 4 ~ "Small",
             TRUE ~ "Large"
    ),
    RelativesFriends = factor(RelativesFriends)) %>%
    
    select(-Title, -Name)
  

  
  return(df)
  
  
}

preprocess_cabin_embarked <- function(df){
  
  df %<>%
    select(-PassengerId, -Ticket, -Name) %>% 
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex),
           
           Cabin = ifelse(is.na(Cabin), "NotAv", str_sub(Cabin,1,1)),
           Cabin = factor(Cabin, levels = c("NotAv", "C", "E", "G", "D", "A", "B", "F", "T")),
           
           Embarked = ifelse(is.na(Embarked), "NotAv", Embarked),
           Embarked = factor(Embarked, levels = c("NotAv", "S", "C", "Q")),
           
           Age = ifelse(is.na(Age), 29.7, Age),
           
           Fare = ifelse(is.na(Fare), 0, Fare))
  
  
  
  return(df)
  
  
}

preprocess_na_overall_mean <- function(df){
  
  df %<>%
    select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked) %>% 
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex),

           Age = ifelse(is.na(Age), 29.7, Age),
           
           Fare = ifelse(is.na(Fare), 0, Fare))
  
  
  
  return(df)
  
}

preprocess_na_drop <- function(df){
  
  df %<>%
    select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked) %>% 
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex)) %>%
    filter(!is.na(Age), !is.na(Fare))

  return(df)
}




df_raw <- read_csv("train.csv")



sink(file.path("log", "EVALUATION_engin.txt"))

df <- preprocess_final(df_raw)


fit <- model_evaluation(df,cores = 4, tuneLength = 1, repeats = 1,
                        do_print = TRUE, adaboost = FALSE)

sink()


