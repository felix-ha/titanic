rm(list = ls())

library(stringr)
library(tidyverse)
library(magrittr)

source("model_selection.r")


preprocess_temporary <- function(df){
  
  df %<>%
    select(-PassengerId, -Ticket, -Cabin, -Embarked) %>% 
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex),
           
           
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

df <- preprocess_temporary(df_raw)


fit <- model_evaluation(df,cores = 3, tuneLength = 3, repeats = 1,
                        do_print = TRUE, adaboost = FALSE)

sink()


