rm(list = ls())

library(stringr)
library(tidyverse)
library(magrittr)

source("model_selection.r")


preprocess_age_overall_mean_convert_cabin <- function(df) {
  df <- df %>%
    select(-PassengerId, -Ticket, -Name)
  
  df <- df %>% 
    mutate(Survived = as_factor(Survived),
           Pclass = as_factor(Pclass),
           Sex = factor(Sex),
           Cabin = ifelse(is.na(Cabin), "NA", str_sub(Cabin,1,1)),
           Cabin = as_factor(Cabin),
           Embarked = ifelse(is.na(Embarked), "NA", Embarked),
           Embarked = as_factor(Embarked))
  
  df <- df %>%
    mutate(Age = ifelse(is.na(Age), 29.7, Age))
  
  return(df)
}

preprocess_age_overall_mean <- function(df){
  
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

preprocess_age_drop_na <- function(df){
  
  # print("Pre-Processing: Drop NA's")
  # cat("\n")
  
  #Filtering not meaningfull variables at first sight (just for now, for example Embarked)
  df <- df %>%
    select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked)
  
  df <- df %>%
    mutate(Survived = factor(Survived),
           Pclass = factor(Pclass),
           Sex = factor(Sex))
  
  df <- df %>%
    filter(!is.na(Age))
  
  return(df)
}




df_raw <- read_csv("train.csv")


df <- preprocess_age_overall_mean(df_raw)
df_cabin <- preprocess_age_overall_mean_convert_cabin(df_raw)


fit <- model_evaluation(df, cores = 4, do_print = TRUE, short_val = FALSE)

