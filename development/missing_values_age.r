# missing age values 

library(tidyverse)


preprocess_df <- function(df){
  #Filtering not meaningfull variables at first sight (just for now, for example Embarked)
  
  df <- df %>%
    select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked)
  
  
  
  df <- df %>%
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex))
  
  df <- df %>%
    mutate(Age = ifelse(is.na(Age) && Pclass == 1, 38.2, Age )) %>%
    mutate(Age = ifelse(is.na(Age) && Pclass == 2, 29.9, Age )) %>%
    mutate(Age = ifelse(is.na(Age) && Pclass == 3, 25.1, Age ))
    

}



df_train <- preprocess_df(read_csv("train.csv"))
df_test_full <- read_csv("test.csv")
df_test <- preprocess_df(df_test_full)

# df_train$Pclass
# 
# df_train %>%
#   filter(!is.na(Age)) %>%
#   group_by(Pclass) %>%
#   summarise(mean = mean(Age))
