rm(list = ls())
library(tidyverse)
library(xgboost)
library(magrittr)
library(rpart)


preprocess_age_overall_mean <- function(df){

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



training <- preprocess_age_overall_mean(read_csv("train.csv"))
test_full <- read_csv("test.csv")
test <- preprocess_age_overall_mean(test_full)

map_dbl(read_csv("train.csv"), ~sum(is.na(.x)))
map_dbl(test_full, ~sum(is.na(.x)))





training %<>% mutate(Survived = factor(Survived))

fit <- glm(Survived ~ ., data=training, family =binomial(link = "logit"))
y_probabilities_glm <- predict(fit, test,  type="response")

fit <- rpart(Survived ~., data=training, method = "class",
             control = rpart.control(maxdepth = 5, minbucket = 10, minsplit = 10), cp = 0.0001)
y_probabilities_rpart <- predict(fit, test)[,2]




threshold <- 0.5
ensemble <- (y_probabilities_glm + y_probabilities_rpart ) / 2

y_predicted <- ifelse(ensemble <= threshold, 0, 1)


prediction_result <- tibble( PassengerId = test_full$PassengerId,
                             Survived= y_predicted)

write_csv(prediction_result, "submission.csv")

