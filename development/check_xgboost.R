rm(list = ls())

library(tidyverse)
library(magrittr)
library(xgboost)


preprocess <- function(df){
  
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

df <- preprocess(read_csv("train.csv"))

trainingRows <- createDataPartition(df$Survived,
                                    p = .80,
                                    list= FALSE)

training <- df[trainingRows, ]
test <- df[-trainingRows, ]



df_model_matrix <- model.matrix(Survived ~ .-1, training)

dtrain <- xgb.DMatrix(df_model_matrix, label = as.numeric(as.character(training$Survived)))

param <- list(max_depth = 2, eta = 0.1, verbose = 0, nthread = 2)
fit <- xgb.train(param, data = dtrain, nrounds = 20, objective = "binary:logistic")

# fit <- xgboost(data = dtrain, max_depth = 5,
#                nthread = 2, nrounds = 5, objective = "binary:logistic")


df_model_matrix <- model.matrix(Survived ~ .-1, test)
dtest <- xgb.DMatrix(df_model_matrix, label = test$Survived)
y_probabilities <- predict(fit, dtest, outputmargin=F)

print(y_probabilities)