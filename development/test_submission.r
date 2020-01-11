rm(list = ls())
library(tidyverse)
library(xgboost)


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






df_model_matrix <- model.matrix(Survived ~ .-1, training)
dtrain <- xgb.DMatrix(df_model_matrix, label = as.numeric(as.character(training$Survived)))

param <- list(max_depth = 2, eta = 0.3, verbose = 0, nthread = 2)

fit <- xgb.train(param, dtrain, nrounds = 100, objective = "binary:logistic")

df_model_matrix <- model.matrix( ~ .-1, test)
dtest <- xgb.DMatrix(df_model_matrix)
y_probabilities <- predict(fit, dtest)

y_predicted <- ifelse(y_probabilities > 0.5, 1, 0)



# fit <- glm(Survived ~ ., data=df_train, family =binomial(link = "logit"))
# 
# 
# y_predicted <- predict(fit, df_test,  type="response")
# 
# threshold <- 0.5
# y_predicted <- ifelse(y_predicted <= threshold, 0, 1)
#  <- ifelse(is.na(y_predicted), 0, y_predicted)

prediction_result <- tibble( PassengerId = test_full$PassengerId,
                             Survived= y_predicted)

write_csv(prediction_result, "submission.csv")

