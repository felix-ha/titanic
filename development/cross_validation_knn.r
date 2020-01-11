library(tidyverse)
library(caret)
library(class)


preprocess_df <- function(df){
  #Filtering not meaningfull variables at first sight (just for now, for example Embarked)
  
  df <- df %>%
    select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked)
  
  
  df <- df %>%
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex))
  
  
  df <- df %>%
    mutate(Age = ifelse(is.na(Age), 29.7, Age))
  

  
  return(df)
  

  

}


accuracy <- function(training, test){
  
  
  
  
  training <- model.matrix(~ .-1, training)
  training <- training[,-1]

  test <- model.matrix(~ .-1, test)
  test <- test[,-1]

  train_points <- training[,-1]
  test_points <- test[,-1]

  train_labels <- training[,1]
  test_labels <- test[,1]

  fit <- knn(train_points, test_points, train_labels, k = 50, prob = TRUE);
  y_probabilities <- (attributes(fit)$prob)
  
  y_prediction <- ifelse(y_probabilities > 0.9999, 1, 0)
  
  acc <- sum(y_prediction == test_labels) / length(test_labels)
  

  
}


accuracy <- function(training, test){

 fit <- glm(Survived ~., data = training, family = binomial(link = "logit"))

 y_probabilities <- predict(fit, test,  type="response")

 y_prediction <- ifelse(y_probabilities > 0.5, 1, 0)
 test_labels <- test$Survived

 acc <- sum(y_prediction == test_labels) / length(test_labels)



}


accuracy_cv <- function(seed, df) {
  set.seed(seed)
  
  cvSplits <- createFolds(df$Survived, k = 10,
                          returnTrain = TRUE)
  
  acc <- map_dbl(cvSplits, ~ accuracy(training = df[.x, ], test = df[-.x, ]) )
}


df <- preprocess_df(read_csv("train.csv"))

acc <- map(1:10, ~ accuracy_cv(seed = .x, df = df))
acc <- unlist(acc)

print(mean(acc))
print(sd(acc))











