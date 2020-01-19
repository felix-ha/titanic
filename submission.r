rm(list = ls())
library(tidyverse)
library(rpart)
library(magrittr)
library(caret)
library(xgboost)

training <- read_csv("train.csv")
test <- read_csv("test.csv")

n_training <- nrow(training)
PassengerId_test <- test$PassengerId


df <- training %>%
  select(-Survived) %>%
  bind_rows(test)


ex_title <- "[a-zA-Z]*\\."

df <- df %>%
  mutate(Pclass = factor(Pclass),
         Sex = factor(Sex),
         Embarked = factor(Embarked, levels = c("C", "Q", "S")),
         
         Fare = ifelse(is.na(Fare), mean(Fare, na.rm = TRUE), Fare),
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
df_impute <- df 

mean_age = mean(df$Age, na.rm = TRUE)
sd_age = sd(df$Age, na.rm = TRUE)


fit  <- preProcess(df_impute, method = c("center", "scale", "knnImpute"), k = 5)
df_imputed <- predict(fit, df_impute)


df <- df %>%
  mutate(Age = df_imputed$Age * sd_age + mean_age)


#-----------------------




training <- df[1:n_training, ] %>%
  mutate(Survived = factor(training$Survived))

test <- df[(n_training+1) : nrow(df), ]


threshold <- 0.5


predict_logistic_regression <- function(training, test) {
  
  fit <- glm(Survived ~ ., data=training, family =binomial(link = "logit"))
  
  y_probabilities <- predict(fit, test,  type="response")
  y_predicted <- ifelse(y_probabilities <= threshold, 0, 1)
  return(y_predicted)
  
}




predict_xgboost <- function(training, test) {
  
  df_model_matrix <- model.matrix(Survived ~ .-1, training)
  dtrain <- xgb.DMatrix(df_model_matrix, label = as.numeric(as.character(training$Survived)))
  
  
  param <- list(eta = 0.1,
                max_depth = 8,
                colsample_bytree = 0.8,
                min_child_weight = 1,
                subsample = 0.9,
                verbose = 0,
                nthread = 2)
  
  fit <- xgb.train(param, dtrain, nrounds = 100, objective = "binary:logistic")
  
  df_model_matrix <- model.matrix( ~ .-1, test)
  dtest <- xgb.DMatrix(df_model_matrix)
  y_probabilities <- predict(fit, dtest)
  
  y_probabilities <- ifelse(y_probabilities <= threshold, 0, 1)
  return(y_probabilities)
}

predict_svm <- function(training, test) {

  train_control <- trainControl(method = "repeatedcv", repeats = 1, number = 2, allowParallel=F)
  
  svm.grid <- expand.grid(sigma = 0.0563,
                          C =  4.5)
  
  
  fit <- train(form = Survived ~.,
               data = training,
               trControl = train_control,
               method = "svmRadial",
               preProc = c("center", "scale"),
               tuneGrid =  svm.grid)
  
  y_probabilities <- predict(fit, test)
  
  return(as.integer(y_probabilities) - 1)
}


predict_gbm <- function(training, test) {
  
  train_control <- trainControl(method = "repeatedcv", repeats = 1, number = 2, allowParallel=F)
  
  gbm.grid <- expand.grid(shrinkage = 0.1,
                          interaction.depth = 10, 
                          n.minobsinnode = 10, 
                          n.trees= 50)
 

  fit  <-  train(
    form = Survived ~.,
    data = training,
    trControl = train_control,
    method = "gbm",
    tuneGrid =  gbm.grid
  )
  
  y_preditions <- as.integer(predict(fit, test)) - 1
  
  return(y_preditions)

}

predict_random_forest <- function(training, test) {
  
  train_control <- trainControl(method = "repeatedcv", repeats = 1, number = 2, allowParallel=F)
  
  ranger.grid <- expand.grid(mtry = 7, min.node.size = 1,  splitrule = "gini")
  
  
  fit  <-  train(
    form = Survived ~.,
    data = training,
    trControl = train_control,
    method = "ranger",
    tuneGrid =   ranger.grid
    
  )

  y_preditions <- as.integer(predict(fit, test)) - 1
  
  return(y_preditions)
  
}




predictions <- c(LogisticRegression = predict_logistic_regression(training, test),
                 xgb = predict_xgboost(training, test),
                 svm = predict_svm(training, test),
                 gbm = predict_gbm(training, test),
                 random_forest = predict_random_forest(training, test))



observations <-  nrow(test)
number_of_models <- as.integer(length(predictions) / observations)

M <- matrix(data = predictions, nrow = number_of_models, ncol = observations , byrow = TRUE)
y_predicted <- colMeans(M)

y_predicted <- as.integer(ifelse(y_predicted <= threshold, 0, 1))

prediction_result <- tibble( PassengerId = PassengerId_test,
                             Survived= y_predicted)

write_csv(prediction_result, "submission.csv")


#-----------------

library(corrplot)

df_cor <- tibble(LogReg = M[1, ], 
                 xgBoost = M[2, ], 
                 svm = M[3, ],
                 gbm = M[4,],
                 random = M[5, ])

correlations <- cor(df_cor)
corrplot.mixed(correlations)
