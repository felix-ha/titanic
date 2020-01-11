rm(list = ls())

library(tidyverse)
library(caret)
library(magrittr)
library(xgboost)
library(doParallel)


grid_search_xgboost <- function(df) {

  result <- tibble(nrounds = vector(mode = "numeric"),
                   eta = vector(mode = "numeric"),
                   max_depth = vector(mode = "numeric"),
                   Accuracy = vector(mode = "numeric"),
                   AccuracySD = vector(mode = "numeric"))
  
  
  etas <- c(0.1, 0.25)
  nroundss <- c(50, 100, 200)
  max_depths <- c(2,4, 10)
  
  
  for(max_depth in max_depths){
    for(eta in etas) {
      for(nrounds in nroundss){
   
  accuracy <- function(training, test){
    
    df_model_matrix <- model.matrix(Survived ~ .-1, training)
    dtrain <- xgb.DMatrix(df_model_matrix, label = as.numeric(as.character(training$Survived)))

    
    param <- list(max_depth = max_depth, eta = eta, verbose = 0, nthread = 2)
    
    fit <- xgb.train(param, dtrain, nrounds = nrounds, objective = "binary:logistic")

    df_model_matrix <- model.matrix(Survived ~ .-1, test)
    dtest <- xgb.DMatrix(df_model_matrix, label =  as.numeric(as.character(test$Survived)))
    y_probabilities <- predict(fit, dtest)

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
  
  
  
  accuracy <- map(1:10, ~ accuracy_cv(seed = .x, df = df))
  accuracy <- unlist(accuracy)
  
  result %<>% add_row(nrounds = nrounds,
                      eta = eta,
                      max_depth = max_depth,
                      Accuracy = mean(accuracy),
                      AccuracySD = sd(accuracy))
  
      }
    }
    
    }
  
  return(result)
  
}


model_evaluation <- function(df, cores = 1, do_print = FALSE, short_val = FALSE){
  
  
  set.seed(2017)
  
  cl <- makePSOCKcluster(cores)
  registerDoParallel(cl)
  on.exit(stopCluster(cl))
  
  
  result <- tibble(Model = vector(mode = "character"),
                   Accuracy = vector(mode = "numeric"))
  
  
  train_control <- trainControl(method = "repeatedcv", repeats = 10, number = 10)

  

# XGBoost -----------------------------------------------------------------

  fit_result <- grid_search_xgboost(df)
  
  
  fit_result <- fit_result %>%
    arrange(desc(Accuracy))
  
  result %<>% add_row(Model = "xgboost",
                      Accuracy = fit_result$Accuracy[1])
  
  
  if(do_print) {
    print("xgboost")
    print(fit_result)
    cat("\n")
  }
  
# Logistic Regression -----------------------------------------------------


  fit  <-  train(
    form = Survived ~.,
    data = df,
    trControl = train_control,
    method = "glm",
    family = "binomial"
  )

  fit_result <- as_tibble(fit$results) %>%
    arrange(desc(Accuracy))
  
  result %<>% add_row(Model = "glm",
                      Accuracy = fit_result$Accuracy[1])
  if(do_print) {
    print("glm")
    print(fit_result)
    cat("\n")
  }
  
  
  if(do_print && short_val) {
    result %<>% arrange(desc(Accuracy))
    print(result)
    return(result)
  }
  

# rpart -------------------------------------------------------------------


  fit  <-  train(
    form = Survived ~.,
    data = df,
    trControl = train_control,
    method = "rpart",
    tuneLength = 10
  )

  fit_result <- as_tibble(fit$results) %>%
    arrange(desc(Accuracy))
  
  result %<>% add_row(Model = "rpart",
                      Accuracy = fit_result$Accuracy[1])
  if(do_print) {
    print("rpart")
    print(fit_result)
    cat("\n")
  }




# bagging -----------------------------------------------------------------

  fit  <-  train(
    form = Survived ~.,
    data = df,
    trControl = train_control,
    method = "treebag"
  )
  
  fit_result <- as_tibble(fit$results) %>%
    arrange(desc(Accuracy))

  result %<>% add_row(Model = "bag",
                      Accuracy = fit_result$Accuracy[1])
  if(do_print) {
    print("bag")
    print(fit_result)
    cat("\n")
  }
  
  
  
  
  # SVM linear --------------------------------------------------------------
  
  
  fit <- train(form = Survived ~.,
               data = df,
               trControl = train_control,
               method = "svmLinear",
               preProc = c("center", "scale"),
               tuneLength = 10)  
  
  fit_result <- as_tibble(fit$results) %>%
    arrange(desc(Accuracy))
  
  result %<>% add_row(Model = "SVM Linear",
                      Accuracy = fit_result$Accuracy[1])
  if(do_print) {
    print("SVM Linear")
    print(fit_result)
    cat("\n")
  }

  

# SVM Radial ---------------------------------------------------------------------

  fit <- train(form = Survived ~.,
                  data = df,
                  trControl = train_control,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneLength = 10)  

  fit_result <- as_tibble(fit$results) %>%
    arrange(desc(Accuracy))
  
  result %<>% add_row(Model = "SVM Radial",
                      Accuracy = fit_result$Accuracy[1])
  if(do_print) {
    print("SVM Radial")
    print(fit_result)
    cat("\n")
  }
  



  result %<>% arrange(desc(Accuracy))
  
  if(do_print) {
    print(result)
  }
  

return(result)


  
}





# Benchmark  --------------------------------------------------------------


# library(microbenchmark)
# res <- microbenchmark(model_evaluation(df, cores = 1), 
#                        model_evaluation(df, cores = 2),
#                       model_evaluation(df, cores = 4), times=3L)
# 
# print(res)