library(tidyverse)
library(caret)
library(rpart)
library(magrittr)


preprocess_df <- function(df){
  #Filtering not meaningfull variables at first sight (just for now, for example Embarked)
  
  df <- df %>%
    select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked)
  
  
  df <- df %>%
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex))
  
  
  # df <- df %>%
  #   mutate(Age = ifelse(is.na(Age), 29.7, Age))
  
  
  
  return(df)
  
  
  
  
}



accuracy_cv <- function(seed, df) {
  set.seed(seed)
  
  cvSplits <- createFolds(df$Survived, k = 10,
                          returnTrain = TRUE)
  
  acc <- map_dbl(cvSplits, ~ accuracy(training = df[.x, ], test = df[-.x, ]) )
}


df <- preprocess_df(read_csv("train.csv"))



result <- tibble(auc = vector(mode = "numeric"),
                 minsplit = vector(mode = "numeric"),
                 minbucket = vector(mode = "numeric"),
                 maxdepth = vector(mode = "numeric"),
                 cp = vector(mode = "numeric"))


# the minimum number of observations that must exist in a node in order for a
# split to be attempted.
minsplits <- c(10, 20, 30)

# the minimum number of observations in any terminal <leaf> node. If only
# one of minbucket or minsplit is specified, the code either sets minsplit to
# minbucket*3 or minbucket to minsplit/3, as appropriate.
minbuckets <-  c(3, 7, 10)

# Set the maximum depth of any node of the final tree, with the root node counted
# as depth 0. Values greater than 30 rpart will give nonsense results on 32-bit
# machines  r
maxdepths <-  c(5, 10, 20)

cps <- c(0.1, 0.01, 0.001)

for(maxdepth in maxdepths) {
  for(cp in cps){
    for(minsplit in minsplits) {
      for(minbucket in minbuckets){
        
        accuracy <- function(training, test){
          
          fit <- rpart(Survived ~., data=training, method = "class",
                       control = rpart.control(maxdepth = maxdepth, minbucket = minbucket, minsplit = minsplit), cp = cp)
          
          y_probabilities <- predict(fit, test)[,2]
          
          
          y_prediction <- ifelse(y_probabilities > 0.5, 1, 0)
          test_labels <- test$Survived
          
          acc <- sum(y_prediction == test_labels) / length(test_labels)
          
          
          
        }
        
        acc <- map(1:10, ~ accuracy_cv(seed = .x, df = df))
        acc <- unlist(acc)
        
        auc <- mean(acc)
        
        result %<>% 
          add_row(auc = auc, 
                  maxdepth = maxdepth,
                  minsplit = minsplit,
                  minbucket = minbucket,
                  cp = cp)
        
        print(auc)
      }
    }
  }
}



print(result %>%
  arrange(desc(auc)))





