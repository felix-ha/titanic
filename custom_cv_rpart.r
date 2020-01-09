library(caret)
library(MLmetrics)
library(rpart)
library(tidyverse)
library(magrittr)

preprocess_df <- function(df){
  #Filtering not meaningfull variables at first sight (just for now, for example Embarked)
  
  df <- df %>%
    select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked)
  
  
  
  df <- df %>%
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex))
}



df <- preprocess_df(read_csv("train.csv"))


set.seed(25)
number_of_folds <- 10
folds <- createFolds(df$Survived, k = number_of_folds)


f <- function(maxdepth, minsplit, minbucket, cp) {
  
  auc <- vector(mode = "numeric", length = number_of_folds)
  
  for(fold_index in c(1:number_of_folds)){
    training <- df[-folds[[fold_index]],]
    test <- df[folds[[fold_index]],]
    
    fit <- rpart(Survived ~ ., data=training, method = "class",
                 control = rpart.control(maxdepth = maxdepth, minbucket = minbucket, minsplit = minsplit), cp = cp)
    # using ratio of poisitive labels as probability
    y_probabilities <- predict(fit, test)[,2]
    
    
    y_true <- test$Survived
    
    auc[fold_index] <- AUC(y_true = y_true, y_pred = y_probabilities)
    
    
  }
  
  return(mean(auc))
  
}


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
        
        auc <- f(maxdepth, minsplit, minbucket,  cp)
        result %<>% 
          add_row(auc = auc, 
                  maxdepth = maxdepth,
                  minsplit = minsplit,
                  minbucket = minbucket,
                  cp = cp)
        
      }
    }
  }
}

result  <- result %>%
  arrange(desc(auc))

head(result, n = 5)
