library(tidyverse)
library(rpart)
library(magrittr)



predict_rpart <- function(training, test, maxdepth = 10, minbucket = 7,  minsplit = 10, cp = 0.1) {
  
  fit <- rpart(Survived ~ ., data=training, method = "class",
               control = rpart.control(maxdepth = maxdepth,
                                       minbucket = minbucket, 
                                       minsplit = minsplit),
               cp = cp)
  
  y_predicted <- predict(fit, test)[,2]
  
  return(y_predicted)
  
}


predict_logistic_regression <- function(training, test) {
  
  fit <- glm(Survived ~ ., data=training, family =binomial(link = "logit"))
  
  y_predicted <- predict(fit, test,  type="response")
  
  return(y_predicted)
  
}

preprocess_df <- function(df){
  #Filtering not meaningfull variables at first sight (just for now, for example Embarked)
  
  df %<>%
    select(-PassengerId, -Ticket, -Embarked) %>% 
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex),
           
           
           Age = ifelse(is.na(Age), 29.7, Age),
           
           Fare = ifelse(is.na(Fare), 0, Fare)) %>%
    
    mutate(Title = str_extract(Name, "[a-zA-Z]*\\."),
           Title = factor(Title),
           TitleGroup = case_when(
             str_detect(Title, "(Mrs|Miss|Ms)\\.") ~ "Miss",
             str_detect(Title, "(Mr)\\.") ~ "Mr",
             str_detect(Title, "(Master)\\.") ~ "Master",
             TRUE ~ "Special"
           ),
           TitleGroup = factor(TitleGroup)) %>%
    
    mutate( Cabin = str_sub(df$Cabin,1,1),
            Cabin = case_when(
              str_detect(Cabin, "(D|E|B)") ~ "Top",
              str_detect(Cabin, "(F|C|G|A|T)") ~ "AVG",
              TRUE ~ "NotAv"
            ),
            Cabin = factor(Cabin)) %>%
    
    select(-Title, -Name)
}





training <- preprocess_df(read_csv("train.csv"))
test_full <- read_csv("test.csv")
test <- preprocess_df(test_full)

training %<>% mutate(Survived = factor(Survived))



predictions <- c(LogisticRegression = predict_logistic_regression(training, test),
                 rpart = predict_rpart(training, test))

observations <-  nrow(test)
number_of_models <- as.integer(length(predictions) / observations)

M <- matrix(data = predictions, nrow = number_of_models, ncol = observations , byrow = TRUE)
y_predicted <- colMeans(M)


threshold <- 0.5
y_predicted <- ifelse(y_predicted <= threshold, 0, 1)


prediction_result <- tibble( PassengerId = test_full$PassengerId,
                             Survived= y_predicted)

write_csv(prediction_result, "submission.csv")
