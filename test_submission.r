library(tidyverse)


preprocess_df <- function(df){
  #Filtering not meaningfull variables at first sight (just for now, for example Embarked)
  
  df <- df %>%
    select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked)
  
  
  
  df <- df %>%
    mutate(Pclass = factor(Pclass),
           Sex = factor(Sex))
}



df_train <- preprocess_df(read_csv("train.csv"))
df_test_full <- read_csv("test.csv")
df_test <- preprocess_df(df_test_full)



fit <- glm(Survived ~ ., data=df_train, family =binomial(link = "logit"))


y_predicted <- predict(fit, df_test,  type="response")

threshold <- 0.5
y_predicted <- ifelse(y_predicted <= threshold, 0, 1)
y_predicted <- ifelse(is.na(y_predicted), 0, y_predicted)

prediction_result <- tibble( PassengerId = df_test_full$PassengerId,
                             Survived= y_predicted)

write_csv(prediction_result, "submission.csv")
