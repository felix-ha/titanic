rm(list = ls())

library(tidyverse)
library(rpart)
library(magrittr)
library(gbm)


# n<- 50
# 
# y_true <- rbinom(n, 1, 0.5)
# 
# eps <- runif(n)
# y_pred <- ifelse(y_true == 0, eps, 1-eps)

plot_calibration <- function(y_true, y_pred) {

df <- tibble(y_true = y_true, 
             y_pred = y_pred) 

df <- df %>%
  mutate(buckets = cut(df$y_pred, breaks = seq(0,1,0.1))) %>%
  group_by(buckets) %>%
  summarise(p_pred = mean(y_pred),
            p_true = mean(y_true),
            count = n()) %>%
  mutate(bucket_midpoints = seq(0.05,1,0.1))



p <- ggplot(df) + 
  geom_point(aes(x = bucket_midpoints, y = p_true), size = 3) +
  geom_line(aes(x = bucket_midpoints, y = p_true)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ylim(c(0,1)) + 
  labs(
    x = "Bucket Midpoints of Predictions",
    y = "Observed Event Percentage",
    title = "Calibration") + 
  theme_bw()  +
  scale_x_continuous(breaks = seq(0.05, 0.95, 0.1))


 print(df)

  return(p)

}




predict_gbm <- function(training, test) {
  
  training$Survived <- as.integer(training$Survived) - 1
  
  fit <- gbm(Survived ~ ., data = training, distribution = "bernoulli",
             n.trees = 50, interaction.depth = 1,
             n.minobsinnode = 10, shrinkage = 0.1)
  
  
  
  y_predicted <- predict(fit, test, type = "response", n.trees = 50)
  
  
  return(y_predicted)
  
}

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
           TitleGroup = factor(TitleGroup,levels = c("Miss", "Mr", "Master", "Special"))) %>%
    
    mutate( Cabin = str_sub(df$Cabin,1,1),
            Cabin = case_when(
              str_detect(Cabin, "(D|E|B)") ~ "Top",
              str_detect(Cabin, "(F|C|G|A|T)") ~ "AVG",
              TRUE ~ "NotAv"
            ),
            Cabin = factor(Cabin, levels = c("Top", "AVG", "NotAv"))) %>%
    
    select(-Title, -Name)
}





training <- preprocess_df(read_csv("train.csv"))


training %<>% mutate(Survived = factor(Survived))

test <- training[1:150, ] 
training <- training[151:nrow(training),] 

predictions <- c(LogisticRegression = predict_logistic_regression(training, test),
                 rpart = predict_rpart(training, test),
                 gbm = predict_gbm(training, test))

observations <-  nrow(test)
number_of_models <- as.integer(length(predictions) / observations)

M <- matrix(data = predictions, nrow = number_of_models, ncol = observations , byrow = TRUE)
y_predicted <- colMeans(M)


y_true <- as.integer(test$Survived) - 1
p <- plot_calibration(y_true, y_predicted)
print(p)






