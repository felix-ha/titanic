library(caret)
library(tidyverse)
library(e1071)

df <- read_csv("train.csv")

#Fix zero values
df <- df %>%
  mutate(Fare = ifelse(df$Fare < 0.0001, 0.0001, df$Fare))

skewness(df$Fare)

ggplot(df, aes(Fare)) + 
  geom_density(kernel = "gaussian") + 
  theme_bw()

ggplot(df, aes(log(Fare))) + 
  geom_density(kernel = "gaussian") + 
  theme_bw()


fit<- BoxCoxTrans(df$Fare)
fit

Fare_trans <- predict(fit, df$Fare)

df <- df %>%
  mutate(Fare_trans = Fare_trans)

ggplot(df, aes(Fare_trans)) + 
  geom_density(kernel = "gaussian") + 
  theme_bw()

skewness(log(df$Fare))
skewness(df$Fare_trans)
