# EDA Univariate
rm(list = ls())


library(tidyverse)
library(caret)
library(e1071)
library(corrplot)


df <- read_csv("train.csv")



# Overview ----------------------------------------------------------------



#Filtering not meaningfull variables at first sight (just for now, for example Embarked)
df <- df %>%
  select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked)

df <- df %>%
  mutate(Pclass = factor(Pclass),
         Sex = factor(Sex))

head(df)


# near zero variance
nearZeroVar(df)


#correlations: impute age with overall mean
correlations <- cor(df %>% 
                      select(Age, SibSp, Parch, Fare) %>%
                      mutate(Age = ifelse(is.na(Age), 29.7, Age)))
corrplot(correlations, method = "square")
corrplot.mixed(correlations)

#find high correlations [Kuhn]
highCorr <- findCorrelation(correlations, cutoff = .4)
length(highCorr)
head(highCorr)
df_filtered <- df[, -highCorr]



# Target ------------------------------------------------------------------
# Slightly more 0 than 1 (60% to 40%)

sum(is.na(df$Survived))

ggplot(df, aes(Survived)) + 
  geom_bar() +
  theme_bw()

table(df$Survived)

length(df$Survived[df$Survived == 1]) / length(df$Survived)


# Categorial --------------------------------------------------------------

# Pclass
summary(df$Pclass)

ggplot(df, aes(Pclass)) + 
  geom_bar() +
  theme_bw()

# Sex
summary(df$Sex)

ggplot(df, aes(Sex)) + 
  geom_bar() +
  theme_bw()


# Continuous --------------------------------------------------------------


# Age

summary(df$Age)
sum(is.na(df$Age)) / length(df$Age)

ggplot(df, aes(Age)) + 
  geom_density(kernel = "gaussian") + 
  theme_bw()

skewness(df$Age, na.rm = TRUE, type = 1)


# Fare

summary(df$Fare)
sum(is.na(df$Fare)) / length(df$Fare)

ggplot(df, aes(Fare)) + 
  geom_density(kernel = "gaussian") + 
  theme_bw()

skewness(df$Fare, na.rm = TRUE, type = 1)

#Apply BoxCox Trafo because of skewnes: ToDo: check 0 entries 
#Fix zero values
df <- df %>%
  mutate(FareBoxCox = ifelse(df$Fare < 0.0001, 0.0001, df$Fare))

fit <- BoxCoxTrans(df$FareBoxCox)

df <- df %>%
  mutate(FareBoxCox = predict(fit, FareBoxCox))

ggplot(df, aes(FareBoxCox)) + 
  geom_density(kernel = "gaussian") + 
  theme_bw()


# SibSp - How to treat integers? In this case maybe bin into two groups: 0 and rest

sum(is.na(df$SibSp))
sum(is.na(df$SibSp)) / length(df$SibSp)

ggplot(df, aes(SibSp)) + 
  geom_bar() + 
  theme_bw()

skewness(df$SibSp, na.rm = TRUE, type = 1)


# Parch - How to treat integers? In this case maybe bin into two groups: 0 and rest

sum(is.na(df$Parch))
sum(is.na(df$Parch)) / length(df$Parch)

ggplot(df, aes(Parch)) + 
  geom_bar() + 
  theme_bw()

skewness(df$Parch, na.rm = TRUE, type = 1)

