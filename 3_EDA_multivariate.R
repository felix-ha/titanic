# EDA Univariate

library(tidyverse)


df <- read_csv("train.csv")

#Filtering not meaningfull variables at first sight (just for now, for example Embarked)
df <- df %>%
  select(-PassengerId, -Ticket, -Name, -Cabin, -Embarked)

df <- df %>%
  mutate(Pclass = factor(Pclass),
         Sex = factor(Sex))


# Continuous  vs Continuous  --------------------------------------------------------------

correlation <- cor(df %>% select(Age, Fare), use = "complete.obs", method = "pearson")
correlation <- correlation[1,2]


ggplot(df, aes(x = Age, y = Fare, color = Survived)) + 
  geom_point()+
  labs(
    title = "Age vs Fare",
    subtitle = paste0("correlation: ",round(correlation,3))
  ) + 
  theme_bw()






correlation <- cor(df %>% select(Age, SibSp ), use = "complete.obs", method = "pearson")
correlation <- correlation[1,2]


ggplot(df, aes(x = Age, y = SibSp , color = Survived)) + 
  geom_point()+
  labs(
    title = "Age vs SibSp ",
    subtitle = paste0("correlation: ",round(correlation,3))
  ) + 
  theme_bw()





correlation <- cor(df %>% select(Age, Parch ), use = "complete.obs", method = "pearson")
correlation <- correlation[1,2]


ggplot(df, aes(x = Age, y = Parch , color = Survived)) + 
  geom_point()+
  labs(
    title = "Age vs Parch ",
    subtitle = paste0("correlation: ",round(correlation,3))
  ) + 
  theme_bw()



correlation <- cor(df %>% select(Fare, Parch ), use = "complete.obs", method = "pearson")
correlation <- correlation[1,2]


ggplot(df, aes(x = Fare, y = Parch , color = Survived)) + 
  geom_point()+
  labs(
    title = "Fare vs Parch ",
    subtitle = paste0("correlation: ",round(correlation,3))
  ) + 
  theme_bw()




correlation <- cor(df %>% select(Fare, SibSp ), use = "complete.obs", method = "pearson")
correlation <- correlation[1,2]


ggplot(df, aes(x = Fare, y = SibSp , color = Survived)) + 
  geom_point()+
  labs(
    title = "Fare vs SibSp ",
    subtitle = paste0("correlation: ",round(correlation,3))
  ) + 
  theme_bw()


correlation <- cor(df %>% select(Parch, SibSp ), use = "complete.obs", method = "pearson")
correlation <- correlation[1,2]


ggplot(df, aes(x = Parch, y = SibSp , color = Survived)) + 
  geom_point()+
  labs(
    title = "Parch vs SibSp ",
    subtitle = paste0("correlation: ",round(correlation,3))
  ) + 
  theme_bw()



# Correlation Matrix ------------------------------------------------------

correlation <- cor(df %>% select(Parch, SibSp, Age, Fare ), use = "complete.obs", method = "pearson")
print(correlation)

#ToDo Check VIF for correlaiton
