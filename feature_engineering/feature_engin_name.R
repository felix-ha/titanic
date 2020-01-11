# Feature Engineering: Name -> TitleGroup


rm(list = ls())

library(stringr)
library(tidyverse)
library(magrittr)

df <- read_csv("train.csv")
df_test <- read_csv("test.csv")

# # Check whole dataset 
# df <- df %>%
#   select(-Survived) %>%
#   bind_rows(df_test)

df$Name

#str_view_all(df$Name, "(Mr|Mrs|Miss)\\.")

ex <- "[a-zA-Z]*\\."
str_view_all(df$Name, ex)
print(df$Name[str_detect(df$Name, ex)])
print(str_extract(df$Name, ex))

titles <- str_extract(df$Name, ex)
titles_distinct <- df %>%
  distinct(str_extract(Name, ex)) %>%
  pull()

df_title <- df %>%
  mutate(Title = str_extract(Name, ex),
         Title = factor(Title))

# title_count <-  df_title %>% 
#   group_by(Title) %>%
#   summarise(count = n()) %>%
#   select(count) %>%
#   pull()



ggplot(df_title, aes(Title)) +
  geom_bar() +
  theme_bw()



df_title <- df_title %>%
  mutate(
    TitleGroup = case_when(
      str_detect(Title, "(Mrs|Miss|Ms)\\.") ~ "Miss",
      str_detect(Title, "(Mr)\\.") ~ "Mr",
      str_detect(Title, "(Master)\\.") ~ "Master",
      TRUE ~ "Special"
    ),
    TitleGroup = factor(TitleGroup),
    Survived = factor(Survived)
  )


ggplot(df_title)  +
  geom_bar(aes(x = TitleGroup)) +
  theme_bw()


stop("plot / calculate survival probabilties here")
ggplot(df_title)  +
  geom_bar(aes(x = TitleGroup, fill = Survived),
           position=position_dodge()) +
  theme_bw()
  


table(df_title$TitleGroup)
table_abs <- table(df_title$TitleGroup, df_title$Survived)

prop.table(table_abs, 1)

prop.test(table_abs)

fit <- glm(Survived ~ TitleGroup, data=df_title, family =binomial(link = "logit"))
summary(fit)


# Summary Processing

df_title <- df %>%
  mutate(Title = str_extract(Name, "[a-zA-Z]*\\."),
         Title = factor(Title),
         TitleGroup = case_when(
           str_detect(Title, "(Mrs|Miss|Ms)\\.") ~ "Miss",
           str_detect(Title, "(Mr)\\.") ~ "Mr",
           str_detect(Title, "(Master)\\.") ~ "Master",
           TRUE ~ "Special"
         )) %>%
  select(-Title, -Name)


