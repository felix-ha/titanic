# Feature Engineering: Name -> TitleGroup


rm(list = ls())

library(stringr)
library(tidyverse)
library(magrittr)
library(boot)


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



  


# Statistik Test ----------------------------------------------------------



table(df_title$TitleGroup)
table_abs <- table(df_title$TitleGroup, df_title$Survived)

prop.table(table_abs, 1)

test <- prop.test(table_abs)
print(test)



fit <- glm(Survived ~ TitleGroup, data=df_title, family =binomial(link = "logit"))
summary(fit)




# Plot with custom bootstrap ci  ------------------------------------------


boot_mean <- function(values,i) mean(values[i])
alpha <- 0.05

number_of_factors <- length(levels(df_title$TitleGroup))
ymin <- vector(mode = "numeric", length = number_of_factors)
ymax <- vector(mode = "numeric", length = number_of_factors)

for(i in 1:number_of_factors) {
  
  factor_current <- levels(df_title$TitleGroup)[i]
  
  values <- df_title %>% 
    filter(TitleGroup == factor_current) %>% 
    select(Survived) %>% 
    mutate(as.integer(Survived) - 1) %>%
    pull()
  
  fit <- boot(values, boot_mean, R = 100)
  quantiles <- quantile(fit$t,c(alpha / 2, 1 - (alpha / 2)))
  
  ymin[i] <- quantiles[1]
  ymax[i] <- quantiles[2]
}


df_plot <- df_title %>%
  group_by(TitleGroup) %>%
  summarise(count = n(),
            prob = mean(as.numeric(Survived) - 1))   %>%
  mutate(TitleGroup = fct_reorder(TitleGroup, prob, .desc = TRUE))


ggplot(df_plot, aes(x = TitleGroup, y = prob)) + 
  geom_col(width = 0.75) + 
  geom_errorbar(ymin = ymin,
                ymax = ymax,
                width = .75) +
  theme_bw() + 
  labs(
    #x = "Models",
    #y = "AUC",
    title = paste0("Survival Probabilities of factor TitleGroup with ", 1 - alpha, " CI"),
    subtitle = paste0(" p value of Test \"H0: probabilities are the same\": ", signif (test$p.value, 5))
  ) +
  ylim(c(0,1))





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


