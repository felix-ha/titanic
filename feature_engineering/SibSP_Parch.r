# Feature Engineering: SibSp Parch


rm(list = ls())

library(stringr)
library(tidyverse)
library(magrittr)
library(boot)


df <- read_csv("train.csv")


df %<>% 
  mutate(RelativesFriends = SibSp + Parch,
         Survived = factor(Survived))

ggplot(df) +
  geom_bar(aes(x = RelativesFriends, fill = Survived)) +
  theme_bw()

df %<>%
  mutate(RelativesFriends = case_when(
    RelativesFriends == 0 ~ "None",
    RelativesFriends  == 1 ~ "One",
    RelativesFriends  <= 4 ~ "Small",
    TRUE ~ "Large"
  ),
  RelativesFriends = factor(RelativesFriends))


ggplot(df, aes(RelativesFriends)) +
  geom_bar() +
  theme_bw()

table(df$RelativesFriends)
table_abs <- table(df$RelativesFriends, df$Survived)

prop.table(table_abs, 1)

test <- prop.test(table_abs)
print(test)



fit <- glm(Survived ~ RelativesFriends, data=df, family =binomial(link = "logit"))
summary(fit)






boot_mean <- function(values,i) mean(values[i])
alpha <- 0.05

number_of_factors <- length(levels(df$RelativesFriends))
ymin <- vector(mode = "numeric", length = number_of_factors)
ymax <- vector(mode = "numeric", length = number_of_factors)

for(i in 1:number_of_factors) {
  
  factor_current <- levels(df$RelativesFriends)[i]
  
  values <- df %>% 
    filter(RelativesFriends == factor_current) %>% 
    select(Survived) %>% 
    mutate(as.integer(Survived) - 1) %>%
    pull()
  
  fit <- boot(values, boot_mean, R = 100)
  quantiles <- quantile(fit$t,c(alpha / 2, 1 - (alpha / 2)))
  
  ymin[i] <- quantiles[1]
  ymax[i] <- quantiles[2]
}


df_plot <- df %>%
  group_by(RelativesFriends) %>%
  summarise(count = n(),
            prob = mean(as.numeric(Survived) - 1))   %>%
  mutate(RelativesFriends = fct_reorder(RelativesFriends, prob, .desc = TRUE))


ggplot(df_plot, aes(x = RelativesFriends, y = prob)) + 
  geom_col(width = 0.75) + 
  geom_errorbar(ymin = ymin,
                ymax = ymax,
                width = .75) +
  theme_bw() + 
  labs(
    #x = "Models",
    #y = "AUC",
    title = paste0("Survival Probabilities of factor RelativesFriends with ", 1 - alpha, " CI"),
    subtitle = paste0(" p value of Test \"H0: probabilities are the same\": ", signif (test$p.value, 5))
  ) +
  ylim(c(0,1))

