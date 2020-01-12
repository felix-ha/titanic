# Feature Engineering: Cabin


rm(list = ls())

library(stringr)
library(tidyverse)
library(magrittr)
library(boot)


df <- read_csv("train.csv")
df_test <- read_csv("test.csv")
# 
# # Check whole dataset
# df <- df %>%
#   select(-Survived) %>%
#   bind_rows(df_test)

sum(is.na(df$Cabin))

df %<>% mutate(Cabin = ifelse(is.na(Cabin), "Z", Cabin ))



# Look for pattern --------------------------------------------------------



ex <- "NA"
str_view_all(df$Cabin, ex)
str_detect(df$Cabin, ex)



# Start mutating here -----------------------------------------------------


df_factor <- df %>%
  mutate(Cabin = str_sub(df$Cabin,1,1),
         Cabin = factor(Cabin))

ggplot(df_factor, aes(Cabin)) +
  geom_bar() +
  theme_bw()


df_factor <-  df %>%
  mutate(
    Cabin = str_sub(df$Cabin,1,1),
    Cabin = case_when(
      str_detect(Cabin, "(D|E|B)") ~ "Top",
      str_detect(Cabin, "(F|C|G|A|T)") ~ "AVG",
      #str_detect(Cabin, "(Master)\\.") ~ "Master",
      TRUE ~ "NotAv"
    ),
    Cabin = factor(Cabin)
  )

ggplot(df_factor, aes(Cabin)) +
  geom_bar() +
  theme_bw()

# Statistik Test ----------------------------------------------------------



table(df_factor$Cabin)
table_abs <- table(df_factor$Cabin, df_factor$Survived)

prop.table(table_abs, 1)

test <- prop.test(table_abs)
print(test)



fit <- glm(Survived ~ Cabin, data=df_factor, family =binomial(link = "logit"))
summary(fit)



# Plot with custom bootstrap ci  ------------------------------------------


boot_mean <- function(values,i) mean(values[i])
alpha <- 0.05

number_of_factors <- length(levels(df_factor$Cabin))
ymin <- vector(mode = "numeric", length = number_of_factors)
ymax <- vector(mode = "numeric", length = number_of_factors)

for(i in 1:number_of_factors) {
  
  factor_current <- levels(df_factor$Cabin)[i]
  
  values <- df_factor %>% 
    filter(Cabin == factor_current) %>% 
    select(Survived) %>% 
    mutate(as.integer(Survived)) %>%
    pull()
  
  fit <- boot(values, boot_mean, R = 100)
  quantiles <- quantile(fit$t,c(alpha / 2, 1 - (alpha / 2)))
  
  ymin[i] <- quantiles[1]
  ymax[i] <- quantiles[2]
}


df_plot <- df_factor %>%
  group_by(Cabin) %>%
  summarise(count = n(),
            prob = mean(as.numeric(Survived)))   %>%
  mutate(Cabin = fct_reorder(Cabin, prob, .desc = TRUE))


ggplot(df_plot, aes(x = Cabin, y = prob)) + 
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


