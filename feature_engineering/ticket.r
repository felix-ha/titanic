# Feature Engineering: Ticket


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

sum(is.na(df$Ticket))


df$Ticket

str_view(df$Ticket, "^\\d")
str_view(df$Ticket, "^[A-Z]")


unique(str_extract(df$Ticket, "^[A-Z]"))



df_factor <-  df %>%
  mutate(
    Ticket = str_sub(Ticket,1,1),
    Ticket = case_when(
      str_detect(Ticket, "^\\d") ~ "Digit",
      TRUE ~ Ticket
    ),
    Ticket = factor(Ticket))

ggplot(df_factor, aes(Ticket)) +
  geom_bar() +
  theme_bw()





table(df_factor$Ticket)
table_abs <- table(df_factor$Ticket, df_factor$Survived)

prop.table(table_abs, 1)

test <- prop.test(table_abs)
print(test)



# with custom bootstrap ci  ------------------------------------------
  
  
boot_mean <- function(values,i) mean(values[i])
alpha <- 0.05

number_of_factors <- length(levels(df_factor$Ticket))
ymin <- vector(mode = "numeric", length = number_of_factors)
ymax <- vector(mode = "numeric", length = number_of_factors)

for(i in 1:number_of_factors) {
  
  factor_current <- levels(df_factor$Ticket)[i]
  
  values <- df_factor %>% 
    filter(Ticket == factor_current) %>% 
    select(Survived) %>% 
    mutate(as.integer(Survived)) %>%
    pull()
  
  fit <- boot(values, boot_mean, R = 100)
  quantiles <- quantile(fit$t,c(alpha / 2, 1 - (alpha / 2)))
  
  ymin[i] <- quantiles[1]
  ymax[i] <- quantiles[2]
}


df_plot <- df_factor %>%
  group_by(Ticket) %>%
  summarise(count = n(),
            prob = mean(as.numeric(Survived)))   %>%
  mutate(Ticket = fct_reorder(Ticket, prob, .desc = TRUE))


ggplot(df_plot, aes(x = Ticket, y = prob)) + 
  geom_col(width = 0.75) + 
  geom_errorbar(ymin = ymin,
                ymax = ymax,
                width = .75) +
  theme_bw() + 
  labs(
    title = paste0("Survival Probabilities of factor Ticket with ", 1 - alpha, " CI"),
    subtitle = paste0(" p value of Test \"H0: probabilities are the same\": ", signif (test$p.value, 5))
  ) +
  ylim(c(0,1))



