library(tidyverse)
library(lubridate)
library(magrittr)
library(car)
library(onewaytests)
load('ads.Rda')
head(data)

# How did Covid affect which items are listed in an ad?
data %<>% mutate(before_covid = as.factor(if_else(s.date < ymd("2020-06-01"), TRUE, FALSE)))
s_data <-  group_by(data, before_covid) %>%
            summarise(mean = mean(perc, na.rm = TRUE),
            median = median(perc, na.rm = TRUE),
            sd = sd(perc, na.rm = TRUE))

anova_results <- aov(perc ~ before_covid, data = data)
summary(anova_results)
# Test for Homoscedasticity
bf.test(perc ~ before_covid, data = data)
leveneTest(perc ~ before_covid, data = data)
bartlett.test(perc ~ before_covid, data = data)
plot(anova_results, 1)

# Test for Normality of Data
qqnorm(subset(data, before_covid == FALSE)$perc)
qqline(subset(data, before_covid == FALSE)$perc)
qqnorm(subset(data, before_covid == TRUE)$perc)
qqline(subset(data, before_covid == TRUE)$perc)
residuals <- residuals(object = anova_results)
shapiro.test(x = residuals)

# Normality is violated so we must use non-parametric approach.
kruskal.test(perc ~ before_covid, data = data)
# We have significant evidence to conclude that percentage discounts are different before and after covid.
data %>% ggplot(aes(x = s.date, y = perc, color = before_covid)) +
  geom_point()

data %>% ggplot(aes(x = perc, color = before_covid)) +
  geom_boxplot()


# How did covid affect whihc items are listed?

