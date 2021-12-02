library(ggplot2)
library(tidyverse)

## Creating Mode Function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

load("RData.Rda")
data <- pivot.df

## Initializing variables
data$Brand <- ""
data$Save <- ""
data$perc <- ""

## Variable Creation via for loop.

for (i in 1:length(data$OCR)) {
  # Brand Variable
  data$Brand[i] <- data$OCR[i]
  data$Brand[i] %>%
    gregexpr("([[:space:]][[:alpha:]]+){1,3}®", .) %>%
    regmatches(data$OCR[i], .) -> data$Brand[i]
  
  # Save Variable 
  data$Save[i] <- data$OCR[i]
  data$Save[i] %>%
    gregexpr("[[:digit:]]?[[:digit:]][[:space:]]?\\%", .) %>%
    regmatches(data$OCR[i], .) -> data$Save[i]
  
  # perc variable
  data$perc[i] <- data$Save[[i]][1] %>% gsub("%", "",.)
}


## Reformatting variables to the correct class
data$s.date <- as.Date(data$s.date, format =  "%m-%d-%Y")
data$e.date <- as.Date(data$e.date, format =  "%m-%d-%Y")
data$p.num <- as.integer(data$p.num)
data$s.year <- as.integer(data$s.year)
data$e.year <- as.integer(data$e.year)
data$s.month <- as.integer(data$s.month)
data$e.month <- as.integer(data$e.month)
data$perc <- as.integer(data$perc)

## Creating a dataframe of summary statistics for s.date
s.date.summary <- data %>% 
  filter(!is.na(perc)) %>%
  group_by(s.date) %>%
  summarize(mean.perc = mean(perc),
            mode.perc = getmode(perc),
            median.perc = median(perc))

## Plotting percentage with a simple lm regression line.
ggplot(data, aes(s.date, perc)) +
  geom_point() +
  geom_smooth(method='lm')

## Plotting s.data summary points

ggplot(s.date.summary, aes(s.date)) +
  geom_point(aes(y = mean.perc, color = "Mean")) +
  geom_point(aes(y = mode.perc, color = "Mode")) +
  geom_point(aes(y = median.perc, color = "Median"))
