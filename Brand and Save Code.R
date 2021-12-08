library(ggplot2)
library(tidyverse)
library(gridExtra)
library(wordcloud)


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
data$BrandName <- ""
data$perc <- ""

## Variable Creation via for loop.

for (i in 1:length(data$OCR)) {
  # Cleaning up some weird characters I noticed (Ragan)
  data$OCR[i] %>%
    gsub("\\â€[[:space:]]", " ", .) -> data$OCR[i]
  # Brand Variable
  data$Brand[i] <- data$OCR[i]
  data$Brand[i] %>%
    # Added more to the regular expression to capture more special cases (Ragan)
    gregexpr("([[:space:]]?[[:alpha:]]+[[:punct:]]?[[:alpha:]]+[[:punct:]]?[[:alpha:]]+){1,3}®", .) %>%
    regmatches(data$OCR[i], .) -> data$Brand[i]
  
  data$BrandName[i] <- data$Brand[i]
  data$BrandName[[i]][1] %>% 
    gsub("®", "", .) %>% 
    gsub("^ | $", "", .) -> data$BrandName[i]
  
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
data$BrandName <- as.character(data$BrandName)

getmode(data$perc) # 20% off is the most common

## Creating a dataframe of summary statistics for s.date
s.date.summary <- data %>% 
  filter(!is.na(perc)) %>%
  group_by(s.date) %>%
  summarize(mean.perc = mean(perc),
            mode.perc = getmode(perc),
            median.perc = median(perc),
            n = n())

## Creating a dataframe of summary statistics for page 1
s.date.pg1 <- data %>% 
  filter(p.num == 1) %>%
  filter(!is.na(perc)) %>%
  group_by(s.date) %>%
  summarize(mean.perc = mean(perc),
            mode.perc = getmode(perc),
            median.perc = median(perc),
            n = n())

## Creating a dataframe of summary statistics for individual pages
s.date.pages <- data %>% 
  filter(!is.na(perc)) %>%
  group_by(s.date, p.num) %>%
  summarize(mean.perc = mean(perc),
            mode.perc = getmode(perc),
            median.perc = median(perc),
            n = n()) %>%
  group_by(s.date) %>%
  summarize(median.n = mean(n))

## Plotting percentage with a simple lm regression line.
ggplot(data, aes(s.date, perc)) +
  geom_point() +
  geom_smooth(method='lm')

## Plotting s.data summary points for all pages and ad matrices

gg.mean.perc.overall <- ggplot(s.date.summary, aes(s.date, mean.perc)) +
  geom_point() +
  geom_smooth(method='lm')
  # Appears to be fairly constant, slightly higher towards the present

gg.mode.perc.overall <- ggplot(s.date.summary, aes(s.date, mode.perc)) +
  geom_point() +
  geom_smooth(method='lm')
  # Also slightly increasing towards the present.  Most common percent off is 20% almost 
  # consistently after the pandemic.  Before the pandemic it was mostly 20 or 15.

gg.median.perc.overall <- ggplot(s.date.summary, aes(s.date, median.perc)) +
  geom_point() +
  geom_smooth(method='lm')
  # basically no change.  Median is higher than the general mode.

grid.arrange(gg.mean.perc.overall, gg.mode.perc.overall, gg.median.perc.overall, nrow = 1)

gg.n.overall <- ggplot(s.date.summary, aes(s.date, n)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Number of products in an ad")
  # Number of products in add was very low in the beginning, very high in the 
  # middle, and then in 2021 it started low and has been increasing ever since.

## Page 1 summary stats

gg.mean.perc.pg1 <- ggplot(s.date.pg1, aes(s.date, mean.perc)) +
  geom_point() +
  geom_smooth(method='lm')

gg.mode.perc.pg1 <- ggplot(s.date.pg1, aes(s.date, mode.perc)) +
  geom_point() +
  geom_smooth(method='lm')

gg.median.perc.pg1 <- ggplot(s.date.pg1, aes(s.date, median.perc)) +
  geom_point() +
  geom_smooth(method='lm')

grid.arrange(gg.mean.perc.pg1, gg.mode.perc.pg1, gg.median.perc.pg1, nrow = 1)
  # All three page ones are a lot more all over the place.  General trendlines have
  # a negative slope, suggesting that the ads have been giving less off over time.
  # What is super interesting is to compare the range of percentages to the overall 
  # ad and we can see here that the range goes up to 50% off a lot of the time, whereas
  # the overall add went only up to around 30%, so it looks like the products that have 
  # bigger markdowns are getting placed on page 1 normally.

gg.n.pg1 <- ggplot(s.date.pg1, aes(s.date, n)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Number of products on page 1")

gg.n.pages <- ggplot(s.date.pages, aes(s.date, median.n)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Median number of Products on a page")


## Top Brands 
wordcloud(data$BrandName)
wordcloud((data %>% filter(p.num == 1))$BrandName)

## Top ten brands overall
data %>%
  filter(!is.na(BrandName) & !is.na(perc)) %>%
  group_by(BrandName) %>%
  summarize(med.perc = median(perc),
            n = n()) %>%
  arrange(desc(n)) %>%
  top_n(8)

## Top ten brands on page 1
data %>%
  filter(p.num == 1 & !is.na(BrandName) & !is.na(perc)) %>%
  group_by(BrandName) %>%
  summarize(med.perc = median(perc),
            n = n()) %>%
  arrange(desc(n)) %>%
  top_n(8)
  # What is interesting to note from the past two dataframes is that while some of 
  # the top brands are the same, not all of them are.  Nabisco has by far the most
  # ads, however they are only on the first page in the fourth spot.  Several brands,
  # including Oscar Mayer, Coleson's Catch, Gillette, and Pillsbury are in the top
  # ten overall, but do not appear in the first page top ten.  Another thing to
  # note, which we would expect from other graphs, is that the ads on the first 
  # page have a higher percentage off, so perhaps Gillette doesn't want to have
  # as good of a deal, and thus is not featured prominently on the first pages.