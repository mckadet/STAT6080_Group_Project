library(ggplot2)
library(tidyverse)
library(gridExtra)
library(wordcloud)
library(knitr)
library(kableExtra)

# creating mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# load in data frame of all images
load("RData.Rda")
data <- pivot.df

# initializing variables
data$Brand <- ""
data$Save <- ""
data$BrandName <- ""
data$perc <- ""

# variable creation via for loop.
for (i in 1:length(data$OCR)) {
  data$OCR[i] %>%
    gsub("\\”[[:space:]]", " ", .) -> data$OCR[i]
  # brand variable
  data$Brand[i] <- data$OCR[i]
  data$Brand[i] %>%
    gregexpr(paste0("([[:space:]]?[[:alpha:]]+[[:punct:]]?",
    "[[:alpha:]]+[[:punct:]]?[[:alpha:]]+){1,3}?"), .) %>%
    regmatches(data$OCR[i], .) -> data$Brand[i]
  data$BrandName[i] <- data$Brand[i]
  data$BrandName[[i]][1] %>% 
    gsub("?", "", .) %>% 
    gsub("^ | $", "", .) -> data$BrandName[i]
  
  # save variable 
  data$Save[i] <- data$OCR[i]
  data$Save[i] %>%
    gregexpr("[[:digit:]]?[[:digit:]][[:space:]]?\\%", .) %>%
    regmatches(data$OCR[i], .) -> data$Save[i]
  
  # percent variable
  data$perc[i] <- data$Save[[i]][1] %>% gsub("%", "",.)
}

# reformatting variables to the correct class
data$s.date <- as.Date(data$s.date, format =  "%m-%d-%Y")
data$e.date <- as.Date(data$e.date, format =  "%m-%d-%Y")
data$p.num <- as.integer(data$p.num)
data$s.year <- as.integer(data$s.year)
data$e.year <- as.integer(data$e.year)
data$s.month <- as.integer(data$s.month)
data$e.month <- as.integer(data$e.month)
data$perc <- as.integer(data$perc)
data$BrandName <- as.character(data$BrandName)

# creating a data frame of summary statistics for s.date
s.date.summary <- data %>% 
  filter(!is.na(perc)) %>%
  group_by(s.date) %>%
  summarize(mean.perc = mean(perc),
            mode.perc = getmode(perc),
            median.perc = median(perc),
            n = n())

# creating a data frame of summary statistics for page 1
s.date.pg1 <- data %>% 
  filter(p.num == 1) %>%
  filter(!is.na(perc)) %>%
  group_by(s.date) %>%
  summarize(mean.perc = mean(perc),
            mode.perc = getmode(perc),
            median.perc = median(perc),
            n = n())

# creating a data frame of summary statistics for individual pages
s.date.pages <- data %>% 
  filter(!is.na(perc)) %>%
  group_by(s.date, p.num) %>%
  summarize(mean.perc = mean(perc),
            mode.perc = getmode(perc),
            median.perc = median(perc),
            n = n()) %>%
  group_by(s.date) %>%
  summarize(median.n = mean(n))

# plotting s.data summary points for all pages and ad matrices
gg.median.perc.overall <- ggplot(s.date.summary, 
                                aes(s.date, median.perc)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_x_date(labels = date_format("%b-%Y")) +
  ggtitle("Median Percentage Off of All Products") +
  xlab("Ad Start Date") +
  ylab("Median Percentage Off") +
  scale_y_continuous(breaks = seq(0, 60, 10),
                     limits = c(0, 60))

gg.median.perc.overall

gg.n.overall <- ggplot(s.date.summary, 
                       aes(s.date, n)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_x_date(labels = date_format("%b-%Y")) +
  xlab("Ad Start Date") +
  ylab("Number of Products in Ad") +
  ggtitle("Total Number of Products in an Ad", 
          "Please note difference in scale compared to other graphic") +
  scale_y_continuous(breaks = seq(0, 150, 50), 
                     limits = c(0, 150))

gg.n.overall
  
# page 1 summary stats
gg.median.perc.pg1 <- ggplot(s.date.pg1,
                             aes(s.date, median.perc)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_x_date(labels = date_format("%b-%Y")) +
  xlab("Ad Start Date") +
  ylab("Median Percentage Off") +
  ggtitle("Median Percentage Off - Page 1") +
  scale_y_continuous(breaks = seq(0, 60, 10), 
                     limits = c(0, 60))

gg.median.perc.pg1

gg.n.pg1 <- ggplot(s.date.pg1, 
                   aes(s.date, n)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_x_date(labels = date_format("%b-%Y")) +
  ggtitle("Number of Products on Page 1") +
  xlab("Ad Start Date") +
  ylab("Number of Products") +
  scale_y_continuous(breaks = seq(0, 12, 2), 
                     limits = c(0, 12))

gg.n.pg1

gg.n.pages <- ggplot(s.date.pages, 
                     aes(s.date, median.n)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  scale_x_date(labels = date_format("%b-%Y")) +
  xlab("Ad Start Date") +
  ylab("Number of Products") +
  ggtitle("Median number of Products on Any Page") +
  scale_y_continuous(breaks = seq(0, 12, 2), 
                     limits = c(0, 12))

gg.n.pages

# top eight brands overall
top.brands.overall <- data %>%
  filter(!is.na(BrandName) & !is.na(perc)) %>%
  group_by(BrandName) %>%
  summarize(med.perc = median(perc),
            n = n()) %>%
  arrange(desc(n)) %>%
  top_n(8) %>%
  kable(caption = "Top Brands Overall") %>%
  kable_styling(latex_options = c("striped", 
                                  "hold_position", 
                                  "repeat_header"))

# top eight brands on page 1
top.brands.pg1 <- data %>%
  filter(p.num == 1 & !is.na(BrandName) & !is.na(perc)) %>%
  group_by(BrandName) %>%
  summarize(med.perc = median(perc),
            n = n()) %>%
  arrange(desc(n)) %>%
  top_n(8) %>%
  kable(caption = "Top Brands on Page 1") %>%
  kable_styling(latex_options = c("striped", 
                                  "hold_position", 
                                  "repeat_header"))
