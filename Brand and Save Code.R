library(magrittr)

load("RData.Rda")
data <- pivot.df

data$Brand <- ""
data$Save <- ""

for (i in 1:length(data$OCR)) {
  data$Brand[i] <- data$OCR[i]
  data$Brand[i] %>%
    gregexpr("([[:space:]][[:alpha:]]+){1,3}®", .) %>%
    regmatches(data$OCR[i], .) -> data$Brand[i]
}

for (i in 1:length(data$OCR)) {
  data$Save[i] <- data$OCR[i]
  data$Save[i] %>%
    gregexpr("[[:digit:]]?[[:digit:]][[:space:]]?\\%", .) %>%
    regmatches(data$OCR[i], .) -> data$Save[i]
}

