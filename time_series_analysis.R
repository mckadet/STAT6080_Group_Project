## Libraries Needed
library(ggplot2)
library(forecast)
library(tidyverse)
library(wordcloud)
library(plyr)
library(gridExtra)
library(reshape2)

## Methods
get_trend_plot <- function(dataset, title, log = FALSE){
  if (log){
    ggplot(dataset, aes(x=s.date, y = log(perc))) +
      geom_point() + 
      geom_line() +
      geom_smooth(method = "loess") +
      ggtitle(title) +
      xlab("date") +
      ylab("Percent Off Offered") +
      theme_minimal()
  }
  else{
    ggplot(dataset, aes(x=s.date, y = perc)) +
      geom_point() + 
      geom_line() +
      geom_smooth(method = "loess") +
      ggtitle(title) +
      xlab("date") +
      ylab("Percent Off Offered") +
      theme_minimal()
  }
}


## Load and subset the data
load("Brand.RDA")
nabisco <- Brand[Brand$BrandName == "Nabisco",]

# by brand
Brand %>% 
  filter(BrandName == "Nabisco") -> nabisco
Brand %>% 
  filter(BrandName == "Kellogg's") -> kellogg
Brand %>% 
  filter(BrandName == "Kraft") -> kraft

# covid
nabisco %>% 
  filter(s.date <= "2020-07-01") -> n.pre.covid
nabisco %>% 
  filter(s.date >= "2020-07-01") -> n.post.covid
kellogg %>% 
  filter(s.date <= "2020-07-01") -> k.pre.covid
kellogg %>% 
  filter(s.date >= "2020-07-01") -> k.post.covid

# recent data
nabisco %>% 
  filter(s.date >= "2021-01-01") -> n.recent
kellogg %>% 
  filter(s.date >= "2021-01-01") -> k.recent
kraft %>% 
  filter(s.date >= "2021-01-01") -> kr.recent





## EDA
# Un-logged
get_trend_plot(nabisco, "Overall Trend in Percent Off for Nabisco")
get_trend_plot(kellogg, "Overall Trend in Percent Off for Kellogg")
get_trend_plot(kraft, "Overall Trend in Percent Off for Kraft")

n1 <- get_trend_plot(n.pre.covid, "Trend for Nabisco Pre-Covid")
n2 <- get_trend_plot(n.post.covid, "Trend for Nabisco Post-Covid")
grid.arrange(n1, n2, nrow = 1)

k1 <- get_trend_plot(k.pre.covid, "Trend for Nabisco Pre-Covid")
k2 <- get_trend_plot(k.post.covid, "Trend for Nabisco Post-Covid")
grid.arrange(k1, k2, nrow = 1)

kr1 <- get_trend_plot(k.pre.covid, "Trend for Kraft Pre-Covid")
kr2 <- get_trend_plot(k.post.covid, "Trend for Kraft Post-Covid")
grid.arrange(kr1, kr2, nrow = 1)

get_trend_plot(n.recent, "Recent Trend in Percent Off for Nabisco")
get_trend_plot(k.recent, "Recent Trend in Percent Off for Kellogg")
get_trend_plot(kr.recent, "Recent Trend in Percent Off for Kraft")


# Logged
get_trend_plot(nabisco, "Overall Trend in Logged Percent Off for Nabisco", log = TRUE)
get_trend_plot(kellogg, "Overall Trend in Logged Percent Off for Kellogg", log = TRUE)

n1 <- get_trend_plot(n.pre.covid, "Logged Trend for Nabisco Pre-Covid", log = TRUE)
n2 <- get_trend_plot(n.post.covid, "Logged Trend for Nabisco Post-Covid", log = TRUE)
grid.arrange(n1, n2, nrow = 1)

k1 <- get_trend_plot(k.pre.covid, "Logged Trend for Nabisco Pre-Covid", log = TRUE)
k2 <- get_trend_plot(k.post.covid, "Logged Trend for Nabisco Post-Covid", log = TRUE)
grid.arrange(k1, k2, nrow = 1)

get_trend_plot(n.recent, "Recent Trend in Logged Percent Off for Nabisco", log = TRUE)
get_trend_plot(k.recent, "Recent Trend in Logged Percent Off for Kellogg", log = TRUE)



################################# NABISCO ######################################
## Analysis for Nabisco with a linear regression model
n.lm <- lm(perc ~ s.date, data = n.recent)
summary(n.lm)

# ACF Plot to view temporal correlation
my.ACF <- acf(resid(n.lm), lag.max = 60)
ACF.dframe <- data.frame(Lag = my.ACF$lag, ACF = my.ACF$acf)
ggplot(data = ACF.dframe, aes(x = Lag, y = ACF)) + 
  geom_col() + 
  ggtitle("ACF Plot for Linear Regression Model Residuals") + 
  theme_minimal()



## Analysis for Nabisco using time series
n.ts <- ts(data = n.recent$perc, start=c(1,1), frequency=1)
X <- model.matrix(n.ts ~ -1 + s.date, data = n.recent)

# Create Sarima Model
auto.arima(n.ts, max.p=1, max.q=1, max.P=1, max.Q=1, d=0, D=0, ic="aic", stepwise=FALSE, xreg=X)
my.sarima.model <- Arima(n.ts, order = c(2,0,0), seasonal=c(0,1,1), xreg = X)


## Check Assumptions and fit model
resids <- resid(my.sarima.model)
fitted <- fitted(my.sarima.model)

# Linearity
ggplot(n.recent, aes(x = s.date, y = perc)) + 
  geom_line() + 
  geom_smooth(method="lm")

# Check Normality and Independence
my.ACF2 <- acf(resids, lag.max=60)
ACF.dframe <- data.frame(Lag = my.ACF2$lag, ACF = my.ACF2$acf)
ggplot(data = ACF.dframe, aes(x = Lag, y = ACF)) + 
  geom_col() + 
  ggtitle("ACF Plot for Time Series Model")

# Equal Variance
ggplot(n.recent, aes(x=fitted, y=resids)) + geom_point() + 
  ggtitle("Standardized Residuals vs Fitted Values for Time Series Model") + 
  xlab("Fitted Values") + ylab("Standardized Resids") + theme_bw()

# Normality
dens <- density(resids)
plot(dens, xlab = "Decorrelated Residuals",main = "Density Plot of Decorrelated Resids")



## Validate Predictions
#Split into test and training set
n.test <- tail(n.recent, n = 10)
n.train <- n.recent[1:(nrow(n.recent) - 10),]

X.test <- tail(X, n = 10)
X.train <- head(X, n = length(X) - 10)

# Fit new Model
new.ts <- ts(data = n.train$perc, start=c(1,1), frequency=10)
train.model <- Arima(new.ts, order=c(2,0,0), seasonal=c(0,1,1), xreg = X.train)
preds <- forecast(train.model, h = 10, xreg = X.test, level = .95)
plot(preds, main="Predicted Percent Off",ylab = "Percent Off", xlab = "Date")

# Calculate RPMSE and coverage
rpmse <- (log(n.test[['perc']]) - preds$mean)^2 %>% mean() %>% sqrt()
rpmse
coverage <- ((log(n.test[['perc']]) > preds$lower) & (log(n.test[['perc']]) < preds$upper)) %>% mean()
coverage


## Predict for 
preds <- forecast(my.sarima.model, h = 10, xreg = X.test + 10, level = .95)
plot(preds, main="Predicted Percent Off for Nabisco", ylab = "Percent Off", xlab = "Date")


################################# KRAFT ######################################
# Seperate into years
kraft %>% 
  filter((s.date >= "2019-01-01") & (s.date <= "2019-12-31")) %>% 
  select(s.date, perc) -> kr.2019
kraft %>% 
  filter((s.date >= "2020-01-01") & (s.date <= "2020-12-31")) %>% 
  select(s.date, perc) -> kr.2020
kraft %>% 
  filter((s.date >= "2021-01-01") & (s.date <= "2021-12-31")) %>% 
  select(s.date, perc) -> kr.2021

newData <- melt(list(kr.2019 = kr.2019, kr.2020 = kr.2020, kr.2021 = kr.2021), id.vars = "s.date")


# Plot years
ggplot(newData, aes(x = s.date, y = value, colour = L1)) + geom_point() + 
  geom_smooth(method = "lm") +
  geom_line() +
  # geom_line(aes(color = variable, linetype = variable)) + 
  scale_color_manual(values = c("darkred", "steelblue", "green")) + theme_minimal()

ggplot(kr.recent, aes(x = s.date, y = perc)) + geom_point(position = position_jitter()) + 
  geom_smooth(method = "loess") +
  ggtitle("Percent Off Offered for Kraft in 2021") +
  theme_minimal()
  
