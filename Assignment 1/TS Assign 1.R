###################################
# TS Assignment 1
###################################

install.packages('tinytex')

library(tinytex)

tinytex::install_tinytex()

options(tinytex.verbose = TRUE)

setwd(
  "C:/Users/samgh/Desktop/Masters of Statistics and Operations Research/Year 2/Sem 1/Time Series Analysis/Assignments/Assignment 1"
)

library(TSA)

dataset <- read.csv('data1.csv', header = FALSE)

str(dataset)
plot(1:nrow(dataset), dataset$X1.351184356, type = 'o')

data_ts <- ts(dataset,
              start = 1927,
              end = 2016,
              frequency = 1)

str(data_ts)

plot(
  data_ts,
  xlab = 'Years',
  ylab = 'Dobson Units (Ozone Thickness)',
  type = 'o',
  main = 'Ozone Thickness (in Dobson units) from 1927 to 2015'
)


y <- data_ts[, 1]
x <- zlag(data_ts[, 1])
index <- 2:length(x)
cor(y[index], x[index])
# 0.87 autocorrelation value



# fit a linear regression model
t <- time(data_ts)
lin_model <- lm(data_ts ~ t)
summary(lin_model)
# R^2: 0.67

# validate model (all p-values below significance level of 0.05, but check assumptions)
rlin <- rstudent(lin_model)
qqnorm(rlin)
qqline(rlin, col = 'red', lty = 2) # some deviation from the quantile-quantile line at the ends of the distrubition

hist(rlin, breaks = 20) # mild normal distribution present

acf(rlin) # 3 intercepts showing stat sig autocor at lags 1, 6, and 7 # THROW MODEL, ACF is harmonic

shapiro.test(rlin) # fail to reject, assume normality

# residuals
plot(y = rlin,
     x = as.vector(time(data_ts)),
     type = 'o')
# some major deviations, but not completly adherent to white noise
# seems to have a slight polynomial trend

# use lin model to forecast
# we want to forecast the following 5 years from 2015
h <- 5
t <- time(data_ts)
# STEP 1
new <- data.frame(t = seq((2016 + 1), 2016 + h), 1)
# STEP 2
forecast <- predict(lin_model, new, interval = 'prediction')

plot(
  data_ts,
  ylim = c(-15, 10),
  xlim = c(1927, 2021),
  lwd = 2,
  xlab = 'Year',
  ylab = 'Dobson Units',
  main = 'Change in Ozone Layer Thickness by Year (1927-2020)'
)
lines(ts(as.vector(forecast[, 1]), start = 2016),
      col = 'red', lwd = 2)
lines(ts(as.vector(forecast[, 2]), start = 2016),
      col = 'blue', lwd = 2)
lines(ts(as.vector(forecast[, 3]), start = 2016),
      col = 'blue', lwd = 2)
legend(
  'topleft',
  lty = 1,
  lwd = 2,
  pch = 1,
  col = c('black', 'blue', 'red'),
  text.width = 18,
  c('Data', '5% Forecasts Limits', 'Forecasts')
)

abline(lin_model, col = 'green2', lyt = 2)

# fit a quadratic regression model
t <- time(data_ts)
t2 <- t ^ 2
quad_model <- lm(data_ts ~ t + t2)
summary(quad_model) # all stat sig
# R^2: 0.74 higher than a linear model

# validate model (all p-values below significance level of 0.05, but check assumptions)
rquad <- rstudent(quad_model)
qqnorm(rquad)
qqline(rquad, col = 'red', lty = 2) # some deviation from the quantile-quantile line at the ends of the distrubition

hist(rquad, breaks = 20)
# more so a normal dist but neg skew

acf(rquad) # at lags 1, 3, 4, 7... many intercepts THROW MODEL, also the residuals of this model are harmonic

shapiro.test(rquad) # fail to reject, assume normality, closer to 1 than lin model

# residuals
plot(y = rquad,
     x = as.vector(time(data_ts)),
     type = 'o')
# some major deviations, but not completly dissimilar to white noise
# change in mean and variance does not occur

# use quad model to forecast
# we want to forecast the following 5 years from 2015
h <- 5
t <- time(data_ts)

# STEP 1
library(dplyr)
new <- data_frame(t = seq((2016 + 1), 2016 + h), 1,
                  t2 = t ^ 2)
# STEP 2
forecast <- predict(quad_model, new, interval = 'prediction')

plot(
  data_ts,
  ylim = c(-15, 10),
  xlim = c(1927, 2021),
  lwd = 2,
  xlab = 'Year',
  ylab = 'Dobson Units',
  main = 'Change in Ozone Layer Thickness over Time (1927-2021)'
)
lines(ts(as.vector(forecast[, 1]), start = 2016),
      col = 'red', lwd = 2)
lines(ts(as.vector(forecast[, 2]), start = 2016),
      col = 'blue', lwd = 2)
lines(ts(as.vector(forecast[, 3]), start = 2016),
      col = 'blue', lwd = 2)
legend(
  'topleft',
  lty = 1,
  lwd = 2,
  pch = 1,
  col = c('black', 'blue', 'red'),
  text.width = 18,
  c('Data', '5% Forecasts Limits', 'Forecasts')
)
quad_pred <- predict(quad_model, list(t = t, t2 = t ^ 2))
years <- seq(1927, 2016, 1)
lines(years, quad_pred, col = 'green3', lty = 2)

# time series plot of standardised residuals

### Use an Auto Regressive Moving Average model to fit

library(tseries)
arma_mod <- arma(x = data_ts,
                 order = c(1, 1),
                 lag = NULL)
summary(arma_mod) # auto reg stat sig, moving average not at all

ar_mod <- arma(x = data_ts,
               order = c(1, 0),
               # this negates MA from model, only AR
               lag = NULL)
summary(ar_mod)

arima_mod_000 <- arima(x = data_ts,
                       order = c(0, 0, 0))
summary(arima_mod_000)

arima_mod_100 <- arima(x = data_ts,
                       order = c(1, 0, 0))
summary(arima_mod_100)

arima_mod_110 <- arima(x = data_ts,
                       order = c(1, 1, 0))
summary(arima_mod_110)

arima_mod_111 <- arima(x = data_ts,
                       order = c(1, 1, 1))
summary(arima_mod_111)

arima_mod_101 <- arima(x = data_ts,
                       order = c(1, 0, 1))
arima_mod_011 <- arima(x = data_ts,
                       order = c(0, 1, 1))
arima_mod_001 <- arima(x = data_ts,
                       order = c(0, 0, 1))

# STEP 1

new <- data.frame(t = seq((2016 + 1), 2016 + h), 1)

# STEP 2

# Forecast
ar_fc_000 <- predict(arima_mod_000, n.ahead = 20)
ar_fc_100 <- predict(arima_mod_100, n.ahead = 20)
ar_fc_110 <- predict(arima_mod_110, n.ahead = 20)
ar_fc_111 <- predict(arima_mod_111, n.ahead = 20)
ar_fc_101 <- predict(arima_mod_101, n.ahead = 20)
ar_fc_011 <- predict(arima_mod_011, n.ahead = 20)
ar_fc_001 <- predict(arima_mod_001, n.ahead = 20)
ar_fc <- predict(ar_mod, new)

# Visualise
plot(
  data_ts,
  xlim = c(1925, 2035),
  ylim = c(-11, 4),
  main = 'Arima orders'
)
lines(ts(as.vector(ar_fc_000$pred), start = 2016), col = 'blue')
lines(ts(as.vector(ar_fc_100$pred), start = 2016), col = 'red')
lines(ts(as.vector(ar_fc_110$pred), start = 2016), col = 'orange')
lines(
  ts(as.vector(ar_fc_101$pred), start = 2016),
  col = 'purple' ,
  lty = 1,
  lwd = 10
)
lines(ts(as.vector(ar_fc_111$pred), start = 2016), col = 'green')
lines(ts(as.vector(ar_fc_011$pred), start = 2016), col = 'yellow')
lines(ts(as.vector(ar_fc_001$pred), start = 2016), col = 'pink')


### MIX HARMONIC and QUADATRIC

# standard fit
t <- time(data_ts)
t2 <- t^2
har <- harmonic(x = data_ts, 1)


# tighter fit
ssp <- spectrum(data_ts)
ssp$freq
per <- 1 / ssp$freq[ssp$freq == max(ssp$freq)]
reslm <- lm(data_ts ~ cos(2 * pi / per * t) + sin(2 * pi / per * t) + t + t2)
summary(reslm)

plot(
  ts(fitted(reslm), start = 1927),
  type = 'l',
  ylim = c(-12, 4),
  col = 'red',                                  
  lwd = 2,
  lty = 2
)
points(data_ts, type = 'l', col = 'green3')
resl <- rstudent(reslm)
acf(resl) # yeah never mind...
qqnorm(resl)
qqline(resl, col = 'red')
pacf(resl)

# plot model
h <- 5
# STEP 1
new <- data.frame(t = seq((2016 + 1), 2016 + h),
                  t2 = seq((2016 + 1), (2016 + h)) ^ 2,
                  1)
# STEP 2
pred <- predict(reslm, new, interval = 'prediction')
# VISUAL
plot(
  ts(fitted(reslm), start = 1927),
  type = 'l',
  ylim = c(-15.5, 4),
  xlim = c(1925, 2020),
  col = 'red4',
  lwd = 2,                                                      ### MIX HARMONIC and QUADATRIC
  lty = 2,
  main = 'Quadratic and Harmonic Fitted Model',
  xlab = 'Year',
  ylab = 'Dobson Units'
)
points(data_ts, type = 'l', col = 'green3')
lines(
  ts(as.vector(pred[, 1]), start = 2016),
  col = 'red',
  lwd = 2,
  lty = 2
)
lines(ts(as.vector(pred[, 2]), start = 2016), col = 'blue', lty = 2, lwd = 2)
lines(ts(as.vector(pred[, 3]), start = 2016), col = 'blue', lty = 2, lwd = 2)

### MIX HARMONIC and LINEAR

# standard fit
t <- time(data_ts)
t2 <- t^2
har <- harmonic(x = data_ts, 1)
s <- sin(t) * 2

# tighter fit
ssp <- spectrum(data_ts)
ssp$freq
per <- 1 / ssp$freq[ssp$freq == max(ssp$freq)]
reslm <- lm(data_ts ~ sin(t) + t + t2)
summary(reslm)

plot(
  ts(fitted(reslm), start = 1927),
  type = 'l',
  ylim = c(-12, 4),
  col = 'red',
  lwd = 2,
  lty = 2
)
points(data_ts, type = 'l', col = 'green3')
resl <- rstudent(reslm)
acf(resl) # yeah never mind...
qqnorm(resl)
qqline(resl, col = 'red')
pacf(resl)

# plot model
h <- 5
# STEP 1
new <- data.frame(t = seq((2016 + 1), 2016 + h),
                  t2 = seq((2016 + 1), (2016 + h)) ^ 2,
                  1)
# STEP 2
pred <- predict(reslm, new, interval = 'prediction')
# VISUAL
plot(
  ts(fitted(reslm), start = 1927),
  type = 'l',
  ylim = c(-15.5, 4),
  xlim = c(1925, 2020),                                  #### LIN AND HARM
  col = 'red4',
  lwd = 2,
  lty = 2,
  main = 'Quadratic and Harmonic Fitted Model',
  xlab = 'Year',
  ylab = 'Dobson Units'
)
points(data_ts, type = 'l', col = 'green3')
lines(
  ts(as.vector(pred[, 1]), start = 2016),
  col = 'red',
  lwd = 2,
  lty = 2
)
lines(ts(as.vector(pred[, 2]), start = 2016), col = 'blue', lty = 2, lwd = 2)
lines(ts(as.vector(pred[, 3]), start = 2016), col = 'blue', lty = 2, lwd = 2)

t <- time(data_ts)
sin_mod <- nls(as.vector(data_ts)~a*sin(b*t+c), start = list(a = 1, b = 1, c = 1), control = list(maxiter = 500))
fitted(sin_mod)

plot(ts(fitted(sin_mod), start = 1927),
     ylim = c(-10,5))
points(data_ts)
lines


### TAKE 2

library(TSA)
# Import date with a differing frequency
data_ts_11 <- ts(dataset, frequency = 11)
har <- harmonic(data_ts_9, 1)
t <- time(data_ts)
t2 <- t^2

quad_sin_mod <- lm(data_ts ~ t + t2 + har[,1])
plot(ts(fitted(quad_sin_mod), start = 1927),
     xlim = c(1927,2020),
     ylim = c(-15 , 5))
lines(data_ts)
summary(quad_sin_mod)

h <- 5
new2 <- data.frame(t = seq(2017, 2017 + h),
                   t2 = (seq(2017, 2017 + h))^2,
                   `har[, 1]` = 1)
pred2 <- predict(quad_sin_mod, newdata = new2)

# WORKS ^^^^

data_ts_9 <- ts(dataset, frequency = 11)
har <- harmonic(data_ts_9, 1)
t <- time(data_ts)
t2 <- t^2

quad_sin_mod <- lm(data_ts ~ t + t2 + har[,2])
plot(ts(fitted(quad_sin_mod), start = 1927),
     xlim = c(1927,2020),
     ylim = c(-15 , 5))
lines(data_ts)
summary(quad_sin_mod)

data_ts_9 <- ts(dataset, frequency = 8)
har <- harmonic(data_ts_9, 1)
t <- time(data_ts)
t2 <- t^2

quad_sin_mod <- lm(data_ts ~ t + t2 + har[,2])
plot(ts(fitted(quad_sin_mod), start = 1927),
     xlim = c(1927,2020),
     ylim = c(-15 , 5))
lines(data_ts)
summary(quad_sin_mod)
summary(quad_sin_mod)$coefficients[,4][4]

mod_sum_vec <- vector() 
for (i in 3:50) {
  data_freq <- ts(dataset, frequency = i)
  har <- harmonic(data_freq, 1)
  t <- time(data_freq)
  t <- t^2
  quad_har_model <- lm(data_freq ~ t + t2 + har[,2])
  mod_sum_vec[i] <- summary(quad_har_model)$coefficients[,4][4]
}
plot(1:length(mod_sum_vec), mod_sum_vec, type = 'b')
mod_sum_vec[min(mod_sum_vec, na.rm = TRUE) == mod_sum_vec]  
library(dplyr)  
df <- data_frame(pval = mod_sum_vec,
                 freq = 1:50)
df <- df[complete.cases(df),]
df[min(df$pval, na.rm = TRUE) == df$pval,]

########### try 7
ozone_ts7 <- ts(dataset, frequency = 7)


t = time(ozone_ts7)
t2 = t^2
har. <- harmonic(ozone_ts7, 1)



ozone_harq = lm(data_ts ~ cos(t) + t2 + t)
summary(ozone_harq)


plot(ts(fitted(ozone_harq)), ylim = c(min(c(fitted(ozone_harq),
                                            as.vector(ozone_ts7))), max(c(fitted(ozone_harq),as.vector(ozone_ts7)))),
     ylab='y' , main = "Fitted harmonic model to Ozone Thickness", type="l",lty=2,col="red")
lines(as.vector(ozone_ts7),type="o")



mean(1,2)













plot(
  ts(fitted(quad_har_model), start = 1927),
  type = 'l',
  ylim = c(-15.5, 4),
  xlim = c(1925, 2020),
  col = 'red4',
  lwd = 2,                                                      ### MIX HARMONIC and QUADATRIC
  lty = 2,
  main = 'Quadratic and Harmonic Fitted Model',
  xlab = 'Year',
  ylab = 'Dobson Units'
)
points(1927:2016,data_ts_11, type = 'l')






### MIX HARMONIC and QUADATRIC ACTUAL GOOD THOUGH
data_ts_11 <- ts(dataset, frequency = 11)
t <- time(data_ts_11)
t2 <- t^2
t3 <- cos(2*pi*t)
t4 <- sin(2*pi*t)             #################USE THIS SHIT

mod_11 <- lm(data_ts_11 ~ t + t2 + t3 + t4)
summary(mod_11) # BETTER

### PREDICT HARMONIC
freq <- 11 # set freq of ts

interval <- 1/freq # find the interval values

h <- 5 # set prediction

t_last <- t[length(t)] # find last value

t_new <- seq((t_last + interval), (t_last + interval * h), by = interval) # 

t <- t_new
t2 <- t^2
t3 <- cos(2*pi*t)
t4 <- sin(2*pi*t)  

new_t_df <- data.frame(t, t2, t3, t4)

predt <- predict(mod_11, new_t_df, interval = 'prediction')

### VALIDATE MODEL
rquad <- rstudent(mod_11)

plot(rquad, type = 'b')

acf(rquad) # still a ~0.5 signif at lag 1, a clearly a trend in the pattern

### VISUALISING MODEL AND FORECAST
plot(ts(data_ts_11, start = 1927), type = 'b')
lines(ts(mod_11$fitted.values, start = 1927))
lines(ts(as.vector(pred[,1]), start = 2016))
lines(ts(as.vector(pred[,2]), start = 2016))
lines(ts(as.vector(pred[,3]), start = 2016))






