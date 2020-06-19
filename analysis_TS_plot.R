
library(forecast)
library(forecastHybrid)
library(ggplot2)
library(Metrics)
library(readxl)
library(tscount)

##### CHINA

## CUMMULATIVE CASES

cumD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/cumD.xlsx", 
                   sheet = "china", col_names = FALSE)

TD=ts(cumD, frequency = 365, start = c(2020,11), end = c(2020,132))
View(cumD)

train = window(TD,start = c(2020,11), end = c(2020,108))
test = window(TD,start = c(2020,109), end = c(2020,132))
h <- length(test)
h

## SINGLE FORECAST MODEL

ARIMA <- forecast(auto.arima(train, lambda=0), h=h)
ARIMA$fitted
ARIMA$x
ARIMA$model
ARIMA$mean
ARIMA$residuals
ARIMA$method
summary(ARIMA)

## HYBRID MODEL

fit <- hybridModel(train, weights="equal")
fit$auto.arima
fit$tbats
fit$fitted
fc <- forecast(fit, h=h)
fc$method
fc$auto.arima
fc$mean
summary(fit)

### COUNT TIME SERIES integer-valued GARCH MODEL

gm=tsglm(train, model = list(past_obs = 1, past_mean = 1))
coef(gm)
summary(gm)

pm=predict(gm, n.head=24, level=0.9, global = TRUE)$pred


## MSE and RMSE

mse(test,ARIMA$mean)
rmse(test,ARIMA$mean)

mse(test,fc$mean)
rmse(test,fc$mean)

mse(test,pm)
rmse(test,pm)

### GRAPH OF FORECAST

df <- cbind(Data=TD, ARIMA=ARIMA$mean, Hybrid=fc$mean)
autoplot(df) +
  xlab("Date") + ylab("Covid 19 Cases")



### INDIA

cumD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/cumD.xlsx", 
                   sheet = "india", col_names = FALSE)

TD1=ts(cumD, frequency = 365, start = c(2020,30), end = c(2020,132))
View(cumD)

train1 = window(TD1,start = c(2020,30), end = c(2020,112))
test1 = window(TD1,start = c(2020,113), end = c(2020,132))
h <- length(test1)
h

## SINGLE FORECAST MODEL

ARIMA1 <- forecast(auto.arima(train1, lambda=0), h=h)
ARIMA1$fitted
ARIMA1$x
ARIMA1$model
ARIMA1$mean
ARIMA1$residuals
ARIMA1$method
summary(ARIMA1)

## HYBRID MODEL

fit1 <- hybridModel(train1, weights="equal")
fit1$auto.arima
fit1$tbats
fit1$fitted
fc1 <- forecast(fit1, h=h)
fc1$method
fc1$auto.arima
fc1$mean
summary(fit1)

### COUNT TIME SERIES integer-valued GARCH MODEL

gm1=tsglm(train1, model = list(past_obs = 1, past_mean = 1))
summary(gm1)

pm1=predict(gm1, n.head=20, level=0.9, global = TRUE)$pred


## MSE and RMSE

mse(test1,ARIMA1$mean)
rmse(test1,ARIMA1$mean)

mse(test1,fc1$mean)
rmse(test1,fc1$mean)

mse(test1,pm1)
rmse(test1,pm1)

## IRAN

cumD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/cumD.xlsx", 
                   sheet = "iran", col_names = FALSE)

TD2=ts(cumD, frequency = 365, start = c(2020,50), end = c(2020,132))
View(cumD)

train2 = window(TD2,start = c(2020,50), end = c(2020,115))
test2 = window(TD2,start = c(2020,116), end = c(2020,132))
h <- length(test2)
h

## SINGLE FORECAST MODEL

ARIMA2 <- forecast(auto.arima(train2, lambda=0), h=h)
ARIMA2$fitted
ARIMA2$x
ARIMA2$model
ARIMA2$mean
ARIMA2$residuals
ARIMA2$method
summary(ARIMA2)

## HYBRID MODEL

fit2 <- hybridModel(train2, weights = "equal")
fit2$auto.arima
fit2$tbats
fit2$fitted
fc2 <- forecast(fit2, h=h)
fc2$method
fc2$auto.arima
fc2$mean
summary(fit2)

### COUNT TIME SERIES integer-valued GARCH MODEL

gm2=tsglm(train2, model = list(past_obs = 1, past_mean = 1))
summary(gm2)

pm2=predict(gm2, n.head=17, level=0.9, global = TRUE)$pred

## MSE and RMSE

mse(test2,ARIMA2$mean)
rmse(test2,ARIMA2$mean)

mse(test2,fc2$mean)
rmse(test2,fc2$mean)

mse(test2,pm2)
rmse(test2,pm2)

### ITALY

cumD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/cumD.xlsx", 
                   sheet = "italy", col_names = FALSE)

TD3=ts(cumD, frequency = 365, start = c(2020,29), end = c(2020,132))
View(cumD)

train3 = window(TD3,start = c(2020,29), end = c(2020,111))
test3 = window(TD3,start = c(2020,112), end = c(2020,132))
h <- length(test3)
h

## SINGLE FORECAST MODEL

ARIMA3 <- forecast(auto.arima(train3, lambda=0), h=h)
ARIMA3$fitted
ARIMA3$x
ARIMA3$model
ARIMA3$mean
ARIMA3$residuals
ARIMA3$method
summary(ARIMA3)

## HYBRID MODEL

fit3 <- hybridModel(train3, weights = "equal")
fit3$auto.arima
fit3$tbats
fit3$fitted
fc3 <- forecast(fit3, h=h)
fc3$method
fc3$auto.arima
fc3$mean
summary(fit3)

### COUNT TIME SERIES integer-valued GARCH MODEL

gm3=tsglm(train3, model = list(past_obs = 1, past_mean = 1))
summary(gm3)

pm3=predict(gm3, n.head=21, level=0.9, global = TRUE)$pred

## MSE and RMSE

mse(test3,ARIMA3$mean)
rmse(test3,ARIMA3$mean)

mse(test3,fc3$mean)
rmse(test3,fc3$mean)

mse(test3,pm3)
rmse(test3,pm3)

###SPAIN

cumD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/cumD.xlsx", 
                   sheet = "spain", col_names = FALSE)


TD4=ts(cumD, frequency = 365, start = c(2020,31), end = c(2020,132))
View(cumD)

train4 = window(TD4,start = c(2020,31), end = c(2020,112))
test4 = window(TD4,start = c(2020,113), end = c(2020,132))
h <- length(test4)
h

## SINGLE FORECAST MODEL

ARIMA4 <- forecast(auto.arima(train4, lambda=0), h=h)
ARIMA4$fitted
ARIMA4$x
ARIMA4$model
ARIMA4$mean
ARIMA4$residuals
ARIMA4$method
summary(ARIMA4)

## HYBRID MODEL

fit4 <- hybridModel(train4, weights = "equal")
fit4$auto.arima
fit4$tbats
fit4$fitted
fc4 <- forecast(fit4, h=h)
fc4$method
fc4$auto.arima
fc4$mean
summary(fit4)

### COUNT TIME SERIES integer-valued GARCH MODEL

gm4=tsglm(train4, model = list(past_obs = 1, past_mean = 1))
coef(gm4)
gm4$fitted.values
summary(gm4)

pm4=predict(gm4, n.head=20, level=0.9, global = TRUE)$pred
pm4

## MSE and RMSE

mse(test4,ARIMA4$mean)
rmse(test4,ARIMA4$mean)

mse(test4,fc4$mean)
rmse(test4,fc4$mean)

mse(test4,pm4)
rmse(test4,pm4)

#### SOUTH KOREA

cumD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/cumD.xlsx", 
                   sheet = "south korea", col_names = FALSE)

TD5=ts(cumD, frequency = 365, start = c(2020,19), end = c(2020,132))
View(cumD)

train5 = window(TD5,start = c(2020,19), end = c(2020,109))
test5 = window(TD5,start = c(2020,110), end = c(2020,132))
h <- length(test5)
h

## SINGLE FORECAST MODEL

ARIMA5 <- forecast(auto.arima(train5, lambda=0), h=h)
ARIMA5$fitted
ARIMA5$x
ARIMA5$model
ARIMA5$mean
ARIMA5$residuals
ARIMA5$method
summary(ARIMA5)

## HYBRID MODEL

fit5 <- hybridModel(train5, weights = "equal")
fit5$auto.arima
fit5$tbats
fit5$fitted
fc5 <- forecast(fit5, h=h)
fc5$method
fc5$auto.arima
fc5$mean
summary(fit5)

### COUNT TIME SERIES integer-valued GARCH MODEL

gm5=tsglm(train5, model = list(past_obs = 1, past_mean = 1))
coef(gm5)
gm5$fitted.values
summary(gm5)

pm5=predict(gm5, n.head=23, level=0.9, global = TRUE)$pred
pm5

## MSE and RMSE

mse(test5,ARIMA5$mean)
rmse(test5,ARIMA5$mean)

mse(test5,fc5$mean)
rmse(test5,fc5$mean)

mse(test5,pm5)
rmse(test5,pm5)

### THAILAND

cumD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/cumD.xlsx", 
                   sheet = "thailand", col_names = FALSE)

TD6=ts(cumD, frequency = 365, start = c(2020,13), end = c(2020,132))
View(cumD)

train6 = window(TD6,start = c(2020,13), end = c(2020,108))
test6 = window(TD6,start = c(2020,109), end = c(2020,132))
h <- length(test6)
h

## SINGLE FORECAST MODEL

ARIMA6 <- forecast(auto.arima(train6, lambda=0), h=h)
ARIMA6$fitted
ARIMA6$x
ARIMA6$model
ARIMA6$mean
ARIMA6$residuals
ARIMA6$method
summary(ARIMA6)

## HYBRID MODEL

fit6 <- hybridModel(train6, weights = "equal")
fit6$auto.arima
fit6$tbats
fit6$fitted
fc6 <- forecast(fit6, h=h)
fc6$method
fc6$auto.arima
fc6$mean
summary(fit6)

### COUNT TIME SERIES integer-valued GARCH MODEL

gm6=tsglm(train6, model = list(past_obs = 1, past_mean = 1))
coef(gm6)
gm6$fitted.values
summary(gm6)

pm6=predict(gm6, n.head=24, level=0.9, global = TRUE)$pred
pm6


## MSE and RMSE

mse(test6,ARIMA6$mean)
rmse(test6,ARIMA6$mean)

mse(test6,fc6$mean)
rmse(test6,fc6$mean)

mse(test6,pm6)
rmse(test6,pm6)

#### TURKEY

cumD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/cumD.xlsx", 
                   sheet = "turkey", col_names = FALSE)

TD7=ts(cumD, frequency = 365, start = c(2020,71), end = c(2020,132))
View(cumD)

train7 = window(TD7,start = c(2020,71), end = c(2020,120))
test7 = window(TD7,start = c(2020,121), end = c(2020,132))
h <- length(test7)
h

## SINGLE FORECAST MODEL

ARIMA7 <- forecast(auto.arima(train7, lambda=0), h=h)
ARIMA7$fitted
ARIMA7$x
ARIMA7$model
ARIMA7$mean
ARIMA7$residuals
ARIMA7$method
summary(ARIMA7)

## HYBRID MODEL

fit7 <- hybridModel(train7, weights = "equal")
fit7$auto.arima
fit7$tbats
fit7$fitted
fc7 <- forecast(fit7, h=h)
fc7$method
fc7$auto.arima
fc7$mean
summary(fit7)

### COUNT TIME SERIES integer-valued GARCH MODEL

gm7=tsglm(train7, model = list(past_obs = 1, past_mean = 1))
coef(gm7)
gm7$fitted.values
summary(gm7)

pm7=predict(gm7, n.head=12, level=0.9, global = TRUE)$pred
pm7

## MSE and RMSE

mse(test7,ARIMA7$mean)
rmse(test7,ARIMA7$mean)

mse(test7,fc7$mean)
rmse(test7,fc7$mean)

mse(test7,pm7)
rmse(test7,pm7)

##### GHANA

cumD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/cumD.xlsx", 
                   sheet = "Ghana", col_names = FALSE)

TD8=ts(cumD, frequency = 365, start = c(2020,74), end = c(2020,138))
View(cumD)

train8 = window(TD8,start = c(2020,74), end = c(2020,125))
test8 = window(TD8,start = c(2020,126), end = c(2020,138))
h <- length(test8)
h

## SINGLE FORECAST MODEL

ARIMA8 <- forecast(auto.arima(train8, lambda=0), h=h)
ARIMA8$fitted
ARIMA8$x
ARIMA8$model
ARIMA8$mean
ARIMA8$residuals
ARIMA8$method
summary(ARIMA8)

## HYBRID MODEL

fit8 <- hybridModel(train8, weights = "equal")
fit8$auto.arima
fit8$tbats
fit8$fitted
fc8 <- forecast(fit8, h=h)
fc8$method
fc8$auto.arima
fc7$mean
summary(fit8)

### COUNT TIME SERIES integer-valued GARCH MODEL

gm8=tsglm(train8, model = list(past_obs = 1, past_mean = 1))
coef(gm8)
gm8$fitted.values
summary(gm8)

pm8=predict(gm8, n.head=13, level=0.9, global = TRUE)$pred
pm8

## MSE and RMSE

mse(test8,ARIMA8$mean)
rmse(test8,ARIMA8$mean)

mse(test8,fc8$mean)
rmse(test8,fc8$mean)

mse(test8,pm8)
rmse(test8,pm8)

###MULTPLE TIME SERIES PLOTS

library(xts)
library(zoo)
library(MASS)

dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "M plot")

TData=xts(dailyD[-1], order.by = as.Date(dailyD$Date))
View(TData)

## Create color scheme using rainbow()
tsRainbow = rainbow(ncol(as.zoo(TData)))

plot.xts(TData, screens = 1, major.ticks = "years", main = "Covid 19 New Infections/Cases", yaxis.right=FALSE, grid.ticks.on="years", col=tsRainbow)
addLegend("topright",legend.names=c("China", "S. Korea", "Thailand", "Turkey"),col=tsRainbow,lty=c(1,1,1,1),lwd=c(2,2,2,2),ncol=2,bg="white", bty="o")


dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "M plot 1")

TData=xts(dailyD[-1], order.by = as.Date(dailyD$Date))
View(TData)

## Create color scheme using rainbow()
tsRainbow = rainbow(ncol(as.zoo(TData)))

plot.xts(TData, screens = 1, major.ticks = "years", main = "Covid 19 New Infections/Cases", yaxis.right=FALSE, grid.ticks.on="years", col=tsRainbow)
addLegend("topleft",legend.names=c("India", "Iran", "Italy", "Spain", "Ghana"),col=tsRainbow,lty=c(1,1,1,1,1),lwd=c(2,2,2,2,2),ncol=2,bg="white", bty="o")

#### CUMMULATIVE CASES

cumD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/cumD.xlsx", 
                   sheet = "Sheet1")

TData=xts(cumD[-1], order.by = as.Date(cumD$Date))
View(TData)

## Create color scheme using rainbow()
tsRainbow = rainbow(ncol(as.zoo(TData)))

plot.xts(TData, screens = 1, major.ticks = "years", main = "Covid 19 New Infections/Cases", yaxis.right=FALSE, grid.ticks.on="years", col=tsRainbow)
addLegend("topleft",legend.names=c("India", "Iran", "Italy", "Spain"),col=tsRainbow,lty=c(1,1,1,1),lwd=c(2,2,2,2),ncol=2,bg="white", bty="o")

