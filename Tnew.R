
library(forecast)
library(forecastHybrid)
library(opera)
library(ggplot2)
library(zoo)
library(Metrics)
library(readxl)
library(tscount)
library(xts)


### CHINA

dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "china", col_names = FALSE)
TData=ts(dailyD, frequency = 365, start = c(2020,11), end = c(2020,132))
View(TData)

train = window(TData,start = c(2020,11), end = c(2020,108))
test = window(TData,start = c(2020,109), end = c(2020,132))
h <- length(test)
h

### Individual models

ETS <- forecast(ets(train), h=h)
ETS$model
ETS$level
ETS$method


### HYBRID 

m1 <- hybridModel(train, weights="equal")
m1$models
m1$auto.arima
fcs1 <- forecast(m1, h=h)
plot(fcs1)

### COUNT TIME SERIES integer-valued GARCH MODEL

dgm=tsglm(train, model = list(past_obs = 1, past_mean = 1))

dpm=predict(dgm, n.head=24, level=0.9, global = TRUE)$pred

## MSE RMSE

mse(test,fcs1$mean)
rmse(test,fcs1$mean)

mse(test,ETS$mean)
rmse(test,ETS$mean)

mse(test,dpm)
rmse(test,dpm)


#### INDIA 

dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "india", col_names = FALSE)

TData1=ts(dailyD, frequency = 365, start = c(2020,30), end = c(2020,132))
View(TData1)

train1 = window(TData1,start = c(2020,30), end = c(2020,112))
test1 = window(TData1,start = c(2020,113), end = c(2020,132))
h <- length(test1)
h

## Individual models

ETS1 <- forecast(ets(train1), h=h)
ARIMA= Arima(train1, order = c(1,0,2), method = "ML")
summary(arima)
ARIMA=forecast(ARIMA, h=h)


### HYBRID 

m2 <- hybridModel(train1, weights="equal")
m2$models
m2$auto.arima
fcs2 <- forecast(m2, h=h)

### COUNT TIME SERIES integer-valued GARCH MODEL

dgm1=tsglm(train1, model = list(past_obs = 1, past_mean = 1))
coef(dgm1)
summary(dgm1)

dpm1=predict(dgm1, n.head=20, level=0.9, global = TRUE)$pred

## MSE RMSE

mse(test1,fcs2$mean)
rmse(test1,fcs2$mean)

mse(test1,ETS1$mean)
rmse(test1,ETS1$mean)

mse(test1,ARIMA$mean)
rmse(test1,ARIMA$mean)

mse(test1,dpm1)
rmse(test1,dpm1)

## IRAN

dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "iran", col_names = FALSE)


TData2=ts(dailyD, frequency = 365, start = c(2020,50), end = c(2020,132))
View(TData2)

train2 = window(TData2,start = c(2020,50), end = c(2020,115))
test2 = window(TData2,start = c(2020,116), end = c(2020,132))
h <- length(test2)
h

## Individual models

ETS2 <- forecast(ets(train2), h=h)
arima1= Arima(train2, order = c(2,0,2), method = "ML")
summary(arima1)
ARIMA1=forecast(arima1, h=h)


### HYBRID 

m3 <- hybridModel(train2, weights="equal")
m3$models
m3$auto.arima
fcs3 <- forecast(m3, h=h)

### COUNT TIME SERIES integer-valued GARCH MODEL

dgm2=tsglm(train2, model = list(past_obs = 1, past_mean = 1))
coef(dgm2)
summary(dgm2)

dpm2=predict(dgm2, n.head=17, level=0.9, global = TRUE)$pred

## MSE RMSE

mse(test2,fcs3$mean)
rmse(test2,fcs3$mean)

mse(test2,ETS2$mean)
rmse(test2,ETS2$mean)

mse(test2,ARIMA1$mean)
rmse(test2,ARIMA1$mean)

mse(test2,dpm2)
rmse(test2,dpm2)


### ITALY

dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "italy", col_names = FALSE)

TData3=ts(dailyD, frequency = 365, start = c(2020,29), end = c(2020,132))
View(TData3)

train3 = window(TData3,start = c(2020,29), end = c(2020,111))
test3 = window(TData3,start = c(2020,112), end = c(2020,132))
h <- length(test3)
h

## Individual models

ETS3 <- forecast(ets(train3), h=h)
arima2= Arima(train3, order = c(2,0,2), method = "ML")
summary(arima2)
ARIMA2=forecast(arima2, h=h)


### HYBRID 

m4 <- hybridModel(train3, weights="equal")
m4$models
m4$auto.arima
fcs4 <- forecast(m4, h=h)

### COUNT TIME SERIES integer-valued GARCH MODEL

dgm3=tsglm(train3, model = list(past_obs = 1, past_mean = 1))
coef(dgm2)
summary(dgm3)

dpm3=predict(dgm3, n.head=21, level=0.9, global = TRUE)$pred

## MSE RMSE

mse(test3,fcs4$mean)
rmse(test3,fcs4$mean)

mse(test3,ETS3$mean)
rmse(test3,ETS3$mean)

mse(test3,ARIMA2$mean)
rmse(test3,ARIMA2$mean)

mse(test3, dpm3)
rmse(test3, dpm3)

### SPAIN

dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "spain", col_names = FALSE)

TData4=ts(dailyD, frequency = 365, start = c(2020,31), end = c(2020,132))
View(TData4)

train4 = window(TData4,start = c(2020,31), end = c(2020,122))
test4 = window(TData4,start = c(2020,123), end = c(2020,132))
h <- length(test4)
h

## Individual models

ETS4 <- forecast(ets(train4), h=h)
ETS4$model
ETS4$method


arima3= Arima(train4, order = c(2,0,2), method = "ML")
summary(arima3)
ARIMA3=forecast(arima3, h=h)


### HYBRID 

m5 <- hybridModel(train4, weights="equal")
m5$models
m5$auto.arima
fcs5 <- forecast(m5, h=h)

### COUNT TIME SERIES integer-valued GARCH MODEL

dgm4=tsglm(train4, model = list(past_obs = 1, past_mean = 1))
coef(dgm4)

dpm4=predict(dgm4, n.head=h, level=0.9, global = TRUE)$pred

## MSE RMSE

mse(test4,fcs5$mean)
rmse(test4,fcs5$mean)

mse(test4,ETS4$mean)
rmse(test4,ETS4$mean)

mse(test4,ARIMA3$mean)
rmse(test4,ARIMA3$mean)

mse(test4, dpm4)
rmse(test4, dpm4)

### SOUTH KOREA

dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "south korea", col_names = FALSE)

TData5=ts(dailyD, frequency = 365, start = c(2020,19), end = c(2020,132))
View(TData5)

train5 = window(TData5,start = c(2020,19), end = c(2020,109))
test5 = window(TData5,start = c(2020,110), end = c(2020,132))
h <- length(test5)

## Individual models

ETS5 <- forecast(ets(train5), h=h)
ETS5$model
ETS5$method

arima4= Arima(train5, order = c(2,0,2), method = "ML")
summary(arima4)
ARIMA4=forecast(arima4, h=h)


### HYBRID 

m6 <- hybridModel(train5, weights="equal")
m6$models
m6$auto.arima
fcs6 <- forecast(m6, h=h)

### COUNT TIME SERIES integer-valued GARCH MODEL

dgm5=tsglm(train5, model = list(past_obs = 1, past_mean = 1))
coef(dgm5)

dpm5=predict(dgm5, n.head=h, level=0.9, global = TRUE)$pred

## MSE RMSE

mse(test5,fcs6$mean)
rmse(test5,fcs6$mean)

mse(test5,ETS5$mean)
rmse(test5,ETS5$mean)

mse(test5,ARIMA4$mean)
rmse(test5,ARIMA4$mean)

mse(test5, dpm5)
rmse(test5, dpm5)


## THAILAND

dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "thailand", col_names = FALSE)

TData6=ts(dailyD, frequency = 365, start = c(2020,13), end = c(2020,132))
View(TData6)

train6 = window(TData6,start = c(2020,13), end = c(2020,108))
test6 = window(TData6,start = c(2020,109), end = c(2020,132))
h <- length(test6)

## Individual models

ETS6 <- forecast(ets(train6), h=h)
arima5= Arima(train6, order = c(1,1,2), method = "ML")
summary(arima5)
ARIMA5=forecast(arima5, h=h)


### HYBRID 

m7 <- hybridModel(train6, weights="equal")
m7$models
m7$auto.arima
fcs7 <- forecast(m7, h=h)

### COUNT TIME SERIES integer-valued GARCH MODEL

dgm6=tsglm(train6, model = list(past_obs = 1, past_mean = 1))
coef(dgm6)
summary(dgm6)

dpm6=predict(dgm6, n.head=h, level=0.9, global = TRUE)$pred


## MSE RMSE

mse(test6,fcs7$mean)
rmse(test6,fcs7$mean)

mse(test6,ETS6$mean)
rmse(test6,ETS6$mean)

mse(test6,ARIMA5$mean)
rmse(test6,ARIMA5$mean)

mse(test6, dpm6)
rmse(test6, dpm6)


## TURKEY

dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "turkey", col_names = FALSE)

TData7=ts(dailyD, frequency = 365, start = c(2020,71), end = c(2020,132))
View(TData7)

train7 = window(TData7,start = c(2020,71), end = c(2020,120))
test7 = window(TData7,start = c(2020,121), end = c(2020,132))
h <- length(test7)

## Individual models

ETS7 <- forecast(ets(train7), h=h)
arima6= Arima(train7, order = c(1,1,2), method = "ML")
summary(arima6)
ARIMA6=forecast(arima6, h=h)


### HYBRID 

m8 <- hybridModel(train7, weights="equal")
m8$models
m8$auto.arima
fcs8 <- forecast(m8, h=h)

### COUNT TIME SERIES integer-valued GARCH MODEL

dgm7=tsglm(train7, model = list(past_obs = 1, past_mean = 1))
coef(dgm7)
summary(dgm7)

dpm7=predict(dgm7, n.head=h, level=0.9, global = TRUE)$pred

## MSE RMSE

mse(test7,fcs8$mean)
rmse(test7,fcs8$mean)

mse(test7,ETS7$mean)
rmse(test7,ETS7$mean)

mse(test7,ARIMA6$mean)
rmse(test7,ARIMA6$mean)

mse(test7, dpm7)
rmse(test7, dpm7)

##### GHANA

dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                   sheet = "Sheet1", col_names = FALSE)

TData8=ts(dailyD, frequency = 365, start = c(2020,74), end = c(2020,138))
View(dailyD)

train8 = window(TData8,start = c(2020,74), end = c(2020,125))
test8 = window(TData8,start = c(2020,126), end = c(2020,138))
h <- length(test8)
h

## SINGLE FORECAST MODEL

ETS8 <- forecast(ets(train8), h=h)
ETS8$model
ETS8$method

arima7= Arima(train8, order = c(2,0,2), method = "ML")
summary(arima7)
ARIMA7=forecast(arima7, h=h)

## HYBRID MODEL

m9 <- hybridModel(train8, weights = "equal")
m9$auto.arima
m9$tbats
m9$fitted
fcs8 <- forecast(m9, h=h)
fcs8$method
fcs8$auto.arima
fcs8$mean
summary(m9)

### COUNT TIME SERIES integer-valued GARCH MODEL

dgm8=tsglm(train8, model = list(past_obs = 1, past_mean = 1))
coef(dgm8)
dgm8$fitted.values

dpm8=predict(dgm8, n.head=h, level=0.9, global = TRUE)$pred
dpm8

## MSE and RMSE

mse(test8,ETS8$mean)
rmse(test8,ETS8$mean)

mse(test8,fcs8$mean)
rmse(test8,fcs8$mean)

mse(test8,ARIMA7$mean)
rmse(test8,ARIMA7$mean)

mse(test8,dpm8)
rmse(test8,dpm8)


##### FORECASTING WITH SELECTED MODELS

## CHINA
dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "M plot")
TData=xts(dailyD[-1], order.by = as.Date(dailyD$Date))
f1=TData[1:121,1]
f1
FETS1=ets(f1)
FETS1
plot(forecast(FETS1,h=10), main = "10 Days Forecast of New Covid Cases for China", xlab = "Days", ylab = "Covid 19 Cases")
## SOUTH KOREA

f2=TData[1:121,2]
f2
FETS2=ets(f2)
FETS2
plot(forecast(FETS2,h=10),main = "10 Days Forecast of New Covid Cases for S. Korea", xlab = "Days", ylab = "Covid 19 Cases")

### THAILAND

f3=TData[1:121,3]
f3
FETS3=ets(f3)
FETS3
plot(forecast(FETS3,h=10),main = "10 Days Forecast of New Covid Cases for Thailand", xlab = "Days", ylab = "Covid 19 Cases")



### TURKEY

f4=TData[1:121,4]
f4
FETS4=ets(f4)
FETS4
plot(forecast(FETS4,h=10),main = "10 Days Forecast of New Covid Cases for Turkey", xlab = "Days", ylab = "Covid 19 Cases")


dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "M plot 1")

TD=xts(dailyD[-1], order.by = as.Date(dailyD$Date))
### INDIA

fn0=TD[1:103,1]
fn0
FETS5=ets(fn0)
FETS5
plot(forecast(FETS5,h=10), main = "10 Days Forecast of New Covid Cases for India", xlab = "Days", ylab = "Covid 19 Cases")

IND=forecast(FETS5,h=10)
IND
## IRAN
fn1=TD[1:103,2]
fn1
FARIMA= Arima(fn1, order = c(2,0,2), method = "ML")
FARIMA
plot(forecast(FARIMA,h=10),main = "10 Days Forecast of New Covid Cases for Iran", xlab = "Days", ylab = "Covid 19 Cases")
## ITALY

fn2=TD[1:103,3]
fn2
FETS6=ets(fn2)
FETS6
plot(forecast(FETS6,h=10), main = "10 Days Forecast of New Covid Cases for Italy", xlab = "Days", ylab = "Covid 19 Cases")


## SPAIN
fn4=TD[1:103,4]
fn4
FARIMA3= Arima(fn4, order = c(2,0,2), method = "ML")
FARIMA3
plot(forecast(FARIMA3,h=10),main = "10 Days Forecast of New Covid Cases for Spain", xlab = "Days", ylab = "Covid 19 Cases")


## GHANA

dailyD <- read_excel("C:/Users/user11/Desktop/COVID 19/dataset/dailyD.xlsx", 
                     sheet = "Ghana")
TD1=xts(dailyD[-1], order.by = as.Date(dailyD$Date))
G=TD1[1:64,1]
G
FETS7=ets(G)
FETS7
Dd=forecast(FETS7,h=10)
Dd
plot(forecast(FETS7,h=10), main = "10 Days Forecast of New Covid Cases for Ghana", xlab = "Days", ylab = "Covid 19 Cases")

