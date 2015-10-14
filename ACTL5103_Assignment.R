library(forecast)
library(zoo)
library(tseries)

ok<-read.csv("oklahoma.csv",sep=",",header=TRUE)
io<-read.csv("iowa.csv",sep=",",header=TRUE)

#######Question 1 ###########
# Okalahoma Annual Data #
#########################
ok.anu<-ts(ok[,c(1,14)])
tsdisplay(ok.anu)
#stationarity test
kpss.test(ok.anu[,2],null="T")
#no. of differencing required
ndiffs(ok.anu,test=c("kpss","adf","pp"))
#auto fitting arima
fit<-auto.arima(ok.anu[,2])
#Diagnostics
tsdiag(fit)
#Ljung-Box test on residuals
Box.test(fit$residuals,lag=30, type="Lj")
#Normality of residuals
shapiro.test(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals,col="RED")
#summary of fitted model
summary(fit)
#Predictions for 2015, 2016, 2017, 2018
forecast(fit,h=4,level=0.05,robust=TRUE)

#######Question 2##########
# Oklahoma Monthly Data #
#########################
#pre-processing
ok.melt<-melt(ok[,-14],id=c("Year"))
date<-format(paste(ok.melt[,2],ok.melt[,1]),format="%B %Y")
tors<-as.numeric(ok.melt[,3])
ok.mon <- data.frame(date,tors)
names(ok.mon)<-c("Date","Tornadoes")
okm.ts<-ts(ok.mon[order(as.Date(ok.mon$Date,format="%B %Y")),],frequency=12,start=c(1950,1),end=c(2014,12))


tsdisplay(okm.ts)
#stationarity test
kpss.test(okm.ts[,2],null="T")
kpss.test(okm.ts[,2],null="L")
#no. of differencing required for achieving stationarity
ndiffs(okm.ts[,2],test=c("kpss","adf","pp"))
#auto fitting arima
fit1<-auto.arima(okm.ts[,2])
#Diagnostics
tsdiag(fit1)
#Ljung-Box test on residuals
Box.test(fit1$residuals,lag=30, type="Lj")
#Normality of residuals
shapiro.test(fit1$residuals)
qqnorm(fit1$residuals)
qqline(fit1$residuals,col="RED")
#summary of fitted model
summary(fit1)
#Predictions for the next 10 months
forecast(fit1,h=10,level=0.05,robust=TRUE)

########Question 3##########
# Iowa Annual tornadoes #
#########################
io.t<-ts(io[,c(1,2)])
tsdisplay(io.t)
#stationarity test
kpss.test(io.t[,2],null="T")
#no. of differencing required for achieving stationarity
ndiffs(io.t,test=c("kpss","adf","pp"))
#auto fitting arima
fit2<-auto.arima(diff(io.t[,2],differences=1))
#Diagnostics
tsdiag(fit2)
#Ljung-Box test on residuals
Box.test(fit2$residuals,lag=30, type="Lj")
#Normality of residuals
shapiro.test(fit2$residuals)
qqnorm(fit2$residuals)
qqline(fit2$residuals,col="RED")
#summary of fitted model
summary(fit2)
#Predictions for 2015, 2016, 2017
forecast(fit2,h=3,level=0.05,robust=TRUE)


########Question 4##########
# Iowa Annual injuries #
#########################
io.i<-ts(io[,c(1,3)])
tsdisplay(io.i)
#stationarity test (Trend only and level)
kpss.test(io.i[,2],null="T")
kpss.test(io.i[,2],null="L")
#no. of differencing required for achieving stationarity
ndiffs(io.i,test=c("kpss","adf","pp"))
#auto fitting arima
fit3<-auto.arima(diff(io.i[,2],differences=1))
#Diagnostics
tsdiag(fit3)
#Ljung-Box test on residuals
Box.test(fit3$residuals,lag=30, type="Lj")
#Normality of residuals
shapiro.test(fit3$residuals)
qqnorm(fit3$residuals)
qqline(fit3$residuals,col="RED")
#summary of fitted model
summary(fit3)
#Predictions for 2015, 2016, 2017
forecast(fit3,h=3,level=0.05,robust=TRUE)



########Question 5##########
# Iowa Annual deaths #
#########################
io.d<-ts(io[,c(1,4)])
tsdisplay(io.d)
#stationarity test (Trend only and level)
kpss.test(io.d[,2],null="T")
kpss.test(io.d[,2],null="L")
#no. of differencing required for achieving stationarity
ndiffs(io.d,test=c("kpss","adf","pp"))
#auto fitting arima
fit4<-auto.arima(diff(io.d[,2],differences=1))
#Diagnostics
tsdiag(fit4)
#Ljung-Box test on residuals
Box.test(fit4$residuals,lag=30, type="Lj")
#Normality of residuals
shapiro.test(fit4$residuals)
qqnorm(fit4$residuals)
qqline(fit4$residuals,col="RED")
#summary of fitted model
summary(fit4)
#Predictions for 2015, 2016, 2017
forecast(fit4,h=3,level=0.05,robust=TRUE)

