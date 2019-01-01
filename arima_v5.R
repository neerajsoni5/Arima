### III Approach :; chane other criteria like AIC/BIS####################
library(data.table)
library(data.table)
data<-fread("file:///C:/Users/SoniNe02/Desktop/Others/Regression Training/forecasting/final_v1/JCS 2.csv")
data<-as.data.frame(data)
data<-log(data)
ts.data = ts(data, frequency=12, start=c(2013,1))

plot(ts.data)
# As per the plot, Is the time series is a additive or multiplicative one?
#dim(as.matrix(ts.data))

# Training and Testing Dataset
data.train = window(ts.data, start = c(2013,1), end = c(2016,12)) 
plot(data.train)
dim(as.matrix(data.train))
data.test = window(ts.data, start = c(2017,1),end=c(2017,4))
plot(data.test)
dim(as.matrix(data.test))
################################################################################
####################check for for Stationarity#################################
#The Augmented Dickey-Fuller (ADF) t-statistic test: 
#small p-values suggest the data is stationary and doesn't need to be differenced stationarity
#The null hypothesis for this test is that data is a unit root or not stationaery
library(tseries)
adf.test(data.train,alternative="stationary")

##I Approach-p=0.63,ADF=-1.8436 -Test show high pvalue(greater than 5 %) it means null hypothesis accepted. ie, Not stationary time series
##II Approach-p=0.25,ADF=-2.80- still pvalue is high but less then approach 1 so some improvement.
########################################################################################################
##null hypothesis for these test is that time series is stationary
kpss.test(data.train)

## I Approach -p=0.01,KPSS level=2.342 Test show p value is less than 0.05 ,ie, null hypothesis is rejected
##II Approach- p=0.01, KPSS =2.39 alomost same as previous, with slight improvement
#####################################################################################

# To bring staionarity in time series we need to do differencing. For that we have two approaches
#Ocsb-null hypothesis is that seasonal unit root exists ,ie, diff=1
#Unit root mean:- Not stationary
library(forecast)
nsdiffs(data.train, test="ch")
# I Approach-0
# II Approach-0
#Output 0:- means  there is no seasonal unit root
#output1:- means there is seasonality

nsdiffs(data.train, test="ocsb")
# 1 Approach -1
# 2 Approach -0
#Output 0:- means  there is no seasonal unit root
#output1:- means there is seasonality/ not stationary

###############################################################################################
##### All the above test are itself did by auto.arima##########################################
# Developing an SARIMA model and Analysis of Model
library(forecast)
arima1 = auto.arima(data.train, trace=TRUE, test="kpss", seasonal.test = "ocsb", ic="bic")
####Approach 1 - numbers##################################
#Best model: ARIMA(0,1,0)(1,1,0)[12]
#AIC=1021.99 
#BIC=1025.32
#######################################################

###Approach 2 criteria####################################################
#Best model: ARIMA(0,1,0)(1,0,0)[12]
#AIC= -145
#BIC=-139

###Approach 3 criteria bic####################################################
#Best model: ARIMA(0,1,0)(1,0,0)[12]
#AIC= -145
#BIC=-141 sligt decrease in bic
#########################################################################################
##I have used aic,first, as sample size is low


summary(arima1)

##Approach 1-RMSE=96326
##Approach 1-MAPE=3.04

##Approach 2- RMSE=0.05
##Approach 2 -MAPE=0.27
## Some improvement is seen in approach 2

##Approach 3- RMSE=0.05
##Approach 3 -MAPE=0.29
## Some improvement is seen in approach 2

confint(arima1)
#Approach 1- -0.6 to 0.06
# Approach 2- 0.37 to 0.8
# Approach 3- 0.4 to 0.83

##Some improvement in approach 2

# Residual Diagonostics
plot.ts(arima1$residuals)

##Ljung test is for autocorrelation. Its a test of independence or test for lack of fit
##Null hypothesis:-The model does not exhibit lack of fit./ The residuals are random.
##The Ljung-Box test examines whether there is significant evidence for non-zero correlations at lags 1-20. 
##Small p-values (i.e., less than 0.05) suggest that the series is stationary./
##The Ljung-Box Q (LBQ) statistic tests the null hypothesis that autocorrelations up to lag k equal zero 
##(that is, the data values are random and independent up to a certain number of lags--in this case 12). If the LBQ is greater than a specified critical value, autocorrelations for one or more lags might be significantly different from zero, indicating the values are not random and independent over time.
## high p values show model is adequate

Box.test(arima1$residuals,lag=20, type="Ljung-Box")
#Approch 1-p-0.4,X-sqr=20.93
#Approac 2-p=0.7, X-sqr=15.4
#Approach 3- p=0.75, xsqr=15.3
## Pvalue is increasing that is good.comapre to previous




acf(arima1$residuals, lag.max=24, main="ACF of the Model")
Box.test(arima1$residuals^2,lag=20, type="Ljung-Box")



library(tseries)
jarque.bera.test(arima1$residuals)
##Its a test of normality
##Its means data is perfectly symmetrical around mean
##null hypothesis:- for the test is that the data is normally distributed; the alternate hypothesis is that the data 
##does not come from a normal distribution.
##if p value greater than 5% then we accept null hypothesis
##high pvalue are good
# approach 1-X-sqr-16,p-0.00029
#approach 2-x-sqr-1.8,p=0.399. we have seen some improvement
#approach 3- xsqr=2.3, p=0.3268

#Forecast for other 12 months
arima1.forecast= forecast.Arima(arima1, h=12)
arima1.forecast
plot(arima1.forecast, xlab="Years", ylab="Number of Tourist Arrivals")
###############################################################################
library(TSPred)
plotarimapred(data.test, arima1, xlim=c(2017, 2018), range.percent = 0.05)

## We can easily see their is lot of diff in actual and predicted value
accuracy(arima1.forecast, data.test)
###Approach 1-Lots of difference seen in accuracy test as well
###Approach 2 some improvemetn seen in trend line
###mapproach 3 .only slight improvement compare to 2
###################################I test Coimpleted######################################################
####Approach 2 where we use log transformation and test by changing other criteria########
#########################################################################################################
########################################################################################################
########################################################################################################

