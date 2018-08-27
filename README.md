# Time-Series-Analysis---Cryptocurrency
Time Series Cryptocurrency Model

# install.packages("FSAdata")
# install.packages("lmtest")
# install.packages("FitAR")
# install.packages("forecast")  
# install.packages("truncnorm") 
# install.packages("rugarch")

library(truncnorm)
library(rugarch)
library(FSAdata)
library(TSA)
library(fUnitRoots)
library(lmtest)
library(FitAR)
library(forecast)

library(readxl)
## Warning: package 'readxl' was built under R version 3.4.4
bitcoin <- read_excel("~/Configuration/Desktop/ts final assignment/Bitcoin_Historical_Price.xlsx") View(bitcoin)
ts.plot(bitcoin$Close)

https://coinmarketcap.com
https://drive.google.com/open?id=1CVAxXDjbNZstl931my4wGEg2sqchSYCH

  closing <- bitcoin$Close class(closing)
## [1] "numeric"
closing.ts <- ts(closing, start = c(2013, 116), frequency = 365) length(closing.ts)
## [1] 1772

plot(closing.ts, type="o")

#Increase as well as decrease | can be non stationary #AR
#no Seonality
#changing variance
# As its non stationaey series, so we cant go for trend models, but as of now we don't know the diagnostic checks
# for ARIMA, ARMA, AR or MA models. So for sake of doing something, we will do diagnostic check for all linear and quadratic # Discarding Harmonic at the first place as its not seasonal series
plot(y=closing.ts,x=zlag(closing.ts),ylab='closing Deposition', xlab='Previous Year closing Deposition' , main = "Scatter plot ")

y = closing.ts
x = zlag(closing.ts) index = 2:length(x) cor(y[index],x[index])
## [1] 0.9970915
#good coorrelation: 0.99


Linear Model
# Linear model
t = time(closing.ts)
model.closing.ln = lm(closing.ts~t) # label the linear trend model as model.ChinookKR.ln summary(model.closing.ln)

plot(closing.ts,type='o',ylab='y') abline(model.closing.ln)


#Coefficent and overall model are significant #R^2 value is not so good, very low
res.model.closing.ln = rstudent(model.closing.ln)
plot(y = res.model.closing.ln, x = as.vector(time(closing.ts)),xlab = 'Time', ylab='Standardized Residuals',type='o')

#this is not random, we can see significant pattern in the residual plot which has not been covered by model and its appearing in residual. I
qqnorm(res.model.closing.ln) qqline(res.model.closing.ln, col = 2, lwd = 1, lty = 2)

#very bad, many points at the beginning and end are far from the qq line, not good! shapiro.test(res.model.closing.ln)
##
## Shapiro-Wilk normality test ##
## data: res.model.closing.ln
## W = 0.73029, p-value < 2.2e-16
#p value is way less than alpha so data is not normally distributed indicates goodness of fit acf(res.model.closing.ln)

#there is slow decaying pattern, which confirms that its not stationary
#By looking at every checks we can say that there is still autocorrelation left in residual. Hence we rejet this model

t = time(closing.ts)
t2 = t^2
model.closing.qa = lm(closing.ts~ t + t2) # label the quadratic trend model as model.ChinookKR.qa summary(model.closing.qa)

plot(ts(fitted(model.closing.qa)), ylim = c(min(c(fitted(model.closing.qa),
as.vector(closing.ts))), max(c(fitted(model.closing.qa),as.vector(closing.ts)))),
ylab='y' , main = "Fitted quadratic curve to random walk data", type="l",lty=2,col="red") lines(as.vector(closing.ts),type="o")



#Coefficent and overall model are significant #R^2 value is not so good, very low
res.model.closing.qa = rstudent(model.closing.qa)
plot(y = res.model.closing.qa, x = as.vector(time(closing.ts)),xlab = 'Time', ylab='Standardized Residuals',type='o')

#this is not random, we can see significant pattern in the residual plot which has not been covered by model and its appearing in residual. I
qqnorm(res.model.closing.qa) qqline(res.model.closing.qa, col = 2, lwd = 1, lty = 2)

#very bad, many points at the beginning and end are far from the qq line, not good! shapiro.test(res.model.closing.qa)
##
## Shapiro-Wilk normality test ##
## data: res.model.closing.qa
## W = 0.78251, p-value < 2.2e-16
#p value is way less than alpha so data is not normally distributed indicates goodness of fit 
acf(res.model.closing.qa)



#there is slow decaying pattern, which confirms that its not stationary
#By looking at residual plot and Shapiro test we can say that there is still autocorrelation left in residual. Hence we rejet this model

Tests and plots
par(mfrow=c(1,2))
acf(closing.ts) #Trend is apparent from ACF and PACf plots pacf(closing.ts)

# Slowly decaying pattern in ACF and very high first correlation in PACF # implies the existence of trend and nonstationarity.
# Apply ADF test adf.test(closing.ts)
##
## Augmented Dickey-Fuller Test ##
## data: closing.ts
## Dickey-Fuller = -1.6976, Lag order = 12, p-value = 0.7063
## alternative hypothesis: stationary
# With a p-value of 0.706, we cannot reject the null hypothesis stating that # the series is non-stationary.
par(mfrow=c(1,1)) qqnorm(closing.ts) qqline(closing.ts, col = 2)

shapiro.test(closing.ts)
##
## Shapiro-Wilk normality test ##
## data: closing.ts
## W = 0.48004, p-value < 2.2e-16
# p<0.05
# QQ plot and Shapiro test suggest that closing is not normally distributed at all
#
# # Let's first apply the box-Cox transformation.
# closing.transform = BoxCox.ar(closing.ts)
# closing.transform$ci
# # Mid-point of interval is 0.1. So we will take lambda as 0.1
# lambda = 0.1
# BC.closing = (closing.ts^lambda-1)/lambda

# qqnorm(BC.closing)
# qqline(BC.closing, col = 2)
# shapiro.test(BC.closing)
# # The Box-Cox transformation did not help to improve normality of the series # # because the dots are not aligned with the red line in QQ plot and
# # p-value of the Shapiro test is less than 0.05.
#
##Taking original series closing
diff.closing = diff(closing.ts,1) plot(diff.closing,type='o',ylab='Quarterly earnings ')

adf.test(diff.closing)
## Warning in adf.test(diff.closing): p-value smaller than printed p-value
##
## Augmented Dickey-Fuller Test ##
## data: diff.closing
## Dickey-Fuller = -11.186, Lag order = 12, p-value = 0.01
## alternative hypothesis: stationary
#Only one difference is able to detrendalize the series, which is confirmed by plot and adf test
# but as we can see changing variance in first plot of time series data oof closing price, lets to log transformation
##Taking log of original series closing
closing.log = log(closing.ts)
diff.closing = diff(closing.log,1) plot(diff.closing,type='o',ylab='Quarterly earnings ')

adf.test(diff.closing)
## Warning in adf.test(diff.closing): p-value smaller than printed p-value
##
## Augmented Dickey-Fuller Test

##
## data: diff.closing
## Dickey-Fuller = -10.167, Lag order = 12, p-value = 0.01
## alternative hypothesis: stationary
# With a p-value less than 0.01, we reject the null hypothesis stating that
# the series is non-stationary; hence, we conclude that the third differencing # make the series staionary.
# Hence d = 1
# Specify the orders using ACf, PACF, EACF and BIC over the differenced series par(mfrow=c(1,2))
acf(diff.closing)
pacf(diff.closing)

# we can select p = 0 q = 1 from ACF and PACF
# There is one significant lag in ACF and one significant lags in PACF. 
# So we can include ARIMA(0,1,1) model among the tentative models.
   eacf(diff.closing)
   
   # p =7 q = 5,6
   
   # ARIMA(7,1,5) ARIMA(7,1,6)
 par(mfrow=c(1,1))
res2 = armasubsets(y=diff.closing,nar=12,nma=12,y.name='test',ar.method='ols')
## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, ## force.in = force.in, : 6 linear dependencies found
## Reordering variables and trying again: plot(res2)

#p = 5,10,11 q = 1
# ARIMA(5,1,1) ARIMA(10,1,1) ARIMA(11,1,1)
# so the candidate models are ARIMA(0,1,1) ARIMA(7,1,5) ARIMA(7,1,6) ARIMA(5,1,1) ARIMA(10,1,1) ARIMA(11,1,1)

#ARIMA(0,1,1)
model_011_css = arima(closing.log,order=c(0,1,1),method='CSS') coeftest(model_011_css)

#ARIMA(7,1,5)
model_715_css = arima(closing.log,order=c(7,1,5),method='CSS') coeftest(model_715_css)

#ARIMA(7,1,6)
model_716_css = arima(closing.log,order=c(7,1,6),method='CSS') coeftest(model_716_css)

#ARIMA(5,1,1)
model_511_css = arima(closing.log,order=c(5,1,1),method='CSS') coeftest(model_511_css)

#ARIMA(10,1,1)
model_1011_css = arima(closing.log,order=c(10,1,1),method='CSS') coeftest(model_1011_css)

#ARIMA(11,1,1)
model_1111_css = arima(closing.log,order=c(11,1,1),method='CSS') coeftest(model_1111_css)

AIC and BIC values
. *
* *
0.05 '.' 0.1 ' ' 1
                  sort.score <- function(x, score = c("bic", "aic")){ if (score == "aic"){
x[with(x, order(AIC)),]
} else if (score == "bic") { x[with(x, order(BIC)),]
} else {
warning('score = "x" only accepts valid arguments ("aic","bic")') }
}
# AIC and BIC values sort.score(AIC(model_011_ml,model_715_ml,model_716_ml,model_511_ml,model_1011_ml,model_1111_ml), score = "aic")
  
  ##
## model_715_ml
## model_716_ml
## model_1111_ml
## model_1011_ml
## model_511_ml
## model_011_ml
df AIC 13 -5979.741 14 -5976.471 13 -5959.784 12 -5959.276
7 -5957.491 2 -5950.421
       sort.score(BIC(model_011_ml,model_715_ml,model_716_ml,model_511_ml,model_1011_ml,model_1111_ml), score = "bic" )
 ##
## model_011_ml
## model_511_ml
## model_715_ml
## model_716_ml
## model_1011_ml
## model_1111_ml
df BIC 2 -5939.463 7 -5919.136
13 -5908.510 14 -5899.761 12 -5893.524 13 -5888.553
       #715(AIC)
#Ar7 component is insignificant
#011(BIC)

#its not significant
 # ARIMA(7,1,5) model is the best one among the set of specified models.
# To check with the overfitting we will fit ARIMA(6,1,5),ARIMA(8,1,5),ARIMA(7,1,4),ARIMA(7,1,6) models

ARIMA Models

# ARIMA(6,1,5)
model_615_css = arima(closing.log,order=c(6,1,5),method='CSS') coeftest(model_615_css)

# ARIMA(8,1,5)
model_815_css = arima(closing.log,order=c(8,1,5),method='CSS') coeftest(model_815_css)

# ARIMA(7,1,4)
model_714_css = arima(closing.log,order=c(7,1,4),method='CSS')
coeftest(model_714_css)

# ARIMA(7,1,6)
model_716_css = arima(closing.log,order=c(7,1,6),method='CSS') coeftest(model_716_css)

sort.score(AIC(model_615_ml,model_815_ml,model_714_ml,model_716_ml), score = "aic")

##
## model_615_ml
## model_716_ml
## model_815_ml
## model_714_ml
df AIC 13 -5979.741 14 -5976.471 14 -5975.061 12 -5957.349
     sort.score(BIC(model_615_ml,model_815_ml,model_714_ml,model_716_ml), score = "bic" )
 ##
## model_615_ml
## model_716_ml
## model_815_ml
## model_714_ml
df BIC 13 -5908.510 14 -5899.761 14 -5898.350 12 -5891.598
    
    
    # ARIMA(6,1,5) is the best model according to both AIC and BIC
    
    residual.analysis <- function(model, std = TRUE){ library(TSA)
library(FitAR)
if (std == TRUE){
res.model = rstandard(model) }else{
res.model = residuals(model) }
par(mfrow=c(3,2))
plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals") abline(h=0)
hist(res.model,main="Histogram of standardised residuals")
qqnorm(res.model,main="QQ plot of standardised residuals")
qqline(res.model, col = 2)
acf(res.model,main="ACF of standardised residuals")
print(shapiro.test(res.model))
k=0
LBQPlot(res.model, lag.max = length(model$residuals)-1 , StartLag = k + 1, k = 0, SquaredQ = FALSE) par(mfrow=c(1,1))
}
residual.analysis(model = model_615_ml)
##
## Shapiro-Wilk normality test ##
## data: res.model
## W = 0.88451, p-value < 2.2e-16

Prediction
## predict using AR,MA, ARMA or ARIMA model fit = Arima(closing.ts,c(6,1,5)) plot(forecast(fit,h=10))

## predict using AR,MA, ARMA or ARIMA model plot(forecast(fit,h=10,level = 99))


          
          
   
   

