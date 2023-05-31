setwd("")
library(tseries)
library(vars)
## ==> does monetary policy have an effect on real output growth?

# real GDP
data.rgdp=read.table("kr_gdp.csv",header=T, sep=",")
colnames(data.rgdp)[2] <- "rGDP"
rgdp=ts(data.rgdp[,2],start=1960,frequency=4) #time series
rgdp=window(rgdp,start=c(1960,1),end=c(2020,1))
lrgdp = 400*diff(log(rgdp)) # quarterly rGDP growth rate

# short-term nominal interest rate/3-month interest rate
data.3mr=read.table("kr_3_month_rates.csv",header=T, sep=",")
colnames(data.3mr)[2] <- "3 MONTH RATES"
threemr=ts(data.3mr[,2],start=1991,end=c(2020,3),frequency=4)

# excluding covid period, establishing same time window for data
y=window(lrgdp,start=c(1991,1),end=c(2020,1))
int=window(threemr,start=c(1991,1),end=c(2020,1))

# getting an idea of what the data looks like
summary(y)
summary(int)

plot(y,main="Quarterly Real GDP Growth Rate (black) and Short-Term Nominal Interest Rate (blue)",col="black",lwd=2)
lines(int,col="blue",lwd=2)

# VAR analysis
data.y.int<-ts(cbind(y,int)) # binding data, order: y 1st, interest rate 2nd
acf(data.y.int)
ccf(y,int)
pacf(data.y.int)

# computing information criteria + model selection
VARselect(data.y.int,lag.max=15,type="const")
# AIC+FPE says 7, HQ+BIC/SC points to 2 (BIC/SC is more conservative)

# preferred VAR specification
# if we were using the VAR for forecasting, we would want to be more parsimonious
# we are using the VAR for causality analysis => might want to use more than just 2 lags
# this will ensure that we catch any interdependence that may exist
# p=7 (7 lags) does mean more parameters (7x4=28 parameters, 117 obs) that need to be estimated -> highlighting the bias-variance tradeoff
# => going to use p=7

# robustness check, see if we still get a negative impulse response at lag 1
var.y.int<-VAR(data.y.int,p=2)
summary(var.y.int) # shows coefficient estimates

var.y.int<-VAR(data.y.int,p=4)
summary(var.y.int)

var.y.int<-VAR(data.y.int,p=5)
summary(var.y.int)
# 1 quarter is robust to the order of the VAR specification

var.y.int<-VAR(data.y.int,p=7)
summary(var.y.int) # shows coefficient estimates

# variance-covariance matrix of residuals
# [19.831 -2.218]
# [-2.218 1.051]
# the innovations are contemporaneously correlated

# checking for covariance stationarity (computing Eigenvalues of the companion form matrix)
roots(var.y.int)
# the values are all less than 1 => our model is stationary

# plotting the fitted values of the VAR estimation and vector of innovations
plot(var.y.int)

# Structural Impulse Response using Sims (1980) solution (Cholesky decomposition)
irf.y.int<-irf(var.y.int,ortho=TRUE,ci=0.95,runs=100,n.ahead=15) # TRUE implements the Cholesky decomposition, makes alpha_i=0
plot(irf.y.int,lwd=2)
# 1st plot: impulse response of output and interest rate to a change in the output shock epsilon_y (what we want is epsilon_i though)
# comment on whether or not this is reasonable (looks like it is), shock to y, change in epsilon y -> change in int?
# plot 2: impulse response of output and interest rate to a change/shock in the interest rate equation epsilon_i (important, what we want)
# look to see if CI line contains the 0 line, null: impulse response is 0 if it contains 0 line
# we can be somewhat confident that the impulse response is negative at j=1 (CI lines contain 0 after j=1)

## if we believe the correct VAR lag choice is 7 lags and we believe that alpha_i should be equal to 0
## then our results say that monetary policy has a negative effect on output growth
## an unexpected increase/shock in the interest rate from the monetary authority (CB of SK) has a negative effect on output growth 1 quarter after the shock
## at 2 quarters, we cannot reject the null hypothesis of the impulse response being 0, and it is the same until quarters 8 -10, then it's the same it was for Q2-7
## the effect is short-lived but comes back
# int panel states that when interest rate gets higher unexpectedly, it will stay high for several periods
# can also explain why it takes a quarter for output to return to the level prior to the shock
