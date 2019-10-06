
# The BEER - BEHAVIOURAL EQUILIBRIUM EXCHANGE RATE model was developed by Clark and MacDonald (1999) and estimates the fair value of currencies according to short, medium and long-run determinants. 
# An important concept is that there is no established theory for the choice of economic variables; hence, the choice of variables is based on economic intuition and data simplicity and availability. 
# In this exercise we chose the inflation differential, interest rate diferential and current payment differential to be regression variables. 

install.packages("urca")
install.packages("vars")
install.packages("tsDyn")
library(readxl)
library(tseries)
library(forecast)
library(magrittr)
library(urca)
library(vars)
library(tsDyn)

DF <- as.data.frame(read_excel(path = "Data-VEC Model.xlsx",sheet="Data"))

data = as.data.frame(cbind(DF$`ln( return )`, DF$`Infl Diff`, DF$`IR Diff`, DF$`CP Diff`))


#COMMENT : Test for stationarity
colnames(data)[1]<-"Rate"
colnames(data)[2]<-"Inflation"
colnames(data)[3]<-"Interest"
colnames(data)[4]<-"Current_Payment"


dif_Rate=diff(data$Rate)
dif_Inflation=diff(data$Inflation)
dif_Interest=diff(data$Interest)
dif_Current_Payment=diff(data$Current_Payment)

# COMMENT : Using ADF test to check for stationarity
adf.test(data$Rate)
adf.test(dif_Rate)

adf.test(data$Inflation)
adf.test(dif_Inflation)

adf.test(data$Interest)
adf.test(dif_Interest)

adf.test(data$Current_Payment)
adf.test(dif_Current_Payment)

#COMMENT : From the result, we can confirm the stationarity of order 1 for the variables.

#COMMENT : Etimate the lag for VECM Model 

VARselect(data,lag.max=10, type="none")$selection

#COMMENT : Based on the AIC result, we can pick the lag = 4
#We can then proceed we the Johansen test using k=4

jotest1a=ca.jo(data, type="eigen", K=4, ecdet="none", spec="longrun") 
summary(jotest1a) 

jotest2a=ca.jo(data, type="trace", K=4, ecdet="none", spec="longrun") 
summary(jotest2a)

#COMMENT : Both tests are showing there are cointegration relations as both tests rejects the null hypothesis of r = 0 at 5%
#r or number of co-integrations is at most = 1

#VECM model implementation

model = VECM(data,4,r = 1, include = "const",estim = "ML", LRinclude = "none") 
summary(model)

#COMMENT on the result : From the results, we can deduce that there is a long term equilibrium between AUD/USD exchange rate and interest rate differential and also current payment differential from the significant ECT.
#Short term wise, the exchange rate is only seem affected by lag-2 quarters current payment account.



