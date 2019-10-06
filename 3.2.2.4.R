
# The BEER - BEHAVIOURAL EQUILIBRIUM EXCHANGE RATE model was developed by Clark and MacDonald (1999) and estimates the fair value of currencies according to short, medium and long-run determinants. 
# An important concept is that there is no established theory for the choice of economic variables; hence, the choice of variables is based on economic intuition and data simplicity and availability. 
# In this exercise we chose the inflation differential, interest rate diferential and to be regression variables for the ln(return) for the AUD?USD exchange rate.  

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

data = as.data.frame(cbind(DF$`ln( return )`, DF$`Infl Diff`, DF$`IR Diff`))


#COMMENT : Test for stationarity
colnames(data)[1]<-"Rate"
colnames(data)[2]<-"Inflation"
colnames(data)[3]<-"Interest"



dif_Rate=diff(data$Rate)
dif_Inflation=diff(data$Inflation)
dif_Interest=diff(data$Interest)


# COMMENT : Using ADF test to check for stationarity
adf.test(data$Rate)
adf.test(dif_Rate)

adf.test(data$Inflation)
adf.test(dif_Inflation)

adf.test(data$Interest)
adf.test(dif_Interest)



#COMMENT : From the result, we can confirm the stationarity of order 1 for the variables.

#COMMENT : Etimate the lag for VECM Model 

VARselect(data,lag.max=10, type="none")$selection

#COMMENT : Based on the AIC result, we can pick the lag = 9
#We can then proceed we the Johansen test using k=9

jotest1a=ca.jo(data, type="eigen", K=4, ecdet="none", spec="longrun") 
summary(jotest1a) 

jotest2a=ca.jo(data, type="trace", K=4, ecdet="none", spec="longrun") 
summary(jotest2a)

#COMMENT : Both tests are showing there are cointegration relations as both tests rejects the null hypothesis of r = 0 at 5%
#r or number of co-integrations is at most = 1 The Johnson cointegration test results eliminates the use of VAR model. 
#So we move on to use VECM in order to determine long run relationship of equilibrium real exchange rate.

#VECM model implementation

model = VECM(data,9,r = 1, include = "const",estim = "ML", LRinclude = "none") 
summary(model)

#COMMENT on the result : From the results, we can deduce that there is a long term equilibrium between AUD/USD exchange rate and interest rate differential and also inflation differential from the significant ECT.
#Both the ECM are significant, even though the ECM, which represents the speed of adjustment for interest rate is showing positive, the overall multivariate system will still be drawn back to equilibrium as the reduction effect on inflation is greater.
#Short term wise, the exchange rate does not seem to be heavily affected by past data. 
#The cointegrating equation shows the interest rate helps in appreciation of AUD/USD while inflation rate is more related to its depreciation.



