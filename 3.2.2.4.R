
install.packages("urca")
install.packages("vars")
install.packages("tsDyn")
library(tidyverse) # loads the tidyverse package
library(dplyr)
library(readxl)
library(stats)
library(tseries)
library(forecast)
library(magrittr)
library(urca)
library(vars)
library(tsDyn)

DF <- as.data.frame(read_excel(path = "Data-VEC Model.xlsx",sheet="Data"))

data = as.data.frame(cbind(DF$`ln( return )`, DF$`Infl Diff`, DF$`IR Diff`, DF$`CP Diff`))
#data = as.data.frame(cbind(DF$`ln( return )`, DF$`Infl Diff`, DF$`IR Diff`))

#1. test for stationarity
colnames(data)[1]<-"Rate"
colnames(data)[2]<-"Inflation"
colnames(data)[3]<-"Interest"
colnames(data)[4]<-"Current_Payment"


dif_Rate=diff(data$Rate)
dif_Inflation=diff(data$Inflation)
dif_Interest=diff(data$Interest)
dif_Current_Payment=diff(data$Current_Payment)

# Using ADF test to check for stationarity
adf.test(data$Rate)
adf.test(dif_Rate)

adf.test(data$Inflation)
adf.test(dif_Inflation)

adf.test(data$Interest)
adf.test(dif_Interest)

adf.test(data$Current_Payment)
adf.test(dif_Current_Payment)
#confirmed stationarity of order 1

#2. estimate the lag for  VECM Model 

VARselect(data,lag.max=10, type="none")$selection

#pick lag = 4 based on AIC
# Johansen test 

jotest1a=ca.jo(data, type="eigen", K=4, ecdet="none", spec="longrun") 
summary(jotest1a) 

jotest2a=ca.jo(data, type="trace", K=4, ecdet="none", spec="longrun") 
summary(jotest2a)

#there are cointegration relations as both tests rejects the null hypothesis of r = 0 at 5%
#r at most = 1

#VECM model implementation


model = VECM(data,4,r = 1, include = "const",estim = "ML", LRinclude = "none") 
summary(model)

#Using the 

#optional for prediction
#newdata = tail(data,5)
#predict(VECM_fit,newdata = newdata)
