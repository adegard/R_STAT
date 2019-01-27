#http://www.tradinggeeks.net/2014/07/technical-analysis-with-r/
#followed by ML: https://www.datacamp.com/community/tutorials/machine-learning-in-r#gs.FPGodtU

library(gtrendsR)
library(ggplot2)
library(reshape2)
library(xts)
library(zoo)
library(quantmod) # data, plotting, quant modelling
library(TTR)


###########parameters##########
initDate <- Sys.Date()-300 #last x days
finDate <- Sys.Date()
Ticker1<- "EURUSD=X" #from yahoo finance
Nshifts<-1 #number o f days for future price shift
ExtractMethod<-"YAHOO" #YAHOO or TIDYQUANT
################################

allData<-NULL

#####################################
## EXTRACTION OF FINANCIAL DATA  1 ##
#####################################
Sys.setenv(TZ = "GMT")
financeData1<-NULL

#### getsymbosl METHOD ####
 # if(ExtractMethod=="YAHOO") {
financeData1 = getSymbols(Ticker1, 
                         src = "yahoo", 
                         from=initDate, to=finDate,
                         auto.assign=FALSE)
dc_hlc <- HLC(financeData1)
dc_hlc <-na.omit(dc_hlc)
head(dc_hlc)
colnames(dc_hlc) = c("High","Low","Close")

#  } ELSE {
    
#################################
#### TIDYQUANT METHOD #############
#require(tidyquant)
#require(dplyr)
#financeData1  <- tq_get(Ticker1, get = "stock.prices.japan", from = "2017-01-01")
#dc_hlc <- financeData1
#colnames(dc_hlc) = c("Open","High","Low","Close","Volume","Adjusted")
#tail(dc_hlc)
#View(dc_hlc)
  
#####################################
# }

bb20 = BBands(dc_hlc, n = 10, sd = 2, maType=EMA)
rsi14 = RSI(dc_hlc[,"Close"], n=14)
sma20 = SMA(dc_hlc[,"Close"],n=20)
macd = MACD(dc_hlc[,"Close"], nFast=12, nSlow=26, nSig=9, maType=SMA, percent = TRUE)

StartDate<-as.Date(index(dc_hlc))

#merging all data in single file for strategy test
dataPlusBB = data.frame(StartDate,dc_hlc,bb20)
rownames(dataPlusBB) <- NULL

head(dataPlusBB)

plot(StartDate,dataPlusBB$Close)

lines(StartDate,dataPlusBB$Close, col = "red")
lines(StartDate,dataPlusBB$up, col = "purple")
lines(StartDate,dataPlusBB$dn, col = "brown")
lines(StartDate,dataPlusBB$mavg, col = "blue")

allData = data.frame(StartDate,dc_hlc,sma20,bb20,rsi14,macd)
#View(allData)

#shifted price function
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

Future_Close=shift(allData$Close,Nshifts)

allData<- data.frame(allData,Future_Close)

#extracting future price ratio
datalen=length(Future_Close)
#datalen
allData$Future_ratio[2:datalen]=allData$Future_Close[2:datalen]/allData$Future_Close[1:datalen-1]
PriceRatio<-allData$Future_ratio[2:datalen]

allData<- data.frame(allData, PriceRatio)

# define Observed variation:
allData$Obs_var <- ifelse(allData$Future_ratio > 1, "UP", "DOWN")

View(allData)




