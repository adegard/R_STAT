library(gtrendsR)
library(ggplot2)
library(reshape2)
library(xts)
library(zoo)
library(quantmod) # data, plotting, quant modelling
library(TTR)

#USDJPY=X-^N225
#^DJI-^N225
#^GSPC-^STOXX50E

###########parameters##########
initDate <- Sys.Date()-90 #last x days
finDate <- Sys.Date()-1
Ticker1<- "BTCEUR=X" #from yahoo finance
Ticker2<- "BTCUSD=X" #from yahoo finance
IsShifted<-TRUE  #false to find correlation without day shift
Nshifts<-1
################################

#####################################
## EXTRACTION OF FINANCIAL DATA  1 ##
#####################################

financeData1<-NULL
financeData1 = getSymbols(Ticker1, 
                         src = "yahoo", 
                         from=initDate, to=finDate,
                         auto.assign=FALSE)
colnames(financeData1) = c("open","high","low","close","volume", "adj")
head(financeData1)

financeData1<-data.frame(StartDate=index(financeData1),
                        Price=financeData1[,4])


rownames(financeData1) <- NULL
head(financeData1)

#extracting ratio
datalen=length(financeData1$close)
financeData1$ratio1[2:datalen]=financeData1$close[2:datalen]/financeData1$close[1:datalen-1]
#plot ratios over the time
#ggplot(data=subset(financeData1, !is.na(ratio1)), aes(x=StartDate, y=ratio1, group=1)) + geom_line()

#head(financeData1)


################################


#####################################
## EXTRACTION OF FINANCIAL DATA  2 ##
#####################################

financeData2<-NULL
financeData2 = getSymbols(Ticker2, 
                          src = "yahoo", 
                          from=initDate, to=finDate,
                          auto.assign=FALSE)
colnames(financeData2) = c("open","high","low","close","volume", "adj")
head(financeData2)

financeData2<-data.frame(StartDate=index(financeData2),
                         Price=financeData2[,4])


rownames(financeData2) <- NULL
head(financeData2)

#extracting ratio
datalen=length(financeData2$close)
financeData2$ratio2[2:datalen]=financeData2$close[2:datalen]/financeData2$close[1:datalen-1]
#plot ratios over the time
#ggplot(data=subset(financeData2, !is.na(ratio2)), aes(x=StartDate, y=ratio2, group=1)) + geom_line()

#head(financeData2)

###################################

#merging Price ratio
DGT_PRICEratio_sft <- merge(financeData1, financeData2,by="StartDate")

if (IsShifted==TRUE) {
  
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

#DGT_PRICEratio_sft<-DGT_PRICEratio
DGT_PRICEratio_sft$ratio2 <- shift(DGT_PRICEratio_sft$ratio2, Nshifts)
head(DGT_PRICEratio_sft)

} else {
  
  head(DGT_PRICEratio_sft)
}  
#regression model##############
mod <- lm(DGT_PRICEratio_sft$ratio1 ~  DGT_PRICEratio_sft$ratio2, 
    data=DGT_PRICEratio_sft) 

#######################

corcoeff<-cor(DGT_PRICEratio_sft$ratio2[1:length(DGT_PRICEratio_sft$ratio2)-1], 
    DGT_PRICEratio_sft$ratio1[1:length(DGT_PRICEratio_sft$ratio2)-1],
    use = "na.or.complete",
    method = "pearson")

plot(DGT_PRICEratio_sft$ratio2[1:length(DGT_PRICEratio_sft$ratio2)-1],
     DGT_PRICEratio_sft$ratio1[1:length(DGT_PRICEratio_sft$ratio2)-1], pch = 16, cex = 1.3, col = "blue", 
     main = paste(Ticker1, Ticker2, sep="-"), xlab = "ratio2", ylab = "ratio1")

abline(lm(DGT_PRICEratio_sft$ratio1 ~ DGT_PRICEratio_sft$ratio2, 
          data=DGT_PRICEratio_sft),
        col = "blue")
#text(0,1, "abline( h = 0 )", col = "gray60", adj = c(0, -.1))
abline(1,0)
abline(v=1)

mod
corcoeff
paste(Ticker1, Ticker2, sep="-")
cor.test(DGT_PRICEratio_sft$ratio2[1:length(DGT_PRICEratio_sft$ratio2)-1], 
    DGT_PRICEratio_sft$ratio1[1:length(DGT_PRICEratio_sft$ratio2)-1],
    use = "na.or.complete",
    method = "pearson")







