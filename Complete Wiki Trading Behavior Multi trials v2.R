library(pageviews)
library(ggplot2)
library(xts)
library(zoo)
library(quantmod) 
library(TTR)
library(bitops)
library(RCurl)
library(jsonlite)

###########parameters##########
initDate<- Sys.Date()-30 #last x days
finDate<- Sys.Date()-1

FinancialSymbol<-"BTCUSD=X"  #symbol from YAHOO finance
ProjectLanguage<-"en.wikipedia.org"
mycomment<-"Wikipedia terms searches"
ideltat<-1 # number of previous DAYS of search volume to compare to
#dataFromFile<-FALSE #if TRUE: put data in myfilename
#myfileName<-"fileKeywordDiscovery_fr.csv"
lastmonthdata<-TRUE  ####FALSE: only yesterday data, 
firstxresults<-400  #number of most viewed pages
sendCompletionNotif<-FALSE
#################### Symbols :
#FTSEMIB.MI  (6%)
#EURJPY=X    (6%)
#BTCUSD=X   BTCEUR=X 
#CL=F (oil)   18%/m.
#EURUSD=X 
#^FTSE   
#^FCHI (FRA40)
#^IBEX (SPA35) 
#^GDAXI (GER30)     
#GBPCAD=X 
#GBPJPY=X 
#GBPCHF=X 


#OTHER
#FCA.MI : FIAT : Terremoto di Casamicciola del 1883, Debito pubblico
#UG.PA : peugeot
#VOW.DE : Volkwagen: Game of Thrones

mylist<-NULL

#if (dataFromFile==TRUE) {
###################extract from a file
#thePath = "C:\\Users\\degar_000\\Google Drive\\Trading\\R\\Tobias Preis Gtrend study\\"
#mylist = read.csv(paste(thePath,myfileName,sep=""))
#mylist<-mylist$Page

#} else {
  


if (lastmonthdata==TRUE) {
  #######JSON EXTRACT MOST VIEWED ARTICLES OF LAST MONTH  
themonth<-as.numeric(substr(finDate, 6,7))-1
themonth<-ifelse(themonth<10,paste("0",themonth,sep=""),themonth)
theURL<-paste("https://wikimedia.org/api/rest_v1/metrics/pageviews/top/",
              substr(ProjectLanguage, 1, 12),"/all-access/",
              substr(finDate, 1, 4),"/",
              themonth,"/all-days", sep="")

} else {
  #######JSON EXTRACT MOST VIEWED ARTICLES OF YESTERDAY  
theURL<-paste("https://wikimedia.org/api/rest_v1/metrics/pageviews/top/",
      substr(ProjectLanguage, 1, 12),"/all-access/",
      substr(finDate, 1, 4),"/",
      substr(finDate, 6, 7),"/",
      substr(finDate, 9, 10), sep="")
}


rawData <- getURL(theURL)
parsedData <- fromJSON(rawData)
str(parsedData)
viewsData <- data.frame(parsedData$items$articles)

df<-viewsData$article

#} ######end test extract from file#######
mylist<-df[1:firstxresults]
head(mylist)

financeData<-NULL
Action<-NULL
allR<-NULL
allR_ordered<-NULL
initDateW<- paste(sub(pattern="-", replacement="", sub(pattern="-", replacement="", initDate)),"00", sep="")
finDateW<- paste(sub(pattern="-", replacement="", sub(pattern="-", replacement="", finDate)),"00", sep="")

#extraction of economic data
financeData = getSymbols(FinancialSymbol, 
                         src = "yahoo", 
                         from=initDate, to=finDate,
                         auto.assign=FALSE)
colnames(financeData) = c("open","high","low","close","volume", "adj")

financeData<-data.frame(StartDate=index(financeData),Price=financeData[,4])
rownames(financeData) <- NULL
#Ploting
bp <- ggplot(data=financeData, aes(x=StartDate, y=close, group=1)) + geom_line()
bp
bp + ggtitle(FinancialSymbol)

######Extraction of wiki for multiple keywords and single Deltat##########

iKeywords<-NULL
for (iKeywords in mylist){ #instead of theKeywords
  #print(iKeywords)
  
  myData2<-article_pageviews(project = ProjectLanguage,
                             article = iKeywords, platform = "all", 
                             user_type = "all", start = initDateW, 
                             end = finDateW, 
                             reformat = TRUE)
  
  myData2$date<-as.Date(myData2$date) #say to R that Date column is date
  StartDate<-myData2$date
  Views<-myData2$views
  tabella<-data.frame(StartDate,Views)
  
  wikidata<-tabella
  head(wikidata)
  
#merging all data in single file for strategy test

TradingBeahaviorFile <- merge(wikidata, financeData,by="StartDate")
head(TradingBeahaviorFile)

dat<-TradingBeahaviorFile

#rimoving NAs
dat<-dat[!rowSums((is.na(dat))),]
head(dat)

#################Strategy Trading Backtest###############

# Parameters
keyword<-"Views"

r<-rep(1,nrow(dat))

# loop Analysis########################
for(i in 1:nrow(dat)) {
  if(i>1) { # Not on first date (no previous search volume)
    r[i]<-r[i-1] # Copy previous return, in case no trading
  }
  if(i>ideltat) { # Wait for first window to pass, 
    # so we can calc past search volume
    if(i<nrow(dat)) { # Not on last date (no future Dow Jones value)
      
      now<-dat[[keyword]][i] # Google Trends search volume for keyword 
      # (e.g. "debt") for this week
      previous<-0
      
      # Calculate average search volume of last ideltat weeks
      for(t in 1:ideltat) {
        previous<-previous+dat[[keyword]][i-t]
      }
      previous<-(previous/ideltat)
      
      # Change in search volume
      value<-(now-previous)
      #print(value)
      
      # DJIA closing price on the first trading day of the coming week
      # *To check this, REFER TO FILE LAYOUT, which also includes dates 
      #  for DJIA values*
      index_now<-dat$close[i]
      #print(index_next)
      
      # DJIA closing price on the first trading day of the week 
      # after the coming week
      index_next<-dat$close[i+1]
      
      # Relative price change of the DJIA
      index_r<-(index_next/index_now)
      
      # Trading algorithm
      if(value>0) { # search volume has gone up
        # long position  < ---- short (divisione) for Normal strategy
        r[i]<-(r[i-1]*(index_r)) 
        Action[i]<-"BUY"
      }
      if(value<0) { # search volume has gone down
        # short position
        r[i]<-(r[i-1]/(index_r))
        Action[i]<-"SELL"
      }
    }
  }
}

# Print result for this heyword
R<-(100*(r[nrow(dat)]-1))

RRow <- data.frame(Keyword=iKeywords, R=R, Action=Action[nrow(dat)-1]) 
#add new row with result for each keyword
allR <- rbind(allR, RRow)
allR$R

}     ################## loop ikeyword ##################

allR_ordered <- allR[order(allR$R, decreasing=TRUE), ]
#View(allR)
View(allR_ordered)
#cat(allR_ordered)

# Fitting Labels 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.

barplot(allR_ordered$R, 
        main=paste(FinancialSymbol,ideltat,sep=", "), 
        sub=mycomment,
        horiz=TRUE, 
        names.arg=allR_ordered$Keyword, cex.names=0.8)


#compare to price over the time " buy and hold" strat
RBuyandHold<-print(100*((dat$close[nrow(dat)-1])-(dat$close[2]))/dat$close[2])
#View(dat)

initDate
finDate
ideltat
FinancialSymbol

#if (sendCompletionNotif==TRUE) {
#  library(RPushbullet)
#  pbPost("note", "Wiki Trading Behavior work", "Completed!")
#  }

