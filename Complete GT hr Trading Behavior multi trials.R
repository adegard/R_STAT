library(ggplot2)
library(xts)
library(zoo)
library(quantmod) 
library(TTR)
library(bitops)
library(RCurl)
library(jsonlite)
library(gtrendsR)
library(reshape2)
library(tibble)
library(RPushbullet)

###########parameters##########
ticker<-"EURUSD"    #forex EURUSD... or google finance symbols
Googleterm<- "eurusd"  ##SINGLE TERM, BUT EXTRACT CORRELATED###skiped if dataFromFile<-TRUE
dataFromHotTrendsFeed<-FALSE ##if FALSE use single Googleterm or File
FeedUrl<-"https://trends.google.com/trends/hottrends/atom/feed?pn=p1"
FeedATOMJSON<-"ATOM" #TYPE OF URL: ATOM or JSON
# world JSON: "http://hawttrends.appspot.com/api/terms/"
# french ATOM "https://trends.google.it/trends/hottrends/atom/feed?pn=p16"
# world ATOM:  "https://trends.google.com/trends/hottrends/atom/feed?pn=p1"
myPeriod <-  "now 7-d"
timeframe<-60 #minuti
StockPeriod<-"7d" #last x days
SellOnIncrease<-FALSE #trading strategy
mycomment<-"correlated terms R"
ideltat<-1 # number of previous time of search volume to compare to the mean
firstxresults<-20  #number of most correlated terms
dataFromFile<-TRUE #if TRUE: put data in myfilename
myfileName<-"fileKeywordDiscovery_bloomberg_eurusd.csv"
sendCompletionNotif<-FALSE
#############################
#PX1: CAC40 non coerente con wiki
#DAX: GER30 non funziona
#IB: SPA35
#EURUSD:  asia (2,69 id 2) group (2,09), indeed job(1,98), stock market news (1,85), forex valuta(1,5), business (1,57), us national debt 1,23
#GBPCHF:  european (2,4 dt2), markets (3), gbpchf chart (2,93), fod economie (2, dt2)
#GBPUSD:  robots (2,35)
#GBPJPY:  european (2,5)

mylist<-NULL

if (dataFromFile==TRUE) {
#######EXTRACTION TERMS FROM FILE CSV#############
thePath = "C:\\Users\\degar_000\\Google Drive\\Trading\\R\\Tobias Preis Gtrend study\\"
mylist = read.csv(paste(thePath,myfileName,sep=""))
mylist<-as.vector(mylist$Page)

} else {
  
  if (dataFromHotTrendsFeed==TRUE) {
    
    if (FeedATOMJSON=="ATOM") {
    #######EXTRACTION GOOGLE HOT TRENDS TERMS (ATOM)##########
    google_terms<-NULL
    library(feedeR)
    google_terms <- feed.extract(FeedUrl)
    str(google_terms)
    mylist<-as.vector(google_terms$items$title)
    } else {   #"JSON"
      #######EXTRACTION GOOGLE HOT TRENDS TERMS (JSON)##########
      rawData <- getURL(FeedUrl)
      parsedData <- fromJSON(rawData)
      str(parsedData$"1")
      mylist<-as.vector(parsedData$"1")
    }

  } else {
#######EXTRACTION GOOGLE CORRELATED TERMS##########
google_terms<-NULL
google_terms <- gtrends(Googleterm,time =  myPeriod)
mylist<-as.vector(google_terms$related_queries$value)

  }
}

mylist<-mylist[1:firstxresults]

Action<-NULL
allR<-NULL
allR_ordered<-NULL
##### Import intraday Stock Data from google ################################
FinacialTable<-NULL
intra2<-NULL
source("C:\\Users\\degar_000\\Google Drive\\Trading\\R\\API GOOGLE\\intraday-data.R")

intra2 <- f.get.google.intraday(ticker, 60 * timeframe, StockPeriod)
#ploting
#candleChart(intra2, multi.col = TRUE, theme = 'white')

intra2<-as.data.frame(intra2) #convert matrix to data frame
intra2<-rownames_to_column(intra2, var = "Date")

head(intra2)

#time convertion:########
intra2$offset <- rep(2,nrow(intra2)) # make new column with offset

localTimesFin <- as.POSIXct(intra2$Date, 
                            format="%Y-%m-%d %H:%M:%S", tz="GMT")

intra2$Date <- localTimesFin + intra2$offset*3600

head(intra2)

#plot stock price over the time
ggplot(data=intra2, aes(x=Date, y=Close, group=1)) + geom_line()


######Extraction of GT for multiple keywords and single Deltat##########

iKeywords<-NULL
for (iKeywords in mylist){ #instead of theKeywords
  
  print(iKeywords)
  
########EXTRACTION GOOGLE VIEWS###########
google_trend<-NULL
google_trend <- gtrends(iKeywords,time =  myPeriod)

GoogleTable<-data.frame(Date=google_trend$interest_over_time$date,
                     Views=google_trend$interest_over_time$hits)

#time convertion:########
GoogleTable$offset <- rep(8,nrow(GoogleTable)) # make new column with offset
localTimes <- as.POSIXct(GoogleTable$Date, 
                         format="%Y-%m-%d %H:%M:%S", tz="GMT")
GoogleTable$Date <- localTimes + GoogleTable$offset*3600
  
#merging all data in single file for strategy test
dat <- merge(GoogleTable, intra2,by="Date")

#rimoving NAs
dat<-dat[!rowSums((is.na(dat))),]

#View(dat)

#################Strategy Trading Backtest###############
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
      index_now<-dat$Close[i]
      #print(index_next)
      
      # DJIA closing price on the first trading day of the week 
      # after the coming week
      index_next<-dat$Close[i+1]
      
      # Relative price change of the DJIA
      index_r<-(index_next/index_now)
      
      # Trading algorithm
      if (SellOnIncrease==TRUE)    { 
        if(value>0) { # search volume has gone up
          r[i]<-(r[i-1]/(index_r))
          Action[i]<-"SELL"
        }
        if(value<0) { # search volume has gone down
          r[i]<-(r[i-1]*(index_r)) 
          Action[i]<-"BUY" 
        }  
      } else {     
        
        if(value>0) { # search volume has gone up
          r[i]<-(r[i-1]*(index_r))
          Action[i]<-"BUY"
        }
        if(value<0) { # search volume has gone down
          r[i]<-(r[i-1]/(index_r)) 
          Action[i]<-"SELL" 
        }  
      }  ####end fo "else"###
    }   ####end of "if(i<nrow(dat))"#####
  }  ####end of "if(i>ideltat)"####
}   #end of "for(i in 1:nrow(dat))"#####

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

# Fitting Labels 
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.

barplot(allR_ordered$R, 
        main=paste(ticker,ideltat,sep=", "), 
        sub=mycomment,
        horiz=TRUE, 
        names.arg=allR_ordered$Keyword, cex.names=0.8)

################## graph ottimized deltat ##################
#library(beepr)
#beep(sound=8)
#compare to price over the time " buy and hold" strat
RBuyandHold<-print(100*((dat$Close[nrow(dat)])-(dat$Close[2]))/dat$Close[2])

dat$Date[nrow(dat)]
ticker
ideltat
RBuyandHold
if (sendCompletionNotif==TRUE) {
pbPost("note", "GT Trading Behavior work", "Completed!")
  }
