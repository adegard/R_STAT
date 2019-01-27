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

iKeywords<-"badante"  
ProjectLanguage<-"it.wikipedia.org"
thePath = "C:\\Users\\degar_000\\Google Drive\\Trading\\R\\Tobias Preis Gtrend study\\Wiki_keyword_hits\\"

initDateW<- paste(sub(pattern="-", replacement="", sub(pattern="-", replacement="", initDate)),"00", sep="")
finDateW<- paste(sub(pattern="-", replacement="", sub(pattern="-", replacement="", finDate)),"00", sep="")

  myData2<-article_pageviews(project = ProjectLanguage,
                             article = iKeywords, platform = "all", 
                             user_type = "all", start = initDateW, 
                             end = finDateW, 
                             reformat = TRUE)
  
  myData2$date<-as.Date(myData2$date) #say to R that Date column is date
  StartDate<-myData2$date
  Views<-myData2$views
  tabella<-data.frame(StartDate,Views)
  
  #saving data in file csv
  write.table(tabella, file=paste(thePath,iKeywords,".csv",sep=""), 
              row.names=FALSE, col.names=FALSE, sep=",")
  

