#read from JSON

## R SCRIPT:

library(RCurl)
library(bitops)
library(RJSONIO)
library(jsonlite)

theURL <- "https://wikimedia.org/api/rest_v1/metrics/pageviews/top/en.wikipedia/all-access/2017/08/25"
rawData <- getURL(theURL)


parsedData <- fromJSON(rawData)

#str(parsedData)
viewsData <- data.frame(parsedData$items$articles)

print(viewsData)
df<-viewsData$article
mylist<-df[1:10]
mylist


## END OF SCRIPT