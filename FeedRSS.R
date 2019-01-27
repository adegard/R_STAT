#http://www.exegetic.biz/blog/2016/08/feeder-reading-rss-atom-feeds-r/

#devtools::install_github("DataWookie/feedeR")

library(feedeR)


myfeed <- feed.extract("https://trends.google.com/trends/hottrends/atom/feed?pn=p1")
names(myfeed)


str(myfeed)

mylist<-as.vector(myfeed$items$title)
col.names(mylist)<-"HotTerms"
View(mylist)
