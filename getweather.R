library(data.table)
library(ggplot2)
dateFrom<-as.Date("2015-09-01")
dateTo<-as.Date("2015-11-17")
dates<-format(seq.Date(dateFrom, dateTo, by=1),"%Y/%m/%d")
urlstart<-"http://www.wunderground.com/history/airport/SBAF/"
urlend<-"/DailyHistory.html?req_city=Afonsos&req_statename=Brazil&reqdb.zip=&reqdb.magic=&reqdb.wmo=&format=1"

weatherDT<-rbindlist(lapply(paste(urlstart,dates,urlend,sep=""), FUN=function(x)
  read.table(x,sep=",",col.names = 1:14, skip=2, header=F, fill=TRUE, stringsAsFactors=FALSE)))

headerDT<-rbindlist(lapply(paste(urlstart,dates[1],urlend,sep=""), FUN=function(x)
    read.table(x,sep=",",  header=T, fill=TRUE, stringsAsFactors=FALSE)))
setnames(weatherDT, names(headerDT))

qplot(DateUTC.br...,TemperatureC,data=weatherDT[abs(TemperatureC)<100])