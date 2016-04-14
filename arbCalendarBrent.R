#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Экспорт истории по инструменту из  архивов сделок по фьючерсам и 
#' опционам с сайта Московской биржи
#' 2016-03-20 | rlukerin@gmail.com
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#' Месяц	Код фьючерса
#' Январь	F
#' Февраль	G
#' Март	H
#' Апрель	J
#' Май	K
#' Июнь	M
#' Июль	N
#' Август	Q
#' Сентябрь	U
#' Октябрь	V
#' Ноябрь	X
#' Декабрь	Z
#' 

library(data.table)
options(digits.secs=3)

homeDir<-"~/repos/Data/MOEX/"
#setwd(homeDir)
#dirList<-dir()
#setwd(dirList[14])
#fileList<-dir()
getSymbData<-function(symbol, fname){
  futFname<-gsub(".ZIP","ft.csv",gsub("FT","",toupper(fname)))
  unzip(fname, files=futFname)
  secDT<-fread(futFname, stringsAsFactors = FALSE)
  file.remove(futFname)
  secDT[code %in%symbol][, dat_time:=as.POSIXct(strptime(dat_time,format="%Y-%m-%d %H:%M:%OS"))][]
}

symbList<-c("GZM5", "GZH5")
symbMonth<-c("F", "G", "H","J","K","M","N","Q","U","V","X","Z")
symb<-"BR"

years<-2016:2016
from<-as.Date("2016-03-20")
to<-as.Date("2016-04-15")
fileFilter<-paste("FT",format(seq.Date(from=from, to=to, by=1),"%y%m%d"),".zip",sep="")

symbList<-unlist(lapply(years-2010, FUN=function(x)paste(symb,symbMonth,x, sep="")))
#symbList<-c("MXH6","MMH6","MXM6", "MMM6")

symbDT<-rbindlist(lapply(years,FUN=function(y){
  setwd(paste(homeDir,y,sep=""))
  fileList<-dir()[dir() %in% fileFilter]
  rbindlist(lapply(fileList,FUN=function(f)getSymbData(symbList,f)))}))


symbPairs<-lapply(1:length(symbList)-1, FUN=function(x) symbList[x:(x+1)])


getSpread<-function(symbPair, coef=1)
{
  spreadDT<-symbDT[,median(price), by=.(code, format(dat_time,"%Y%m%d%H%M%S"))]
  spreadDT<-spreadDT[,.(.N,code, V1),by=format][N>1]
  spreadDT<-merge(spreadDT[code==symbPair[1]],spreadDT[code==symbPair[2]],by="format")
  spreadDT[,price:=V1.x-V1.y*coef]
  spreadDT[,code:=paste(code.x,code.y,sep="")]
  spreadDT[,datetime:=as.POSIXct(strptime(format,format="%Y%m%d%H%M%S"))]
  spreadDT[,.(datetime,code, price)]
  
}


allSpreadDT<-rbindlist(lapply(symbPairs, getSpread,1))

ggplot()+
  #geom_point(data=DT[X3>= -260],aes(x=X1, y=X3), color="darkgreen")+
  #geom_point(data=DT[X4<= -25],aes(x=X1, y=X4), color="darkred")+ scale_y_continuous(breaks=seq(-400,0,10))
  geom_density(data=allSpreadDT,aes(price, color=code))


library(ggplot2)
ggplot(data=allSpreadDT, aes(x=datetime, y=price,color=code))+geom_line()+stat_smooth()+ scale_y_continuous(breaks=seq(-2,2,0.1))


ret<-allSpreadDT[,quantile(price)[3]-quantile(price)[2],by=code]
ret[,id:=1:.N]
qplot(id,fill=code,weight=cumsum(V1),data=ret, geom="bar")


ggplot(data=allSpreadDT[code %in% c("BRM6BRN6","BRK6BRM6") ], aes(x=datetime, y=price,color=code))+geom_line()+stat_smooth() +scale_y_continuous(breaks=seq(-2,2,0.1))

#ggplot(data=allSpreadDT[code %in% c("BRG6BRH6")& format(datetime,"%Y%m%d")=="20160201"], aes(x=datetime, y=price,color=code))+geom_line()+stat_smooth()+ scale_y_continuous(breaks=seq(-2,2,0.1))
#ggplot(data=allSpreadDT[code %in% c("BRH6BRJ6") & format(datetime,"%Y%m%d")=="20160301"], aes(x=datetime, y=price,color=code))+geom_line()+stat_smooth()+ scale_y_continuous(breaks=seq(-2,2,0.1))


#ggplot(data=allSpreadDT[code %in% c("MXH6MMH6","MXM6MMM6") & format(datetime,"%Y%m%d")=="20160315"], aes(x=datetime, y=price,color=code))+
#  geom_point()+stat_smooth()#+ scale_y_continuous(breaks=seq(-2,2,0.1))

