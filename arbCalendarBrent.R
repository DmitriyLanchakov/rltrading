#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#'  Календарный арбитраж по фьючерсу Брент. 
#' 
#' 
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
library(rusquant)
options(digits.secs=3)

symbMonth<-c("F", "G", "H","J","K","M","N","Q","U","V","X","Z")
symb<-"BR"

year<-2016
month<-1:12
period<-"1min"
from<-as.Date("2016-01-01")
to<-Sys.Date()

symbList<-unlist(lapply(month, FUN=function(x)paste(symb,symbMonth[x],year-2010," (",formatC(x,width = 2,flag = "0"),".",year,")", sep="")))
year<-2017
month<-1:3
symbList<-c(symbList,unlist(lapply(month, FUN=function(x)paste(symb,symbMonth[x],year-2010," (",formatC(x,width = 2,flag = "0"),".",year,")", sep=""))))


#symbList<-c("MXH6","MMH6","MXM6", "MMM6")




for(s in symbList)
{
    #start<-as.Date(paste(year,m-1, "01", sep="-"))
    getSymbols(s, from=from,to=to, period=period, src='mfd',adjust=TRUE,auto.assign=T)
    
}



symbDT<-(lapply(symbList[-1],FUN = function(x) data.table(datetime=index(get(x)), symb=x,get(x))))

symbDT<-rbindlist(symbDT)


ggplot(symbDT)+
    geom_line(aes(x=datetime,y=Close, colour=symb))


dd<-dcast(symbDT, datetime~symb, value.var = "Close")
spread<-t(diff(t(dd[,.SD, .SDcols=symbList[-1]])))
spread<-data.table(spread)
dd<-cbind(dd[,datetime], spread)

dd<-melt(dd, id="V1")
setnames(dd, c("datetime", "Spread", "Value"))

ggplot(dd)+
    geom_line(aes(x=datetime,y=Value, colour=Spread))+facet_grid(Spread~.)

ggplot(dd)+
    geom_histogram(aes(Value, fill=Spread))+facet_grid(Spread~.)

dd[, expDate:=.SD[!is.na(Value)][.N][,datetime],by=Spread]
dd[,daysToExp:=as.numeric(difftime(expDate,datetime,units="days"))]

ggplot(dd[daysToExp>=0 & daysToExp<=60][Spread %in% symbList[13:13]])+
    geom_line(aes(x=-daysToExp,y=Value, colour=Spread))+facet_grid(Spread~.)



dd[, .(mean(Value, na.rm = T), sd(Value, na.rm = T),mean(Value, na.rm = T)+sd(Value, na.rm = T),mean(Value, na.rm = T)-sd(Value, na.rm = T)), by=Spread]
# 
# getSpread<-function(symbPair, coef=1)
# {
#   spreadDT<-symbDT[,median(price), by=.(code, format(dat_time,"%Y%m%d%H%M%S"))]
#   spreadDT<-spreadDT[,.(.N,code, V1),by=format][N>1]
#   spreadDT<-merge(spreadDT[code==symbPair[1]],spreadDT[code==symbPair[2]],by="format")
#   spreadDT[,price:=V1.x-V1.y*coef]
#   spreadDT[,code:=paste(code.x,code.y,sep="")]
#   spreadDT[,datetime:=as.POSIXct(strptime(format,format="%Y%m%d%H%M%S"))]
#   spreadDT[,.(datetime,code, price)]
#   
# }
# 
# 
# allSpreadDT<-rbindlist(lapply(symbPairs, getSpread,1))
# 
# ggplot()+
#   #geom_point(data=DT[X3>= -260],aes(x=X1, y=X3), color="darkgreen")+
#   #geom_point(data=DT[X4<= -25],aes(x=X1, y=X4), color="darkred")+ scale_y_continuous(breaks=seq(-400,0,10))
#   geom_density(data=allSpreadDT,aes(price, color=code))
# 
# 
# library(ggplot2)
# ggplot(data=allSpreadDT, aes(x=datetime, y=price,color=code))+geom_line()+stat_smooth()+ scale_y_continuous(breaks=seq(-2,2,0.1))
# 
# 
# ret<-allSpreadDT[,quantile(price)[3]-quantile(price)[2],by=code]
# ret[,id:=1:.N]
# qplot(id,fill=code,weight=cumsum(V1),data=ret, geom="bar")
# 
# 
# ggplot(data=allSpreadDT[code %in% c("BRM6BRN6","BRK6BRM6") ], aes(x=datetime, y=price,color=code))+geom_line()+stat_smooth() +scale_y_continuous(breaks=seq(-2,2,0.1))
# 
# #ggplot(data=allSpreadDT[code %in% c("BRG6BRH6")& format(datetime,"%Y%m%d")=="20160201"], aes(x=datetime, y=price,color=code))+geom_line()+stat_smooth()+ scale_y_continuous(breaks=seq(-2,2,0.1))
# #ggplot(data=allSpreadDT[code %in% c("BRH6BRJ6") & format(datetime,"%Y%m%d")=="20160301"], aes(x=datetime, y=price,color=code))+geom_line()+stat_smooth()+ scale_y_continuous(breaks=seq(-2,2,0.1))
# 
# 
# #ggplot(data=allSpreadDT[code %in% c("MXH6MMH6","MXM6MMM6") & format(datetime,"%Y%m%d")=="20160315"], aes(x=datetime, y=price,color=code))+
# #  geom_point()+stat_smooth()#+ scale_y_continuous(breaks=seq(-2,2,0.1))
# 
